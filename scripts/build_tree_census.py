# /// script
# dependencies = [
#   "pandas",
#   "pyarrow",
#   "pyreadr",
# ]
# ///

from __future__ import annotations

import argparse
import importlib
import subprocess
import sys
import tempfile
from pathlib import Path
repo_root = Path(__file__).resolve().parent.parent
if str(repo_root) not in sys.path:
    sys.path.insert(0, str(repo_root))
from typing import Dict, Iterable, List, Optional

import pandas as pd
import pyreadr

from scripts.lib import (
    coerce_boolean,
    ensure_canonical_order,
    load_full_config,
    write_dataframe_outputs,
)

CANONICAL_KEY_COLUMNS = ["survey_id", "tree_code", "trunk_number"]


def _write_rds_via_rscript(csv_path: Path, rds_path: Path) -> None:
    script_path = repo_root / "scripts" / "r" / "save_tree_census.R"
    cmd = [
        "Rscript",
        str(script_path),
        str(csv_path),
        str(rds_path),
    ]
    subprocess.run(cmd, check=True)


def _dataset_configurations(config: Dict[str, Dict]) -> List[Dict[str, object]]:
    return list(config["surveys"]["data"].get("datasets", []))


def _format_numeric(value: object) -> str:
    if pd.isna(value):
        return ""
    if isinstance(value, float) and value.is_integer():
        return str(int(value))
    return str(value).strip()


def _build_key(df: pd.DataFrame) -> pd.Series:
    parts = []
    for col in CANONICAL_KEY_COLUMNS:
        if col not in df.columns:
            raise KeyError(f"Dataframe missing key column '{col}'")
        parts.append(df[col].map(_format_numeric))
    return parts[0] + "::" + parts[1] + "::" + parts[2]


def _normalize_booleans(df: pd.DataFrame, column: str) -> pd.DataFrame:
    if column in df.columns:
        df[column] = coerce_boolean(df[column])
    return df


def _prepare_dataframe(df: pd.DataFrame, canonical_columns: Iterable[str]) -> pd.DataFrame:
    df = df.copy()
    df["survey_id"] = df["survey_id"].astype("string")
    df["date"] = pd.to_datetime(df["date"], errors="coerce").dt.date.astype(str).replace({"NaT": None})
    df["zone"] = df["zone"].astype(str).str.strip().str.upper()
    df["plot"] = df["plot"].astype(str).str.strip()
    df["tree_code"] = df["tree_code"].astype("string")
    df["tree_number"] = pd.to_numeric(df["tree_number"], errors="coerce").astype("Int64")
    df["trunk_number"] = pd.to_numeric(df["trunk_number"], errors="coerce").astype("Int64")
    df["dbh_mm"] = pd.to_numeric(df["dbh_mm"], errors="coerce")
    df["health"] = pd.to_numeric(df["health"], errors="coerce")
    df = _normalize_booleans(df, "alive")
    df = _normalize_booleans(df, "standing")
    df["old_tag"] = pd.to_numeric(df.get("old_tag"), errors="coerce")
    df["notes"] = df.get("notes")
    df["genus"] = df["genus"].astype(str)
    df["spp"] = df["spp"].astype(str)
    return ensure_canonical_order(df, canonical_columns)


def _load_existing_tree_census(path: Path, canonical_columns: Iterable[str]) -> pd.DataFrame:
    if not path.exists():
        return pd.DataFrame(columns=canonical_columns)
    try:
        result = pyreadr.read_r(path)
        if not result:
            return pd.DataFrame(columns=canonical_columns)
        df = next(iter(result.values()))
    except Exception as exc:
        print(f"pyreadr failed to read {path}: {exc}. Falling back to Rscript export.")
        with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as tmp:
            tmp_path = Path(tmp.name)
        try:
            cmd = [
                "Rscript",
                "-e",
                f"df<-readRDS('{path.as_posix()}'); write.csv(df, '{tmp_path.as_posix()}', row.names=FALSE)",
            ]
            subprocess.run(cmd, check=True, capture_output=True)
            df = pd.read_csv(tmp_path)
        finally:
            tmp_path.unlink(missing_ok=True)
    return _prepare_dataframe(df, canonical_columns)


def _record_changes(
    overlap_keys: pd.Index,
    existing_with_keys: pd.DataFrame,
    new_with_keys: pd.DataFrame,
    canonical_columns: Iterable[str],
) -> pd.DataFrame:
    records: List[Dict[str, object]] = []
    existing_lookup = existing_with_keys.set_index("__key__")
    new_lookup = new_with_keys.set_index("__key__")

    column_list = list(canonical_columns)

    for key in overlap_keys:
        old_rows = existing_lookup.loc[[key]].copy()
        new_rows_for_key = new_lookup.loc[[key]].copy()

        old_values = old_rows[column_list].reset_index(drop=True)
        new_values = new_rows_for_key[column_list].reset_index(drop=True)

        max_len = max(len(old_values), len(new_values))
        old_aligned = old_values.reindex(range(max_len))
        new_aligned = new_values.reindex(range(max_len))

        for column in canonical_columns:
            series_old = old_aligned[column]
            series_new = new_aligned[column]
            if series_old.equals(series_new):
                continue

            def _series_to_list(series: pd.Series) -> List[object]:
                return [
                    None if pd.isna(val) else val
                    for val in series.tolist()
                ]

            records.append(
                {
                    "key": key,
                    "survey_id": new_rows_for_key.iloc[0]["survey_id"],
                    "tree_code": new_rows_for_key.iloc[0]["tree_code"],
                    "trunk_number": new_rows_for_key.iloc[0]["trunk_number"],
                    "column": column,
                    "old": _series_to_list(series_old),
                    "new": _series_to_list(series_new),
                }
            )
    return pd.DataFrame(records)


def _write_summary(summary_path: Path, combined: pd.DataFrame, new_rows: pd.DataFrame) -> None:
    lines = ["# Tree Census Update Summary", ""]
    lines.append("## Records by Survey")
    survey_counts = combined.groupby("survey_id").size().reset_index(name="n")
    lines.append("```")
    lines.append(survey_counts.to_string(index=False))
    lines.append("```")

    lines.append("")
    lines.append("## Newly Added Records")
    new_counts = new_rows.groupby("survey_id").size().reset_index(name="n")
    lines.append("```")
    lines.append(new_counts.to_string(index=False))
    lines.append("```")

    lines.append("")
    lines.append("## Notes")
    lines.append("- Summary generated by scripts/build_tree_census.py")

    summary_path.parent.mkdir(parents=True, exist_ok=True)
    summary_path.write_text("\n".join(lines))


def _backup_file(path: Path, backup_path: Path) -> None:
    if path.exists():
        backup_path.write_bytes(path.read_bytes())


def run_adapters(
    config: Dict[str, Dict],
    canonical_columns: Iterable[str],
    save_standardized: bool = True,
) -> List[pd.DataFrame]:
    datasets = _dataset_configurations(config)
    adapted_frames: List[pd.DataFrame] = []
    for dataset in datasets:
        module_name = dataset["module"]
        module = importlib.import_module(module_name)
        if not hasattr(module, "transform"):
            raise AttributeError(f"Adapter module '{module_name}' does not expose a transform() function")
        print(f"Running adapter for {dataset['survey_id']} ({module_name})")
        adapted = module.transform(config=config, save=save_standardized)
        adapted_frames.append(_prepare_dataframe(adapted, canonical_columns))
    return adapted_frames


def main() -> None:
    parser = argparse.ArgumentParser(description="Build the combined tree census dataset.")
    parser.add_argument("--dry-run", action="store_true", help="Do not write combined outputs")
    parser.add_argument(
        "--skip-standardized",
        action="store_true",
        help="Do not re-run the individual adapters",
    )
    parser.add_argument(
        "--no-standardized-save",
        action="store_true",
        help="Run adapters but skip writing their per-survey outputs",
    )
    args = parser.parse_args()

    config = load_full_config()
    canonical_columns = config["columns"]["canonical"]

    if args.skip_standardized:
        new_frames: List[pd.DataFrame] = []
    else:
        new_frames = run_adapters(
            config,
            canonical_columns,
            save_standardized=not args.no_standardized_save,
        )

    if not new_frames:
        print("No new datasets were processed. Nothing to merge.")
        return

    new_combined = pd.concat(new_frames, ignore_index=True)
    new_combined["__key__"] = _build_key(new_combined)

    existing_path = Path(config["output"]["combined_rds"])
    existing = _load_existing_tree_census(existing_path, canonical_columns)
    if not existing.empty:
        existing["__key__"] = _build_key(existing)
    else:
        existing["__key__"] = pd.Series(dtype=str)

    existing_keys = set(existing["__key__"]) if not existing.empty else set()
    overlap_mask = new_combined["__key__"].isin(existing_keys)
    overlap_keys = new_combined.loc[overlap_mask, "__key__"].unique()

    new_rows = new_combined.loc[~overlap_mask].copy()

    changes = pd.DataFrame()
    if len(overlap_keys) > 0:
        changes = _record_changes(
            overlap_keys,
            existing,
            new_combined,
            canonical_columns,
        )
        if not changes.empty:
            print("Detected updates to existing records; see changes CSV for details.")

    combined = pd.concat([existing.drop(columns=["__key__"], errors="ignore"), new_rows.drop(columns=["__key__"], errors="ignore")], ignore_index=True)

    combined = combined.sort_values(["date", "plot", "tree_number", "trunk_number"], ignore_index=True)

    new_rows = new_rows.drop(columns=["__key__"], errors="ignore")

    summary_path = Path(config["output"]["summary_md"])
    _write_summary(summary_path, combined, new_rows)

    if args.dry_run:
        print("Dry run complete. Combined dataset not written.")
        print(f"New rows: {len(new_rows)}; potential changes: {len(changes)}")
        return

    output_csv = Path(config["output"]["combined_csv"])
    output_parquet = Path(config["output"]["combined_parquet"])
    write_dataframe_outputs(combined, csv_path=output_csv, parquet_path=output_parquet)

    new_rows_csv = Path(config["output"]["new_rows_csv"])
    write_dataframe_outputs(new_rows, csv_path=new_rows_csv)

    if not changes.empty:
        changes_csv = Path(config["output"]["changed_rows_csv"])
        write_dataframe_outputs(changes, csv_path=changes_csv)

    backup_path = Path(config["output"]["combined_rds_backup"])
    _backup_file(existing_path, backup_path)

    _write_rds_via_rscript(output_csv, existing_path)

    print(f"Wrote combined dataset with {len(combined)} rows.")
    print(f"New rows added: {len(new_rows)}")
    if not changes.empty:
        print(f"Updated rows: {len(changes['key'].unique())}")


if __name__ == "__main__":
    main()
