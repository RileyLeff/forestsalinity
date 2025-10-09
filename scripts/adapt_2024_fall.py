# /// script
# dependencies = [
#   "pandas",
#   "pyarrow",
# ]
# ///

from __future__ import annotations

import argparse
from pathlib import Path
import sys
repo_root = Path(__file__).resolve().parent.parent
if str(repo_root) not in sys.path:
    sys.path.insert(0, str(repo_root))
from typing import Dict, Optional

import pandas as pd

from scripts.lib import (
    TaxonomyMapper,
    canonicalize_columns,
    coerce_boolean,
    compute_tree_code,
    ensure_canonical_order,
    load_full_config,
    write_dataframe_outputs,
)


COLUMN_RENAMES = {
    "dbh": "dbh_mm",
    "stem_number": "trunk_number",
    "oldtag": "old_tag",
}


def _dataset_config(config: Dict[str, Dict], survey_id: str) -> Dict[str, object]:
    for entry in config["surveys"]["data"].get("datasets", []):
        if entry["survey_id"] == survey_id:
            return entry
    raise KeyError(f"Survey configuration not found for {survey_id}")


def _prefer_new_value(primary: object, fallback: object) -> object:
    if isinstance(primary, str) and primary.strip():
        return primary
    if pd.notna(primary):
        return primary
    return fallback


def _choose_taxon(row: pd.Series) -> pd.Series:
    row["genus"] = _prefer_new_value(row.get("new_genus"), row.get("genus"))
    row["spp"] = _prefer_new_value(row.get("new_species"), row.get("spp"))
    return row


def transform(config: Optional[Dict[str, Dict]] = None, save: bool = True) -> pd.DataFrame:
    config = load_full_config() if config is None else config
    survey_cfg = _dataset_config(config, "October_2024")
    csv_path = Path(survey_cfg["source_csv"])

    df = pd.read_csv(csv_path)

    taxonomy = TaxonomyMapper.from_config(config["taxonomy"]["data"])

    df = df.apply(_choose_taxon, axis=1)
    df = canonicalize_columns(df, COLUMN_RENAMES)

    df["zone"] = df["zone"].astype(str).str.strip().str.upper()
    df["plot_numeric"] = pd.to_numeric(df["plot"], errors="coerce").astype("Int64")
    df["plot"] = df.apply(
        lambda row: f"{row['zone']}{row['plot_numeric']}"
        if pd.notna(row["plot_numeric"]) and pd.notna(row["zone"])
        else None,
        axis=1,
    )

    df["tree_number"] = pd.to_numeric(df["tree_number"], errors="coerce").astype("Int64")
    df["trunk_number"] = pd.to_numeric(df["trunk_number"], errors="coerce").astype("Int64")

    df["genus"] = df["genus"].apply(taxonomy.canonical_genus)
    df["spp"] = df["spp"].apply(taxonomy.canonical_species)

    df["dbh_mm"] = pd.to_numeric(df["dbh_mm"], errors="coerce")
    df["health"] = pd.to_numeric(df["health"], errors="coerce")

    df["alive"] = coerce_boolean(df.get("alive"))
    df["standing"] = coerce_boolean(df.get("standing"))

    df["site"] = survey_cfg.get("site", "BRNV")
    df["survey_id"] = survey_cfg["survey_id"]
    df["date"] = survey_cfg.get("default_date")

    df["tree_code"] = df.apply(
        lambda row: compute_tree_code(
            row["plot"],
            row["tree_number"],
            taxonomy.abbreviate_genus(row["genus"]),
        ),
        axis=1,
    )

    df["old_tag"] = pd.to_numeric(df.get("old_tag"), errors="coerce")
    df["notes"] = df.get("notes")

    canonical_cols = config["columns"]["canonical"]
    combined = ensure_canonical_order(df, canonical_cols)

    if save:
        standardized_dir = Path(config["output"]["standardized_dir"]) / survey_cfg["survey_id"]
        write_dataframe_outputs(
            combined,
            csv_path=standardized_dir / "tree_census.csv",
            parquet_path=standardized_dir / "tree_census.parquet",
        )

    return combined


def main() -> None:
    parser = argparse.ArgumentParser(description="Standardize the October 2024 census data.")
    parser.add_argument("--no-save", action="store_true", help="Skip writing output files")
    args = parser.parse_args()

    df = transform(save=not args.no_save)
    print(df.head())
    print(f"Rows processed: {len(df)}")


if __name__ == "__main__":
    main()
