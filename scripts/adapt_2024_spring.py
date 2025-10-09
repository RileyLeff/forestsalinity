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
    coerce_boolean,
    compute_tree_code,
    ensure_canonical_order,
    load_full_config,
    write_dataframe_outputs,
)


def _dataset_config(config: Dict[str, Dict], survey_id: str) -> Dict[str, object]:
    for entry in config["surveys"]["data"].get("datasets", []):
        if entry["survey_id"] == survey_id:
            return entry
    raise KeyError(f"Survey configuration not found for {survey_id}")


def transform(config: Optional[Dict[str, Dict]] = None, save: bool = True) -> pd.DataFrame:
    config = load_full_config() if config is None else config
    survey_cfg = _dataset_config(config, "May_2024")
    csv_path = Path(survey_cfg["source_csv"])

    df = pd.read_csv(csv_path, dtype={"plot": "float"})

    taxonomy = TaxonomyMapper.from_config(config["taxonomy"]["data"])

    df = df.rename(columns={col: col.strip() for col in df.columns})

    df["date"] = pd.to_datetime(df["date"], errors="coerce").dt.date
    df["date"] = df["date"].astype(str).replace({"NaT": None})

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
    df["old_tag"] = pd.to_numeric(df.get("old_tag"), errors="coerce")

    df["alive"] = coerce_boolean(df["alive"])
    df["standing"] = coerce_boolean(df["standing"])

    df["site"] = survey_cfg.get("site", "BRNV")
    df["survey_id"] = survey_cfg["survey_id"]

    df["tree_code"] = df.apply(
        lambda row: compute_tree_code(
            row["plot"],
            row["tree_number"],
            taxonomy.abbreviate_genus(row["genus"]),
        ),
        axis=1,
    )

    df = df.assign(notes=df.get("notes"))

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
    parser = argparse.ArgumentParser(description="Standardize the May 2024 census data.")
    parser.add_argument("--no-save", action="store_true", help="Skip writing output files")
    args = parser.parse_args()

    df = transform(save=not args.no_save)
    print(df.head())
    print(f"Rows processed: {len(df)}")


if __name__ == "__main__":
    main()
