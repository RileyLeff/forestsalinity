from __future__ import annotations

from typing import Dict, Iterable, List, Optional

import pandas as pd


def canonicalize_columns(df: pd.DataFrame, rename_map: Dict[str, str]) -> pd.DataFrame:
    return df.rename(columns=rename_map)


def coerce_boolean(series: pd.Series) -> pd.Series:
    truthy = {True, 1, "1", "true", "TRUE", "True", "yes", "Y", "y"}
    falsy = {False, 0, "0", "false", "FALSE", "False", "no", "N", "n"}

    def _convert(value: object) -> object:
        if pd.isna(value):
            return pd.NA
        if value in truthy:
            return True
        if value in falsy:
            return False
        if isinstance(value, str):
            stripped = value.strip()
            if stripped.isdigit():
                return bool(int(stripped))
        if isinstance(value, (int, float)):
            return bool(value)
        return pd.NA

    return pd.Series([_convert(v) for v in series], dtype="boolean")


def _format_numeric(value: object) -> Optional[str]:
    if pd.isna(value):
        return None
    if isinstance(value, (int, str)):
        text = str(value).strip()
        if text == "":
            return None
        return text
    if isinstance(value, float):
        if value.is_integer():
            return str(int(value))
        return f"{value}"
    return str(value)


def compute_tree_code(plot_label: str, tree_number: object, genus_abbrev: Optional[str]) -> Optional[str]:
    if plot_label is None or genus_abbrev is None:
        return None
    tree_component = _format_numeric(tree_number)
    if tree_component is None:
        return None
    return f"{plot_label}_{tree_component}_{genus_abbrev}"


def ensure_canonical_order(df: pd.DataFrame, column_order: Iterable[str]) -> pd.DataFrame:
    columns: List[str] = list(column_order)
    missing = [col for col in columns if col not in df.columns]
    if missing:
        raise KeyError(f"Dataframe missing expected columns: {missing}")
    return df.loc[:, columns]
