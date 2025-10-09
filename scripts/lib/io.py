from __future__ import annotations

from pathlib import Path
from typing import Mapping, Optional

import pandas as pd


def ensure_directory(path: Path | str) -> Path:
    path_obj = Path(path)
    path_obj.mkdir(parents=True, exist_ok=True)
    return path_obj


def write_dataframe_outputs(
    df: pd.DataFrame,
    csv_path: Optional[Path | str] = None,
    parquet_path: Optional[Path | str] = None,
    index: bool = False,
) -> None:
    if csv_path is not None:
        csv_path = Path(csv_path)
        ensure_directory(csv_path.parent)
        df.to_csv(csv_path, index=index)
    if parquet_path is not None:
        parquet_path = Path(parquet_path)
        ensure_directory(parquet_path.parent)
        df.to_parquet(parquet_path, index=index)
