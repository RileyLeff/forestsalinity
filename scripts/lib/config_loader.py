from __future__ import annotations

from pathlib import Path
from typing import Any, Dict

try:  # Python 3.11+
    import tomllib  # type: ignore[attr-defined]
except ModuleNotFoundError:  # pragma: no cover - fallback for <3.11
    import tomli as tomllib  # type: ignore


def _resolve_path(path_str: str, base_dir: Path) -> Path:
    path = Path(path_str)
    if path.is_absolute():
        return path
    return (base_dir / path).resolve()


def _load_toml(path: Path) -> Dict[str, Any]:
    with path.open("rb") as handle:
        return tomllib.load(handle)


def load_full_config(config_path: Path | None = None) -> Dict[str, Any]:
    """Load the root config.toml along with referenced sub-configs."""
    if config_path is None:
        config_path = Path(__file__).resolve().parents[1] / "config" / "config.toml"

    config_path = config_path.resolve()
    repo_root = config_path.parents[2]

    config = _load_toml(config_path)
    config["paths"] = {
        "repo_root": repo_root,
        "config": config_path,
    }

    taxonomy_info = config.get("taxonomy", {})
    taxonomy_path = _resolve_path(taxonomy_info.get("file", "taxonomy.toml"), repo_root)
    config.setdefault("taxonomy", {})["path"] = taxonomy_path
    config["taxonomy"]["data"] = _load_toml(taxonomy_path)

    surveys_info = config.get("surveys", {})
    surveys_path = _resolve_path(surveys_info.get("file", "surveys.toml"), repo_root)
    config.setdefault("surveys", {})["path"] = surveys_path
    config["surveys"]["data"] = _load_toml(surveys_path)

    return config
