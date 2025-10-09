from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Optional


def _normalize_key(value: str) -> str:
    return "".join(ch for ch in value.lower() if ch.isalpha())


@dataclass
class TaxonomyMapper:
    genus_aliases: Dict[str, str]
    species_aliases: Dict[str, str]
    unknown_genus: str = "Unidentifiable"
    unknown_species: str = "spp."

    @classmethod
    def from_config(cls, taxonomy_section: Dict[str, Dict[str, Dict[str, str]]]) -> "TaxonomyMapper":
        genus_aliases_section = taxonomy_section.get("genus", {}).get("aliases", {})
        species_aliases_section = taxonomy_section.get("species", {}).get("aliases", {})

        genus_aliases = {_normalize_key(k): v for k, v in genus_aliases_section.items()}
        species_aliases = {_normalize_key(k): v for k, v in species_aliases_section.items()}

        canonical = taxonomy_section.get("canonical", {})
        return cls(
            genus_aliases=genus_aliases,
            species_aliases=species_aliases,
            unknown_genus=canonical.get("unknown_genus_placeholder", "Unidentifiable"),
            unknown_species=canonical.get("unknown_species_placeholder", "spp."),
        )

    def canonical_genus(self, value: Optional[str]) -> Optional[str]:
        if value is None:
            return None
        stripped = value.strip()
        if not stripped:
            return None
        key = _normalize_key(stripped)
        if key in self.genus_aliases:
            return self.genus_aliases[key]
        return stripped.title()

    def canonical_species(self, value: Optional[str]) -> Optional[str]:
        if value is None:
            return None
        stripped = value.strip()
        if not stripped:
            return None
        key = _normalize_key(stripped)
        if key in self.species_aliases:
            return self.species_aliases[key]
        candidate = stripped.lower()
        if len(candidate) <= 3 and candidate not in {"sp", "spp"}:
            return self.unknown_species
        return candidate

    def abbreviate_genus(self, genus: Optional[str]) -> Optional[str]:
        canonical = self.canonical_genus(genus)
        if canonical is None:
            return None
        return canonical[:3].title()
