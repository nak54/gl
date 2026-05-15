"""
Step 1 — Data ingest
====================

Loads ``mda_merged_sic_naics_24_25.parquet`` and returns a list of
``SECDocument`` objects.

Expected columns (per claude.md)::

    ['accession', 'year', 'mda_text', 'cik', 'name', 'sic', 'ein',
     'address', 'naics3', 'naics17Title', 'is_ambiguous_naics3']

The "document_id" is derived from ``accession`` so that all downstream
artefacts (tables, chunks, vectors) can be re-joined to the original
filing.
"""

from __future__ import annotations

import pandas as pd
from dataclasses import dataclass, field
from typing import Dict, List, Optional


# Columns the pipeline is willing to forward into metadata.  Any column
# present in the parquet that is not in this list is also forwarded, but
# these are the ones we *expect*.
EXPECTED_COLUMNS = [
    "accession", "year", "mda_text", "cik", "name", "sic", "ein",
    "address", "naics3", "naics17Title", "is_ambiguous_naics3",
]


@dataclass
class SECDocument:
    """One SEC filing (Item 7 MD&A) ready for the pipeline."""
    document_id: str          # derived from accession
    accession: str
    year: str
    cik: str
    name: str                 # company name
    sic: str
    ein: str
    address: str
    naics3: str
    naics17Title: str
    is_ambiguous_naics3: bool
    mda_text: str
    metadata: Dict[str, str] = field(default_factory=dict)


class SecDataIngest:
    """Reads a parquet file of SEC MD&A filings into ``SECDocument`` objects."""

    def __init__(self, parquet_path: str, limit: Optional[int] = None):
        """
        Args:
            parquet_path: Path to ``mda_merged_sic_naics_24_25.parquet``.
            limit: If given, only load the first ``limit`` rows (handy for
                Colab smoke tests).
        """
        self.parquet_path = parquet_path
        self.limit = limit
        self.df: Optional[pd.DataFrame] = None
        self.documents: List[SECDocument] = []

    # ------------------------------------------------------------------
    # Loading
    # ------------------------------------------------------------------
    def load_parquet(self) -> pd.DataFrame:
        print(f"[ingest] Loading parquet: {self.parquet_path}")
        self.df = pd.read_parquet(self.parquet_path)
        if self.limit is not None:
            self.df = self.df.head(self.limit).copy()
        print(f"[ingest] Loaded {len(self.df):,} rows")
        print(f"[ingest] Columns: {self.df.columns.tolist()}")
        return self.df

    def validate(self) -> bool:
        if self.df is None:
            raise RuntimeError("Call load_parquet() first")

        required = ["mda_text", "accession", "cik"]
        missing = [c for c in required if c not in self.df.columns]
        if missing:
            raise ValueError(f"Missing required columns: {missing}")

        null_text = self.df["mda_text"].isna().sum()
        if null_text:
            print(f"[ingest] Warning: {null_text} rows have null mda_text")
        return True

    # ------------------------------------------------------------------
    # Conversion
    # ------------------------------------------------------------------
    @staticmethod
    def _to_str(value) -> str:
        if value is None:
            return ""
        try:
            if pd.isna(value):
                return ""
        except (TypeError, ValueError):
            pass
        return str(value)

    def extract_documents(self) -> List[SECDocument]:
        if self.df is None:
            raise RuntimeError("Call load_parquet() first")

        docs: List[SECDocument] = []
        skipped = 0
        for idx, row in self.df.iterrows():
            mda_text = self._to_str(row.get("mda_text", ""))
            if not mda_text or mda_text.lower() == "nan":
                skipped += 1
                continue

            accession = self._to_str(row.get("accession", f"doc_{idx}"))
            doc_id = accession or f"doc_{idx}"

            # Build metadata dict for *every* column on the row so that
            # downstream filters can pivot on naics17Title etc.
            metadata: Dict[str, str] = {}
            for col in self.df.columns:
                if col == "mda_text":
                    continue
                metadata[col] = self._to_str(row.get(col))

            doc = SECDocument(
                document_id=doc_id,
                accession=accession,
                year=self._to_str(row.get("year")),
                cik=self._to_str(row.get("cik")),
                name=self._to_str(row.get("name")),
                sic=self._to_str(row.get("sic")),
                ein=self._to_str(row.get("ein")),
                address=self._to_str(row.get("address")),
                naics3=self._to_str(row.get("naics3")),
                naics17Title=self._to_str(row.get("naics17Title")),
                is_ambiguous_naics3=bool(row.get("is_ambiguous_naics3", False)),
                mda_text=mda_text,
                metadata=metadata,
            )
            docs.append(doc)

        if skipped:
            print(f"[ingest] Skipped {skipped} rows with empty mda_text")
        print(f"[ingest] Built {len(docs):,} SECDocument objects")
        self.documents = docs
        return docs

    # ------------------------------------------------------------------
    # Stats / driver
    # ------------------------------------------------------------------
    def statistics(self) -> Dict:
        if not self.documents:
            return {}
        lengths = [len(d.mda_text) for d in self.documents]
        return {
            "documents": len(self.documents),
            "total_chars": sum(lengths),
            "avg_chars": sum(lengths) / len(lengths),
            "min_chars": min(lengths),
            "max_chars": max(lengths),
            "unique_companies": len({d.name for d in self.documents}),
            "unique_ciks": len({d.cik for d in self.documents}),
            "years": sorted({d.year for d in self.documents}),
        }

    def run(self) -> List[SECDocument]:
        print("\n" + "=" * 60)
        print("STEP 1: DATA INGEST")
        print("=" * 60)
        self.load_parquet()
        self.validate()
        docs = self.extract_documents()
        print("\n[ingest] Statistics:")
        for k, v in self.statistics().items():
            print(f"  {k}: {v}")
        return docs


if __name__ == "__main__":
    import sys
    path = sys.argv[1] if len(sys.argv) > 1 else "mda_merged_sic_naics_24_25.parquet"
    docs = SecDataIngest(path, limit=5).run()
    print(f"\nFirst doc id: {docs[0].document_id}")
    print(f"First doc preview: {docs[0].mda_text[:200]}...")
