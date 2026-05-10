"""
SEC MDA Data Ingest Module

This module handles loading and preprocessing SEC filing data from parquet files.
Step 1: Data Ingestion
"""

import pandas as pd
from typing import List, Dict, Optional
from dataclasses import dataclass


@dataclass
class SECDocument:
    """Data structure for SEC filing documents"""
    document_id: str
    filing_date: str
    company: str
    cik: str
    mda_text: str
    metadata: Dict[str, str]


class SecDataIngest:
    """Handles ingestion of SEC filing data from parquet files"""

    def __init__(self, parquet_path: str):
        """
        Initialize the ingest handler

        Args:
            parquet_path: Path to the parquet file containing SEC filings
        """
        self.parquet_path = parquet_path
        self.df = None
        self.documents = []

    def load_parquet(self) -> pd.DataFrame:
        """
        Load parquet file

        Returns:
            DataFrame with SEC filing data
        """
        print(f"Loading parquet file: {self.parquet_path}")
        self.df = pd.read_parquet(self.parquet_path)
        print(f"Loaded {len(self.df)} documents")
        print(f"Columns: {self.df.columns.tolist()}")
        return self.df

    def validate_data(self) -> bool:
        """
        Validate that required columns exist

        Returns:
            True if validation passes
        """
        required_columns = ['mda_text']
        missing = [col for col in required_columns if col not in self.df.columns]

        if missing:
            raise ValueError(f"Missing required columns: {missing}")

        # Check for null MDA texts
        null_count = self.df['mda_text'].isna().sum()
        if null_count > 0:
            print(f"Warning: {null_count} documents have null MDA text")

        print("Data validation passed ✓")
        return True

    def extract_documents(self) -> List[SECDocument]:
        """
        Extract documents from dataframe

        Returns:
            List of SECDocument objects
        """
        self.documents = []

        for idx, row in self.df.iterrows():
            # Handle potentially missing columns gracefully
            doc_id = str(row.get('document_id', f'doc_{idx}'))
            filing_date = str(row.get('filing_date', 'unknown'))
            company = str(row.get('company', 'unknown'))
            cik = str(row.get('cik', 'unknown'))
            mda_text = str(row.get('mda_text', ''))

            # Skip documents with empty MDA text
            if not mda_text or mda_text.lower() == 'nan':
                print(f"Skipping document {doc_id}: empty MDA text")
                continue

            # Extract additional metadata
            metadata = {
                'filing_date': filing_date,
                'company': company,
                'cik': cik,
            }

            # Add any other columns as metadata
            for col in self.df.columns:
                if col not in ['mda_text', 'document_id', 'filing_date', 'company', 'cik']:
                    metadata[col] = str(row.get(col, 'unknown'))

            doc = SECDocument(
                document_id=doc_id,
                filing_date=filing_date,
                company=company,
                cik=cik,
                mda_text=mda_text,
                metadata=metadata
            )
            self.documents.append(doc)

        print(f"Extracted {len(self.documents)} valid documents")
        return self.documents

    def get_statistics(self) -> Dict:
        """
        Get statistics about ingested data

        Returns:
            Dictionary with data statistics
        """
        if not self.documents:
            return {}

        mda_lengths = [len(doc.mda_text) for doc in self.documents]

        return {
            'total_documents': len(self.documents),
            'total_characters': sum(mda_lengths),
            'avg_document_length': sum(mda_lengths) / len(mda_lengths),
            'min_document_length': min(mda_lengths),
            'max_document_length': max(mda_lengths),
            'unique_companies': len(set(doc.company for doc in self.documents)),
            'date_range': (
                min(doc.filing_date for doc in self.documents),
                max(doc.filing_date for doc in self.documents)
            )
        }

    def run(self) -> List[SECDocument]:
        """
        Run complete ingest pipeline

        Returns:
            List of ingested SECDocument objects
        """
        print("\n" + "="*60)
        print("STEP 1: DATA INGEST")
        print("="*60)

        self.load_parquet()
        self.validate_data()
        self.extract_documents()

        stats = self.get_statistics()
        print(f"\nData Statistics:")
        for key, value in stats.items():
            print(f"  {key}: {value}")

        return self.documents


if __name__ == "__main__":
    # Example usage
    ingest = SecDataIngest("path/to/mda_24_25_merged.parquet")
    documents = ingest.run()
    print(f"\nSuccessfully ingested {len(documents)} documents")
