"""
Table Extraction and SQLite Storage Module

This module handles extracting tables from SEC filings and storing them
in SQLite while keeping text clean for embeddings.
Step 0: Table Extraction (runs before chunking)
"""

import sqlite3
import re
from typing import List, Dict, Tuple, Optional
from dataclasses import dataclass
from datetime import datetime
import json


@dataclass
class ExtractedTable:
    """Extracted table with metadata"""
    table_id: str
    source_document_id: str
    table_html: str  # Original HTML or formatted table
    table_data: List[List[str]]  # Structured row/column data
    table_title: str
    cik: str
    ein: str = ""
    sic: str = ""
    naics: str = ""
    year: str = ""
    metadata: Dict = None

    def __post_init__(self):
        if self.metadata is None:
            self.metadata = {}


class TableExtractor:
    """Detects and extracts tables from SEC filing text"""

    def __init__(self):
        """Initialize table extractor"""
        self.extracted_tables = []

    def _detect_html_tables(self, text: str) -> List[Tuple[int, int, str]]:
        """
        Detect HTML table patterns in text

        Returns:
            List of (start_pos, end_pos, table_content) tuples
        """
        html_table_pattern = r'<table[^>]*>.*?</table>'
        tables = []

        for match in re.finditer(html_table_pattern, text, re.IGNORECASE | re.DOTALL):
            tables.append((match.start(), match.end(), match.group()))

        return tables

    def _extract_html_table_data(self, html_table: str) -> List[List[str]]:
        """
        Extract structured data from HTML table

        Args:
            html_table: HTML table string

        Returns:
            List of rows, each row is a list of cell values
        """
        rows = []

        # Extract rows
        row_pattern = r'<tr[^>]*>(.*?)</tr>'
        for row_match in re.finditer(row_pattern, html_table, re.IGNORECASE | re.DOTALL):
            row_content = row_match.group(1)

            # Extract cells (both th and td)
            cell_pattern = r'<t[dh][^>]*>(.*?)</t[dh]>'
            cells = []
            for cell_match in re.finditer(cell_pattern, row_content, re.IGNORECASE | re.DOTALL):
                cell_text = cell_match.group(1)
                # Clean HTML tags and entities
                cell_text = re.sub(r'<[^>]+>', '', cell_text)
                cell_text = re.sub(r'&[a-z]+;', '', cell_text, flags=re.IGNORECASE)
                cell_text = cell_text.strip()
                cells.append(cell_text)

            if cells:
                rows.append(cells)

        return rows

    def _detect_formatted_tables(self, text: str) -> List[Tuple[int, int, str]]:
        """
        Detect formatted/text-based tables (columns aligned with spaces/tabs)

        Returns:
            List of (start_pos, end_pos, table_content) tuples
        """
        tables = []
        lines = text.split('\n')

        # Look for sequences of aligned columns (multiple spaces or tabs)
        table_start = None
        table_lines = []

        for i, line in enumerate(lines):
            # Check if line looks like a table row (multiple aligned columns)
            if re.search(r'\s{2,}|\t', line) and len(line.strip()) > 0:
                if table_start is None:
                    table_start = i
                table_lines.append(line)
            else:
                # End of table
                if table_start is not None and len(table_lines) > 2:
                    # Found a table with at least 3 lines
                    table_content = '\n'.join(table_lines)
                    # Calculate positions in original text
                    start_pos = text.find(table_lines[0])
                    end_pos = text.find(table_lines[-1]) + len(table_lines[-1])
                    tables.append((start_pos, end_pos, table_content))

                table_start = None
                table_lines = []

        return tables

    def _extract_formatted_table_data(self, table_text: str) -> List[List[str]]:
        """
        Extract structured data from formatted table

        Args:
            table_text: Multi-line table text

        Returns:
            List of rows, each row is a list of cell values
        """
        rows = []
        lines = table_text.split('\n')

        for line in lines:
            if line.strip():
                # Split by multiple spaces or tabs
                cells = re.split(r'\s{2,}|\t+', line.strip())
                if cells:
                    rows.append(cells)

        return rows

    def extract_tables(self, text: str, document_id: str,
                      cik: str, metadata: Dict) -> Tuple[str, List[ExtractedTable]]:
        """
        Extract all tables from text and return cleaned text

        Args:
            text: Full document text
            document_id: Document ID
            cik: CIK number
            metadata: Document metadata (should contain ein, sic, naics, year if available)

        Returns:
            Tuple of (cleaned_text, list_of_extracted_tables)
        """
        extracted = []
        cleaned_text = text

        # Extract HTML tables
        html_tables = self._detect_html_tables(text)
        for i, (start, end, table_html) in enumerate(html_tables):
            table_data = self._extract_html_table_data(table_html)

            # Extract table title (usually text before <table>)
            title_search = re.search(r'([^\n]*?)\s*<table', table_html[:200], re.IGNORECASE)
            title = title_search.group(1).strip() if title_search else f"Table {i+1}"

            table = ExtractedTable(
                table_id=f"{document_id}_html_table_{i}",
                source_document_id=document_id,
                table_html=table_html,
                table_data=table_data,
                table_title=title,
                cik=cik,
                ein=metadata.get('ein', ''),
                sic=metadata.get('sic', ''),
                naics=metadata.get('naics', ''),
                year=metadata.get('year', ''),
                metadata=metadata
            )
            extracted.append(table)

        # Extract formatted tables
        formatted_tables = self._detect_formatted_tables(text)
        for i, (start, end, table_text) in enumerate(formatted_tables):
            # Skip if overlaps with HTML table
            skip = False
            for h_start, h_end, _ in html_tables:
                if (start >= h_start and start < h_end) or (end > h_start and end <= h_end):
                    skip = True
                    break

            if skip:
                continue

            table_data = self._extract_formatted_table_data(table_text)
            if len(table_data) > 2:  # Only keep if at least 3 rows
                # Try to extract title from context
                lines_before = cleaned_text[:start].split('\n')
                title = lines_before[-1].strip() if lines_before else f"Table {i+1}"

                table = ExtractedTable(
                    table_id=f"{document_id}_formatted_table_{i}",
                    source_document_id=document_id,
                    table_html=table_text,
                    table_data=table_data,
                    table_title=title,
                    cik=cik,
                    ein=metadata.get('ein', ''),
                    sic=metadata.get('sic', ''),
                    naics=metadata.get('naics', ''),
                    year=metadata.get('year', ''),
                    metadata=metadata
                )
                extracted.append(table)

        # Remove tables from text
        sorted_tables = sorted(html_tables + formatted_tables, key=lambda x: x[0], reverse=True)
        for start, end, _ in sorted_tables:
            cleaned_text = cleaned_text[:start] + cleaned_text[end:]

        self.extracted_tables.extend(extracted)
        return cleaned_text, extracted

    def extract_from_documents(self, documents: List) -> Tuple[List, List[ExtractedTable]]:
        """
        Extract tables from a list of documents

        Args:
            documents: List of SECDocument objects

        Returns:
            Tuple of (modified_documents_with_cleaned_text, all_extracted_tables)
        """
        all_extracted_tables = []
        modified_documents = []

        for doc in documents:
            cleaned_text, extracted = self.extract_tables(
                doc.mda_text,
                doc.document_id,
                doc.cik,
                doc.metadata
            )

            # Create modified document with cleaned text
            modified_doc = doc.__class__(
                document_id=doc.document_id,
                filing_date=doc.filing_date,
                company=doc.company,
                cik=doc.cik,
                mda_text=cleaned_text,  # Use cleaned text without tables
                metadata=doc.metadata
            )
            modified_documents.append(modified_doc)
            all_extracted_tables.extend(extracted)

        print(f"Extracted {len(all_extracted_tables)} tables from {len(documents)} documents")
        return modified_documents, all_extracted_tables


class SQLiteTableStore:
    """SQLite storage for extracted tables"""

    def __init__(self, db_path: str = "sec_tables.db"):
        """
        Initialize SQLite table store

        Args:
            db_path: Path to SQLite database file
        """
        self.db_path = db_path
        self.conn = None
        self._initialize_db()

    def _initialize_db(self):
        """Initialize SQLite database and create table"""
        self.conn = sqlite3.connect(self.db_path)
        cursor = self.conn.cursor()

        # Create main tables table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS extracted_tables (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                table_id TEXT UNIQUE NOT NULL,
                source_document_id TEXT NOT NULL,
                table_title TEXT,
                table_data TEXT,
                cik TEXT NOT NULL,
                ein TEXT,
                sic TEXT,
                naics TEXT,
                year TEXT,
                extraction_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                metadata TEXT
            )
        ''')

        # Create index for faster lookups
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_cik ON extracted_tables(cik)
        ''')
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_year ON extracted_tables(year)
        ''')
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_source_document ON extracted_tables(source_document_id)
        ''')

        self.conn.commit()
        print(f"SQLite database initialized at {self.db_path}")

    def store_tables(self, extracted_tables: List[ExtractedTable],
                    skip_duplicates: bool = True) -> int:
        """
        Store extracted tables in SQLite

        Args:
            extracted_tables: List of ExtractedTable objects
            skip_duplicates: Skip tables with duplicate table_id

        Returns:
            Number of tables stored
        """
        cursor = self.conn.cursor()
        stored_count = 0

        print(f"Storing {len(extracted_tables)} tables in SQLite...")

        for table in extracted_tables:
            try:
                # Convert table data to JSON for storage
                table_data_json = json.dumps(table.table_data)
                metadata_json = json.dumps(table.metadata)

                cursor.execute('''
                    INSERT OR REPLACE INTO extracted_tables
                    (table_id, source_document_id, table_title, table_data,
                     cik, ein, sic, naics, year, metadata)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ''', (
                    table.table_id,
                    table.source_document_id,
                    table.table_title,
                    table_data_json,
                    table.cik,
                    table.ein,
                    table.sic,
                    table.naics,
                    table.year,
                    metadata_json
                ))

                stored_count += 1
            except sqlite3.IntegrityError as e:
                if skip_duplicates:
                    print(f"  Skipping duplicate table: {table.table_id}")
                else:
                    raise

        self.conn.commit()
        print(f"Successfully stored {stored_count} tables in SQLite")
        return stored_count

    def query_tables(self, cik: Optional[str] = None,
                    year: Optional[str] = None,
                    limit: int = 100) -> List[Dict]:
        """
        Query tables from SQLite

        Args:
            cik: Filter by CIK
            year: Filter by year
            limit: Maximum number of results

        Returns:
            List of table records as dictionaries
        """
        cursor = self.conn.cursor()
        query = "SELECT * FROM extracted_tables WHERE 1=1"
        params = []

        if cik:
            query += " AND cik = ?"
            params.append(cik)

        if year:
            query += " AND year = ?"
            params.append(year)

        query += f" LIMIT {limit}"

        cursor.execute(query, params)
        columns = [description[0] for description in cursor.description]

        results = []
        for row in cursor.fetchall():
            results.append(dict(zip(columns, row)))

        return results

    def get_statistics(self) -> Dict:
        """
        Get database statistics

        Returns:
            Dictionary with statistics
        """
        cursor = self.conn.cursor()

        cursor.execute("SELECT COUNT(*) FROM extracted_tables")
        total_tables = cursor.fetchone()[0]

        cursor.execute("SELECT COUNT(DISTINCT cik) FROM extracted_tables")
        unique_ciks = cursor.fetchone()[0]

        cursor.execute("SELECT COUNT(DISTINCT year) FROM extracted_tables")
        unique_years = cursor.fetchone()[0]

        cursor.execute("SELECT COUNT(DISTINCT source_document_id) FROM extracted_tables")
        unique_documents = cursor.fetchone()[0]

        return {
            'total_tables': total_tables,
            'unique_ciks': unique_ciks,
            'unique_years': unique_years,
            'unique_documents': unique_documents,
            'db_file': self.db_path
        }

    def close(self):
        """Close database connection"""
        if self.conn:
            self.conn.close()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()


if __name__ == "__main__":
    # Example usage
    from data_ingest import SecDataIngest

    # Load documents
    ingest = SecDataIngest("path/to/mda_24_25_merged.parquet")
    documents = ingest.run()

    # Extract tables
    extractor = TableExtractor()
    cleaned_docs, extracted_tables = extractor.extract_from_documents(documents)

    # Store in SQLite
    db_store = SQLiteTableStore("sec_tables.db")
    db_store.store_tables(extracted_tables)

    # Get statistics
    stats = db_store.get_statistics()
    print(f"\nDatabase Statistics:")
    for key, value in stats.items():
        print(f"  {key}: {value}")

    db_store.close()

    print(f"\nProcessed {len(cleaned_docs)} documents")
    print(f"Extracted {len(extracted_tables)} tables")
