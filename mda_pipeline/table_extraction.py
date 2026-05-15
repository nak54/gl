"""
Step 2 — Table extraction
=========================

Two responsibilities:

1. ``TableExtractor`` — detect HTML and whitespace-aligned tables inside
   ``mda_text`` and remove them, returning *cleaned* text and a list of
   ``ExtractedTable`` records.
2. ``SQLiteTableStore`` — persist extracted tables to SQLite.

SQLite schema (per claude.md):

    extracted_tables(
        cik, ein, sic, naics3, year,
        extracted_table1, extracted_table2, ...
    )

Internally we store **one row per filing** (keyed by ``source_document_id``)
and lay each table out as a JSON-encoded column ``extracted_table_N`` so the
"one row per company" shape matches the spec while still being queryable.
A long-form ``tables_long`` table is also written for easy SQL filtering.
"""

from __future__ import annotations

import json
import re
import sqlite3
from collections import defaultdict
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple


@dataclass
class ExtractedTable:
    """One table extracted from an MD&A document."""
    table_id: str
    source_document_id: str
    table_html: str
    table_data: List[List[str]]
    table_title: str
    cik: str
    ein: str = ""
    sic: str = ""
    naics3: str = ""
    year: str = ""
    table_index: int = 0          # ordinal within the filing (0-based)
    metadata: Dict = field(default_factory=dict)


# ----------------------------------------------------------------------
# Extraction
# ----------------------------------------------------------------------
_HTML_TABLE_RE = re.compile(r"<table[^>]*>.*?</table>", re.IGNORECASE | re.DOTALL)
_ROW_RE = re.compile(r"<tr[^>]*>(.*?)</tr>", re.IGNORECASE | re.DOTALL)
_CELL_RE = re.compile(r"<t[dh][^>]*>(.*?)</t[dh]>", re.IGNORECASE | re.DOTALL)
_TAG_RE = re.compile(r"<[^>]+>")
_ENTITY_RE = re.compile(r"&[a-z]+;|&#\d+;", re.IGNORECASE)


class TableExtractor:
    """Detect + remove tables from SEC filing MD&A text."""

    def __init__(self, min_formatted_rows: int = 3):
        """
        Args:
            min_formatted_rows: Whitespace-aligned blocks need at least this
                many lines before we treat them as a table (avoids stripping
                ordinary indented prose).
        """
        self.min_formatted_rows = min_formatted_rows
        self.all_tables: List[ExtractedTable] = []

    # -- HTML --------------------------------------------------------
    def _detect_html_tables(self, text: str) -> List[Tuple[int, int, str]]:
        return [(m.start(), m.end(), m.group()) for m in _HTML_TABLE_RE.finditer(text)]

    def _parse_html_table(self, html: str) -> List[List[str]]:
        rows: List[List[str]] = []
        for row_match in _ROW_RE.finditer(html):
            cells = []
            for cell_match in _CELL_RE.finditer(row_match.group(1)):
                text = cell_match.group(1)
                text = _TAG_RE.sub("", text)
                text = _ENTITY_RE.sub(" ", text)
                cells.append(text.strip())
            if cells:
                rows.append(cells)
        return rows

    # -- whitespace-aligned -----------------------------------------
    def _detect_formatted_tables(self, text: str) -> List[Tuple[int, int, str]]:
        """Find blocks of consecutive lines that look like aligned columns."""
        tables = []
        lines = text.split("\n")
        block_start_line = None
        block_lines: List[str] = []

        # We need to know each line's character offset in the original text.
        line_offsets: List[int] = []
        offset = 0
        for line in lines:
            line_offsets.append(offset)
            offset += len(line) + 1   # +1 for the newline we split on

        def flush(end_line_idx: int) -> None:
            if block_start_line is None or len(block_lines) < self.min_formatted_rows:
                return
            start_pos = line_offsets[block_start_line]
            end_line = end_line_idx - 1
            end_pos = line_offsets[end_line] + len(lines[end_line])
            tables.append((start_pos, end_pos, "\n".join(block_lines)))

        for i, line in enumerate(lines):
            if re.search(r"\s{2,}|\t", line) and line.strip():
                if block_start_line is None:
                    block_start_line = i
                block_lines.append(line)
            else:
                flush(i)
                block_start_line = None
                block_lines = []
        flush(len(lines))
        return tables

    def _parse_formatted_table(self, table_text: str) -> List[List[str]]:
        rows = []
        for line in table_text.split("\n"):
            if line.strip():
                cells = re.split(r"\s{2,}|\t+", line.strip())
                if cells:
                    rows.append(cells)
        return rows

    # -- orchestrator ------------------------------------------------
    def extract(
        self,
        text: str,
        document_id: str,
        cik: str,
        ein: str,
        sic: str,
        naics3: str,
        year: str,
        metadata: Dict,
    ) -> Tuple[str, List[ExtractedTable]]:
        """Return ``(cleaned_text, [ExtractedTable])`` for one filing."""
        if not text:
            return text, []

        html_spans = self._detect_html_tables(text)
        formatted_spans_raw = self._detect_formatted_tables(text)

        # Drop formatted spans that overlap an HTML span (already covered).
        formatted_spans = []
        for s, e, body in formatted_spans_raw:
            overlap = any(
                s < he and e > hs for hs, he, _ in html_spans
            )
            if not overlap:
                formatted_spans.append((s, e, body))

        extracted: List[ExtractedTable] = []
        running_idx = 0

        for s, e, html in html_spans:
            rows = self._parse_html_table(html)
            title_match = re.search(r"([^\n]*?)\s*<table", html[:300], re.IGNORECASE)
            title = title_match.group(1).strip() if title_match else f"Table {running_idx + 1}"
            extracted.append(ExtractedTable(
                table_id=f"{document_id}_html_{running_idx}",
                source_document_id=document_id,
                table_html=html,
                table_data=rows,
                table_title=title or f"Table {running_idx + 1}",
                cik=cik, ein=ein, sic=sic, naics3=naics3, year=year,
                table_index=running_idx,
                metadata=dict(metadata),
            ))
            running_idx += 1

        for s, e, body in formatted_spans:
            rows = self._parse_formatted_table(body)
            if len(rows) < self.min_formatted_rows:
                continue
            line_before = text[:s].split("\n")
            title = line_before[-1].strip() if line_before else f"Table {running_idx + 1}"
            extracted.append(ExtractedTable(
                table_id=f"{document_id}_fmt_{running_idx}",
                source_document_id=document_id,
                table_html=body,
                table_data=rows,
                table_title=title or f"Table {running_idx + 1}",
                cik=cik, ein=ein, sic=sic, naics3=naics3, year=year,
                table_index=running_idx,
                metadata=dict(metadata),
            ))
            running_idx += 1

        # Remove all table spans from the text (back-to-front so offsets stay valid).
        all_spans = sorted(
            [(s, e) for s, e, _ in html_spans] + [(s, e) for s, e, _ in formatted_spans],
            reverse=True,
        )
        cleaned = text
        for s, e in all_spans:
            cleaned = cleaned[:s] + " [TABLE_REMOVED] " + cleaned[e:]

        # Tidy up the cleaned text (collapse runs of blank lines).
        cleaned = re.sub(r"\n{3,}", "\n\n", cleaned)

        self.all_tables.extend(extracted)
        return cleaned, extracted

    def extract_from_documents(self, documents: List) -> Tuple[List, List[ExtractedTable]]:
        """Process a list of ``SECDocument``s and return cleaned docs + tables."""
        cleaned_docs = []
        all_tables: List[ExtractedTable] = []

        for doc in documents:
            cleaned_text, tables = self.extract(
                text=doc.mda_text,
                document_id=doc.document_id,
                cik=doc.cik,
                ein=doc.ein,
                sic=doc.sic,
                naics3=doc.naics3,
                year=doc.year,
                metadata=doc.metadata,
            )
            cleaned_doc = doc.__class__(
                document_id=doc.document_id,
                accession=doc.accession,
                year=doc.year,
                cik=doc.cik,
                name=doc.name,
                sic=doc.sic,
                ein=doc.ein,
                address=doc.address,
                naics3=doc.naics3,
                naics17Title=doc.naics17Title,
                is_ambiguous_naics3=doc.is_ambiguous_naics3,
                mda_text=cleaned_text,
                metadata=doc.metadata,
            )
            cleaned_docs.append(cleaned_doc)
            all_tables.extend(tables)

        print(f"[tables] Extracted {len(all_tables)} tables from {len(documents)} documents")
        return cleaned_docs, all_tables


# ----------------------------------------------------------------------
# Storage
# ----------------------------------------------------------------------
class SQLiteTableStore:
    """SQLite store with the shape described in claude.md.

    Two tables are maintained:

    * ``filings_tables`` — *one row per filing*, with columns
      ``cik, ein, sic, naics3, year, extracted_table1, extracted_table2, ...``.
      Each ``extracted_table_N`` cell holds the JSON of one table.

    * ``tables_long`` — *one row per extracted table*, for downstream
      SQL-style querying.
    """

    def __init__(self, db_path: str = "sec_tables.db"):
        self.db_path = db_path
        self.conn = sqlite3.connect(db_path)
        self.conn.execute("PRAGMA journal_mode=WAL;")
        self._max_table_columns = 0
        self._init_schema()
        print(f"[tables] SQLite ready at {db_path}")

    # -- schema ----------------------------------------------------
    def _init_schema(self) -> None:
        c = self.conn.cursor()
        c.execute(
            """
            CREATE TABLE IF NOT EXISTS tables_long (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                table_id TEXT UNIQUE NOT NULL,
                source_document_id TEXT NOT NULL,
                table_index INTEGER NOT NULL,
                table_title TEXT,
                table_data TEXT,            -- JSON list-of-lists
                table_html TEXT,
                cik TEXT NOT NULL,
                ein TEXT,
                sic TEXT,
                naics3 TEXT,
                year TEXT,
                metadata TEXT,              -- JSON
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """
        )
        c.execute("CREATE INDEX IF NOT EXISTS idx_long_cik ON tables_long(cik)")
        c.execute("CREATE INDEX IF NOT EXISTS idx_long_year ON tables_long(year)")
        c.execute("CREATE INDEX IF NOT EXISTS idx_long_naics3 ON tables_long(naics3)")
        c.execute("CREATE INDEX IF NOT EXISTS idx_long_doc ON tables_long(source_document_id)")

        c.execute(
            """
            CREATE TABLE IF NOT EXISTS filings_tables (
                source_document_id TEXT PRIMARY KEY,
                cik TEXT,
                ein TEXT,
                sic TEXT,
                naics3 TEXT,
                year TEXT,
                n_tables INTEGER DEFAULT 0
                -- extracted_table1..N added dynamically
            )
            """
        )
        self.conn.commit()

    def _ensure_table_columns(self, needed: int) -> None:
        """Add ``extracted_table_N`` columns up to ``needed`` if not present."""
        c = self.conn.cursor()
        existing = {row[1] for row in c.execute("PRAGMA table_info(filings_tables)")}
        for i in range(1, needed + 1):
            col = f"extracted_table{i}"
            if col not in existing:
                c.execute(f"ALTER TABLE filings_tables ADD COLUMN {col} TEXT")
        self.conn.commit()

    # -- writes ----------------------------------------------------
    def store_tables(self, tables: List[ExtractedTable]) -> int:
        if not tables:
            print("[tables] No tables to store")
            return 0

        c = self.conn.cursor()
        stored = 0

        # 1) Long-form rows
        for t in tables:
            try:
                c.execute(
                    """
                    INSERT OR REPLACE INTO tables_long
                    (table_id, source_document_id, table_index, table_title,
                     table_data, table_html, cik, ein, sic, naics3, year, metadata)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    """,
                    (
                        t.table_id,
                        t.source_document_id,
                        t.table_index,
                        t.table_title,
                        json.dumps(t.table_data, ensure_ascii=False),
                        t.table_html[:200_000],   # cap to keep DB compact
                        t.cik, t.ein, t.sic, t.naics3, t.year,
                        json.dumps(t.metadata, ensure_ascii=False, default=str),
                    ),
                )
                stored += 1
            except sqlite3.Error as exc:
                print(f"[tables] long-form insert failed for {t.table_id}: {exc}")

        # 2) Wide-form one-row-per-filing
        grouped: Dict[str, List[ExtractedTable]] = defaultdict(list)
        for t in tables:
            grouped[t.source_document_id].append(t)

        max_n = max(len(v) for v in grouped.values())
        self._ensure_table_columns(max_n)
        self._max_table_columns = max(self._max_table_columns, max_n)

        for doc_id, group in grouped.items():
            group.sort(key=lambda t: t.table_index)
            first = group[0]
            cols = ["source_document_id", "cik", "ein", "sic", "naics3", "year", "n_tables"]
            vals = [doc_id, first.cik, first.ein, first.sic, first.naics3, first.year, len(group)]
            for i, t in enumerate(group, start=1):
                cols.append(f"extracted_table{i}")
                vals.append(json.dumps({
                    "title": t.table_title,
                    "data": t.table_data,
                }, ensure_ascii=False))
            placeholders = ",".join(["?"] * len(cols))
            colnames = ",".join(cols)
            c.execute(
                f"INSERT OR REPLACE INTO filings_tables ({colnames}) VALUES ({placeholders})",
                vals,
            )

        self.conn.commit()
        print(f"[tables] Stored {stored} tables ({len(grouped)} filings, "
              f"widest row has {max_n} table columns)")
        return stored

    # -- reads -----------------------------------------------------
    def query_tables(
        self,
        cik: Optional[str] = None,
        year: Optional[str] = None,
        naics3: Optional[str] = None,
        limit: int = 100,
    ) -> List[Dict]:
        sql = "SELECT * FROM tables_long WHERE 1=1"
        params: List = []
        if cik:
            sql += " AND cik = ?"; params.append(cik)
        if year:
            sql += " AND year = ?"; params.append(year)
        if naics3:
            sql += " AND naics3 = ?"; params.append(naics3)
        sql += f" LIMIT {int(limit)}"
        c = self.conn.cursor()
        c.execute(sql, params)
        cols = [d[0] for d in c.description]
        out = []
        for row in c.fetchall():
            rec = dict(zip(cols, row))
            try:
                rec["table_data"] = json.loads(rec["table_data"])
            except Exception:
                pass
            out.append(rec)
        return out

    def get_filing_row(self, source_document_id: str) -> Optional[Dict]:
        c = self.conn.cursor()
        c.execute("SELECT * FROM filings_tables WHERE source_document_id = ?",
                  (source_document_id,))
        row = c.fetchone()
        if not row:
            return None
        cols = [d[0] for d in c.description]
        return dict(zip(cols, row))

    def statistics(self) -> Dict:
        c = self.conn.cursor()
        total = c.execute("SELECT COUNT(*) FROM tables_long").fetchone()[0]
        ciks = c.execute("SELECT COUNT(DISTINCT cik) FROM tables_long").fetchone()[0]
        years = c.execute("SELECT COUNT(DISTINCT year) FROM tables_long").fetchone()[0]
        docs = c.execute("SELECT COUNT(DISTINCT source_document_id) FROM tables_long").fetchone()[0]
        filings = c.execute("SELECT COUNT(*) FROM filings_tables").fetchone()[0]
        max_n = c.execute("SELECT COALESCE(MAX(n_tables), 0) FROM filings_tables").fetchone()[0]
        return {
            "tables_long_rows": total,
            "unique_ciks": ciks,
            "unique_years": years,
            "unique_documents": docs,
            "filings_rows": filings,
            "max_tables_in_one_filing": max_n,
            "db_path": self.db_path,
        }

    def close(self) -> None:
        if self.conn:
            self.conn.close()
            self.conn = None

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()


if __name__ == "__main__":
    # Tiny self-test
    sample = """
    Item 7. MD&A

    Overview

    Revenue increased.

    <table><tr><th>Year</th><th>Revenue</th></tr><tr><td>2024</td><td>100</td></tr></table>

    Liquidity

    Cash flow was strong.
    """
    extractor = TableExtractor()
    cleaned, tabs = extractor.extract(
        sample, "DOC1", "0000001", "12-34", "9999", "311", "2024", {},
    )
    print("Cleaned text:\n", cleaned)
    print("Tables:", tabs)
