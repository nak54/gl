"""
Step 3 — Text chunking (SEC MD&A structure aware)
=================================================

Implements the chunking strategy from claude.md:

1. **MD&A subsection recognition** — 15+ standard Item 7 subsection
   headers are detected with fuzzy regex patterns
   (``_find_mda_sections``).
2. **15% overlap** — overlap is expressed as a *percentage* of the
   chunk size in characters.  For a 512-token chunk
   (~2048 chars), that is ~307 characters of overlap.
3. **Section-aware metadata** — every ``TextChunk`` carries the name of
   its MD&A section in ``TextChunk.section_name``.
4. **Section-aware overlapped chunking** — large sections are split
   into sub-chunks with overlap (``_chunk_section_with_overlap``); if no
   section structure is found, falls back to ``chunk_by_size``.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Dict, List, Tuple


# ----------------------------------------------------------------------
# Data model
# ----------------------------------------------------------------------
@dataclass
class TextChunk:
    """A piece of cleaned MD&A text with traceability metadata."""
    chunk_id: str
    source_document_id: str
    text: str
    chunk_index: int
    total_chunks: int
    metadata: Dict[str, str]
    start_char: int
    end_char: int
    section_name: str = ""


# ----------------------------------------------------------------------
# Chunker
# ----------------------------------------------------------------------
class TextChunker:
    """SEC MD&A structure-aware chunker with percentage overlap."""

    # 15+ standard MD&A subsections (Item 7). Patterns are case-insensitive
    # and allow common variants/synonyms.
    MDA_SUBSECTIONS: List[str] = [
        r"Overview",
        r"Executive\s+Summary",
        r"Business\s+(?:Overview|Outlook|Environment)",
        r"Results?\s+of\s+Operations?",
        r"Financial\s+(?:Performance|Highlights|Condition)",
        r"Liquidity(?:\s+and\s+Capital\s+Resources)?",
        r"Capital\s+Resources",
        r"Cash\s+Flows?",
        r"Critical\s+Accounting\s+(?:Policies|Estimates)",
        r"Risk\s+Factors?",
        r"Risks?\s+and\s+Uncertainties",
        r"Market\s+Risk(?:\s+Disclosures?)?",
        r"Interest\s+Rate\s+Risk",
        r"Commodity\s+Risk",
        r"(?:Foreign\s+)?Currency\s+Risk",
        r"Quantitative\s+and\s+Qualitative\s+Disclosures?(?:\s+About\s+Market\s+Risk)?",
        r"Derivative\s+Instruments",
        r"Off[\s-]?Balance[\s-]?Sheet\s+Arrangements",
        r"Contractual\s+Obligations",
        r"Related\s+Party\s+Transactions",
        r"Segment\s+Information",
        r"Impact\s+of\s+Inflation",
        r"Environmental\s+Matters",
        r"Legal\s+Proceedings",
        r"Changes?\s+in\s+Accounting\s+(?:Principles|Estimates)",
        r"Recently\s+Issued\s+(?:Accounting\s+Standards|Pronouncements)",
        r"New\s+Accounting\s+Standards",
        r"Forward[\s-]?Looking\s+Statements",
    ]

    def __init__(self, chunk_size_tokens: int = 512, overlap_percent: int = 15):
        """
        Args:
            chunk_size_tokens: Target chunk size in *tokens*.  We use the
                conventional ~4 characters per token approximation, so the
                character size becomes ``chunk_size_tokens * 4``.
            overlap_percent: Overlap between adjacent chunks, in percent.
                15 is recommended for SEC filings.
        """
        self.chunk_size_tokens = chunk_size_tokens
        self.chunk_size = chunk_size_tokens * 4            # in characters
        self.overlap_percent = overlap_percent
        self.overlap = int((overlap_percent / 100.0) * self.chunk_size)
        self.chunks: List[TextChunk] = []

        # Pre-compile the combined header regex once.
        pat = "|".join(f"(?:{p})" for p in self.MDA_SUBSECTIONS)
        # Match a subsection header on its own line.  We allow optional
        # numbering ("1." or "a)"), trailing punctuation, and arbitrary
        # whitespace.
        self._header_re = re.compile(
            rf"(?m)^[\s]*(?:[0-9IVX]+[\.\)]\s*)?({pat})[\s:\.\-]*$",
            re.IGNORECASE,
        )

    # ------------------------------------------------------------------
    # Section detection
    # ------------------------------------------------------------------
    def _find_mda_sections(self, text: str) -> List[Tuple[int, str]]:
        """Return a list of ``(start_offset, section_name)`` for every
        detected MD&A header in ``text``."""
        out: List[Tuple[int, str]] = []
        for m in self._header_re.finditer(text):
            label = m.group(1).strip()
            # Canonicalise: drop trailing colons / dashes etc.
            label = re.sub(r"[\s:\.\-]+$", "", label)
            out.append((m.start(), label.title()))
        return out

    # ------------------------------------------------------------------
    # Section-aware chunking with overlap
    # ------------------------------------------------------------------
    def _chunk_section_with_overlap(
        self,
        text: str,
        source_id: str,
        metadata: Dict[str, str],
        section_name: str,
        section_offset: int,
        global_chunk_index_start: int,
    ) -> List[TextChunk]:
        """Split one section into overlapping sub-chunks."""
        out: List[TextChunk] = []
        start = 0
        idx = global_chunk_index_start

        n = len(text)
        if n == 0:
            return out

        while start < n:
            end = min(start + self.chunk_size, n)

            # Try to break at a sentence / line boundary in the last half.
            if end < n:
                half = start + self.chunk_size // 2
                bp = max(
                    text.rfind(". ", half, end),
                    text.rfind("\n", half, end),
                    text.rfind(" ",  half, end),
                )
                if bp > start:
                    end = bp + 1

            piece = text[start:end].strip()
            if piece:
                out.append(TextChunk(
                    chunk_id=f"{source_id}_chunk_{idx}",
                    source_document_id=source_id,
                    text=piece,
                    chunk_index=idx,
                    total_chunks=1,   # filled in by caller
                    metadata=dict(metadata),
                    start_char=section_offset + start,
                    end_char=section_offset + end,
                    section_name=section_name,
                ))
                idx += 1

            if end >= n:
                break
            start = max(end - self.overlap, start + 1)

        return out

    def chunk_by_mda_structure(
        self,
        text: str,
        source_id: str,
        metadata: Dict[str, str],
    ) -> List[TextChunk]:
        """Split text using MD&A subsection structure + 15% overlap.

        Falls back to size-based chunking if no headers are detected.
        """
        if not text:
            return []

        headers = self._find_mda_sections(text)
        if not headers:
            return self.chunk_by_size(text, source_id, metadata)

        # Build (section_name, body, offset) tuples.
        sections = []
        for i, (pos, name) in enumerate(headers):
            end = headers[i + 1][0] if i + 1 < len(headers) else len(text)
            body = text[pos:end]
            if len(body.strip()) > 50:
                sections.append((name, body, pos))

        if not sections:
            return self.chunk_by_size(text, source_id, metadata)

        out: List[TextChunk] = []
        idx = 0
        for name, body, offset in sections:
            if len(body) <= self.chunk_size:
                out.append(TextChunk(
                    chunk_id=f"{source_id}_chunk_{idx}",
                    source_document_id=source_id,
                    text=body.strip(),
                    chunk_index=idx,
                    total_chunks=1,
                    metadata=dict(metadata),
                    start_char=offset,
                    end_char=offset + len(body),
                    section_name=name,
                ))
                idx += 1
            else:
                sub = self._chunk_section_with_overlap(
                    body, source_id, metadata, name, offset, idx,
                )
                out.extend(sub)
                idx += len(sub)

        total = len(out)
        for c in out:
            c.total_chunks = total
        return out

    # ------------------------------------------------------------------
    # Fallback: size-based chunking with overlap (also section-aware
    # metadata = "" since no headers found)
    # ------------------------------------------------------------------
    def chunk_by_size(
        self,
        text: str,
        source_id: str,
        metadata: Dict[str, str],
    ) -> List[TextChunk]:
        if not text:
            return []
        out: List[TextChunk] = []
        start = 0
        idx = 0
        n = len(text)

        while start < n:
            end = min(start + self.chunk_size, n)
            if end < n:
                half = start + self.chunk_size // 2
                bp = max(
                    text.rfind(". ", half, end),
                    text.rfind("\n", half, end),
                    text.rfind(" ",  half, end),
                )
                if bp > start:
                    end = bp + 1
            piece = text[start:end].strip()
            if piece:
                out.append(TextChunk(
                    chunk_id=f"{source_id}_chunk_{idx}",
                    source_document_id=source_id,
                    text=piece,
                    chunk_index=idx,
                    total_chunks=1,
                    metadata=dict(metadata),
                    start_char=start,
                    end_char=end,
                    section_name="",
                ))
                idx += 1
            if end >= n:
                break
            start = max(end - self.overlap, start + 1)

        total = len(out)
        for c in out:
            c.total_chunks = total
        return out

    # ------------------------------------------------------------------
    # Driver
    # ------------------------------------------------------------------
    def chunk_documents(self, documents: List, strategy: str = "mda") -> List[TextChunk]:
        all_chunks: List[TextChunk] = []
        for doc in documents:
            metadata = {
                "source_id": doc.document_id,
                "accession": doc.accession,
                "company": doc.name,
                "cik": doc.cik,
                "ein": doc.ein,
                "sic": doc.sic,
                "naics3": doc.naics3,
                "naics17Title": doc.naics17Title,
                "year": doc.year,
                "is_ambiguous_naics3": doc.is_ambiguous_naics3,
                "address": doc.address,
            }
            if strategy == "mda":
                chunks = self.chunk_by_mda_structure(doc.mda_text, doc.document_id, metadata)
            else:
                chunks = self.chunk_by_size(doc.mda_text, doc.document_id, metadata)
            all_chunks.extend(chunks)
        self.chunks = all_chunks
        return all_chunks

    def statistics(self) -> Dict:
        if not self.chunks:
            return {}
        lens = [len(c.text) for c in self.chunks]
        sections = [c.section_name for c in self.chunks if c.section_name]
        from collections import Counter
        top_sections = Counter(sections).most_common(8)
        return {
            "chunks": len(self.chunks),
            "documents": len({c.source_document_id for c in self.chunks}),
            "avg_chunks_per_doc": len(self.chunks)
                                  / max(1, len({c.source_document_id for c in self.chunks})),
            "avg_chunk_chars": sum(lens) / len(lens),
            "min_chunk_chars": min(lens),
            "max_chunk_chars": max(lens),
            "chunks_with_section": sum(1 for c in self.chunks if c.section_name),
            "top_sections": top_sections,
        }

    def run(self, documents: List, strategy: str = "mda") -> List[TextChunk]:
        print("\n" + "=" * 60)
        print("STEP 3: TEXT CHUNKING (SEC MD&A STRUCTURE-AWARE)")
        print("=" * 60)
        print(f"  strategy        : {strategy}")
        print(f"  chunk_size      : {self.chunk_size} chars "
              f"(~{self.chunk_size_tokens} tokens)")
        print(f"  overlap_percent : {self.overlap_percent}%  "
              f"({self.overlap} chars)")

        chunks = self.chunk_documents(documents, strategy=strategy)
        print("\n[chunk] Statistics:")
        for k, v in self.statistics().items():
            print(f"  {k}: {v}")
        return chunks


if __name__ == "__main__":
    sample = """
    Item 7. Management's Discussion and Analysis

    Overview

    The company had a great year. Revenue grew. Margins improved.

    Results of Operations

    Revenue increased by 10%. Costs increased by 5%.

    Liquidity and Capital Resources

    Cash and equivalents totaled $5B. We expect that to grow.

    Risk Factors

    Macroeconomic conditions and supply chain issues continue.
    """
    chunker = TextChunker(chunk_size_tokens=64, overlap_percent=15)
    sects = chunker._find_mda_sections(sample)
    print("Found sections:", sects)
    fake_doc = type("D", (), dict(
        document_id="DOC1", accession="A", year="2024", cik="000",
        name="Acme", sic="", ein="", address="", naics3="", naics17Title="",
        is_ambiguous_naics3=False, mda_text=sample, metadata={}))()
    chunks = chunker.run([fake_doc], strategy="mda")
    for c in chunks:
        print(f"  [{c.section_name}] {c.text[:60]}...")
