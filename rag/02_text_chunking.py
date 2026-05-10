"""
Text Chunking Module - SEC MD&A Specific

This module handles splitting large SEC MDA (Management Discussion & Analysis)
documents into manageable chunks using the official SEC filing structure.
Step 2: Text Chunking with SEC-specific structure awareness
"""

from typing import List, Dict, Tuple
from dataclasses import dataclass
import re


@dataclass
class TextChunk:
    """Data structure for text chunks with metadata"""
    chunk_id: str
    source_document_id: str
    text: str
    chunk_index: int
    total_chunks: int
    metadata: Dict[str, str]
    start_char: int
    end_char: int
    section_name: str = ""  # New: Track which MD&A section this chunk is from


class TextChunker:
    """Handles text chunking with SEC MD&A structure awareness"""

    # SEC MD&A Common Subsection Headers
    MDA_SUBSECTIONS = [
        r'Overview',
        r'(?:Results? of Operations?|Financial Performance)',
        r'(?:Liquidity|Capital Resources|Cash Flow)',
        r'(?:Critical Accounting (?:Policies|Estimates))',
        r'(?:Risk Factors?|Risks and Uncertainties)',
        r'(?:Market Risk|Interest Rate Risk|Commodity Risk|Currency Risk)',
        r'(?:Quantitative and Qualitative Disclosure|Derivative Instruments)',
        r'(?:Off-Balance Sheet Arrangements)',
        r'(?:Contractual Obligations)',
        r'(?:Related Party Transactions)',
        r'(?:Segment Information)',
        r'(?:Impact of Inflation)',
        r'(?:Environmental Matters)',
        r'(?:Legal Proceedings)',
        r'(?:Changes in Accounting Principles)',
        r'(?:Recently Issued (?:Accounting Standards|Pronouncements))',
    ]

    def __init__(self, chunk_size: int = 512, overlap_percent: int = 15):
        """
        Initialize text chunker for SEC MD&A documents

        Args:
            chunk_size: Target size for chunks (in tokens, ~4 chars per token)
            overlap_percent: Overlap percentage between chunks (15% recommended for SEC)
        """
        self.chunk_size = chunk_size * 4  # Convert tokens to characters
        self.overlap_percent = overlap_percent
        self.overlap = int((overlap_percent / 100) * self.chunk_size)
        self.chunks = []

    def _find_mda_sections(self, text: str) -> List[Tuple[int, int, str]]:
        """
        Identify MD&A subsections in text based on common SEC structure

        Returns:
            List of (start_pos, end_pos, section_name) tuples
        """
        sections = []

        # Create pattern from subsection headers
        pattern = '|'.join(self.MDA_SUBSECTIONS)
        combined_pattern = rf'\n\s*(?:{pattern})[\s:]*\n'

        for match in re.finditer(combined_pattern, text, re.IGNORECASE):
            section_name = match.group().strip()
            sections.append((match.start(), section_name))

        return sections

    def chunk_by_mda_structure(self, text: str, source_id: str,
                              metadata: Dict[str, str]) -> List[TextChunk]:
        """
        Split text by SEC MD&A subsection structure with 15% overlap

        This method respects the natural structure of Item 7 (MD&A) in SEC filings:
        - Overview
        - Results of Operations
        - Liquidity and Capital Resources
        - Critical Accounting Policies
        - Risk Factors
        - Market Risk Disclosures
        - etc.

        Args:
            text: Full MD&A text
            source_id: ID of source document
            metadata: Document metadata

        Returns:
            List of TextChunk objects
        """
        if not text or len(text) == 0:
            return []

        chunks_list = []
        chunk_index = 0

        # Find MD&A subsection boundaries
        section_positions = self._find_mda_sections(text)

        if not section_positions:
            # Fallback to size-based chunking if no structure found
            return self.chunk_by_size(text, source_id, metadata)

        # Create sections with their boundaries
        sections_with_bounds = []
        for i, (start_pos, section_name) in enumerate(section_positions):
            if i + 1 < len(section_positions):
                end_pos = section_positions[i + 1][0]
            else:
                end_pos = len(text)

            section_text = text[start_pos:end_pos].strip()

            if len(section_text) > 50:  # Only keep substantial sections
                sections_with_bounds.append({
                    'name': section_name,
                    'text': section_text,
                    'start': start_pos,
                    'end': end_pos
                })

        total_chunks = len(sections_with_bounds)

        # Process each subsection
        for section_idx, section_info in enumerate(sections_with_bounds):
            section_text = section_info['text']
            section_name = section_info['name']

            # If section is small enough, keep as single chunk
            if len(section_text) <= self.chunk_size:
                chunk = TextChunk(
                    chunk_id=f"{source_id}_chunk_{chunk_index}",
                    source_document_id=source_id,
                    text=section_text,
                    chunk_index=chunk_index,
                    total_chunks=total_chunks,
                    metadata=metadata.copy(),
                    start_char=section_info['start'],
                    end_char=section_info['end'],
                    section_name=section_name
                )
                chunks_list.append(chunk)
                chunk_index += 1
            else:
                # Split large section with 15% overlap
                sub_chunks = self._chunk_section_with_overlap(
                    section_text,
                    source_id,
                    metadata,
                    section_name
                )
                for sub_chunk in sub_chunks:
                    sub_chunk.chunk_index = chunk_index
                    sub_chunk.total_chunks = total_chunks
                    chunk_index += 1
                chunks_list.extend(sub_chunks)

        return chunks_list

    def _chunk_section_with_overlap(self, text: str, source_id: str,
                                    metadata: Dict[str, str],
                                    section_name: str = "") -> List[TextChunk]:
        """
        Chunk a single section with configurable overlap (15% default)

        Args:
            text: Section text to chunk
            source_id: Document ID
            metadata: Document metadata
            section_name: Name of the section being chunked

        Returns:
            List of TextChunk objects from this section
        """
        chunks_list = []
        start_pos = 0
        chunk_index = 0

        while start_pos < len(text):
            # Calculate end position
            end_pos = min(start_pos + self.chunk_size, len(text))

            # Try to break at sentence boundary
            if end_pos < len(text):
                last_period = text.rfind('.', start_pos, end_pos)
                last_newline = text.rfind('\n', start_pos, end_pos)
                last_space = text.rfind(' ', start_pos, end_pos)

                breakpoint = max(last_period, last_newline, last_space)
                if breakpoint > start_pos + (self.chunk_size // 2):  # At least halfway
                    end_pos = breakpoint + 1

            chunk_text = text[start_pos:end_pos].strip()

            if chunk_text:
                chunk = TextChunk(
                    chunk_id=f"{source_id}_chunk_{chunk_index}",
                    source_document_id=source_id,
                    text=chunk_text,
                    chunk_index=chunk_index,
                    total_chunks=1,  # Will be updated by caller
                    metadata=metadata.copy(),
                    start_char=start_pos,
                    end_char=end_pos,
                    section_name=section_name
                )
                chunks_list.append(chunk)
                chunk_index += 1

            # Move to next chunk with 15% overlap
            start_pos = end_pos - self.overlap
            if start_pos >= len(text):
                break

        return chunks_list

    def chunk_by_size(self, text: str, source_id: str,
                     metadata: Dict[str, str]) -> List[TextChunk]:
        """
        Split text into fixed-size chunks with 15% overlap (fallback method)

        Args:
            text: Text to chunk
            source_id: ID of source document
            metadata: Document metadata to include

        Returns:
            List of TextChunk objects
        """
        if not text or len(text) == 0:
            return []

        chunks_list = []
        start_pos = 0
        chunk_index = 0

        # Calculate total chunks
        total_chunks = max(1, (len(text) - self.overlap) // (self.chunk_size - self.overlap))

        while start_pos < len(text):
            end_pos = min(start_pos + self.chunk_size, len(text))

            # Try to break at sentence boundary
            if end_pos < len(text):
                last_period = text.rfind('.', start_pos, end_pos)
                last_newline = text.rfind('\n', start_pos, end_pos)
                last_space = text.rfind(' ', start_pos, end_pos)

                breakpoint = max(last_period, last_newline, last_space)
                if breakpoint > start_pos:
                    end_pos = breakpoint + 1

            chunk_text = text[start_pos:end_pos].strip()

            if chunk_text:
                chunk = TextChunk(
                    chunk_id=f"{source_id}_chunk_{chunk_index}",
                    source_document_id=source_id,
                    text=chunk_text,
                    chunk_index=chunk_index,
                    total_chunks=total_chunks,
                    metadata=metadata.copy(),
                    start_char=start_pos,
                    end_char=end_pos
                )
                chunks_list.append(chunk)
                chunk_index += 1

            # Move to next chunk with overlap
            start_pos = end_pos - self.overlap
            if start_pos >= len(text):
                break

        return chunks_list

    def chunk_documents(self, documents: List, strategy: str = 'mda',
                       use_overlap: bool = True) -> List[TextChunk]:
        """
        Chunk a list of documents

        Args:
            documents: List of SECDocument objects
            strategy: 'mda' (default - uses SEC MD&A structure), 'size', or legacy 'section'
            use_overlap: Whether to use overlapping chunks (always True for SEC filings)

        Returns:
            List of all TextChunk objects
        """
        all_chunks = []

        for doc in documents:
            metadata = {
                'source_id': doc.document_id,
                'company': doc.company,
                'filing_date': doc.filing_date,
                'cik': doc.cik,
                **doc.metadata
            }

            if strategy == 'mda' or strategy == 'section':
                # Use SEC MD&A structure-aware chunking (default)
                chunks = self.chunk_by_mda_structure(doc.mda_text, doc.document_id, metadata)
            else:  # fallback to size strategy
                chunks = self.chunk_by_size(doc.mda_text, doc.document_id, metadata)

            all_chunks.extend(chunks)

        self.chunks = all_chunks
        return all_chunks

    def get_statistics(self) -> Dict:
        """
        Get chunking statistics

        Returns:
            Dictionary with chunking stats
        """
        if not self.chunks:
            return {}

        chunk_lengths = [len(chunk.text) for chunk in self.chunks]

        return {
            'total_chunks': len(self.chunks),
            'total_characters': sum(chunk_lengths),
            'avg_chunk_length': sum(chunk_lengths) / len(chunk_lengths),
            'min_chunk_length': min(chunk_lengths),
            'max_chunk_length': max(chunk_lengths),
            'unique_documents': len(set(c.source_document_id for c in self.chunks)),
            'avg_chunks_per_document': len(self.chunks) / len(set(c.source_document_id for c in self.chunks))
        }

    def run(self, documents: List, strategy: str = 'mda') -> List[TextChunk]:
        """
        Run complete chunking pipeline

        Args:
            documents: List of SECDocument objects from ingest step
            strategy: Chunking strategy ('mda' [default], 'size', or legacy 'section')
                      - 'mda': Uses SEC MD&A subsection structure (Item 7) [RECOMMENDED]
                      - 'size': Fixed-size chunks with overlap
                      - 'section': Legacy section-based chunking

        Returns:
            List of TextChunk objects
        """
        print("\n" + "="*60)
        print("STEP 2: TEXT CHUNKING (SEC MD&A STRUCTURE-AWARE)")
        print("="*60)
        print(f"Strategy: {strategy}")
        print(f"Chunk size: {self.chunk_size} characters (~{self.chunk_size // 4} tokens)")
        print(f"Overlap: {self.overlap} characters ({self.overlap_percent}%)")

        if strategy == 'mda':
            print(f"Using SEC MD&A subsection structure (Item 7)")
            print(f"Subsections: Overview, Results of Operations, Liquidity, Risk Factors, etc.")

        chunks = self.chunk_documents(documents, strategy=strategy)

        stats = self.get_statistics()
        print(f"\nChunking Statistics:")
        for key, value in stats.items():
            print(f"  {key}: {value}")

        return chunks


if __name__ == "__main__":
    # Example usage
    from data_ingest import SecDataIngest

    ingest = SecDataIngest("path/to/mda_24_25_merged.parquet")
    documents = ingest.run()

    # Create chunker with 15% overlap (recommended for SEC MD&A)
    chunker = TextChunker(chunk_size=512, overlap_percent=15)

    # Use MD&A structure-aware chunking (default, recommended)
    chunks = chunker.run(documents, strategy='mda')
    print(f"\nSuccessfully created {len(chunks)} chunks using SEC MD&A structure")

    # Example of different strategies
    # chunks = chunker.run(documents, strategy='size')  # Fixed-size chunks
    # chunks = chunker.run(documents, strategy='section')  # Legacy section-based

    # Print sample chunk with section info
    if chunks:
        sample = chunks[0]
        print(f"\nSample Chunk:")
        print(f"  ID: {sample.chunk_id}")
        print(f"  Section: {sample.section_name}")
        print(f"  Length: {len(sample.text)} characters")
        print(f"  Preview: {sample.text[:150]}...")
