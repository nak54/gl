#!/usr/bin/env python3
"""
Verification Script for Table Extraction Implementation

Run this to verify all components are correctly implemented.
"""

import sys
import os
from pathlib import Path


def check_file_exists(filepath, description):
    """Check if a file exists"""
    if os.path.exists(filepath):
        print(f"✓ {description}")
        return True
    else:
        print(f"✗ {description} - NOT FOUND")
        return False


def check_imports():
    """Verify all required imports work"""
    print("\n" + "=" * 70)
    print("Checking Imports...")
    print("=" * 70)

    try:
        from data_ingest import SecDataIngest
        print("✓ SecDataIngest imported")
    except ImportError as e:
        print(f"✗ SecDataIngest import failed: {e}")
        return False

    try:
        from text_chunking import TextChunker
        print("✓ TextChunker imported")
    except ImportError as e:
        print(f"✗ TextChunker import failed: {e}")
        return False

    try:
        from embeddings import EmbeddingGenerator
        print("✓ EmbeddingGenerator imported")
    except ImportError as e:
        print(f"✗ EmbeddingGenerator import failed: {e}")
        return False

    try:
        from qdrant_store import QdrantVectorStore
        print("✓ QdrantVectorStore imported")
    except ImportError as e:
        print(f"✗ QdrantVectorStore import failed: {e}")
        return False

    try:
        from table_extraction import TableExtractor, SQLiteTableStore, ExtractedTable
        print("✓ TableExtractor imported")
        print("✓ SQLiteTableStore imported")
        print("✓ ExtractedTable imported")
    except ImportError as e:
        print(f"✗ table_extraction imports failed: {e}")
        return False

    try:
        from rag_pipeline import RAGPipeline
        print("✓ RAGPipeline imported")
    except ImportError as e:
        print(f"✗ RAGPipeline import failed: {e}")
        return False

    return True


def check_table_extractor():
    """Verify TableExtractor functionality"""
    print("\n" + "=" * 70)
    print("Testing TableExtractor...")
    print("=" * 70)

    from table_extraction import TableExtractor

    extractor = TableExtractor()
    print("✓ TableExtractor initialized")

    # Test HTML table detection
    html_test = """
    <table>
    <tr><td>Header1</td><td>Header2</td></tr>
    <tr><td>Value1</td><td>Value2</td></tr>
    </table>
    """

    cleaned, tables = extractor.extract_tables(
        html_test,
        "test_doc_1",
        "0000789019",
        {"year": "2024"}
    )

    if len(tables) > 0:
        print(f"✓ HTML table extraction working ({len(tables)} table extracted)")
    else:
        print(f"✗ HTML table extraction not working")
        return False

    # Verify table structure
    table = tables[0]
    if table.cik == "0000789019":
        print(f"✓ Table metadata preserved (CIK: {table.cik})")
    else:
        print(f"✗ Table metadata not correct")
        return False

    if table.table_data:
        print(f"✓ Table data extracted ({len(table.table_data)} rows)")
    else:
        print(f"✗ Table data not extracted")
        return False

    # Verify text cleaning
    if '<table' not in cleaned.lower():
        print(f"✓ Text cleaned (tables removed)")
    else:
        print(f"✗ Text not properly cleaned")
        return False

    return True


def check_sqlite_store():
    """Verify SQLiteTableStore functionality"""
    print("\n" + "=" * 70)
    print("Testing SQLiteTableStore...")
    print("=" * 70)

    import tempfile
    from table_extraction import SQLiteTableStore, ExtractedTable

    # Create temporary database
    with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
        db_path = f.name

    try:
        db = SQLiteTableStore(db_path)
        print(f"✓ SQLiteTableStore initialized at {db_path}")

        # Create a test table
        test_table = ExtractedTable(
            table_id="test_table_1",
            source_document_id="doc_1",
            table_html="<table>test</table>",
            table_data=[["Col1", "Col2"], ["Val1", "Val2"]],
            table_title="Test Table",
            cik="0000789019",
            ein="12-3456789",
            sic="5812",
            naics="722110",
            year="2024"
        )

        # Store the table
        count = db.store_tables([test_table])
        if count == 1:
            print(f"✓ Table stored successfully")
        else:
            print(f"✗ Table storage failed")
            return False

        # Query the table
        results = db.query_tables(cik="0000789019")
        if len(results) > 0:
            print(f"✓ Table query working ({len(results)} results)")
        else:
            print(f"✗ Table query not working")
            return False

        # Get statistics
        stats = db.get_statistics()
        if stats['total_tables'] > 0:
            print(f"✓ Statistics available (total_tables: {stats['total_tables']})")
        else:
            print(f"✗ Statistics not working")
            return False

        db.close()

    finally:
        # Clean up
        if os.path.exists(db_path):
            os.remove(db_path)
            print(f"✓ Temporary database cleaned up")

    return True


def check_pipeline_integration():
    """Verify RAGPipeline integration"""
    print("\n" + "=" * 70)
    print("Testing RAGPipeline Integration...")
    print("=" * 70)

    from rag_pipeline import RAGPipeline

    # Check if pipeline can be instantiated
    try:
        rag = RAGPipeline(
            parquet_path="dummy.parquet",  # Won't be used for instantiation
            db_path="test_verify.db"
        )
        print("✓ RAGPipeline instantiated with db_path parameter")
    except TypeError as e:
        print(f"✗ RAGPipeline initialization failed: {e}")
        return False

    # Check if new methods exist
    if hasattr(rag, 'table_extractor'):
        print("✓ RAGPipeline has table_extractor attribute")
    else:
        print("✗ RAGPipeline missing table_extractor")
        return False

    if hasattr(rag, 'table_store'):
        print("✓ RAGPipeline has table_store attribute")
    else:
        print("✗ RAGPipeline missing table_store")
        return False

    if hasattr(rag, 'query_tables'):
        print("✓ RAGPipeline has query_tables method")
    else:
        print("✗ RAGPipeline missing query_tables method")
        return False

    if hasattr(rag, 'cleaned_documents'):
        print("✓ RAGPipeline has cleaned_documents attribute")
    else:
        print("✗ RAGPipeline missing cleaned_documents")
        return False

    if hasattr(rag, 'extracted_tables'):
        print("✓ RAGPipeline has extracted_tables attribute")
    else:
        print("✗ RAGPipeline missing extracted_tables")
        return False

    # Clean up test database
    if os.path.exists("test_verify.db"):
        os.remove("test_verify.db")

    return True


def main():
    """Run all verification checks"""
    print("\n" + "=" * 70)
    print("TABLE EXTRACTION IMPLEMENTATION VERIFICATION")
    print("=" * 70)

    checks = []

    # Check files
    print("\n" + "=" * 70)
    print("Checking Files...")
    print("=" * 70)

    checks.append(check_file_exists(
        "06_table_extraction.py",
        "06_table_extraction.py (Table extraction module)"
    ))
    checks.append(check_file_exists(
        "05_rag_pipeline.py",
        "05_rag_pipeline.py (Updated pipeline)"
    ))
    checks.append(check_file_exists(
        "TABLE_EXTRACTION_GUIDE.md",
        "TABLE_EXTRACTION_GUIDE.md (Documentation)"
    ))
    checks.append(check_file_exists(
        "EXAMPLE_TABLE_QUERIES.py",
        "EXAMPLE_TABLE_QUERIES.py (Examples)"
    ))
    checks.append(check_file_exists(
        "IMPLEMENTATION_SUMMARY.md",
        "IMPLEMENTATION_SUMMARY.md (Summary)"
    ))

    # Check imports
    checks.append(check_imports())

    # Check functionality
    checks.append(check_table_extractor())
    checks.append(check_sqlite_store())
    checks.append(check_pipeline_integration())

    # Summary
    print("\n" + "=" * 70)
    print("VERIFICATION SUMMARY")
    print("=" * 70)

    passed = sum(checks)
    total = len(checks)

    if all(checks):
        print(f"\n✓ ALL CHECKS PASSED ({passed}/{total})")
        print("\nImplementation is complete and ready to use!")
        print("\nNext steps:")
        print("  1. Review TABLE_EXTRACTION_GUIDE.md for detailed documentation")
        print("  2. Run EXAMPLE_TABLE_QUERIES.py to see usage examples")
        print("  3. Initialize your RAG pipeline with db_path parameter")
        print("  4. Call rag.build() to extract tables and build vector DB")
        return 0
    else:
        print(f"\n✗ SOME CHECKS FAILED ({passed}/{total})")
        print("\nPlease review the errors above and fix any issues.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
