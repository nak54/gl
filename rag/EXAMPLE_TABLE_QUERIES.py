"""
Example Usage: Table Extraction and Querying

This file demonstrates how to use the new table extraction functionality
in your RAG pipeline.
"""

from rag_pipeline import RAGPipeline
from table_extraction import TableExtractor, SQLiteTableStore
import json


def example_1_basic_pipeline():
    """Example 1: Basic pipeline setup with table extraction"""
    print("=" * 70)
    print("EXAMPLE 1: Basic Pipeline with Table Extraction")
    print("=" * 70)

    # Initialize pipeline with SQLite database
    rag = RAGPipeline(
        parquet_path="mda_24_25_merged.parquet",
        collection_name="sec-mda-filings",
        db_path="sec_tables.db"
    )

    # Build pipeline (includes table extraction)
    print("\nBuilding pipeline...")
    stats = rag.build()

    print(f"\n✓ Pipeline built successfully")
    print(f"  Documents ingested: {stats['documents_ingested']}")
    print(f"  Tables extracted: {stats['tables_extracted']}")
    print(f"  Text chunks created: {stats['chunks_created']}")
    print(f"  Embeddings generated: {stats['embeddings_generated']}")
    print(f"  Database file: {stats['database_file']}")


def example_2_query_text():
    """Example 2: Query text content from vector database"""
    print("\n" + "=" * 70)
    print("EXAMPLE 2: Query Text Content (Vector Database)")
    print("=" * 70)

    # Load existing pipeline
    rag = RAGPipeline(
        parquet_path="mda_24_25_merged.parquet",
        db_path="sec_tables.db"
    )

    # Assuming pipeline is already built
    # rag.build()

    # Query text embeddings
    queries = [
        "What are the main risk factors?",
        "How has revenue changed year-over-year?",
        "Describe the competitive landscape"
    ]

    for query in queries:
        print(f"\n📝 Query: {query}")
        result = rag.query(query, top_k=3)

        print(f"   Found {result.num_results} results in {result.retrieval_time:.3f}s")

        for i, res in enumerate(result.results, 1):
            print(f"\n   {i}. Score: {res.score:.4f}")
            print(f"      Company: {res.metadata.get('company')}")
            print(f"      Filing Date: {res.metadata.get('filing_date')}")
            print(f"      Text: {res.text[:150]}...")


def example_3_query_tables():
    """Example 3: Query structured table data from SQLite"""
    print("\n" + "=" * 70)
    print("EXAMPLE 3: Query Tables (SQLite Database)")
    print("=" * 70)

    # Load existing pipeline
    rag = RAGPipeline(
        parquet_path="mda_24_25_merged.parquet",
        db_path="sec_tables.db"
    )

    # Query all tables
    print("\n1. Get all available tables:")
    tables = rag.query_tables(limit=5)
    print(f"   Found {len(tables)} tables")

    for table in tables[:3]:
        print(f"\n   Table: {table['table_title']}")
        print(f"   - ID: {table['table_id']}")
        print(f"   - CIK: {table['cik']}")
        print(f"   - Year: {table['year']}")

        # Parse and display table data
        if table['table_data']:
            data = json.loads(table['table_data'])
            print(f"   - Rows: {len(data)}")
            if data:
                print(f"   - Columns: {len(data[0])}")

    # Query tables by CIK
    if tables:
        cik = tables[0]['cik']
        print(f"\n2. Get tables for CIK {cik}:")
        cik_tables = rag.query_tables(cik=cik, limit=10)
        print(f"   Found {len(cik_tables)} tables")

        for table in cik_tables[:2]:
            print(f"   - {table['table_title']}")

    # Query tables by year
    print(f"\n3. Get tables from year 2024:")
    year_tables = rag.query_tables(year="2024", limit=10)
    print(f"   Found {len(year_tables)} tables")

    for table in year_tables[:2]:
        print(f"   - {table['table_id']}: {table['table_title']}")


def example_4_combined_search():
    """Example 4: Combined text and table search"""
    print("\n" + "=" * 70)
    print("EXAMPLE 4: Combined Text + Table Search")
    print("=" * 70)

    # Load existing pipeline
    rag = RAGPipeline(
        parquet_path="mda_24_25_merged.parquet",
        db_path="sec_tables.db"
    )

    # Search for text
    query = "revenue growth by segment"
    print(f"\nSearching for: '{query}'")

    text_result = rag.query(query, top_k=3)
    print(f"\n1. Text Search Results ({len(text_result.results)} matches):")

    # Extract metadata from text results
    relevant_ciks = set()
    relevant_years = set()

    for res in text_result.results:
        print(f"\n   Text Match (Score: {res.score:.4f}):")
        print(f"   - Company: {res.metadata.get('company')}")
        print(f"   - CIK: {res.metadata.get('cik')}")
        print(f"   - Year: {res.metadata.get('filing_date', '')[:4] if res.metadata.get('filing_date') else 'N/A'}")

        relevant_ciks.add(res.metadata.get('cik'))
        year = res.metadata.get('filing_date', '')[:4]
        if year:
            relevant_years.add(year)

    # Now find related tables
    print(f"\n2. Related Tables from Same Companies:")
    for cik in list(relevant_ciks)[:2]:  # Check first 2 CIKs
        tables = rag.query_tables(cik=cik, limit=5)
        print(f"\n   Tables for CIK {cik}: {len(tables)} found")

        for table in tables[:3]:
            print(f"   - {table['table_title']}")
            if table['table_data']:
                data = json.loads(table['table_data'])
                print(f"     ({len(data)} rows, {len(data[0]) if data else 0} columns)")


def example_5_direct_database_access():
    """Example 5: Direct SQLite database access"""
    print("\n" + "=" * 70)
    print("EXAMPLE 5: Direct SQLite Database Access")
    print("=" * 70)

    import sqlite3

    db_path = "sec_tables.db"

    # Connect to database
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    print(f"\nConnected to {db_path}")

    # Get database statistics
    print("\n1. Database Statistics:")

    cursor.execute("SELECT COUNT(*) FROM extracted_tables")
    total_tables = cursor.fetchone()[0]
    print(f"   Total tables: {total_tables}")

    cursor.execute("SELECT COUNT(DISTINCT cik) FROM extracted_tables")
    unique_ciks = cursor.fetchone()[0]
    print(f"   Unique CIKs: {unique_ciks}")

    cursor.execute("SELECT COUNT(DISTINCT year) FROM extracted_tables")
    unique_years = cursor.fetchone()[0]
    print(f"   Years covered: {unique_years}")

    # Query specific tables
    print("\n2. Sample Queries:")

    # Get tables by year
    cursor.execute("""
        SELECT table_id, table_title, cik, year
        FROM extracted_tables
        WHERE year = '2024'
        LIMIT 3
    """)

    print("\n   Tables from 2024:")
    for row in cursor.fetchall():
        table_id, title, cik, year = row
        print(f"   - {title} (CIK: {cik})")

    # Get tables with specific SIC code
    cursor.execute("""
        SELECT DISTINCT sic, COUNT(*) as count
        FROM extracted_tables
        WHERE sic IS NOT NULL AND sic != ''
        GROUP BY sic
        LIMIT 5
    """)

    print("\n   SIC Codes represented:")
    for sic, count in cursor.fetchall():
        print(f"   - SIC {sic}: {count} tables")

    conn.close()
    print("\n✓ Database connection closed")


def example_6_table_export():
    """Example 6: Export tables to CSV/Excel"""
    print("\n" + "=" * 70)
    print("EXAMPLE 6: Export Tables to CSV")
    print("=" * 70)

    try:
        import pandas as pd
    except ImportError:
        print("pandas not installed. Install with: pip install pandas")
        return

    import sqlite3

    db_path = "sec_tables.db"

    # Load all tables into a DataFrame
    conn = sqlite3.connect(db_path)

    print(f"\nLoading tables from {db_path}...")

    df = pd.read_sql_query(
        "SELECT table_id, table_title, cik, sic, naics, year FROM extracted_tables",
        conn
    )

    conn.close()

    print(f"Loaded {len(df)} tables")

    # Save to CSV
    csv_file = "sec_tables_export.csv"
    df.to_csv(csv_file, index=False)
    print(f"\n✓ Exported to {csv_file}")

    # Show summary
    print("\n📊 Summary by Year:")
    print(df.groupby('year').size())

    print("\n📊 Summary by SIC Code:")
    print(df.groupby('sic').size().head(10))

    # Optional: Save to Excel with formatting
    try:
        excel_file = "sec_tables_export.xlsx"
        df.to_excel(excel_file, index=False)
        print(f"\n✓ Also exported to {excel_file}")
    except ImportError:
        print("\nopenpyxl not installed for Excel export")


def example_7_table_statistics():
    """Example 7: Detailed table statistics"""
    print("\n" + "=" * 70)
    print("EXAMPLE 7: Table Statistics and Analysis")
    print("=" * 70)

    rag = RAGPipeline(
        parquet_path="mda_24_25_merged.parquet",
        db_path="sec_tables.db"
    )

    stats = rag.table_store.get_statistics()

    print("\nDatabase Statistics:")
    for key, value in stats.items():
        print(f"  {key}: {value}")

    # Additional analysis
    import sqlite3
    conn = sqlite3.connect("sec_tables.db")
    cursor = conn.cursor()

    # Average tables per document
    cursor.execute("""
        SELECT AVG(table_count)
        FROM (
            SELECT source_document_id, COUNT(*) as table_count
            FROM extracted_tables
            GROUP BY source_document_id
        )
    """)
    avg_tables = cursor.fetchone()[0]
    print(f"\n  Avg tables per document: {avg_tables:.1f}")

    # Distribution by metadata
    cursor.execute("""
        SELECT
            CASE WHEN cik IS NOT NULL AND cik != '' THEN 'Has CIK' ELSE 'No CIK' END as has_cik,
            COUNT(*) as count
        FROM extracted_tables
        GROUP BY has_cik
    """)

    print("\n  CIK Coverage:")
    for label, count in cursor.fetchall():
        pct = (count / stats['total_tables'] * 100) if stats['total_tables'] > 0 else 0
        print(f"    {label}: {count} ({pct:.1f}%)")

    conn.close()


if __name__ == "__main__":
    print("\n" + "=" * 70)
    print("TABLE EXTRACTION EXAMPLES")
    print("=" * 70)

    # Uncomment the examples you want to run:

    # example_1_basic_pipeline()          # Build pipeline with table extraction
    # example_2_query_text()              # Query text from vector DB
    # example_3_query_tables()            # Query tables from SQLite
    # example_4_combined_search()         # Combined text + table search
    # example_5_direct_database_access()  # Raw SQLite queries
    # example_6_table_export()            # Export to CSV/Excel
    # example_7_table_statistics()        # Statistics and analysis

    print("\n" + "=" * 70)
    print("To run examples, uncomment them in the __main__ section")
    print("=" * 70)
