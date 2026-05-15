"""
CLI entry point — run the full pipeline from the command line.

Example::

    python -m mda_pipeline.run_pipeline \
        --parquet mda_merged_sic_naics_24_25.parquet \
        --qdrant-path ./qdrant_db \
        --sqlite-path sec_tables.db \
        --collection sec_mda
"""

from __future__ import annotations

import argparse

from .pipeline import RAGPipeline


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="SEC MD&A pipeline runner")
    p.add_argument("--parquet", required=True,
                   help="Path to mda_merged_sic_naics_24_25.parquet")
    p.add_argument("--qdrant-path", default="./qdrant_db",
                   help="Local Qdrant directory (ignored if --qdrant-url is set)")
    p.add_argument("--qdrant-url", default=None, help="Optional remote Qdrant URL")
    p.add_argument("--qdrant-api-key", default=None)
    p.add_argument("--sqlite-path", default="sec_tables.db")
    p.add_argument("--collection", default="sec_mda")
    p.add_argument("--chunk-size-tokens", type=int, default=512)
    p.add_argument("--overlap-percent", type=int, default=15)
    p.add_argument("--model", default="sentence-transformers/all-MiniLM-L6-v2")
    p.add_argument("--ingest-limit", type=int, default=None,
                   help="Only process the first N filings (debug)")
    p.add_argument("--recreate", action="store_true",
                   help="Drop the Qdrant collection before re-loading")
    p.add_argument("--embedding-batch", type=int, default=64)
    p.add_argument("--device", default=None,
                   help="cuda | cpu (default: auto-detect)")
    p.add_argument("--query", action="append", default=[],
                   help="Optional query to run after build (repeatable)")
    p.add_argument("--top-k", type=int, default=5)
    return p.parse_args()


def main() -> None:
    args = parse_args()

    pipe = RAGPipeline(
        parquet_path=args.parquet,
        collection_name=args.collection,
        chunk_size_tokens=args.chunk_size_tokens,
        overlap_percent=args.overlap_percent,
        embedding_model=args.model,
        qdrant_path=args.qdrant_path,
        qdrant_url=args.qdrant_url,
        qdrant_api_key=args.qdrant_api_key,
        sqlite_path=args.sqlite_path,
        recreate_collection=args.recreate,
        ingest_limit=args.ingest_limit,
        embedding_batch_size=args.embedding_batch,
        device=args.device,
    )
    pipe.build()

    for q in args.query:
        pipe.query(q, top_k=args.top_k)


if __name__ == "__main__":
    main()
