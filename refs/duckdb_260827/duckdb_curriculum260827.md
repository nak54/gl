You are a senior database educator, DuckDB practitioner, and lakehouse curriculum designer.

GOAL
Create a rigorous, hands-on DuckDB learning program combining practical data-wrangling exercises with an implementation-aware deep dive into DuckDB's Apache Iceberg support and significant features introduced in the current calendar year.

VERSION VERIFICATION
Before designing the curriculum, determine today's date and verify the latest stable DuckDB release from current official sources. Do not rely on memory. State the verified stable version and verification date at the top. Separately identify preview, experimental, roadmap, or upcoming-release functionality and never present it as current stable behavior.

TEACHING CONTEXT

Format: Instructor-led workshop
Learners: Data and analytics engineers comfortable with SQL, Parquet, joins, aggregations, and basic Python, but limited DuckDB internals or Iceberg catalog experience
Prerequisites: SQL joins, CTEs, aggregates, window functions; terminal basics; Python helpful but optional; basic awareness of Parquet and object storage
Outcomes: Build repeatable DuckDB analytics workflows; clean multi-format data; diagnose query behavior; work with Iceberg metadata and catalog-managed tables where supported; explain DuckDB-Iceberg implementation boundaries; use significant features released this year
Structure: 6 modules, 60–75 minutes each
Balance: 45% data wrangling/query craftsmanship, 35% Iceberg/lakehouse implementation, 20% recent DuckDB features and performance/debugging
ENVIRONMENT

Execution: Local DuckDB CLI plus Python notebooks on macOS/Linux. Docker available. Internet available for installation/documentation lookup. Core labs must not require paid cloud services
Data assets: Generate small event, customer, and transaction datasets locally in CSV, JSON, and Parquet. Optionally use one small public Parquet dataset
Iceberg infrastructure: Core path should work without a persistent catalog where possible. Advanced path may use Dockerized Iceberg REST catalog such as officially supported Polaris or Lakekeeper setup. Design the core learning path to avoid fragile or paid infrastructure. If Iceberg writes require a REST catalog, credentials, cloud services, or extra infrastructure, provide a clearly labeled advanced path and a simpler read-only/local fallback where feasible
DATA-WRANGLING TRACK

Topics: CSV/Parquet/JSON ingestion; schema inference and repair; NULL handling; regex/string cleanup; joins; windows; list/struct/JSON operations; pivot/unpivot; time-series aggregation; deduplication; COPY; partitioned Parquet output; reusable SQL patterns; EXPLAIN and EXPLAIN ANALYZE
Performance/internals: Predicate/projection pushdown, Parquet row-group pruning, vectorized execution concepts, profiling, memory-aware query design
Exercise style: Each module starts with imperfect raw data and ends with a validated analytical artifact. Include deliberate data-quality defects and at least one performance problem to diagnose. Use realistic, progressively messy datasets. Prefer exercises that produce an inspectable artifact, measurable result, validation check, or performance observation rather than isolated syntax drills
ICEBERG IMPLEMENTATION DEEP DIVE

Questions to answer: How DuckDB reads Iceberg metadata/data; path-based read-only behavior versus catalog-managed writes; current format-version support; schema evolution; partition transforms; MERGE/UPDATE/DELETE; time travel; table properties; metadata functions; V3 features; limitations and interoperability
Comparison dimensions: Architecture and responsibility boundaries; format/spec vs SQL engine/library; catalog interaction; commits; DDL/DML; delete semantics; partitioning; schema evolution; time travel; nested types; supported backends; interoperability and operational complexity
Catalog/platform targets: Generic Iceberg REST Catalog first, then short examples for Apache Polaris and Lakekeeper when currently supported. Mention cloud-specific catalogs only when current official DuckDB docs support them
Explicitly distinguish:

Apache Iceberg as an open table format/specification
Upstream Apache Iceberg libraries/reference implementations and ecosystem behavior
DuckDB's Iceberg extension and DuckDB SQL-engine integration
Do not loosely describe DuckDB's implementation as equivalent to the complete Apache Iceberg ecosystem. Compare capability by capability using current official documentation.

Create a version-aware support matrix covering where applicable:

Iceberg format/spec versions
Direct/path-based table reads
Catalog-managed reads and writes
Iceberg REST Catalog requirements
CREATE/DROP SCHEMA and TABLE
INSERT
UPDATE and DELETE
MERGE INTO
ALTER TABLE and schema evolution
Partition transforms and partition evolution
Delete-file semantics and merge-on-read/copy-on-write behavior
Time travel and snapshots
Metadata inspection
Table properties
Nested/semi-structured types and relevant V3 features
Supported REST catalogs/cloud integrations
Interoperability with other engines/tools
Known limitations or unsupported behavior
For every matrix row classify the current state as exactly one of: Supported | Partial | Unsupported | Preview | Needs verification. Include the DuckDB version/extension context and an official source for every version-sensitive claim.

Include runnable Iceberg labs demonstrating both supported behavior and at least one meaningful implementation boundary or limitation. Never invent SQL syntax or claim parity merely to complete an exercise.

RECENT DUCKDB FEATURES
Using the verified current date, review official DuckDB release material from January 1 of the current calendar year through today.

Feature areas to prioritize: Core SQL/types, CLI, storage/optimizer/concurrency, semi-structured data, geometry/spatial foundations, lakehouse extensions, Iceberg, client/server capabilities, notable connectors
Target feature count: 8–12 high-value features released during the current calendar year
Stable/preview policy: Stable features first. Put experimental, preview, and upcoming-release capabilities in a separate appendix and never present them as current stable functionality
Select high-value features rather than dumping a changelog. For each feature provide:

Feature name
Release/version in which it became available
Core vs extension-specific classification
Stable, experimental, preview, or upcoming status
Why it matters to a DuckDB practitioner
A small runnable exercise when appropriate
Official source
Pay special attention to current-year changes that materially affect SQL, data types, storage, optimizer/concurrency, CLI workflows, file formats, semi-structured data, connectors, lakehouse formats, Iceberg, and client/server usage when verified and relevant.

LESSON DESIGN

Required lesson pattern: Objectives → prerequisites/setup → short concept primer → worked demo → guided lab → independent challenge → expected output/checks → troubleshooting → debrief → stretch task
Assessment/checks: Exact row-count/schema checks, SQL assertions, expected aggregates, explain-plan observations, before/after comparisons, 2–3 concept questions per module
Difficulty progression: Modules 1–2 intermediate fundamentals; 3–4 advanced wrangling/performance; 5–6 lakehouse/Iceberg and current-feature synthesis
Every hands-on lab must contain:

Learning objectives
Prerequisites and setup
A concise concept explanation
Exact commands, SQL, or code
A guided exercise
An independent challenge
Expected output or an objective validation check
Common errors and troubleshooting
Debrief questions
A stretch exercise
Cleanup/reset instructions where relevant
Keep datasets and workloads small enough to run on an ordinary laptop unless the stated environment explicitly supports more.

SOURCE AND ACCURACY RULES

Verify against current official DuckDB docs, release calendar, release/engineering material, and DuckDB Iceberg extension docs
Use official Apache Iceberg docs/spec for upstream behavior
Record version and verification date
Cite version-sensitive claims and mark unresolved items Needs verification
Prefer current documentation over old tutorials or third-party summaries
If official sources conflict, identify the discrepancy rather than silently reconciling it
If a capability cannot be verified, mark it Needs verification and do not make it a required lab step
Do not claim you executed commands or validated outputs unless tools available in the current environment actually performed that execution
OUTPUT PACKAGE
At minimum return:

Course map: sequence, duration, prerequisites, outcomes, lab artifact for each module
Current DuckDB baseline: verified stable version, verification date, official sources
Detailed hands-on lessons with runnable commands/code, expected checks, troubleshooting, challenges
DuckDB vs Apache Iceberg implementation/support matrix
Current-year DuckDB feature table with version, status, why it matters, lesson/lab tie-in
Simplest setup path plus optional advanced Iceberg REST-catalog setup when needed
Learner-facing exercises separated from solution/instructor notes
Official-source reference section
Constraints: No fabricated syntax; no stale version assumptions; no paid-cloud dependency for core labs; no preview behavior presented as stable; no claiming full Apache Iceberg parity without capability-by-capability verification.

QUALITY GATE
Before finalizing:

Re-check every version number and release-status claim
Re-check every Iceberg support-matrix entry
Verify every SQL statement that depends on recent or extension-specific behavior
Distinguish Iceberg format/spec behavior from DuckDB-specific implementation behavior
Separate current stable behavior from preview/upcoming functionality
Flag environment-dependent steps
Ensure substantial hands-on data-wrangling practice remains in the course rather than allowing Iceberg theory to dominate
Remove fabricated, stale, contradictory, or unverified claims
Return the finished curriculum directly.