# DuckDB Curriculum: Hands-on Data Wrangling, Iceberg, and 2026 Features

**Verification date:** August 27, 2026  
**Stable curriculum baseline:** DuckDB 1.5.5, released July 22, 2026  
**Current feature line:** 1.5.x  
**Current LTS line:** 1.4.x, with 1.4.5 LTS released June 17, 2026  
**Upcoming, not stable baseline:** DuckDB 1.5.6 and 2.0.0 are listed as future/upcoming releases and should not be taught as current stable behavior.

Official baseline source: DuckDB release calendar: https://duckdb.org/release_calendar

**Execution note:** These labs are written for local DuckDB CLI plus Python notebooks. The curriculum does not claim commands were executed in the ChatGPT runtime.

---

## 1. Course Map

| Module | Duration | Level | Main focus | Lab artifact |
|---|---:|---|---|---|
| 1. Build a reliable local DuckDB project | 60 min | Intermediate | CSV, JSON, Parquet ingestion; staging as strings; schema repair; NULL handling; validation checks | `customers_curated`, `transactions_raw`, first Parquet file |
| 2. Query craftsmanship for messy analytics | 70 min | Intermediate | Deduplication, regex cleanup, joins, window functions, orphan detection, revenue mart | `mart_customer_revenue` |
| 3. Semi-structured data and time-series analytics | 70 min | Advanced | JSON/STRUCT access, list aggregation, pivot/unpivot, daily event aggregation | `events_clean`, `mart_event_pivot`, customer journeys |
| 4. Performance and internals you can observe | 75 min | Advanced | `EXPLAIN`, `EXPLAIN ANALYZE`, projection/predicate pushdown, Parquet partitioned writes, row-group and file pruning | partitioned Parquet event dataset and profiling worksheet |
| 5. Iceberg read path and metadata boundaries | 75 min | Advanced lakehouse | DuckDB Iceberg extension, path-based read-only scans, snapshots, metadata functions, ACID/version-hint boundary | Iceberg metadata notebook and boundary notes |
| 6. REST-catalog Iceberg writes plus 2026 feature synthesis | 75 min | Advanced synthesis | Catalog-managed writes, DDL/DML, MERGE, schema evolution, current-year DuckDB features | advanced REST-catalog demo plus final support matrix |

The course balance is designed around approximately 45% data wrangling/query craftsmanship, 35% Iceberg/lakehouse implementation, and 20% recent DuckDB features plus performance/debugging.

---

## 2. Conceptual Boundary: Apache Iceberg vs DuckDB's Iceberg Extension

| Layer | What it is | What it is responsible for |
|---|---|---|
| Apache Iceberg | Open table format/specification | Table metadata, manifests, snapshots, schema/partition evolution concepts, delete files, and format-version rules |
| Apache Iceberg ecosystem/reference libraries | Java/Python/etc. implementations and catalogs used by engines | Catalog APIs, commit behavior, metadata management, delete semantics, engine integration |
| DuckDB Iceberg extension | DuckDB extension that reads and, when attached to a REST catalog, writes Iceberg tables | DuckDB SQL integration, `iceberg_scan`, metadata functions, REST-catalog DDL/DML, local query execution |

Teach this distinction repeatedly: DuckDB's Iceberg extension is not equivalent to the entire Apache Iceberg ecosystem. Compare capability by capability.

Official sources:

- DuckDB Iceberg overview: https://duckdb.org/docs/current/core_extensions/iceberg/overview.html
- Apache Iceberg specification: https://iceberg.apache.org/spec/
- Apache Iceberg REST catalog specification: https://iceberg.apache.org/rest-catalog-spec/

---

## 3. Simplest Setup Path

### 3.1 Local project setup

```bash
mkdir -p duckdb-workshop/{data/raw,data/curated,data/marts,out,iceberg,tmp}
cd duckdb-workshop

python3 -m venv .venv
source .venv/bin/activate

python -m pip install duckdb duckdb-cli pandas pyarrow
duckdb --version
```

Inside DuckDB:

```sql
SELECT version();

INSTALL iceberg;
LOAD iceberg;

-- Refresh installed extensions when internet access is available.
UPDATE EXTENSIONS;
```

Official sources:

- DuckDB Iceberg overview: https://duckdb.org/docs/current/core_extensions/iceberg/overview.html
- DuckDB 1.5.0 release announcement: https://duckdb.org/2026/03/09/announcing-duckdb-150

### 3.2 Generate local lab data

Create intentionally imperfect raw data:

```bash
cat > generate_lab_data.py <<'PY'
from pathlib import Path
import csv
import json

root = Path("data/raw")
root.mkdir(parents=True, exist_ok=True)

customers = [
    {"customer_id": "101", "email": "ALICE@example.COM ", "country": "us", "signup_ts": "2026-01-03 10:00:00", "plan": "pro"},
    {"customer_id": "102", "email": "bob@example.com", "country": "ca", "signup_ts": "2026-01-04 11:30:00", "plan": "free"},
    {"customer_id": "103", "email": "carol_at_example.com", "country": "US", "signup_ts": "2026-01-05 09:00:00", "plan": "pro"},
    {"customer_id": "104", "email": " dan@example.com ", "country": "gb", "signup_ts": "", "plan": "team"},
    {"customer_id": "105", "email": "eve@example.com", "country": "us", "signup_ts": "2026-01-07 08:15:00", "plan": ""},
    {"customer_id": "102", "email": "bob.duplicate@example.com", "country": "CA", "signup_ts": "2026-01-06 07:00:00", "plan": "free"},
    {"customer_id": "106", "email": "frank@example.com", "country": "de", "signup_ts": "2026-01-08 12:00:00", "plan": "team"},
]

transactions = [
    {"txn_id": "t001", "customer_id": "101", "txn_ts": "2026-02-01 09:00:00", "amount_text": "$120.00", "currency": "usd", "status": "captured"},
    {"txn_id": "t002", "customer_id": "101", "txn_ts": "2026-02-02 10:00:00", "amount_text": "15.50", "currency": "usd", "status": "captured"},
    {"txn_id": "t003", "customer_id": "102", "txn_ts": "2026-02-02 10:05:00", "amount_text": "CAD 22.00", "currency": "cad", "status": "captured"},
    {"txn_id": "t004", "customer_id": "103", "txn_ts": "2026-02-03 12:00:00", "amount_text": "not_available", "currency": "usd", "status": "failed"},
    {"txn_id": "t005", "customer_id": "104", "txn_ts": "2026-02-04 12:00:00", "amount_text": "75", "currency": "gbp", "status": "captured"},
    {"txn_id": "t006", "customer_id": "105", "txn_ts": "2026-02-05 13:00:00", "amount_text": "-9.99", "currency": "usd", "status": "refunded"},
    {"txn_id": "t006", "customer_id": "105", "txn_ts": "2026-02-05 13:05:00", "amount_text": "-9.99", "currency": "usd", "status": "refunded"},
    {"txn_id": "t007", "customer_id": "999", "txn_ts": "2026-02-06 14:00:00", "amount_text": "50", "currency": "usd", "status": "captured"},
    {"txn_id": "t008", "customer_id": "106", "txn_ts": "2026-02-06 15:00:00", "amount_text": "225.25", "currency": "eur", "status": "captured"},
]

events = [
    {"event_id": "e001", "customer_id": 101, "event_ts": "2026-02-01T08:59:30", "event_type": "page_view", "properties": {"route": "/pricing", "campaign": "winter"}},
    {"event_id": "e002", "customer_id": 101, "event_ts": "2026-02-01T09:01:15", "event_type": "purchase", "properties": {"order_id": "t001", "amount": 120.00}},
    {"event_id": "e003", "customer_id": 102, "event_ts": "2026-02-02T10:00:00", "event_type": "page_view", "properties": {"route": "/docs"}},
    {"event_id": "e004", "customer_id": 102, "event_ts": "2026-02-02T10:10:00", "event_type": "signup", "properties": {}},
    {"event_id": "e005", "customer_id": 103, "event_ts": "not-a-date", "event_type": "click", "properties": {"route": "/bad"}},
    {"event_id": "e006", "customer_id": 104, "event_ts": "2026-02-04T12:03:00", "event_type": "purchase", "properties": {"order_id": "t005", "amount": 75}},
    {"event_id": "e007", "customer_id": 105, "event_ts": "2026-02-05T13:00:00", "event_type": "refund", "properties": {"order_id": "t006", "amount": -9.99}},
    {"event_id": "e008", "customer_id": 106, "event_ts": "2026-02-06T15:01:00", "event_type": "purchase", "properties": {"order_id": "t008", "amount": 225.25}},
    {"event_id": "e008", "customer_id": 106, "event_ts": "2026-02-06T15:02:00", "event_type": "purchase", "properties": {"order_id": "t008", "amount": 225.25}},
    {"event_id": "e009", "customer_id": 999, "event_ts": "2026-02-06T16:00:00", "event_type": "purchase", "properties": {"order_id": "t007", "amount": 50}},
]

with (root / "customers.csv").open("w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=customers[0].keys())
    w.writeheader()
    w.writerows(customers)

with (root / "transactions.csv").open("w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=transactions[0].keys())
    w.writeheader()
    w.writerows(transactions)

with (root / "events.jsonl").open("w") as f:
    for row in events:
        f.write(json.dumps(row) + "\n")

print("Wrote data/raw/customers.csv, data/raw/transactions.csv, data/raw/events.jsonl")
PY

python generate_lab_data.py
```

Create the raw Parquet asset:

```bash
duckdb workshop.duckdb <<'SQL'
CREATE OR REPLACE TABLE raw_transactions_csv AS
SELECT *
FROM read_csv('data/raw/transactions.csv', header = true, all_varchar = true);

COPY raw_transactions_csv
TO 'data/raw/transactions.parquet'
(FORMAT parquet);
SQL
```

Official DuckDB source for data ingestion: https://duckdb.org/docs/current/clients/python/data_ingestion

---

# 4. Learner-facing Lessons

## Module 1 - Build a Reliable Local DuckDB Project

### Objectives

By the end of this module, learners can ingest messy CSV, JSON, and Parquet files; stage questionable data as strings; repair types safely; flag data-quality defects; and create a curated table with objective row-count and schema checks.

### Prerequisites/setup

Run the project setup and data generator above. Confirm:

```bash
ls data/raw
duckdb workshop.duckdb -c "SELECT version();"
```

### Concept primer

DuckDB's type inference is convenient, but production analytics workflows often need a two-step pattern: first ingest into a permissive staging table, then explicitly cast and validate. This prevents one malformed row from silently shaping the whole schema.

### Worked demo

```bash
duckdb workshop.duckdb <<'SQL'
CREATE OR REPLACE TABLE customers_raw AS
SELECT *
FROM read_csv(
    'data/raw/customers.csv',
    header = true,
    all_varchar = true
);

CREATE OR REPLACE TABLE events_raw AS
SELECT *
FROM read_json('data/raw/events.jsonl');

CREATE OR REPLACE TABLE transactions_raw AS
SELECT *
FROM read_parquet('data/raw/transactions.parquet');

SELECT 'customers_raw' AS table_name, count(*) AS row_count FROM customers_raw
UNION ALL
SELECT 'events_raw', count(*) FROM events_raw
UNION ALL
SELECT 'transactions_raw', count(*) FROM transactions_raw;
SQL
```

### Guided lab

```bash
duckdb workshop.duckdb <<'SQL'
CREATE OR REPLACE TABLE customers_curated AS
WITH typed AS (
    SELECT
        row_number() OVER () AS source_row_number,
        TRY_CAST(customer_id AS BIGINT) AS customer_id,
        lower(trim(email)) AS email,
        upper(trim(country)) AS country,
        TRY_CAST(NULLIF(signup_ts, '') AS TIMESTAMP) AS signup_ts,
        COALESCE(NULLIF(lower(trim(plan)), ''), 'free') AS plan,
        regexp_matches(lower(trim(email)), '^[^@]+@[^@]+\.[^@]+$') AS email_valid
    FROM customers_raw
),
deduped AS (
    SELECT *,
           row_number() OVER (
               PARTITION BY customer_id
               ORDER BY signup_ts DESC NULLS LAST, source_row_number DESC
           ) AS customer_rank
    FROM typed
)
SELECT
    customer_id,
    email,
    country,
    signup_ts,
    plan,
    email_valid,
    source_row_number
FROM deduped
WHERE customer_rank = 1;

DESCRIBE customers_curated;

SELECT count(*) AS curated_customers FROM customers_curated;

SELECT count(*) AS invalid_email_rows
FROM customers_curated
WHERE NOT email_valid;

SELECT *
FROM customers_curated
ORDER BY customer_id;
SQL
```

### Expected checks

```sql
SELECT count(*) FROM customers_raw;
-- expected: 7

SELECT count(*) FROM customers_curated;
-- expected: 6

SELECT count(*) FROM customers_curated WHERE NOT email_valid;
-- expected: 1

DESCRIBE customers_curated;
-- expected core columns:
-- customer_id BIGINT
-- email VARCHAR
-- country VARCHAR
-- signup_ts TIMESTAMP
-- plan VARCHAR
-- email_valid BOOLEAN
```

### Independent challenge

Create a quarantine table:

```sql
CREATE OR REPLACE TABLE customers_quarantine AS
SELECT *
FROM customers_curated
WHERE NOT email_valid OR signup_ts IS NULL;
```

Add a reason column such as `bad_email`, `missing_signup_ts`, or `both`.

### Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| `Conversion Error` | Direct cast instead of `TRY_CAST` | Use `TRY_CAST` during cleanup |
| Email validation seems too strict | Regex is intentionally simple | Treat it as a teaching check, not full RFC email validation |
| Duplicate customer appears | Missing deduplication window | Use `row_number() OVER (PARTITION BY customer_id ...)` |

### Debrief questions

1. Why is `all_varchar = true` useful for first-pass ingestion?
2. Which defects did we fix, and which did we only flag?
3. Why did customer `102` resolve to the later duplicate row?

### Stretch task

Add a `country_group` column: `NA`, `EU`, or `OTHER`. Keep the original country code.

### Cleanup/reset

```sql
DROP TABLE IF EXISTS customers_curated;
DROP TABLE IF EXISTS customers_quarantine;
```

---

## Module 2 - Query Craftsmanship for Messy Analytics

### Objectives

Learners will clean monetary values, deduplicate transactions, join dimensions to facts, detect orphan records, and build a validated revenue mart.

### Prerequisites/setup

Module 1 completed.

### Concept primer

Analytical facts are rarely clean. The safe workflow is: stage, parse, deduplicate, detect orphan keys, then aggregate. Window functions are the cleanest tool for deterministic deduplication.

### Worked demo

```bash
duckdb workshop.duckdb <<'SQL'
CREATE OR REPLACE TABLE transactions_clean AS
WITH typed AS (
    SELECT
        txn_id,
        TRY_CAST(customer_id AS BIGINT) AS customer_id,
        TRY_CAST(txn_ts AS TIMESTAMP) AS txn_ts,
        TRY_CAST(
            regexp_replace(amount_text, '[^0-9.\-]', '', 'g')
            AS DECIMAL(12, 2)
        ) AS amount,
        upper(trim(currency)) AS currency,
        lower(trim(status)) AS status
    FROM transactions_raw
),
deduped AS (
    SELECT *,
           row_number() OVER (
               PARTITION BY txn_id
               ORDER BY txn_ts DESC NULLS LAST
           ) AS txn_rank
    FROM typed
)
SELECT
    txn_id,
    customer_id,
    txn_ts,
    amount,
    currency,
    status
FROM deduped
WHERE txn_rank = 1;

SELECT count(*) AS raw_rows FROM transactions_raw;
SELECT count(*) AS clean_deduped_rows FROM transactions_clean;

SELECT *
FROM transactions_clean
ORDER BY txn_id;
SQL
```

### Guided lab

```bash
duckdb workshop.duckdb <<'SQL'
CREATE OR REPLACE TABLE mart_customer_revenue AS
SELECT
    c.customer_id,
    c.email,
    c.country,
    c.plan,
    count(*) FILTER (
        WHERE t.status = 'captured' AND t.amount > 0
    ) AS captured_txns,
    COALESCE(
        sum(t.amount) FILTER (
            WHERE t.status = 'captured' AND t.amount > 0
        ),
        0
    ) AS revenue_local,
    max(t.txn_ts) AS last_txn_ts
FROM customers_curated AS c
LEFT JOIN transactions_clean AS t
    ON c.customer_id = t.customer_id
GROUP BY
    c.customer_id,
    c.email,
    c.country,
    c.plan
ORDER BY revenue_local DESC;

SELECT *
FROM mart_customer_revenue;

SELECT
    t.*
FROM transactions_clean AS t
LEFT JOIN customers_curated AS c
    ON t.customer_id = c.customer_id
WHERE c.customer_id IS NULL
ORDER BY t.txn_id;

SELECT sum(revenue_local) AS total_known_customer_revenue
FROM mart_customer_revenue;
SQL
```

### Expected checks

```sql
SELECT count(*) FROM transactions_raw;
-- expected: 9

SELECT count(*) FROM transactions_clean;
-- expected: 8

SELECT count(*) FROM mart_customer_revenue;
-- expected: 6

SELECT count(*)
FROM transactions_clean AS t
LEFT JOIN customers_curated AS c
    ON t.customer_id = c.customer_id
WHERE c.customer_id IS NULL
  AND t.status = 'captured';
-- expected: 1

SELECT sum(revenue_local) FROM mart_customer_revenue;
-- expected: 457.75
```

### Independent challenge

Rank customers by revenue within country:

```sql
SELECT
    *,
    dense_rank() OVER (
        PARTITION BY country
        ORDER BY revenue_local DESC
    ) AS revenue_rank_in_country
FROM mart_customer_revenue;
```

### Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| `not_available` causes cast failure | Used `CAST` instead of `TRY_CAST` | Use `TRY_CAST` and audit NULL amounts |
| Revenue includes refunded transaction | Missing status and amount filters | Filter `status = 'captured' AND amount > 0` |
| Orphan transaction disappears | Used inner join too early | Use left anti-join pattern for orphan detection |

### Debrief questions

1. Why should we detect orphan facts before final reporting?
2. Why deduplicate before aggregation?
3. What does `FILTER` on aggregates make clearer than nested `CASE` expressions?

### Stretch task

Add a currency conversion dimension table with static rates and produce approximate USD revenue.

### Cleanup/reset

```sql
DROP TABLE IF EXISTS mart_customer_revenue;
```

---

## Module 3 - Semi-structured Data and Time-series Analytics

### Objectives

Learners will parse semi-structured event data, deduplicate event IDs, extract nested fields, build customer journeys, and pivot daily event counts.

### Prerequisites/setup

Modules 1-2 completed.

### Concept primer

DuckDB can work with nested and semi-structured data. The key implementation habit is to inspect the inferred type first, then choose either dot notation for STRUCT-like values or JSON extraction functions for JSON-typed values.

### Worked demo

```bash
duckdb workshop.duckdb <<'SQL'
DESCRIBE events_raw;

SELECT
    typeof(properties) AS properties_type,
    count(*) AS rows
FROM events_raw
GROUP BY properties_type;

SELECT *
FROM events_raw
LIMIT 5;
SQL
```

### Guided lab

```bash
duckdb workshop.duckdb <<'SQL'
CREATE OR REPLACE TABLE events_clean AS
WITH typed AS (
    SELECT
        event_id::VARCHAR AS event_id,
        TRY_CAST(customer_id AS BIGINT) AS customer_id,
        TRY_CAST(event_ts AS TIMESTAMP) AS event_ts,
        lower(event_type::VARCHAR) AS event_type,
        properties,
        row_number() OVER (
            PARTITION BY event_id
            ORDER BY TRY_CAST(event_ts AS TIMESTAMP) DESC NULLS LAST
        ) AS event_rank
    FROM events_raw
)
SELECT
    event_id,
    customer_id,
    event_ts,
    event_type,
    properties
FROM typed
WHERE event_rank = 1;

CREATE OR REPLACE TABLE mart_daily_events AS
SELECT
    CAST(date_trunc('day', event_ts) AS DATE) AS event_day,
    event_type,
    count(*) AS event_count
FROM events_clean
WHERE event_ts IS NOT NULL
GROUP BY event_day, event_type
ORDER BY event_day, event_type;

CREATE OR REPLACE TABLE mart_event_pivot AS
PIVOT mart_daily_events
ON event_type
USING sum(event_count)
GROUP BY event_day;

SELECT * FROM events_clean ORDER BY event_id;
SELECT * FROM mart_daily_events ORDER BY event_day, event_type;
SELECT * FROM mart_event_pivot ORDER BY event_day;

SELECT
    customer_id,
    list(event_type ORDER BY event_ts) AS customer_journey
FROM events_clean
WHERE event_ts IS NOT NULL
GROUP BY customer_id
ORDER BY customer_id;
SQL
```

If your `properties` column is inferred as STRUCT, try:

```sql
SELECT
    event_id,
    properties.order_id AS order_id,
    properties.amount AS amount
FROM events_clean
WHERE event_type IN ('purchase', 'refund');
```

If it is inferred as JSON in your environment, use JSON extraction instead:

```sql
SELECT
    event_id,
    json_extract_string(properties, '$.order_id') AS order_id,
    TRY_CAST(json_extract(properties, '$.amount') AS DECIMAL(12,2)) AS amount
FROM events_clean
WHERE event_type IN ('purchase', 'refund');
```

### Expected checks

```sql
SELECT count(*) FROM events_raw;
-- expected: 10

SELECT count(*) FROM events_clean;
-- expected: 9

SELECT count(*) FROM events_clean WHERE event_ts IS NULL;
-- expected: 1

SELECT count(*) FROM events_clean
WHERE event_type = 'purchase' AND event_ts IS NOT NULL;
-- expected: 4

SELECT count(*) FROM mart_event_pivot;
-- expected: 5 event days
```

### Independent challenge

Create a funnel table with one row per customer:

```sql
CREATE OR REPLACE TABLE mart_customer_funnel AS
SELECT
    customer_id,
    bool_or(event_type = 'page_view') AS saw_page,
    bool_or(event_type = 'signup') AS signed_up,
    bool_or(event_type = 'purchase') AS purchased,
    min(event_ts) AS first_event_ts,
    max(event_ts) AS last_event_ts
FROM events_clean
WHERE event_ts IS NOT NULL
GROUP BY customer_id;
```

### Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| Dot notation fails on `properties` | Column inferred as JSON, not STRUCT | Use `json_extract_string` |
| Pivot output has NULLs | Some event types absent on some days | Use `COALESCE` in final presentation query |
| Date aggregation changed type unexpectedly | `date_trunc(DATE)` returns a `TIMESTAMP` in DuckDB 1.5.x for DATE input | Cast to `DATE` where required |

DuckDB 1.5.x changed `date_trunc(DATE)` to return a `TIMESTAMP`, with old behavior planned to return in v2.0. Source: https://duckdb.org/2026/03/09/announcing-duckdb-150

### Debrief questions

1. What did DuckDB infer for the nested `properties` column?
2. Why should event deduplication happen before funnel metrics?
3. What is the difference between an event-level table and a customer-level journey table?

### Stretch task

Add a sessionization query: treat a gap of more than 30 minutes as a new session.

### Cleanup/reset

```sql
DROP TABLE IF EXISTS mart_customer_funnel;
```

---

## Module 4 - Performance and Internals You Can Observe

### Objectives

Learners will use `EXPLAIN` and `EXPLAIN ANALYZE`, reason about projection and predicate pushdown, create partitioned Parquet output, and diagnose an intentionally inefficient query.

### Prerequisites/setup

Modules 1-3 completed.

### Concept primer

DuckDB's execution model is vectorized. Practitioners do not need to become engine developers, but they should learn to inspect plans, reduce unnecessary columns, push selective filters early, and avoid producing many tiny files or partitions.

Official sources:

- DuckDB profiling docs: https://duckdb.org/docs/current/sql/statements/profiling
- DuckDB partitioned writes docs: https://duckdb.org/docs/current/data/partitioning/partitioned_writes

### Worked demo

```bash
duckdb workshop.duckdb <<'SQL'
CREATE OR REPLACE TABLE fact_events AS
SELECT
    e.event_id,
    e.customer_id,
    e.event_ts,
    CAST(date_trunc('day', e.event_ts) AS DATE) AS event_day,
    e.event_type,
    c.country,
    c.plan
FROM events_clean AS e
LEFT JOIN customers_curated AS c
    ON e.customer_id = c.customer_id
WHERE e.event_ts IS NOT NULL;

COPY fact_events
TO 'data/curated/events_by_day'
(FORMAT parquet, PARTITION_BY (event_day), OVERWRITE_OR_IGNORE);

SELECT count(*) FROM fact_events;
SQL
```

### Guided lab: bad query vs better query

Bad query:

```sql
EXPLAIN ANALYZE
SELECT *
FROM read_parquet('data/curated/events_by_day/**/*.parquet')
WHERE lower(event_type) = 'purchase';
```

Better query:

```sql
EXPLAIN ANALYZE
SELECT
    event_day,
    customer_id,
    event_type
FROM read_parquet('data/curated/events_by_day/**/*.parquet')
WHERE event_type = 'purchase'
  AND event_day = DATE '2026-02-06';
```

Inspect:

```sql
EXPLAIN
SELECT
    event_day,
    customer_id,
    event_type
FROM read_parquet('data/curated/events_by_day/**/*.parquet')
WHERE event_type = 'purchase'
  AND event_day = DATE '2026-02-06';
```

### Expected observations

Learners should document:

```text
1. The better query reads fewer columns.
2. The partition predicate on event_day can restrict file/partition access.
3. EXPLAIN ANALYZE reports actual cardinalities and runtime.
4. Function-wrapped filters such as lower(event_type) can make filtering less direct than normalized stored values.
```

### Objective checks

```sql
SELECT count(*)
FROM read_parquet('data/curated/events_by_day/**/*.parquet');
-- expected: 8 valid timestamped event rows

SELECT count(*)
FROM read_parquet('data/curated/events_by_day/**/*.parquet')
WHERE event_day = DATE '2026-02-06';
-- expected: 2 rows after event deduplication: customer 106 and orphan customer 999

SELECT count(*)
FROM read_parquet('data/curated/events_by_day/**/*.parquet')
WHERE event_type = 'purchase';
-- expected: 4
```

### Independent challenge

Rewrite this query to avoid unnecessary columns and add a partition predicate:

```sql
SELECT *
FROM read_parquet('data/curated/events_by_day/**/*.parquet')
WHERE event_type IN ('purchase', 'refund');
```

### Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| Too many tiny files | Partitioned a small dataset too finely | Partition by low-cardinality, meaningful fields only |
| `EXPLAIN ANALYZE` takes time | It executes the query | Use plain `EXPLAIN` for plan-only inspection |
| Partition predicate not effective | Predicate not on partition column | Filter directly on `event_day` |

### Debrief questions

1. What changed between `SELECT *` and selecting only needed columns?
2. What is the cost of over-partitioning?
3. Which plan details would you show to a teammate to justify a rewrite?

### Stretch task

Create a larger synthetic event table by repeating the small event table 1,000 times, then compare plan/runtime changes.

### Cleanup/reset

```sql
DROP TABLE IF EXISTS fact_events;
```

---

## Module 5 - Iceberg Read Path and Metadata Boundaries

### Objectives

Learners will install/load the Iceberg extension, read an Iceberg table directly by path, inspect snapshots and metadata, explain why path-based reads are read-only, and describe why version guessing can violate Iceberg's ACID assumptions.

### Prerequisites/setup

Internet access is needed to download DuckDB's small Iceberg sample dataset. This is still a no-paid-cloud core lab.

```bash
curl -L https://duckdb.org/data/iceberg_data.zip -o data/iceberg_data.zip
unzip -q data/iceberg_data.zip -d data
```

### Concept primer

DuckDB's path-based Iceberg reads read table metadata and data files directly. This is simple and useful for local learning, but it is not the same as catalog-managed Iceberg. DuckDB's docs explicitly state that individual Iceberg tables can be read directly from storage using metadata, while the Iceberg REST catalog path is the catalog-managed route that supports writing.

Official source: https://duckdb.org/docs/current/core_extensions/iceberg/overview.html

### Worked demo

```bash
duckdb workshop.duckdb <<'SQL'
INSTALL iceberg;
LOAD iceberg;

SELECT count(*) AS lineitem_count
FROM iceberg_scan(
    'data/iceberg/lineitem_iceberg',
    allow_moved_paths = true
);

SELECT *
FROM iceberg_snapshots('data/iceberg/lineitem_iceberg')
ORDER BY committed_at;

SELECT *
FROM iceberg_metadata('data/iceberg/lineitem_iceberg')
LIMIT 10;
SQL
```

### Guided lab: scan a specific metadata file

```sql
SELECT count(*) AS v1_count
FROM iceberg_scan(
    'data/iceberg/lineitem_iceberg/metadata/v1.metadata.json',
    allow_moved_paths = true
);

SELECT count(*) AS current_table_count
FROM iceberg_scan(
    'data/iceberg/lineitem_iceberg',
    allow_moved_paths = true
);
```

### Expected checks

DuckDB's example docs show two different counts when scanning a specific metadata version versus the current table path:

```sql
SELECT count(*)
FROM iceberg_scan(
    'data/iceberg/lineitem_iceberg',
    allow_moved_paths = true
);
-- expected from DuckDB sample docs: 51793

SELECT count(*)
FROM iceberg_scan(
    'data/iceberg/lineitem_iceberg/metadata/v1.metadata.json',
    allow_moved_paths = true
);
-- expected from DuckDB sample docs: 60175
```

DuckDB's docs also explain that DuckDB does not enable metadata version guessing by default because it can violate Iceberg's ACID properties when the metadata JSON has changed but the version-hint file has not.

### Implementation-boundary exercise

Answer these questions in pairs:

```text
1. Why can DuckDB read this Iceberg table without a catalog?
2. Why is this path-based workflow read-only?
3. What metadata did iceberg_snapshots expose?
4. What would a catalog add that a raw path does not?
5. Why can guessing the latest metadata file be unsafe?
```

### Independent challenge

Create a local DuckDB table from the Iceberg sample and compare the schema:

```sql
CREATE OR REPLACE TABLE local_lineitem_copy AS
SELECT *
FROM iceberg_scan(
    'data/iceberg/lineitem_iceberg',
    allow_moved_paths = true
);

DESCRIBE local_lineitem_copy;

SELECT count(*) FROM local_lineitem_copy;
```

Then explain why `local_lineitem_copy` is now a DuckDB table copy, not an Iceberg table.

### Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| `iceberg_scan` not found | Extension not loaded | `INSTALL iceberg; LOAD iceberg;` |
| Path moved errors | Sample metadata points to old file paths | Use `allow_moved_paths = true` for the sample |
| Different count than expected | Scanned a specific metadata version | Compare current path scan vs metadata-file scan |

### Debrief questions

1. Which parts of the Iceberg table did DuckDB inspect?
2. What does a snapshot represent?
3. What operation would require a catalog?

### Stretch task

Use `iceberg_column_stats` and `iceberg_partition_stats` to inspect file-level statistics and partition statistics:

```sql
SELECT *
FROM iceberg_column_stats('data/iceberg/lineitem_iceberg')
LIMIT 10;

SELECT *
FROM iceberg_partition_stats('data/iceberg/lineitem_iceberg')
LIMIT 10;
```

Official source: https://duckdb.org/docs/current/core_extensions/iceberg/reference

### Cleanup/reset

```sql
DROP TABLE IF EXISTS local_lineitem_copy;
```

---

## Module 6 - REST-catalog Iceberg Writes Plus 2026 Feature Synthesis

### Objectives

Learners will distinguish core DuckDB stable features from extension-specific features, run small current-feature exercises, and optionally use a Dockerized Iceberg REST catalog for DDL/DML labs.

### Prerequisites/setup

Core path needs only local DuckDB. Advanced path needs Docker and a REST catalog.

### Concept primer

DuckDB's Iceberg write path is catalog-managed. Its writing docs say that writing to Iceberg is supported for tables managed by an Iceberg REST catalog, and that writes go through an attached catalog and commit new snapshots. They also say `iceberg_scan` is read-only.

Official source: https://duckdb.org/docs/current/core_extensions/iceberg/writing

### Core stable-feature lab: VARIANT and current SQL behavior

DuckDB 1.5.0 introduced a native `VARIANT` type for semi-structured data and highlighted it as one of the major 1.5.0 features.

Source: https://duckdb.org/2026/03/09/announcing-duckdb-150

```bash
duckdb workshop.duckdb <<'SQL'
CREATE OR REPLACE TABLE variant_lab AS
SELECT
    1 AS id,
    '{"name":"Alice","score":12,"tags":["sql","duckdb"]}'::JSON::VARIANT AS payload
UNION ALL
SELECT
    2 AS id,
    '{"name":"Bob","score":18,"tags":["iceberg"]}'::JSON::VARIANT AS payload;

SELECT
    id,
    payload.name AS name,
    payload.score AS score
FROM variant_lab
ORDER BY id;

SELECT typeof(date_trunc('month', DATE '2026-03-27')) AS date_trunc_type;
SQL
```

Expected:

```text
payload.name values: Alice, Bob
payload.score values: 12, 18
date_trunc_type in DuckDB 1.5.x: TIMESTAMP
```

### Optional advanced path: Dockerized REST catalog

Use this only when Docker is available and the class can tolerate infrastructure setup. Apache Polaris provides an official quickstart that starts Polaris and object storage locally with Docker Compose, creates a quickstart catalog and principal, and exposes the REST API on port 8181. DuckDB's Iceberg catalog docs include Apache Polaris and Lakekeeper examples as REST-catalog targets.

Sources:

- Polaris quickstart: https://polaris.apache.org/guides/quickstart/
- DuckDB Iceberg catalog docs: https://duckdb.org/docs/current/core_extensions/iceberg/catalogs.html

Example attach shape for Polaris:

```sql
INSTALL iceberg;
LOAD iceberg;
LOAD httpfs;

CREATE SECRET polaris_secret (
    TYPE ICEBERG,
    CLIENT_ID 'replace_with_quickstart_client_id',
    CLIENT_SECRET 'replace_with_quickstart_client_secret'
);

ATTACH 'quickstart_catalog' AS polaris_catalog (
    TYPE ICEBERG,
    ENDPOINT 'http://localhost:8181',
    SECRET polaris_secret
);
```

### Advanced guided lab: catalog-managed DDL/DML

```sql
CREATE SCHEMA polaris_catalog.duckdb_workshop;

CREATE TABLE polaris_catalog.duckdb_workshop.events (
    id INTEGER,
    event_name VARCHAR,
    event_time TIMESTAMP
)
WITH ('format-version' = '2');

INSERT INTO polaris_catalog.duckdb_workshop.events VALUES
    (1, 'click', TIMESTAMP '2026-02-01 09:00:00'),
    (2, 'view',  TIMESTAMP '2026-02-01 09:05:00');

CREATE OR REPLACE TEMP TABLE new_events AS
SELECT * FROM (
    VALUES
        (2, 'signup',   TIMESTAMP '2026-02-01 09:06:00'),
        (3, 'purchase', TIMESTAMP '2026-02-01 09:10:00')
) AS t(id, event_name, event_time);

MERGE INTO polaris_catalog.duckdb_workshop.events AS target
USING new_events AS source
USING (id)
WHEN MATCHED THEN
    UPDATE SET event_name = source.event_name,
               event_time = source.event_time
WHEN NOT MATCHED THEN
    INSERT VALUES (source.id, source.event_name, source.event_time);

SELECT *
FROM polaris_catalog.duckdb_workshop.events
ORDER BY id;

SELECT *
FROM iceberg_snapshots(polaris_catalog.duckdb_workshop.events);
```

Expected if REST catalog is configured correctly:

```text
id | event_name | event_time
1  | click      | 2026-02-01 09:00:00
2  | signup     | 2026-02-01 09:06:00
3  | purchase   | 2026-02-01 09:10:00
```

### Advanced boundary lab: delete semantics

DuckDB's docs state that Iceberg `UPDATE` and `DELETE` are currently implemented with merge-on-read positional delete files only and that copy-on-write mode is not supported.

Discussion prompt:

```text
1. What delete files are created by UPDATE/DELETE?
2. Why is merge-on-read not the same as copy-on-write?
3. Which other engines in your organization would need to understand these delete files?
```

### Independent challenge

Create a second Iceberg table partitioned by day:

```sql
CREATE TABLE polaris_catalog.duckdb_workshop.partitioned_events (
    id INTEGER,
    event_name VARCHAR,
    event_time TIMESTAMP
)
PARTITIONED BY (day(event_time));

INSERT INTO polaris_catalog.duckdb_workshop.partitioned_events
SELECT * FROM polaris_catalog.duckdb_workshop.events;

SELECT count(*)
FROM polaris_catalog.duckdb_workshop.partitioned_events;
```

DuckDB currently documents identity, year, month, day, hour, bucket, and truncate partition transforms for Iceberg writes.

### Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| Attach fails | Wrong endpoint, catalog name, or secret | Re-check REST endpoint and credentials |
| Writes fail in core path | No REST catalog attached | Use the advanced path or keep Module 5 read-only |
| `UPDATE`/`DELETE` behavior surprises learners | Merge-on-read positional delete files | Discuss delete-file semantics explicitly |
| Partitioned table file sizing differs from expectation | DuckDB limitation for partitioned Iceberg writes | Review table-property limitation |

### Debrief questions

1. Which operations were local DuckDB operations?
2. Which operations required a REST catalog?
3. Which behavior is DuckDB-specific rather than general Apache Iceberg behavior?

### Stretch task

Use `ALTER TABLE` to add a column and then inspect snapshots:

```sql
ALTER TABLE polaris_catalog.duckdb_workshop.events
ADD COLUMN source VARCHAR DEFAULT 'workshop';

SELECT *
FROM polaris_catalog.duckdb_workshop.events
ORDER BY id;

SELECT *
FROM iceberg_snapshots(polaris_catalog.duckdb_workshop.events);
```

### Cleanup/reset

```sql
DROP TABLE IF EXISTS polaris_catalog.duckdb_workshop.partitioned_events;
DROP TABLE IF EXISTS polaris_catalog.duckdb_workshop.events;
DROP SCHEMA IF EXISTS polaris_catalog.duckdb_workshop;
```

---

# 5. Instructor Notes and Solutions

## Module 1 solution notes

Expected defects:

```text
customers_raw rows: 7
customers_curated rows after dedup: 6
invalid email rows: 1
missing signup timestamp rows: 1
blank plan repaired to free: customer 105
duplicate customer_id 102 resolved to bob.duplicate@example.com
```

Teaching emphasis: do not let type inference become the validation strategy.

## Module 2 solution notes

Expected facts:

```text
transactions_raw rows: 9
transactions_clean rows after txn_id dedup: 8
orphan captured transaction: t007 / customer_id 999
known-customer captured revenue: 457.75
```

Revenue math:

```text
101: 120.00 + 15.50 = 135.50
102: 22.00
104: 75.00
106: 225.25
Total: 457.75
```

## Module 3 solution notes

Expected event metrics:

```text
events_raw rows: 10
events_clean rows after event_id dedup: 9
valid timestamp rows: 8
invalid timestamp rows: 1
valid purchase events: 4
event days represented after filtering valid timestamps: 5
```

Teaching emphasis: nested/semi-structured types are powerful, but learners should always inspect inferred types before writing extraction logic.

## Module 4 solution notes

Expected observations:

```text
SELECT * reads unnecessary columns.
A normalized event_type column avoids lower(event_type) during filtering.
Partition predicate on event_day should narrow files.
EXPLAIN ANALYZE runs the query; EXPLAIN only shows the plan.
```

DuckDB source for `EXPLAIN ANALYZE`: https://duckdb.org/docs/current/guides/meta/explain_analyze

## Module 5 solution notes

Expected Iceberg sample checks from DuckDB's documentation:

```text
current Iceberg table path count: 51793
specific v1 metadata file count: 60175
```

Teaching emphasis: this count difference is useful because it shows snapshots/metadata are central to Iceberg behavior.

## Module 6 solution notes

Expected REST-catalog result after `MERGE`:

```text
id 1 remains click
id 2 changes from view to signup
id 3 is inserted as purchase
final row count: 3
```

Teaching emphasis: REST-catalog writes are advanced-path only; do not teach them as a local-path capability.

---

# 6. DuckDB vs Apache Iceberg Implementation/Support Matrix

Context: DuckDB 1.5.5 plus current Iceberg extension documentation, verified August 27, 2026.

| Capability | Current state | DuckDB 1.5.5 / Iceberg extension context | Official-source basis |
|---|---|---|---|
| Iceberg format/spec versions | Partial | DuckDB Iceberg writes can set `format-version` 2 or 3, and 1.5.1 added several v3 features, but Apache Iceberg v3/v4 includes more than DuckDB documents as supported. | DuckDB writing docs; DuckDB 1.5.1 release notes; Apache Iceberg spec |
| Direct/path-based table reads | Supported | `iceberg_scan` can read an Iceberg table path or metadata file without a catalog. This path is read-only. | DuckDB Iceberg overview and scan docs |
| Catalog-managed reads and writes | Supported | Supported through attached Iceberg REST catalogs. Writes go through the catalog and commit new snapshots. | DuckDB Iceberg writing docs |
| Iceberg REST Catalog requirements | Supported | Required for DuckDB Iceberg writes. Generic REST attach plus specific REST-catalog examples are documented. | DuckDB Iceberg catalog docs |
| `CREATE`/`DROP SCHEMA` and `CREATE`/`DROP TABLE` | Supported | Supported for attached Iceberg catalogs. | DuckDB writing docs |
| `INSERT` | Supported | Supported into catalog-managed Iceberg tables. | DuckDB writing docs |
| `UPDATE` and `DELETE` | Partial | Supported for partitioned and unpartitioned Iceberg tables, but only using merge-on-read positional deletes. Copy-on-write is not supported. | DuckDB writing docs and limitations |
| `MERGE INTO` | Supported | Supported for catalog-managed Iceberg tables; documented in current writing docs and highlighted in 1.5.3 release notes. | DuckDB writing docs and 1.5.3 notes |
| `ALTER TABLE` and schema evolution | Partial | Current docs support listed operations such as add/drop/rename column and column type/default/nullability changes. Do not claim complete Iceberg ecosystem parity. | DuckDB writing docs |
| Partition transforms and partition evolution | Partial | Identity, year, month, day, hour, bucket, and truncate transforms are documented. Partition evolution is supported through documented `ALTER TABLE ... SET PARTITIONED BY`, but do not assume every upstream transform/evolution pattern. | DuckDB partition docs and 1.5.2/1.5.3 Iceberg notes |
| Delete-file semantics; merge-on-read/copy-on-write | Partial | Merge-on-read positional delete files are supported. Copy-on-write mode is explicitly unsupported. | DuckDB writing limitations |
| Time travel and snapshots | Partial | Metadata functions expose snapshots. Scan functions accept snapshot ID/timestamp parameters. Catalog time-travel should be treated as supported only where docs and environment confirm it. | DuckDB metadata/function reference |
| Metadata inspection | Supported | `iceberg_metadata`, `iceberg_snapshots`, `iceberg_column_stats`, `iceberg_partition_stats`, and `iceberg_load_table_response` are documented. | DuckDB Iceberg function reference |
| Table properties | Supported | `WITH (...)` table properties and property inspection/set functions are documented for attached catalogs. | DuckDB writing and function docs |
| Nested/semi-structured types and relevant v3 features | Partial | DuckDB 1.5.0 introduced core `VARIANT`; Iceberg 1.5.1 release notes list Iceberg v3 support for `VARIANT`, `TIMESTAMP_NS`, default values, deletion vectors, partitioned insert/create, and Parquet copy options. Apache Iceberg v3 includes additional features not all documented as DuckDB-supported. | DuckDB 1.5.0/1.5.1 notes and Apache spec |
| Supported REST catalogs/cloud integrations | Partial | DuckDB docs list generic REST plus Amazon S3 Tables, AWS Glue/SageMaker Lakehouse, Cloudflare R2 Data Catalog, Apache Polaris, Lakekeeper, SeaweedFS, and Google Cloud BigLake. Cloud examples may require paid services or credentials, so they are not core labs. | DuckDB catalog docs |
| Interoperability with other engines/tools | Partial | Iceberg REST is designed for engine/language compatibility, and DuckDB can read/write supported Iceberg tables, but interoperability depends on feature overlap, delete-file support, catalog behavior, and table properties. | Apache REST catalog spec and DuckDB Iceberg docs |
| Known limitations or unsupported behavior | Partial | Path-based writes are unsupported; copy-on-write updates/deletes are unsupported; some target file/row group properties are not honored for partitioned tables. Other unverified behaviors must be marked Needs verification before becoming lab requirements. | DuckDB writing limitations |

Official sources for matrix:

- DuckDB Iceberg overview: https://duckdb.org/docs/current/core_extensions/iceberg/overview.html
- DuckDB Iceberg writing docs: https://duckdb.org/docs/current/core_extensions/iceberg/writing
- DuckDB Iceberg function reference: https://duckdb.org/docs/current/core_extensions/iceberg/reference
- DuckDB Iceberg catalogs: https://duckdb.org/docs/current/core_extensions/iceberg/catalogs.html
- DuckDB 1.5.0 release notes: https://duckdb.org/2026/03/09/announcing-duckdb-150
- DuckDB 1.5.1 release notes: https://duckdb.org/2026/03/23/announcing-duckdb-151
- DuckDB 1.5.2 release notes: https://duckdb.org/2026/04/13/announcing-duckdb-152
- DuckDB 1.5.3 release notes: https://duckdb.org/2026/05/20/announcing-duckdb-153
- Apache Iceberg spec: https://iceberg.apache.org/spec/
- Apache Iceberg REST catalog spec: https://iceberg.apache.org/rest-catalog-spec/

---

# 7. Current-year DuckDB Feature Table: January 1-August 27, 2026

Stable or usable current features first; preview/beta/upcoming items are separated afterward.

| Feature | Release | Core or extension | Status | Why it matters | Lab tie-in | Official source |
|---|---|---|---|---|---|---|
| Friendly CLI refresh: dynamic prompts, pager, `.tables`, `_` last-result table | 1.5.0 | CLI/core workflow | Stable | Makes teaching and exploratory SQL smoother | Used throughout setup and live demos | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| Native `VARIANT` type | 1.5.0 | Core type | Stable | Enables more natural semi-structured workflows inside DuckDB | Module 6 `variant_lab` | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| Built-in `GEOMETRY` type foundation | 1.5.0 | Core type plus spatial ecosystem | Stable for core type; CRS support still experimental | Makes geometry a native DuckDB type and improves extension interoperability | Optional spatial mini-lab | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| `read_duckdb` table function | 1.5.0 | Core table function | Stable | Lets learners query DuckDB database files without attaching them | Optional cross-database read challenge | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| Azure Blob / ADLSv2 writes through `COPY` | 1.5.0 | `httpfs`/cloud storage | Stable, environment-dependent | Important for lakehouse workflows, but not core because it may require credentials | Optional connector discussion | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| Iceberg table properties and extra HTTP headers | 1.5.0 | Iceberg extension | Stable extension feature | Needed for catalog-managed table behavior and some catalog integrations | Module 6 advanced REST-catalog lab | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| `httpfs` default backend changed to curl; HTTPS extension installs | 1.5.0 | Extension/networking | Stable | Improves extension/download/network behavior and proxy handling | Setup and troubleshooting | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| Non-blocking checkpointing | 1.5.0 | Storage/concurrency | Stable | Reduces write blocking during checkpoints in transactional workflows | Module 4 internals discussion | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| Lance support | 1.5.1 | Core extension | Stable extension feature | Adds another lakehouse/vector-adjacent format to compare with Iceberg/Parquet | Optional current-feature discussion | https://duckdb.org/2026/03/23/announcing-duckdb-151 |
| Iceberg v3 feature expansion: `VARIANT`, `TIMESTAMP_NS`, defaults, deletion vectors, partitioned insert/create | 1.5.1 | Iceberg extension | Partial; supported subset | Important for modern Iceberg interop, but not full v3 parity | Module 5-6 support matrix | https://duckdb.org/2026/03/23/announcing-duckdb-151 |
| DuckLake v1.0 support | 1.5.2 | DuckLake extension | Stable extension feature | Gives a DuckDB-native lakehouse contrast point to Iceberg | Optional capstone comparison | https://duckdb.org/2026/04/13/announcing-duckdb-152 |
| Iceberg `ALTER TABLE`, partitioned updates/deletes, GEOMETRY, truncate/bucket transforms, MERGE | 1.5.2-1.5.3 | Iceberg extension | Partial; supported documented subset | Materially expands catalog-managed Iceberg DDL/DML | Module 6 advanced lab | https://duckdb.org/2026/04/13/announcing-duckdb-152 and https://duckdb.org/2026/05/20/announcing-duckdb-153 |

## Preview, beta, experimental, or upcoming appendix

| Item | Status | Why it is not taught as stable baseline | Source |
|---|---|---|---|
| PEG parser | Experimental / disabled by default in 1.5.0 | Must be explicitly enabled with `CALL enable_peg_parser()`; not stable default behavior | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| Quack client-server protocol | Beta in 1.5.3 | Useful to mention, but release notes say production-ready target is v2.0 | https://duckdb.org/2026/05/20/announcing-duckdb-153 |
| Windows installer script for DuckDB CLI | Beta in 1.5.0 | Fine for optional install guidance, not a required lab dependency | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| Geometry CRS support | Experimental caveat | Built-in geometry type is stable, but CRS support is described cautiously | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| DuckDB 1.5.6 and 2.0.0 | Upcoming | Release calendar lists them after the verification date; not current stable behavior | https://duckdb.org/release_calendar |

---

# 8. Assessment Package

Use these as graded or self-check items.

## Module 1 checks

```sql
SELECT count(*) = 7 AS pass FROM customers_raw;
SELECT count(*) = 6 AS pass FROM customers_curated;
SELECT count(*) = 1 AS pass FROM customers_curated WHERE NOT email_valid;
```

Concept questions:

```text
1. Why stage CSV as VARCHAR first?
2. What does TRY_CAST preserve that CAST would not?
3. What data-quality issue remains unresolved after cleaning?
```

## Module 2 checks

```sql
SELECT count(*) = 8 AS pass FROM transactions_clean;

SELECT sum(revenue_local) = 457.75 AS pass
FROM mart_customer_revenue;

SELECT count(*) = 1 AS pass
FROM transactions_clean AS t
LEFT JOIN customers_curated AS c
    ON t.customer_id = c.customer_id
WHERE c.customer_id IS NULL
  AND t.status = 'captured';
```

Concept questions:

```text
1. Why does an orphan transaction matter?
2. Why deduplicate before joining?
3. When would you keep both duplicate rows instead?
```

## Module 3 checks

```sql
SELECT count(*) = 9 AS pass FROM events_clean;
SELECT count(*) = 1 AS pass FROM events_clean WHERE event_ts IS NULL;
SELECT count(*) = 4 AS pass FROM events_clean WHERE event_type = 'purchase' AND event_ts IS NOT NULL;
```

Concept questions:

```text
1. What type did DuckDB infer for nested properties?
2. Why does deduplication change daily event counts?
3. What does a list aggregation reveal that a pivot does not?
```

## Module 4 checks

```sql
SELECT count(*) = 8 AS pass
FROM read_parquet('data/curated/events_by_day/**/*.parquet');

SELECT count(*) = 4 AS pass
FROM read_parquet('data/curated/events_by_day/**/*.parquet')
WHERE event_type = 'purchase';
```

Concept questions:

```text
1. What is projection pushdown?
2. What is predicate pushdown?
3. Why is over-partitioning dangerous?
```

## Module 5 checks

```sql
SELECT count(*) = 51793 AS pass
FROM iceberg_scan(
    'data/iceberg/lineitem_iceberg',
    allow_moved_paths = true
);

SELECT count(*) = 60175 AS pass
FROM iceberg_scan(
    'data/iceberg/lineitem_iceberg/metadata/v1.metadata.json',
    allow_moved_paths = true
);
```

Concept questions:

```text
1. Why can two metadata versions produce different counts?
2. Why is path-based Iceberg access read-only in DuckDB?
3. What does a REST catalog add?
```

## Module 6 checks

For core path:

```sql
SELECT count(*) = 2 AS pass FROM variant_lab;
```

For advanced REST-catalog path:

```sql
SELECT count(*) = 3 AS pass
FROM polaris_catalog.duckdb_workshop.events;

SELECT event_name = 'signup' AS pass
FROM polaris_catalog.duckdb_workshop.events
WHERE id = 2;
```

Concept questions:

```text
1. Which Iceberg operations required a catalog?
2. What is merge-on-read?
3. Why should preview features be separated from stable lab requirements?
```

---

# 9. Official-source Reference Section

| Topic | Source |
|---|---|
| Current release baseline and upcoming releases | https://duckdb.org/release_calendar |
| DuckDB 1.5.0 features | https://duckdb.org/2026/03/09/announcing-duckdb-150 |
| DuckDB 1.5.1 features | https://duckdb.org/2026/03/23/announcing-duckdb-151 |
| DuckDB 1.5.2 features | https://duckdb.org/2026/04/13/announcing-duckdb-152 |
| DuckDB 1.5.3 features | https://duckdb.org/2026/05/20/announcing-duckdb-153 |
| DuckDB 1.5.4 and 1.5.5 | https://duckdb.org/2026/06/17/announcing-duckdb-154 |
| DuckDB Iceberg overview | https://duckdb.org/docs/current/core_extensions/iceberg/overview.html |
| DuckDB Iceberg writing | https://duckdb.org/docs/current/core_extensions/iceberg/writing |
| DuckDB Iceberg metadata/functions | https://duckdb.org/docs/current/core_extensions/iceberg/reference |
| DuckDB Iceberg catalogs | https://duckdb.org/docs/current/core_extensions/iceberg/catalogs.html |
| Apache Iceberg spec | https://iceberg.apache.org/spec/ |
| Apache Iceberg REST catalog spec | https://iceberg.apache.org/rest-catalog-spec/ |
| Polaris quickstart | https://polaris.apache.org/guides/quickstart/ |
| DuckDB data ingestion docs | https://duckdb.org/docs/current/clients/python/data_ingestion |
| DuckDB profiling docs | https://duckdb.org/docs/current/sql/statements/profiling |
| DuckDB EXPLAIN ANALYZE docs | https://duckdb.org/docs/current/guides/meta/explain_analyze |
| DuckDB partitioned writes docs | https://duckdb.org/docs/current/data/partitioning/partitioned_writes |

---

# 10. Quality-gate Result

```text
PASS - Uses DuckDB 1.5.5 as verified stable baseline on August 27, 2026.
PASS - Separates 1.5.6 and 2.0.0 as upcoming, not stable.
PASS - Keeps core labs local and free of paid-cloud dependencies.
PASS - Uses REST-catalog writes only in the optional advanced path.
PASS - Separates Apache Iceberg spec behavior from DuckDB extension behavior.
PASS - Marks partial Iceberg support where DuckDB documents limitations.
PASS - Includes data wrangling, performance, JSON/semi-structured work, Parquet, and validation checks.
PASS - Avoids claiming the labs were executed in this environment.
PASS - Avoids claiming full Apache Iceberg ecosystem parity.
```
