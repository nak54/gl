# Dataverse vs. SQL Server for Power Apps: Technical Comparison

Act as a Microsoft Power Platform solution architect. Create a technically precise comparison of Dataverse versus SQL Server as the data platform for Power Apps development.

## Project Context

Provide this information (state assumptions if missing):

- **Application type**: canvas app | model-driven app | both
- **Existing platform**: greenfield | existing SQL | existing Dataverse
- **SQL deployment**: Azure SQL | SQL MI | on-premises
- **Users**: expected total and concurrent
- **Data volume**: rows and growth rate
- **Mobile offline**: required | not required
- **Complex transactions**: low | medium | high
- **Stored procedures**: dependency exists | no dependency
- **Security**: row/field-level requirements
- **Auditing/compliance**: requirements
- **Integrations**: external systems
- **Reporting**: Power BI | SSRS | direct SQL | other
- **Team skills**: Power Platform | SQL | .NET | Azure
- **Constraints**: licensing, budget, data residency, network

## Architectures to Evaluate

Distinguish these patterns:
- **A**: Canvas app → Dataverse
- **B**: Canvas app → SQL Server (direct connector)
- **C**: Model-driven app → Dataverse tables
- **D**: Model-driven app → Dataverse virtual tables → SQL
- **E**: Hybrid (Dataverse + SQL)

Clarify: Dataverse SQL/TDS endpoint ≠ writable SQL database; virtual tables ≠ native tables.

## Status Classification

Use exactly one per capability:
- **NATIVE** — built-in, standard configuration
- **SUPPORTED** — documented connector/feature
- **CUSTOM BUILD** — requires code, plug-ins, flows, middleware, Azure services
- **LIMITED** — supported with significant restrictions
- **NOT DIRECTLY SUPPORTED** — not available in stated architecture
- **NOT APPLICABLE** — doesn't logically apply

## Research Requirements

- Cite official Microsoft Learn documentation
- State current date
- Mark preview/deprecated features
- Verify licensing (connectors, Dataverse capacity, gateways)
- Explain performance dependencies (delegation, volume, indexing, network)
- Distinguish Azure SQL from on-premises SQL (auth, gateways, features)

## Required Output

### 1. Executive Summary

- Strongest reasons to choose Dataverse
- Strongest reasons to choose SQL Server
- When hybrid is preferable
- Three most critical limitations
- Preliminary recommendation

### 2. Capability Matrix

Table format:
| Capability | Dataverse Status | What It Can Do | Limitations | SQL Status | What It Can Do | Limitations | Workarounds | Best Fit |

**Categories** (include all):

- **App Types**: canvas, model-driven, custom pages, Power Pages, auto-generated UI, custom UI, mobile
- **Data Model**: tables, relationships (1:N, N:N), lookups, choices, ownership, polymorphic, files, calculated/rollup columns, alternate keys, duplicate detection, integrity, schema changes
- **Data Access**: CRUD, filtering, Power Fx delegation, large tables, views, stored procedures, joins, CTEs, window functions, direct SQL, bulk ops, unsupported types
- **Transactions & Logic**: multi-record transactions, validation, business rules, BPFs, plug-ins, custom APIs, triggers, stored procedures, scheduled/async processing, error handling, low-code vs. pro-code
- **Security**: environment access, table/row/field permissions, ownership, sharing, business units, hierarchies, Entra ID, service principals, shared vs. per-user connections, SQL RLS, credential management, direct access prevention
- **Auditing & Governance**: change auditing, access auditing, audit UI, DLP, retention, compliance, Purview, custom audit tables
- **Offline & Mobile**: native offline, profiles, sync/conflict handling, LoadData/SaveData, lookup limits, browser vs. Mobile app, custom sync
- **ALM**: solutions (managed/unmanaged), environment promotion, metadata deployment, connection references, environment variables, SQL schema deployment, pipelines, DevOps/GitHub, source control, rollback
- **Integration**: Power Automate triggers/actions, Dataverse events, SQL connector, Azure Functions, Logic Apps, Service Bus, webhooks, custom connectors, REST APIs, import/export, dataflows, virtual tables, dual-write
- **Reporting & Analytics**: Power BI, DirectQuery, SQL reporting, views/charts, dashboards, operational vs. analytical, export/replication, Fabric/lake integration
- **Operations & Scale**: indexing, query tuning, capacity management, storage costs, backup/restore, HA/DR, environment copying, admin overhead, monitoring, vendor-managed vs. customer-managed, throttling, service limits
- **Licensing & Cost**: Power Apps licensing, premium connectors, Dataverse capacity (DB/file/log), SQL licensing/Azure costs, gateway requirements, admin/dev effort, custom feature costs

### 3. Explicit Can/Cannot Sections

**A. What Dataverse provides natively that SQL-backed apps don't**

**B. What SQL provides natively that Dataverse doesn't**

**C. What both do, but via different architectures**

**D. What neither solves without additional components**

For each "cannot" or "not supported" item, state:
- Exact boundary
- Workaround existence and architecture
- Workaround limitations
- Microsoft support status
- Licensing/security/ops impact

### 4. Scenario Recommendations

Provide architecture, rationale, custom build needs, risks, licensing questions, and conditions that change the recommendation for:

- Greenfield departmental LOB app
- Enterprise app with complex security
- CRM-style app (forms, views, processes, auditing)
- Existing app with SQL system of record
- Heavy stored procedure/transaction dependency
- High-volume operational database
- Mobile field app with offline
- Model-driven app displaying SQL data
- Direct reporting and SQL ecosystem compatibility
- Small Power Platform team, strong SQL skills
- Low-code admin, rapid delivery focus
- Regulated app requiring auditing
- Hybrid Dataverse + SQL

### 5. Architecture Patterns

For each pattern (Dataverse-first, SQL-first canvas, model-driven + virtual tables, Dataverse operational + SQL sync, SQL system of record + Dataverse workflow), describe:

- System of record
- UI type
- Data movement
- Security enforcement point
- Transaction boundary
- Offline approach
- ALM/deployment
- Advantages/limitations
- Appropriate/inappropriate use cases

### 6. Final Decision

- Weighted decision score (Dataverse | SQL | Hybrid)
- Weighting assumptions
- Recommended architecture
- Top 5 reasons
- Top 5 risks
- Proof-of-concept checklist
- Questions requiring answers before final approval

## Accuracy Guardrails

- Don't claim SQL lacks security/auditing/workflow/transactions — explain database-native vs. Power Apps-native vs. custom integration
- Don't say Dataverse = SQL Server with different interface
- Don't assume virtual table features = native table features
- Don't claim model-driven apps directly use SQL tables without Dataverse
- Compare development effort, security behavior, admin, ops, licensing, UX, ALM — not just feature names
- Don't treat workarounds as equivalent to native features unless genuinely comparable
- Don't hide delegation warnings, connector limits, unsupported types, auth requirements, gateway needs, transaction boundaries
- State "depends" when answer varies by auth method, connector, app type, hosting, or licensing

Use clear language for Power Apps developers, architects, DBAs, security teams, and decision-makers.