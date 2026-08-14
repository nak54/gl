# Dataverse vs. SQL Server for Power Apps — Sources

**Assessment date:** August 14, 2026  
**Source policy:** Official Microsoft documentation and Microsoft product/licensing pages.

## Architecture and application types

1. [Define model-driven app data](https://learn.microsoft.com/en-us/power-apps/maker/model-driven-apps/define-data-model-driven-app)  
   Microsoft Learn. Used for the boundary that model-driven apps are built on Dataverse and do not directly bind to SQL tables.

2. [Use SQL to query data in Microsoft Dataverse](https://learn.microsoft.com/en-us/power-apps/developer/data-platform/dataverse-sql-query)  
   Microsoft Learn. Used for Dataverse SQL/TDS endpoint behavior, read-only access, supported SQL subset, query limits, and reporting considerations.

3. [Get started with virtual tables](https://learn.microsoft.com/en-us/power-apps/developer/data-platform/virtual-entities/get-started-ve)  
   Microsoft Learn. Used for the distinction between native Dataverse tables and virtual tables, including capability restrictions.

4. [Virtual tables limitations and troubleshooting](https://learn.microsoft.com/en-us/power-apps/maker/data-platform/limits-tshoot-virtual-tables)  
   Microsoft Learn. Used for SQL virtual-table restrictions involving keys, relationships, views, types, paging, security, and unsupported features.

## Canvas apps, delegation, and SQL connectivity

5. [Understand delegation in a canvas app](https://learn.microsoft.com/en-us/power-apps/maker/canvas-apps/delegation-overview)  
   Microsoft Learn. Used for delegation behavior, the default 500-row local processing limit, the configurable 2,000-row maximum, and the risk of incomplete results.

6. [SQL Server connector reference](https://learn.microsoft.com/en-us/connectors/sql/)  
   Microsoft Learn. Used for connector classification, supported operations, authentication, SQL deployment differences, gateway restrictions, connector throttling, stored-procedure limits, timeouts, unsupported types, trigger behavior, and deprecated V1 actions/triggers.

## Data model and transactions

7. [Create and edit table relationships](https://learn.microsoft.com/en-us/power-apps/maker/data-platform/create-edit-entity-relationships)  
   Microsoft Learn. Used for Dataverse relationship modeling, including one-to-many and many-to-many relationships.

8. [Use ExecuteTransaction to execute multiple requests in a single database transaction](https://learn.microsoft.com/en-us/power-apps/developer/data-platform/org-service/use-executetransaction)  
   Microsoft Learn. Used for Dataverse transaction boundaries and the distinction between transactional API operations and separate Power Apps connector calls.

## Security, auditing, and governance

9. [Security concepts in Microsoft Dataverse](https://learn.microsoft.com/en-us/power-platform/admin/wp-security-cds)  
   Microsoft Learn. Used for Dataverse security roles, ownership, business units, teams, sharing, hierarchy, and column-level security concepts.

10. [Manage Dataverse auditing](https://learn.microsoft.com/en-us/power-platform/admin/manage-dataverse-auditing)  
    Microsoft Learn. Used for Dataverse change auditing, user-access auditing, audit history, audit summary, retention, and capacity considerations.

11. [Dataverse storage capacity](https://learn.microsoft.com/en-us/power-platform/admin/capacity-storage)  
    Microsoft Learn. Used for Dataverse database, file, and log capacity management and the operational effect of capacity deficits.

12. [Managed Environments licensing](https://learn.microsoft.com/en-us/power-platform/admin/managed-environment-licensing)  
    Microsoft Learn. Used for Managed Environments licensing requirements and related administrative considerations.

## Mobile offline

13. [Limitations of mobile offline for canvas apps](https://learn.microsoft.com/en-us/power-apps/mobile/limitations-canvas-apps)  
    Microsoft Learn. Used for native Dataverse mobile-offline scope, supported app types, unsupported connectors and table types, profile limits, browser-versus-mobile behavior, and relationship restrictions.

## ALM and deployment

14. [Solutions overview](https://learn.microsoft.com/en-us/power-apps/maker/data-platform/solutions-overview)  
    Microsoft Learn. Used for managed and unmanaged solutions, solution-aware components, environment promotion, connection references, and Power Platform ALM concepts.

## Integration and automation

15. [Microsoft Dataverse connector overview for Power Automate](https://learn.microsoft.com/en-us/power-automate/dataverse/overview)  
    Microsoft Learn. Used for Dataverse triggers, actions, and low-code automation integration patterns.

## Reporting, analytics, and data export

16. [Azure Synapse Link for Dataverse](https://learn.microsoft.com/en-us/power-apps/maker/data-platform/azure-synapse-link-synapse)  
    Microsoft Learn. Used for supported Dataverse analytical export/link patterns and the replacement of older data-lake export approaches.

## Performance, scale, and service protection

17. [Optimize query performance using QueryExpression](https://learn.microsoft.com/en-us/power-apps/developer/data-platform/org-service/queryexpression/optimize-performance)  
    Microsoft Learn. Used for Dataverse query-design and performance guidance.

18. [Service protection API limits](https://learn.microsoft.com/en-us/power-apps/developer/data-platform/api-limits)  
    Microsoft Learn. Used for Dataverse service-protection limits, throttling behavior, concurrency, execution-time constraints, and retry handling.

## Licensing and pricing

19. [Power Apps pricing](https://www.microsoft.com/en/power-platform/products/power-apps/pricing)  
    Microsoft product page. Used for public list pricing, Premium licensing examples, pay-as-you-go pricing, and included Dataverse capacity. Contract pricing, geography, taxes, and future changes may differ.

## Notes

- Product capabilities, licensing terms, prices, service limits, preview status, and deprecated features can change. Revalidate these pages against the target tenant, region, contract, and release wave before final architecture approval.
- The SQL connector is a premium connector for Power Apps and Power Automate.
- The Dataverse SQL/TDS endpoint is read-only and is not a substitute for a writable SQL database.
- Dataverse virtual tables do not provide the same feature set as native Dataverse tables.
- Generic SQL-to-Dataverse synchronization should not be described as Dynamics 365 dual-write unless it is an explicitly supported dual-write scenario.
