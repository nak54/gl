---
title: "Official Sources for the Power Query-to-ArcGIS GIS-Ready Data Workflow Report"
date_compiled: "2026-08-14"
source_scope: "Attached workflow brief plus official Microsoft and Esri documentation"
---

# Official Sources for the Power Query-to-ArcGIS GIS-Ready Data Workflow Report

## Scope

This bibliography lists the source framework and official vendor documentation used in, or directly supporting, the detailed Power Query-to-ArcGIS workflow report. It is organized for traceability and future verification.

- Official Microsoft documentation is from Microsoft Learn.
- Official Esri documentation is from ArcGIS documentation and Esri Developer.
- Product behavior, licensing, supported formats, service capabilities, and version-dependent requirements should be rechecked in the target environment before implementation.
- The attached workflow brief intentionally leaves project-specific facts such as the business objective, source systems, data volume, cadence, source CRS, target CRS, and destination unspecified. Those items remain project decisions rather than source-derived facts.

## Source Framework

### [LOCAL-01] GIS Data Workflow Architect Prompt

- **File:** `pbi_esri.md`
- **Type:** User-provided project framework
- **Role in report:** Defines the workflow objective, required inputs, Power Query and ArcGIS responsibility boundaries, handoff contract, validation rules, phasing requirements, quality bar, required report sections, and target-state diagram requirement.
- **External URL:** None; this is the attached source file.

## Microsoft Official Sources

### [MS-01] Query folding guidance in Power BI Desktop

- **Publisher:** Microsoft Learn
- **URL:** https://learn.microsoft.com/en-us/power-bi/guidance/power-query-folding
- **Supports:** Query folding, source-side processing, transformation ordering, native-query cautions, read-only access, and performance guidance for relational sources.

### [MS-02] Best practices when working with Power Query

- **Publisher:** Microsoft Learn
- **URL:** https://learn.microsoft.com/en-us/power-query/best-practices
- **Supports:** Connector selection, early filtering, delaying expensive operations, query resilience, modular transformation design, and maintainable Power Query workflows.

### [MS-03] Using the data profiling tools

- **Publisher:** Microsoft Learn
- **URL:** https://learn.microsoft.com/en-us/power-query/data-profiling-tools
- **Supports:** Column quality, column distribution, column profile, distinct and unique values, errors and empty values, and the distinction between profiling the first 1,000 rows and the entire dataset.

### [MS-04] Data Types in Power Query

- **Publisher:** Microsoft Learn
- **URL:** https://learn.microsoft.com/en-us/power-query/data-types
- **Supports:** Field-level data types, explicit type assignment, automatic type detection, locale-sensitive conversion, and type-specific transformations.

### [MS-05] Dataflow Gen2 data destinations and managed settings

- **Publisher:** Microsoft Learn
- **URL:** https://learn.microsoft.com/en-us/fabric/data-factory/dataflow-gen2-data-destinations-and-managed-settings
- **Supports:** Persistent Dataflow Gen2 destinations, table and file outputs, destination mapping, replace behavior, fixed versus managed schemas, and destination parameterization.

### [MS-06] On-premises data gateway - Power BI

- **Publisher:** Microsoft Learn
- **URL:** https://learn.microsoft.com/en-us/power-bi/connect-data/service-gateway-onprem
- **Supports:** Gateway purpose, cloud-to-on-premises data access, gateway types, administration, configuration, refresh use, and operational dependencies.

### [MS-07] Configure incremental refresh and real-time data for Power BI semantic models

- **Publisher:** Microsoft Learn
- **URL:** https://learn.microsoft.com/en-us/power-bi/connect-data/incremental-refresh-overview
- **Supports:** Semantic-model partition management, incremental refresh policies, RangeStart and RangeEnd parameters, refresh windows, and real-time DirectQuery options.

## Esri Official Sources

### [ESRI-01] XY Table To Point (Data Management Tools)

- **Publisher:** ArcGIS Pro documentation, Esri
- **URL:** https://doc.esri.com/en/arcgis-pro/latest/tool-reference/data-management/xy-table-to-point.html
- **Supports:** Point creation from numeric X, Y, and optional Z fields; interpretation of coordinates using a specified coordinate system; null geometry behavior; and coordinate-field requirements.

### [ESRI-02] Geocode Addresses (Geocoding Tools)

- **Publisher:** ArcGIS Pro documentation, Esri
- **URL:** https://doc.esri.com/en/arcgis-pro/latest/tool-reference/geocoding/geocode-addresses.html
- **Supports:** Geocoding address tables with a locator, address-field mapping, point output, retained locator results, and ArcGIS World Geocoding Service credit considerations.

### [ESRI-03] Define Projection (Data Management Tools)

- **Publisher:** ArcGIS Pro documentation, Esri
- **URL:** https://doc.esri.com/en/arcgis-pro/latest/tool-reference/data-management/define-projection.html
- **Supports:** Assigning or correcting coordinate-system metadata without changing geometry, and the distinction between defining a projection and transforming coordinates.

### [ESRI-04] Project (Data Management Tools)

- **Publisher:** ArcGIS Pro documentation, Esri
- **URL:** https://doc.esri.com/en/arcgis-pro/latest/tool-reference/data-management/project.html
- **Supports:** Transforming spatial data between coordinate systems, geographic and vertical transformations, output spatial reference, and projection-related geometry considerations.

### [ESRI-05] Check Geometry (Data Management Tools)

- **Publisher:** ArcGIS Pro documentation, Esri
- **URL:** https://doc.esri.com/en/arcgis-pro/latest/tool-reference/data-management/check-geometry.html
- **Supports:** Geometry-problem reporting, supported spatial data formats, geometry validation methods, and post-processing geometry checks.

### [ESRI-06] Repair Geometry (Data Management Tools)

- **Publisher:** ArcGIS Pro documentation, Esri
- **URL:** https://doc.esri.com/en/arcgis-pro/latest/tool-reference/data-management/repair-geometry.html
- **Supports:** Controlled geometry repair, the fact that the tool modifies input data, repair outcomes, and the need to recheck geometry after modification.

### [ESRI-07] Introduction to attribute domains

- **Publisher:** ArcGIS Pro documentation, Esri
- **URL:** https://doc.esri.com/en/arcgis-pro/latest/help/data/geodatabases/overview/an-overview-of-attribute-domains.html
- **Supports:** Coded-value and range domains, field-value constraints, geodatabase data integrity, domain ownership, and domain assignment.

### [ESRI-08] Append (Data Management Tools)

- **Publisher:** ArcGIS Pro documentation, Esri
- **URL:** https://doc.esri.com/en/arcgis-pro/latest/tool-reference/data-management/append.html
- **Supports:** Appending to and optionally updating target datasets, field mapping, matching fields, domain enforcement, feature-service optimization, and inserted or updated row counts.

### [ESRI-09] Add and update features in a hosted feature layer

- **Publisher:** ArcGIS Online documentation, Esri
- **URL:** https://doc.arcgis.com/en/arcgis-online/manage-data/add-update-features.htm
- **Supports:** Bulk insert and update formats, unique matching-field requirements, field-name considerations, and warnings against unsafe use of ObjectID or FID as a durable update identifier.

### [ESRI-10] arcgis.features module - FeatureLayer methods

- **Publisher:** ArcGIS API for Python documentation, Esri Developer
- **URL:** https://developers.arcgis.com/python/latest/api-reference/arcgis.features.toc.html
- **Supports:** `FeatureLayer.append`, upsert options, matching fields, rollback, supported append formats, `edit_features`, and feature-edit result handling.

### [ESRI-11] Appending Features

- **Publisher:** ArcGIS API for Python guide, Esri Developer
- **URL:** https://developers.arcgis.com/python/latest/guide/appending-features/
- **Supports:** Practical hosted-layer append workflows, source-item preparation, capability checks such as `supportsAppend`, format checks, field mappings, and post-append verification.

### [ESRI-12] Enable editor tracking on a feature layer

- **Publisher:** ArcGIS Enterprise documentation, Esri
- **URL:** https://doc.esri.com/en/arcgis-enterprise/latest/share/enable-editor-tracking.html
- **Supports:** Creator and editor identities, creation and edit timestamps, hosted and referenced feature-layer behavior, and the effect of automated update processes on editor-tracking fields.

## Report-to-Source Crosswalk

| Report topic | Primary source IDs |
|---|---|
| Workflow objective, boundaries, handoff contract, validation, phasing, and required outputs | LOCAL-01 |
| Power Query architecture and transformation practices | MS-01, MS-02, MS-03, MS-04 |
| Query folding, source-side processing, and least-privilege source access | MS-01 |
| Whole-dataset profiling and data-quality inspection | MS-03 |
| Explicit data types and locale-aware conversion | MS-04 |
| Persistent handoff destinations and schema behavior | MS-05 |
| On-premises refresh connectivity and gateway dependencies | MS-06 |
| Power BI semantic-model incremental refresh | MS-07 |
| Point creation from coordinates | ESRI-01 |
| Address geocoding and locator requirements | ESRI-02 |
| Coordinate-system metadata assignment | ESRI-03 |
| Coordinate transformation and geographic transformations | ESRI-04 |
| Geometry validation and controlled repair | ESRI-05, ESRI-06 |
| Geodatabase domain enforcement | ESRI-07 |
| ArcGIS append and optional update workflows | ESRI-08 |
| Hosted-layer bulk updates and durable unique identifiers | ESRI-09 |
| Automated append, upsert, edits, rollback, and result handling | ESRI-10, ESRI-11 |
| Editor identity and timestamp tracking | ESRI-12 |

## Suggested Citation Format

Use the organization as the corporate author when a named individual author is not provided.

### Generic Markdown citation

```markdown
[Document title](official-url), Organization, accessed August 14, 2026.
```

### Example

```markdown
[Query folding guidance in Power BI Desktop](https://learn.microsoft.com/en-us/power-bi/guidance/power-query-folding), Microsoft Learn, accessed August 14, 2026.
```

## Verification Note

The URLs above were checked against official Microsoft and Esri documentation on August 14, 2026. "Latest" ArcGIS documentation can change as Esri releases new product versions, and Microsoft Learn pages can be revised without changing their URL. Revalidate version-specific behavior, supported destinations, supported append formats, licensing, capacity, gateway requirements, locator credits, and ArcGIS service capabilities during implementation.
