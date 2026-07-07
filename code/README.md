# Processing workflow

The MigraFISH database was compiled by harmonising information from 13 global, regional, and national sources. Species taxonomy was standardised against the Eschmeyer Catalog of Fishes. Migration classifications, migration-distance categories, confidence scores, and population homogeneity were assessed using a structured decision framework, source information, targeted literature review, and expert validation.

## Spatial processing

`01_prepare_spatial_data.R` prepares the basin-level spatial fields included in the public release. It:

1. harmonises species names across spatial input tables;
2. links IUCN-derived and GBIF-derived occurrences to HydroBASINS;
3. derives Pfafstetter level-5 basin codes;
4. combines spatial information across sources;
5. creates final species-level spatial fields and provenance information.

The script uses local source-derived input files that are not redistributed in this repository. These include IUCN-derived spatial products, GBIF occurrence-derived tables, and HydroBASINS lookup tables. Their use is documented in the manuscript and script comments.

## Release preparation

`02_create_release_files.R` creates the versioned public dataset from the processed database. It writes the CSV and Excel versions, adds release metadata, and runs basic quality-control checks.

## Reproducibility

The repository provides the workflow used to generate the published MigraFISH release. Some raw inputs cannot be redistributed because they originate from third-party sources with their own access and reuse conditions. Users can obtain those source data from the original providers and use the scripts as a documented workflow.
