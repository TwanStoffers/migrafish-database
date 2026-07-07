# ------------------------------------------------------------------------------
# Created by:
# Dr Twan Stoffers
# Assistant Professor Fish Ecology
# Aquaculture Biology and Fisheries Ecology Group (AFI)
# Wageningen University & Research
# The Netherlands
# E:  Twan.Stoffers@wur.nl
#
# Purpose:
# This script prepares HydroBASINS-based spatial information for the MigraFish
# database. It combines IUCN-derived HYBAS records, GBIF-derived Pfafstetter
# records, and manually corrected Level-5 HYBAS records into one cleaned
# species-level database.
#
# Main outputs:
# 1. final_migratory_hybas_table_enriched.csv
#    Row-level IUCN/HYBAS table enriched with Pfafstetter Level-5 information.
#
# 2. migratory_species_pfaf5_hybas5_summary.csv
#    Species-level IUCN-derived Pfafstetter Level-5 and HYBAS Level-5 summaries.
#
# 3. final_migratory_clean
#    Clean final database prepared for publication.
#
# Important:
# HYBAS_ID and PFAF_ID fields are identifiers, not numbers. They are always read
# and processed as character strings to avoid scientific notation or truncation.
# ------------------------------------------------------------------------------

# Set path for packages
.libPaths(c(
  "~/Library/R/arm64/4.5/library",
  .libPaths()
))

# Set the working directory to the location of the current script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Empty the environment
remove(list = ls(all = TRUE))

# --- 0. Setup ----------------------------------------------------------------
library(tidyverse)
library(here)
library(stringr)
library(purrr)
library(shiny)
library(DT)
library(tidyverse)
library(sf)
library(leaflet)
library(here)

# --- 1. Read & Combine Fish HYBAS Parts ---------------------------------------
fish_hybas <- 
  paste0("fish_hybas_table_part", 1:10, ".csv") %>% 
  map_dfr(~ read_csv(
    here("data", .x),
    col_types = cols(
      hybas_id = col_character()
    ),
    show_col_types = FALSE
  ) %>%
    select(-subspecies, -subpop, -source))

# --- 2. Read Other HYBAS Tables ------------------------------------------------
chondrichs_hybas <- read_csv(
  here("data", "chondrichythes_hybas_table.csv"),
  col_types = cols(
    hybas_id = col_character(),
    .default = col_guess()
  ),
  show_col_types = FALSE
)

other_hybas <- read_csv(
  here("data", "other_hybas_table.csv"),
  col_types = cols(
    hybas_id = col_character(),
    .default = col_guess()
  ),
  show_col_types = FALSE
)

all_hybas <- bind_rows(fish_hybas, chondrichs_hybas, other_hybas)


# Check if 'Chondrostoma nasus' is present in the sci_name column
"Chondrostoma nasus" %in% all_hybas$sci_name

# --- 3. Filter by Presence & Origin -------------------------------------------
filtered_hybas <- 
  all_hybas %>%
  filter(presence %in% c(1,2,3), origin %in% c(1,2,5))

all_hybas %>%
  filter(sci_name == "Achiroides melanorhijnchus") %>%
  select(hybas_id, presence, origin) # Check presence and origin of species

# --- 4. Load Migratory Species List & Filter ----------------------------------
migratory <- read_csv(
  here("data", "Master_Database_TS_v1.csv"),
  show_col_types = FALSE
) %>%
  select(1:9)

# Account for small typos
library(stringdist)
library(fuzzyjoin)

#hybas_migratory <- 
#  filtered_hybas %>%
#  semi_join(migratory, by = c("sci_name" = "Name"))

# Filter out rows with NA in sci_name or Name
filtered_hybas_clean <- filtered_hybas %>% filter(!is.na(sci_name))
migratory_clean <- migratory %>% filter(!is.na(Species))

# Perform fuzzy join allowing for small typos
hybas_migratory <- stringdist_semi_join(
  filtered_hybas_clean, 
  migratory_clean, 
  by = c("sci_name" = "Species"),
  method = "osa",       # optimal string alignment
  max_dist = 2          # allow 1-character difference
)

# Check if 'Chondrostoma nasus' is present in the sci_name column
"Chondrostoma nasus" %in% hybas_migratory$sci_name

write_csv(hybas_migratory, here("output", "final_migratory_hybas_table.csv"))

# --- 5. Flag Migratory Species in the Master List -----------------------------
migratory_flagged <- 
  migratory %>%
  mutate(hybas_present = if_else(Species %in% hybas_migratory$sci_name, "Yes", "No"))

write_csv(migratory_flagged, here("output", "migratory_data_with_presence_flag.csv"))

# --- 6. Report Summary of Non-present Species ---------------------------------
present_count <- sum(migratory_flagged$hybas_present == "Yes")
message("Number of migratory species present in HYBAS: ", present_count)

# Check which species are in all_hybas at all
not_present <- migratory_flagged %>%
  filter(hybas_present == "No") %>%
  mutate(
    in_all_hybas = Species %in% all_hybas$sci_name,
    reason = case_when(
      !in_all_hybas ~ "Not in HYBAS",
      in_all_hybas  ~ "Filtered out"
    )
  ) %>%
  select(Species, Migration_class, reason)

# Summary message
message("Number of migratory species present in HYBAS: ", sum(migratory_flagged$hybas_present == "Yes"))
message("Number of migratory species not found in HYBAS at all: ", sum(not_present$reason == "Not in HYBAS"))
message("Number of migratory species filtered out (e.g. presence/origin): ", sum(not_present$reason == "Filtered out"))

# Print table
print(not_present)
print(table(not_present$reason))
print(table(not_present$Migration_class, not_present$reason))

# --- 7. Enrich with River Basin Level-5 IDs & Count Unique Basins ------------

library(foreign)

# Continents/regions available in HydroBASINS
continents <- c("af", "ar", "as", "au", "eu", "gr", "na", "sa", "si")

# HydroBASINS levels occurring in the IUCN HYBAS data
levels_needed <- c("05", "06", "07", "08", "09", "10", "11", "12")

# Helper to clean HYBAS IDs
clean_hybas_id <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_replace("\\.0$", "")
}

# Read HydroBASINS DBF lookup tables
hybas_lookup_all <- read_csv(
  here("data", "hybas_lookup_all.csv"),
  col_types = cols(
    HYBAS_ID = col_character(),
    hybas_level = col_character(),
    PFAF_ID = col_character(),
    pfaf5 = col_character(),
    .default = col_guess()
  ),
  show_col_types = FALSE
)

# Level-5 lookup to translate PFAF5 back to HYBAS level-5 ID
hybas5_lookup <- hybas_lookup_all %>%
  filter(hybas_level == "05") %>%
  transmute(
    pfaf5,
    hybas5_id = HYBAS_ID
  ) %>%
  distinct()

# First join: exact HYBAS_ID match
hybas_migratory_enriched <- hybas_migratory %>%
  mutate(
    hybas_id = clean_hybas_id(hybas_id),
    hybas_level = str_sub(hybas_id, 2, 3)
  ) %>%
  filter(
    hybas_level %in% levels_needed,
    !str_detect(hybas_id, "e\\+|\\.")
  ) %>%
  left_join(
    hybas_lookup_all,
    by = c("hybas_id" = "HYBAS_ID", "hybas_level")
  )

# Second join: fallback where final HYBAS side digit is replaced by 0
hybas_migratory_enriched <- hybas_migratory_enriched %>%
  mutate(
    hybas_id_0 = str_replace(hybas_id, ".$", "0")
  ) %>%
  left_join(
    hybas_lookup_all %>%
      select(HYBAS_ID, hybas_level, PFAF_ID, pfaf5) %>%
      rename(
        HYBAS_ID_0 = HYBAS_ID,
        PFAF_ID_fallback = PFAF_ID,
        pfaf5_fallback = pfaf5
      ),
    by = c("hybas_id_0" = "HYBAS_ID_0", "hybas_level")
  ) %>%
  mutate(
    PFAF_ID = coalesce(PFAF_ID, PFAF_ID_fallback),
    pfaf5 = coalesce(pfaf5, pfaf5_fallback)
  ) %>%
  select(-PFAF_ID_fallback, -pfaf5_fallback)

# Add corresponding HydroBASINS level-5 ID
hybas_migratory_enriched <- hybas_migratory_enriched %>%
  left_join(hybas5_lookup, by = "pfaf5")

# Check unmatched HYBAS IDs after exact + fallback matching
unmatched_hybas <- hybas_migratory_enriched %>%
  filter(is.na(pfaf5)) %>%
  distinct(hybas_id, hybas_level)

message("Number of unmatched HYBAS IDs after fallback matching: ", nrow(unmatched_hybas))

unmatched_hybas %>%
  count(hybas_level, sort = TRUE) %>%
  print()

# Summarise unique level-5 PFAF codes and HYBAS level-5 IDs per species
agg_pfaf5 <- hybas_migratory_enriched %>%
  filter(!is.na(pfaf5)) %>%
  distinct(sci_name, pfaf5, hybas5_id) %>%
  group_by(sci_name) %>%
  summarise(
    pfaf5_ids = str_c(sort(unique(na.omit(pfaf5))), collapse = ";"),
    hybas5_ids = str_c(sort(unique(na.omit(hybas5_id))), collapse = ";"),
    count_pfaf5_ids = n_distinct(pfaf5, na.rm = TRUE),
    count_hybas5_ids = n_distinct(hybas5_id, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pfaf5_ids = na_if(pfaf5_ids, ""),
    hybas5_ids = na_if(hybas5_ids, "")
  )

# Export enriched row-level and species-level tables
write_csv(
  hybas_migratory_enriched,
  here("output", "final_migratory_hybas_table_enriched.csv")
)

write_csv(
  agg_pfaf5,
  here("output", "migratory_species_pfaf5_hybas5_summary.csv")
)

# --- 7b. Merge IUCN-derived Pfaf5 info into master database -------------------

final_migratory <- migratory_flagged %>%
  left_join(
    agg_pfaf5,
    by = c("Species" = "sci_name")
  )


# --- 7c. Create supplementary HydroBASINS Level-5 basin lookup table ----------

# Region names based on first digit of HYBAS_ID
region_lookup <- tibble(
  hydro_region_code = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
  hydro_region_name = c(
    "Africa",
    "Europe_MiddleEast",
    "Siberia",
    "Asia",
    "Australia",
    "South_America",
    "North_America",
    "Arctic_North_America",
    "Greenland"
  )
)

hybas5_lookup <- hybas_lookup_all %>%
  filter(hybas_level == "05") %>%
  transmute(
    pfaf5 = PFAF_ID,
    hybas5_id = HYBAS_ID,
    pfaf3 = str_sub(PFAF_ID, 1, 3),
    hydro_region_code = str_sub(HYBAS_ID, 1, 1)
  ) %>%
  left_join(region_lookup, by = "hydro_region_code") %>%
  distinct(pfaf5, .keep_all = TRUE)

# --- 8. Merge manually corrected Level-5 HYBAS IDs ----------------------------
# This is still IUCN-derived information, but corrected manually for species
# where the standard HydroBASINS overlay did not work well.

library(tidyr)

shapefile_basins_long <- 
  read_csv2(
    here("data", "actual_finale_species_hybas_id_lvl_05.csv"),
    col_types = cols(
      HYBAS_id_final = col_character(),
      .default = col_guess()
    ),
    show_col_types = FALSE
  ) %>%
  mutate(
    HYBAS_id_final = as.character(HYBAS_id_final)
  ) %>%
  separate_rows(HYBAS_id_final, sep = ",") %>%
  mutate(
    shapefile_hybas5 = clean_hybas_id(HYBAS_id_final)
  ) %>%
  filter(!is.na(shapefile_hybas5), shapefile_hybas5 != "") %>%
  left_join(
    hybas5_lookup %>%
      rename(
        shapefile_pfaf5 = pfaf5,
        shapefile_hybas5 = hybas5_id
      ),
    by = "shapefile_hybas5"
  ) %>%
  mutate(
    shapefile_pfaf3 = str_sub(shapefile_pfaf5, 1, 3)
  )

# Summarise manually corrected IUCN shapefile basin information per species
shapefile_hybas_summary <- 
  shapefile_basins_long %>%
  distinct(sci_name, shapefile_hybas5, shapefile_pfaf5) %>%
  group_by(sci_name) %>%
  summarize(
    shapefile_hybas5_ids = str_c(sort(unique(na.omit(shapefile_hybas5))), collapse = ";"),
    shapefile_pfaf5_ids = str_c(sort(unique(na.omit(shapefile_pfaf5))), collapse = ";"),
    count_shapefile_hybas5_ids = n_distinct(shapefile_hybas5, na.rm = TRUE),
    count_shapefile_pfaf5_ids = n_distinct(shapefile_pfaf5, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    shapefile_hybas5_ids = na_if(shapefile_hybas5_ids, ""),
    shapefile_pfaf5_ids = na_if(shapefile_pfaf5_ids, "")
  )

# Remove old shapefile columns if rerunning the script interactively
final_migratory <- final_migratory %>%
  select(
    -matches("^shapefile_"),
    -matches("^count_shapefile_")
  )

# Merge manually corrected IUCN shapefile information into final_migratory
final_migratory <- final_migratory %>%
  left_join(
    shapefile_hybas_summary,
    by = c("Species" = "sci_name")
  )


# --- 9. Read GBIF Pfafstetter codes and filter using IUCN Pfaf3 --------------
# GBIF is treated as an addition to the IUCN-based distribution.
# Therefore, for species with any IUCN spatial information, GBIF records are kept
# only if they fall within the same Pfafstetter level-3 basin as either:
# 1) standard IUCN HYBAS data, or
# 2) manually corrected IUCN shapefile data.
# If no IUCN spatial information exists at all, GBIF is retained.

gbif_basins <- 
  read_csv(
    here("data", "gbif_migratory_fish_hybas00_pfafstetter.csv"),
    col_types = cols(
      HYBAS_ID = col_character(),
      PFAF_12  = col_character(),
      .default = col_guess()
    ),
    show_col_types = FALSE
  ) %>%
  mutate(
    gbif_hybas00 = str_trim(HYBAS_ID),
    gbif_pfaf12  = str_trim(PFAF_12),
    gbif_pfaf8   = str_sub(gbif_pfaf12, 1, 8),
    gbif_pfaf5   = str_sub(gbif_pfaf12, 1, 5),
    gbif_pfaf3   = str_sub(gbif_pfaf12, 1, 3)
  ) %>%
  filter(!is.na(gbif_pfaf12), gbif_pfaf12 != "")

# IUCN Pfaf3 reference per species, including manually corrected IUCN shapefile data
iucn_pfaf3_reference <- final_migratory %>%
  select(Species, pfaf5_ids, shapefile_pfaf5_ids) %>%
  rowwise() %>%
  mutate(
    combined_iucn_pfaf5 = list(c(
      str_split(coalesce(pfaf5_ids, ""), ";")[[1]],
      str_split(coalesce(shapefile_pfaf5_ids, ""), ";")[[1]]
    )),
    combined_iucn_pfaf5 = list(
      combined_iucn_pfaf5[
        combined_iucn_pfaf5 != "" &
          !is.na(combined_iucn_pfaf5)
      ]
    ),
    iucn_pfaf3_set = list(
      sort(unique(str_sub(combined_iucn_pfaf5, 1, 3)))
    )
  ) %>%
  ungroup() %>%
  filter(lengths(iucn_pfaf3_set) > 0) %>%
  select(Species, iucn_pfaf3_set)

# Add IUCN Pfaf3 reference to GBIF records and filter
gbif_basins_filtered <- gbif_basins %>%
  left_join(
    iucn_pfaf3_reference,
    by = c("verbatim_name" = "Species")
  ) %>%
  rowwise() %>%
  mutate(
    has_iucn_reference =
      !(length(iucn_pfaf3_set) == 0 || all(is.na(iucn_pfaf3_set))),
    
    keep_gbif = if (has_iucn_reference) {
      gbif_pfaf3 %in% iucn_pfaf3_set
    } else {
      TRUE
    }
  ) %>%
  ungroup() %>%
  filter(keep_gbif)

# QC: how much GBIF was removed by Pfaf3 filtering?
gbif_filter_qc <- gbif_basins %>%
  distinct(verbatim_name, gbif_pfaf5, gbif_pfaf3) %>%
  group_by(verbatim_name) %>%
  summarise(
    gbif_pfaf5_before_filter = n_distinct(gbif_pfaf5),
    .groups = "drop"
  ) %>%
  left_join(
    gbif_basins_filtered %>%
      distinct(verbatim_name, gbif_pfaf5, gbif_pfaf3) %>%
      group_by(verbatim_name) %>%
      summarise(
        gbif_pfaf5_after_filter = n_distinct(gbif_pfaf5),
        .groups = "drop"
      ),
    by = "verbatim_name"
  ) %>%
  mutate(
    gbif_pfaf5_after_filter = replace_na(gbif_pfaf5_after_filter, 0L),
    gbif_pfaf5_removed = gbif_pfaf5_before_filter - gbif_pfaf5_after_filter
  )

print(gbif_filter_qc %>% summarise(
  species_with_gbif_removed = sum(gbif_pfaf5_removed > 0),
  total_gbif_pfaf5_removed = sum(gbif_pfaf5_removed)
))

# Species-level GBIF summary after filtering
gbif_pfaf_summary <- 
  gbif_basins_filtered %>%
  distinct(verbatim_name, gbif_hybas00, gbif_pfaf5, gbif_pfaf8, gbif_pfaf12) %>%
  group_by(verbatim_name) %>%
  summarise(
    gbif_pfaf5_ids = str_c(sort(unique(na.omit(gbif_pfaf5))), collapse = ";"),
    gbif_pfaf8_ids = str_c(sort(unique(na.omit(gbif_pfaf8))), collapse = ";"),
    gbif_pfaf12_ids = str_c(sort(unique(na.omit(gbif_pfaf12))), collapse = ";"),
    gbif_hybas00_ids = str_c(sort(unique(na.omit(gbif_hybas00))), collapse = ";"),
    count_gbif_pfaf5_ids = n_distinct(gbif_pfaf5, na.rm = TRUE),
    count_gbif_pfaf8_ids = n_distinct(gbif_pfaf8, na.rm = TRUE),
    count_gbif_pfaf12_ids = n_distinct(gbif_pfaf12, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    gbif_pfaf5_ids = na_if(gbif_pfaf5_ids, ""),
    gbif_pfaf8_ids = na_if(gbif_pfaf8_ids, ""),
    gbif_pfaf12_ids = na_if(gbif_pfaf12_ids, ""),
    gbif_hybas00_ids = na_if(gbif_hybas00_ids, "")
  )

# Remove old GBIF columns if rerunning the script interactively
final_migratory <- final_migratory %>%
  select(
    -matches("^gbif_"),
    -matches("^count_gbif_")
  )

# Merge filtered GBIF information into final_migratory
final_migratory <- final_migratory %>%
  left_join(
    gbif_pfaf_summary,
    by = c("Species" = "verbatim_name")
  )

# --- 9b. Add additional source/name/original migration information ------------

# Read full database as character to avoid type issues
fullinfo_raw <- read_csv(
  here("data", "Master_Database_TS_fullinfo.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# The first row contains the detailed column labels; extract them
detail_names <- fullinfo_raw[1, ]

# Remove the first descriptive row so only species records remain
fullinfo_data <- fullinfo_raw %>%
  slice(-1)

# Helper to clean column names
clean_colname <- function(x) {
  x %>%
    str_replace_all("[\r\n]", "_") %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    str_to_lower()
}

# Column groups in the full database
additional_source_cols <- names(fullinfo_raw)[10:19]
database_name_cols     <- names(fullinfo_raw)[20:32]
original_migration_cols <- names(fullinfo_raw)[33:45]

# Rename grouped columns using the detailed labels from the first row
rename_from_detail_row <- function(data, cols, prefix) {
  
  new_names <- detail_names %>%
    select(all_of(cols)) %>%
    unlist(use.names = FALSE) %>%
    clean_colname()
  
  new_names <- paste0(prefix, new_names)
  
  data %>%
    select(Species, all_of(cols)) %>%
    rename_with(
      ~ new_names,
      all_of(cols)
    )
}

additional_sources_info <- rename_from_detail_row(
  fullinfo_data,
  additional_source_cols,
  "additional_"
)

database_names_info <- rename_from_detail_row(
  fullinfo_data,
  database_name_cols,
  "database_name_"
)

original_migration_info <- rename_from_detail_row(
  fullinfo_data,
  original_migration_cols,
  "original_migration_"
)

# Combine selected full-info columns into one table
fullinfo_selected <- additional_sources_info %>%
  left_join(database_names_info, by = "Species") %>%
  left_join(original_migration_info, by = "Species") %>%
  distinct(Species, .keep_all = TRUE)

# Join to final migratory database before publication cleanup
final_migratory <- final_migratory %>%
  left_join(fullinfo_selected, by = "Species")


# --- 10. Clean final database for publication ---------------------------------

final_migratory_clean <- final_migratory %>%
  rowwise() %>%
  mutate(
    
    final_pfaf5_ids = {
      ids <- c(
        str_split(coalesce(pfaf5_ids, ""), ";")[[1]],
        str_split(coalesce(gbif_pfaf5_ids, ""), ";")[[1]],
        str_split(coalesce(shapefile_pfaf5_ids, ""), ";")[[1]]
      )
      
      ids <- ids[ids != "" & !is.na(ids)]
      
      if (length(ids) == 0) {
        NA_character_
      } else {
        str_c(sort(unique(ids)), collapse = ";")
      }
    },
    
    final_hybas5_ids = {
      ids <- c(
        str_split(coalesce(hybas5_ids, ""), ";")[[1]],
        str_split(coalesce(shapefile_hybas5_ids, ""), ";")[[1]]
      )
      
      ids <- ids[ids != "" & !is.na(ids)]
      
      if (length(ids) == 0) {
        NA_character_
      } else {
        str_c(sort(unique(ids)), collapse = ";")
      }
    },
    
    count_final_pfaf5_ids = if_else(
      is.na(final_pfaf5_ids),
      0L,
      str_count(final_pfaf5_ids, ";") + 1L
    ),
    
    spatial_data_sources = {
      sources <- c(
        if (!is.na(pfaf5_ids)) "IUCN",
        if (!is.na(gbif_pfaf5_ids)) "GBIF_filtered_Pfaf3",
        if (!is.na(shapefile_pfaf5_ids)) "Manual_shapefile"
      )
      
      if (length(sources) == 0) {
        NA_character_
      } else {
        str_c(sources, collapse = ";")
      }
    }
  ) %>%
  ungroup()

final_migratory_clean <- final_migratory_clean %>%
  select(
    Species,
    ID,
    Threat_Category,
    Population_Trend,
    Last_Updated,
    Migration_class,
    Length_km,
    Population_homogeneity,
    Confidence,
    hybas_present,
    final_pfaf5_ids,
    final_hybas5_ids,
    count_final_pfaf5_ids,
    spatial_data_sources,
    starts_with("additional_"),
    starts_with("database_name_"),
    starts_with("original_migration_")
  )

# --- 10b. Add spatial context columns to final database -----------------------
# Basin names are not included in this release. This step only adds:
# 1) HydroBASINS continental regions, and
# 2) occupied Pfafstetter level-3 regions.
#
# A major-basin-name lookup can be added in a future release once a robust global
# basin-name table is available.

species_spatial_context <- final_migratory_clean %>%
  select(Species, final_pfaf5_ids) %>%
  separate_rows(final_pfaf5_ids, sep = ";") %>%
  mutate(pfaf5 = str_trim(final_pfaf5_ids)) %>%
  filter(!is.na(pfaf5), pfaf5 != "") %>%
  left_join(hybas5_lookup, by = "pfaf5") %>%
  group_by(Species) %>%
  summarise(
    hydrobasin_regions = str_c(
      sort(unique(na.omit(hydro_region_name))),
      collapse = ";"
    ),
    pfaf3_regions = str_c(
      sort(unique(na.omit(pfaf3))),
      collapse = ";"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    hydrobasin_regions = na_if(hydrobasin_regions, ""),
    pfaf3_regions = na_if(pfaf3_regions, "")
  )

final_migratory_clean <- final_migratory_clean %>%
  left_join(species_spatial_context, by = "Species")

# --- 11. Export final publication-ready database ------------------------------

dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)

write_csv(
  final_migratory_clean,
  here("output", "final_migratory_clean.csv")
)

write_csv(
  final_migratory_clean,
  here("output", "MigraFISH_v2026.1.csv")
)

# --- 12. Split final database by HydroBASINS region ---------------------------

dir.create(
  here("output", "regional_species_lists"),
  showWarnings = FALSE,
  recursive = TRUE
)

hydrobasin_region_names <- final_migratory_clean %>%
  separate_rows(hydrobasin_regions, sep = ";") %>%
  mutate(hydrobasin_regions = str_trim(hydrobasin_regions)) %>%
  filter(!is.na(hydrobasin_regions), hydrobasin_regions != "") %>%
  distinct(hydrobasin_regions) %>%
  arrange(hydrobasin_regions) %>%
  pull(hydrobasin_regions)

for (region in hydrobasin_region_names) {

  regional_species_list <- final_migratory_clean %>%
    filter(str_detect(hydrobasin_regions, fixed(region)))

  write_csv(
    regional_species_list,
    here(
      "output",
      "regional_species_lists",
      paste0("MigraFISH_", region, "_species_list.csv")
    )
  )
}

# --- 13. Final checks ---------------------------------------------------------

message("Final database rows: ", nrow(final_migratory_clean))
message("Species with spatial information: ", sum(!is.na(final_migratory_clean$final_pfaf5_ids)))
message("Final database written to: ", here("output", "MigraFISH_v2026.1.csv"))
