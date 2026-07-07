# ------------------------------------------------------------------------------
# MigraFISH v2026.1 - create public release files
#
# Purpose:
# This script takes the publication-ready MigraFISH CSV produced by
# 01_prepare_spatial_data.R and creates release-support files for GitHub/Zenodo:
#   - optional Excel workbook
#   - data dictionary template
#   - QC summaries
#   - version history
#
# Run after 01_prepare_spatial_data.R.
# ------------------------------------------------------------------------------

rm(list = ls(all = TRUE))

RELEASE_VERSION <- "v2026.1"
RELEASE_DATE <- Sys.Date()

required_packages <- c("tidyverse", "here", "readr", "stringr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(tidyverse)
library(here)
library(readr)
library(stringr)

release_dir <- here("data_release")
metadata_dir <- here("metadata")
documentation_dir <- here("documentation")

dir.create(release_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(metadata_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(documentation_dir, showWarnings = FALSE, recursive = TRUE)

db_file <- file.path(release_dir, paste0("MigraFISH_", RELEASE_VERSION, ".csv"))

if (!file.exists(db_file)) {
  stop(
    "Could not find ", db_file,
    ". Run 01_prepare_spatial_data.R first.",
    call. = FALSE
  )
}

# Read as character to preserve identifiers exactly
migrafish <- read_csv(
  db_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# --- 1. Data dictionary template ---------------------------------------------

get_allowed_values <- function(x, max_values = 20) {
  vals <- sort(unique(na.omit(x)))
  vals <- vals[vals != ""]
  if (length(vals) == 0 || length(vals) > max_values) return(NA_character_)
  paste(vals, collapse = "; ")
}

data_dictionary <- tibble(
  variable = names(migrafish),
  description = NA_character_,
  type = vapply(migrafish, function(x) class(x)[1], character(1)),
  allowed_values_or_format = vapply(migrafish, get_allowed_values, character(1)),
  source = NA_character_,
  notes = NA_character_
)

write_csv(
  data_dictionary,
  file.path(metadata_dir, paste0("MigraFISH_data_dictionary_template_", RELEASE_VERSION, ".csv"))
)

# --- 2. QC summaries ----------------------------------------------------------

migration_class_counts <- migrafish %>%
  count(Migration_class, sort = TRUE)

spatial_source_counts <- migrafish %>%
  count(spatial_data_sources, sort = TRUE)

threat_category_counts <- migrafish %>%
  count(Threat_Category, sort = TRUE)

duplicate_species <- migrafish %>%
  count(Species) %>%
  filter(n > 1)

unique_pfaf5_basins <- migrafish %>%
  separate_rows(final_pfaf5_ids, sep = ";") %>%
  mutate(final_pfaf5_ids = str_trim(final_pfaf5_ids)) %>%
  filter(!is.na(final_pfaf5_ids), final_pfaf5_ids != "") %>%
  summarise(n = n_distinct(final_pfaf5_ids)) %>%
  pull(n)

qc_summary <- tibble(
  check = c(
    "release_version",
    "release_date",
    "n_rows",
    "n_unique_species",
    "n_duplicate_species",
    "n_species_with_spatial_information",
    "n_unique_pfaf5_basins",
    "n_columns"
  ),
  value = c(
    RELEASE_VERSION,
    as.character(RELEASE_DATE),
    as.character(nrow(migrafish)),
    as.character(n_distinct(migrafish$Species)),
    as.character(nrow(duplicate_species)),
    as.character(sum(!is.na(migrafish$final_pfaf5_ids) & migrafish$final_pfaf5_ids != "")),
    as.character(unique_pfaf5_basins),
    as.character(ncol(migrafish))
  )
)

write_csv(qc_summary, file.path(documentation_dir, paste0("MigraFISH_QC_summary_", RELEASE_VERSION, ".csv")))
write_csv(migration_class_counts, file.path(documentation_dir, paste0("MigraFISH_migration_class_counts_", RELEASE_VERSION, ".csv")))
write_csv(spatial_source_counts, file.path(documentation_dir, paste0("MigraFISH_spatial_source_counts_", RELEASE_VERSION, ".csv")))
write_csv(threat_category_counts, file.path(documentation_dir, paste0("MigraFISH_threat_category_counts_", RELEASE_VERSION, ".csv")))
write_csv(duplicate_species, file.path(documentation_dir, paste0("MigraFISH_duplicate_species_check_", RELEASE_VERSION, ".csv")))

# --- 3. Version history -------------------------------------------------------

version_history_file <- file.path(documentation_dir, "MigraFISH_version_history.csv")

new_version_row <- tibble(
  version = RELEASE_VERSION,
  release_date = as.character(RELEASE_DATE),
  n_species = nrow(migrafish),
  n_species_with_spatial_information = sum(!is.na(migrafish$final_pfaf5_ids) & migrafish$final_pfaf5_ids != ""),
  n_unique_pfaf5_basins = unique_pfaf5_basins,
  notes = "First public MigraFISH release accompanying the data descriptor manuscript."
)

if (file.exists(version_history_file)) {
  version_history <- read_csv(version_history_file, show_col_types = FALSE) %>%
    filter(version != RELEASE_VERSION) %>%
    bind_rows(new_version_row) %>%
    arrange(version)
} else {
  version_history <- new_version_row
}

write_csv(version_history, version_history_file)

# --- 4. Optional Excel workbook ----------------------------------------------

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    list(
      MigraFISH = migrafish,
      data_dictionary_template = data_dictionary,
      QC_summary = qc_summary,
      migration_class_counts = migration_class_counts,
      spatial_source_counts = spatial_source_counts,
      threat_category_counts = threat_category_counts
    ),
    path = file.path(release_dir, paste0("MigraFISH_", RELEASE_VERSION, ".xlsx"))
  )
} else {
  message(
    "Package 'writexl' is not installed, so no Excel workbook was created. ",
    "Install it with install.packages('writexl') if you want an .xlsx release file."
  )
}

# --- 5. Final console summary -------------------------------------------------

print(qc_summary)
message("Release-support files written to:")
message("  ", release_dir)
message("  ", metadata_dir)
message("  ", documentation_dir)
