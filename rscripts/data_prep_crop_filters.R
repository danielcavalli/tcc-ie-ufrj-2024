# ==============================================================================
# DATA PREPARATION: CROP-SPECIFIC FILTERS
# ==============================================================================
# Purpose: Filter dataset to include only microregions where specific crops are
#          economically viable, ensuring valid counterfactuals for DiD analysis
#
# Filter Logic:
#   Keep microregions that produce the crop in at least one year (default)
#   - Distinguishes viable regions from structurally impossible regions
#   - Preserves both extensive margin (adoption) and intensive margin (growth)
#   - Maintains full panel (all years) for qualifying microregions
#
# Author: Daniel Cavalli
# Date: 2025-01-16
# Updated: 2025-01-16 (generalized for all crops)
# ==============================================================================

library(dplyr)
library(tidyr)
library(cli)

#' Filter dataset for crop-producing microregions (GENERIC)
#'
#' @param df Data frame with microregion-year panel structure
#' @param crop_var Name of crop area variable (e.g., "area_plantada_cana")
#' @param crop_name Crop name for display purposes (e.g., "Cana-de-açúcar")
#' @param min_years_producing Minimum years with crop > 0. If NULL or 0, includes
#'        any microregion with production in at least one year (default: NULL)
#' @param verbose Print diagnostic information (default: TRUE)
#'
#' @return Filtered data frame with all years for qualifying microregions
#'
#' @details
#' This is a GENERIC function that works for any crop. It identifies microregions
#' where the specified crop cultivation occurs. By default (min_years_producing = NULL),
#' it includes ALL microregions that produced the crop in at least one year.
#'
#' All years of data are retained for microregions that meet the criteria.
#'
#' The filter ensures the analysis focuses on regions where:
#' - The crop is part of the agricultural landscape
#' - Weather information quality is likely to matter for planting decisions
#' - Both extensive (adoption) and intensive (growth) margins are relevant
#'
#' @examples
#' # Filter for sugarcane producers
#' df_cana <- filter_crop_producers(df, "area_plantada_cana", "Cana-de-açúcar")
#'
#' # Filter for soybean producers
#' df_soja <- filter_crop_producers(df, "area_plantada_soja", "Soja")
#'
#' # Filter for rice producers with minimum 5 years
#' df_arroz <- filter_crop_producers(df, "area_plantada_arroz", "Arroz", min_years_producing = 5)
filter_crop_producers <- function(df,
                                   crop_var,
                                   crop_name,
                                   min_years_producing = NULL,
                                   verbose = TRUE) {

  # Validate inputs
  if (!crop_var %in% names(df)) {
    cli::cli_abort("Variable {.var {crop_var}} not found in dataset")
  }

  # Store original dimensions
  original_n_obs <- nrow(df)
  original_n_microregions <- n_distinct(df$id_microrregiao)

  # Set default threshold if not provided
  if (is.null(min_years_producing)) {
    min_years_producing <- 1
    filter_label <- "any production (≥1 year)"
  } else if (min_years_producing == 0) {
    min_years_producing <- 1
    filter_label <- "any production (≥1 year)"
  } else {
    filter_label <- sprintf("persistent production (≥%d years)", min_years_producing)
  }

  if (verbose) {
    cat("\n==============================================================================\n")
    cat(sprintf("FILTERING FOR %s-PRODUCING MICROREGIONS\n", toupper(crop_name)))
    cat("==============================================================================\n\n")
    cat("Original dataset:\n")
    cat(sprintf("  - Observations: %d\n", original_n_obs))
    cat(sprintf("  - Microregions: %d\n", original_n_microregions))
    cat(sprintf("  - Years per microregion: %.1f\n\n",
                original_n_obs / original_n_microregions))
    cat(sprintf("Filter criterion: %s\n\n", filter_label))
  }

  # Identify qualifying microregions based on years with crop production
  crop_producers <- df %>%
    group_by(id_microrregiao) %>%
    summarise(
      years_with_crop = sum(!is.na(.data[[crop_var]]) & .data[[crop_var]] > 0),
      total_crop_area = sum(.data[[crop_var]], na.rm = TRUE),
      avg_crop_area = mean(.data[[crop_var]][.data[[crop_var]] > 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(years_with_crop >= min_years_producing)

  qualifying_microregions <- crop_producers %>% pull(id_microrregiao)

  if (verbose) {
    cat(sprintf("Qualifying microregions: %d\n", length(qualifying_microregions)))
    cat(sprintf("  - Distribution of years with %s:\n", crop_name))
    cat(sprintf("    Min: %d, Median: %.0f, Mean: %.1f, Max: %d\n\n",
                min(crop_producers$years_with_crop),
                median(crop_producers$years_with_crop),
                mean(crop_producers$years_with_crop),
                max(crop_producers$years_with_crop)))
  }

  # Filter dataset: keep all years for qualifying microregions
  df_filtered <- df %>%
    filter(id_microrregiao %in% qualifying_microregions)

  # Calculate retention statistics
  filtered_n_obs <- nrow(df_filtered)
  filtered_n_microregions <- n_distinct(df_filtered$id_microrregiao)
  retention_rate <- filtered_n_microregions / original_n_microregions * 100

  if (verbose) {
    cat("Filtered dataset:\n")
    cat(sprintf("  - Observations: %d (%.1f%% retained)\n",
                filtered_n_obs,
                filtered_n_obs / original_n_obs * 100))
    cat(sprintf("  - Microregions: %d (%.1f%% retained)\n",
                filtered_n_microregions,
                retention_rate))
    cat(sprintf("  - Years per microregion: %.1f\n\n",
                filtered_n_obs / filtered_n_microregions))

    # Treatment distribution in filtered data
    if ("tratado" %in% names(df_filtered)) {
      treated_micro <- df_filtered %>%
        filter(tratado == 1) %>%
        distinct(id_microrregiao) %>%
        nrow()

      cat("Treatment distribution in filtered data:\n")
      cat(sprintf("  - Treated microregions: %d (%.1f%%)\n",
                  treated_micro,
                  treated_micro / filtered_n_microregions * 100))
      cat(sprintf("  - Control microregions: %d (%.1f%%)\n\n",
                  filtered_n_microregions - treated_micro,
                  (filtered_n_microregions - treated_micro) / filtered_n_microregions * 100))
    }

    cat("==============================================================================\n\n")
  }

  # Add attributes to track filter parameters
  attr(df_filtered, "filter_type") <- sprintf("%s_producers", tolower(crop_name))
  attr(df_filtered, "crop_var") <- crop_var
  attr(df_filtered, "crop_name") <- crop_name
  attr(df_filtered, "min_years_producing") <- min_years_producing
  attr(df_filtered, "n_qualifying_microregions") <- filtered_n_microregions
  attr(df_filtered, "retention_rate") <- retention_rate

  return(df_filtered)
}


#' Filter dataset for sugarcane-producing microregions (CONVENIENCE WRAPPER)
#'
#' @param df Data frame with microregion-year panel structure
#' @param min_years_producing Minimum years with cana > 0. If NULL or 0, includes
#'        any microregion with cana production in at least one year (default: NULL)
#' @param cana_var Name of sugarcane area variable (default: "area_plantada_cana")
#' @param verbose Print diagnostic information (default: TRUE)
#'
#' @return Filtered data frame with all years for qualifying microregions
#'
#' @examples
#' df_filtered <- filter_cana_producers(df_raw)  # All cana producers
#' df_filtered <- filter_cana_producers(df_raw, min_years_producing = 10)  # Persistent only
filter_cana_producers <- function(df,
                                   min_years_producing = NULL,
                                   cana_var = "area_plantada_cana",
                                   verbose = TRUE) {
  filter_crop_producers(
    df = df,
    crop_var = cana_var,
    crop_name = "Cana-de-açúcar",
    min_years_producing = min_years_producing,
    verbose = verbose
  )
}


#' Filter dataset for soybean-producing microregions (CONVENIENCE WRAPPER)
#'
#' @param df Data frame with microregion-year panel structure
#' @param min_years_producing Minimum years with soja > 0 (default: NULL = any production)
#' @param soja_var Name of soybean area variable (default: "area_plantada_soja")
#' @param verbose Print diagnostic information (default: TRUE)
#'
#' @return Filtered data frame with all years for qualifying microregions
filter_soja_producers <- function(df,
                                   min_years_producing = NULL,
                                   soja_var = "area_plantada_soja",
                                   verbose = TRUE) {
  filter_crop_producers(
    df = df,
    crop_var = soja_var,
    crop_name = "Soja",
    min_years_producing = min_years_producing,
    verbose = verbose
  )
}


#' Filter dataset for rice-producing microregions (CONVENIENCE WRAPPER)
#'
#' @param df Data frame with microregion-year panel structure
#' @param min_years_producing Minimum years with arroz > 0 (default: NULL = any production)
#' @param arroz_var Name of rice area variable (default: "area_plantada_arroz")
#' @param verbose Print diagnostic information (default: TRUE)
#'
#' @return Filtered data frame with all years for qualifying microregions
filter_arroz_producers <- function(df,
                                    min_years_producing = NULL,
                                    arroz_var = "area_plantada_arroz",
                                    verbose = TRUE) {
  filter_crop_producers(
    df = df,
    crop_var = arroz_var,
    crop_name = "Arroz",
    min_years_producing = min_years_producing,
    verbose = verbose
  )
}


#' Get summary statistics for filtered dataset
#'
#' @param df_filtered Filtered data frame (output from filter_cana_producers)
#'
#' @return Named list with summary statistics
get_filter_summary <- function(df_filtered) {
  list(
    filter_type = attr(df_filtered, "filter_type"),
    min_years_producing = attr(df_filtered, "min_years_producing"),
    n_microregions = attr(df_filtered, "n_qualifying_microregions"),
    retention_rate = attr(df_filtered, "retention_rate"),
    n_observations = nrow(df_filtered),
    n_years = n_distinct(df_filtered$ano)
  )
}


#' Validate filter results
#'
#' @param df_original Original unfiltered data
#' @param df_filtered Filtered data
#' @param verbose Print validation results (default: TRUE)
#'
#' @return Logical indicating whether validation passed
validate_filter <- function(df_original, df_filtered, verbose = TRUE) {

  validation_passed <- TRUE

  if (verbose) {
    cat("\n==============================================================================\n")
    cat("FILTER VALIDATION\n")
    cat("==============================================================================\n\n")
  }

  # Check 1: All years retained for qualifying microregions
  years_per_micro_filtered <- df_filtered %>%
    count(id_microrregiao) %>%
    pull(n)

  expected_years <- n_distinct(df_original$ano)
  years_check <- all(years_per_micro_filtered == expected_years)

  if (verbose) {
    cat("Check 1 - All years retained for qualifying microregions:\n")
    cat(sprintf("  - Expected years per microregion: %d\n", expected_years))
    cat(sprintf("  - Actual range: [%d, %d]\n",
                min(years_per_micro_filtered),
                max(years_per_micro_filtered)))
    cat(sprintf("  - Status: %s\n\n", ifelse(years_check, "PASS", "FAIL")))
  }

  if (!years_check) validation_passed <- FALSE

  # Check 2: No microregions partially included
  filtered_microregions <- unique(df_filtered$id_microrregiao)
  partial_inclusion_check <- df_original %>%
    filter(id_microrregiao %in% filtered_microregions) %>%
    nrow() == nrow(df_filtered)

  if (verbose) {
    cat("Check 2 - No partial inclusion of microregions:\n")
    cat(sprintf("  - All observations for qualifying microregions included: %s\n\n",
                ifelse(partial_inclusion_check, "PASS", "FAIL")))
  }

  if (!partial_inclusion_check) validation_passed <- FALSE

  # Check 3: Variable completeness
  key_vars <- c("ano", "id_microrregiao", "tratado", "pib_agropecuario")
  missing_vars <- setdiff(key_vars, names(df_filtered))

  if (verbose) {
    cat("Check 3 - Key variables present:\n")
    if (length(missing_vars) == 0) {
      cat("  - All key variables present: PASS\n\n")
    } else {
      cat(sprintf("  - Missing variables: %s\n", paste(missing_vars, collapse = ", ")))
      cat("  - Status: FAIL\n\n")
      validation_passed <- FALSE
    }
  }

  if (verbose) {
    cat("==============================================================================\n")
    cat(sprintf("Overall validation: %s\n",
                ifelse(validation_passed, "PASSED", "FAILED")))
    cat("==============================================================================\n\n")
  }

  return(validation_passed)
}
