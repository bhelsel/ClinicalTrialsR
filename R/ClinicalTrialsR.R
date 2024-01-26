# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".", "dob", "race8", "choices", "date_fields", "field",
    "field_type", "id_name", "id_num", "race1", "value", "variable_field_name",
    "enrollment_by_week", "enrollment_cumulative", "screen_by_week",
    "screening_cumulative", "week", "year")
)

#' @title ClinicalTrialsR: R Package for Automating Clinical Trial Reporting.
#'
#' @description The purpose of this R Package is to automate clinical trial reporting for
#'   federally funded research studies.
#'
#' @section ClinicalTrialsR functions:
#'
#' \code{\link{get_dsmc_report}}
#'
#' \code{\link{load_data}}
#'
#' \code{\link{clean_merge}}
#'
#' @docType package
#'
#' @name ClinicalTrialsR
#'
#' @import magrittr

NULL
