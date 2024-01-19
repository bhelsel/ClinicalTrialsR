#' @title clean_merge
#' @description Extracts all date and datetime variables and from the data dictionary and cleans REDCap data
#' @param redcap Data from the REDCap instruments
#' @param codebook REDCap Codebook containing the adverse event, reporting, and protocol deviation instruments.
#' @return A list containing the date and datetime variables and cleaned REDCap data
#' @details Extracts all date and datetime variables and from the data dictionary and cleans REDCap data
#' @seealso
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}
#'  \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{mutate_all}}
#'  \code{\link[dplyr]{vars}}, \code{\link[dplyr]{any_of}}
#'  \code{\link[lubridate]{ymd}}
#' @rdname clean_merge
#' @export
#' @importFrom dplyr mutate select case_when mutate_at vars any_of
#' @importFrom lubridate ymd

clean_merge <- function(redcap, codebook){

  #### Prep Data Dictionary ####
  txtval <- codebook$text_validation_type_or_show_slider_number

  date_fields <- c("date", "date_mdy", "datetime_seconds_mdy", "datetime_mdy")

  date_fields %<>%
    lapply(., function(x){
      codebook[grepl(x, txtval), "variable_field_name", drop = TRUE]
  }) %>%
    `names<-`(date_fields) %>%
    Filter(length, .)

    redcap %<>%
    `colnames<-`(gsub("scrn_|___", "", colnames(redcap))) %>%
    dplyr::mutate(age = floor(as.numeric(difftime(date, dob, units = "days") / 365.25)),
                  mixed_race = ifelse(rowSums(dplyr::select(., race8:race1)) >= 2, 1, 0),
                  race = dplyr::case_when(
                    mixed_race == 1 ~ "Mixed Race",
                    race1 == 1 ~ "White",
                    race2 == 1 ~ "Black, African American, or African",
                    race4 == 1 ~ "Asian",
                    race8 == 1 ~ "American Indian or Alaska Native",
                    race16 == 1 ~ "Native Hawaiian or Other Pacific Islanders",
                    race32 == 1 ~ "None of these fully describe me",
                    race64 == 1 ~ "Prefer not to answer",
                    race256 == 1 ~ "Middle Eastern or North African",
                    TRUE ~ NA),
                  sex = dplyr::case_when(sex == 1 ~ "Man", sex == 2 ~ "Woman", TRUE ~ NA),
                  ethnicity  = dplyr::case_when(
                    ethn == 1 ~ "Not Hispanic or Latino",
                    ethn == 2 ~ "Hispanic or Latino",
                    TRUE ~ NA)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::any_of(date_fields[[1]])), ~lubridate::ymd(.))

  return(list(date_fields = date_fields, redcap = redcap))
}




