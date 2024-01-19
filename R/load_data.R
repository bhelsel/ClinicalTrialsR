#' @title load_data
#' @description Creates a lookup table from the data dictionary
#' @param codebook REDCap Codebook containing the adverse event, reporting, and protocol deviation instruments.
#' @return Returns a lookup tibble generated from the data dictionary
#' @details Creates a lookup table from the data dictionary
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[tidyr]{separate}}, \code{\link[tidyr]{pivot_longer}}
#' @rdname load_data
#' @export
#' @importFrom dplyr filter rename contains select mutate
#' @importFrom tidyr separate pivot_longer


load_data <- function(codebook){

  #### Convert Data Dictionary into Join Table ####
  look_up_table <-
    codebook %>%
    dplyr::filter(field_type %in% c("radio","dropdown")) %>%
    dplyr::rename(field = variable_field_name, choices = dplyr::contains("Choices")) %>%
    dplyr::select(field, choices) %>%
    tidyr::separate(choices, into = as.character(1:25), sep = "[|]")  %>%
    tidyr::pivot_longer(cols = -field) %>%
    dplyr::filter(!is.na(value)) %>%
    tidyr::separate(value, into = c("id_num","id_name"), sep = "[,]")  %>%
    dplyr::select(field, id_num, id_name)  %>%
    dplyr::mutate(id_num = as.numeric(trimws(id_num)))


  return(look_up_table)

}
