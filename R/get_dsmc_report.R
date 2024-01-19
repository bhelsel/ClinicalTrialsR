#' @title get_dsmc_report
#' @description Run the Data Safety Monitoring Committee (DSMC) Report
#' @param datadir Directory with the input files, Default: NULL
#' @return Returns a Word document containing the DSMC Report
#' @details Run the Data Safety Monitoring Committee (DSMC) Report
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[janitor]{clean_names}}
#'  \code{\link[rmarkdown]{render}}
#' @rdname get_dsmc_report
#' @export
#' @importFrom readr read_csv
#' @importFrom janitor make_clean_names
#' @importFrom rmarkdown render

get_dsmc_report <- function(datadir = NULL){

  redcap <- readr::read_csv(list.files(datadir, pattern = "REDCap", full.names = TRUE))
  data_dictionary <- readr::read_csv(list.files(datadir, pattern = "data_dictionary", full.names = TRUE))
  data_dictionary <- janitor::clean_names(data_dictionary)

  look_up_table <- load_data(data_dictionary)

  bold <- clean_merge(redcap, data_dictionary)

  #rmarkdown::render(input = file.path(data_dir,"dsmc_report","dsmc_report_open.Rmd"), output_file = file.path(output_data_dir,"dsmc_report_open.docx"))
  #rmarkdown::render(input = file.path(data_dir,'dsmc_report','dsmc_report_closed.Rmd'), output_file = file.path(output_data_dir,'dsmc_report_closed.docx'))

  return(NULL)

}




