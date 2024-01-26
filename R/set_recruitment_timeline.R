#' @title set_recruitment_timeline
#' @description A function to generate a recruitment timeline that can be
#'     adjusted manually to set an exact schedule.
#' @param start_date The clinical trial start date
#' @param end_date The clinical trial end date
#' @param recruited_by A vector of dates for
#' @param n The total number of participants that will be recruited for the clinical trial
#' @param screening_multiple A target goal for participant screening that equals `n` multiplied by the `screening_multiple`, Default: 2
#' @param weeks_to_enroll The number of weeks prior to the dates expressed in `recruited_by` that is needed to recruit the participants
#' @return Returns a data set and a plot of the recruitment timeline
#' @details A function to generate a recruitment timeline that can be
#'     adjusted manually to set an exact schedule.
#' @seealso
#'  \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{mutate}}
#' @rdname set_recruitment_timeline
#' @export
#' @importFrom dplyr as_tibble mutate first group_by summarise
#' @import ggplot2

set_recruitment_timeline <- function(start_date, end_date, recruited_by, n,
                                     screening_multiple = 2, weeks_to_enroll){

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  recruited_by <- as.Date(recruited_by)
  days_to_enroll <- weeks_to_enroll * 7

  all_dates <- seq(start_date, end_date, 1)

  rp <- matrix(nrow = length(all_dates), ncol = 6)

  labels <- c("Date", "Grant Year", "Screen Goal by Week", "Screen Goal Cumulative",
              "Enrollment Goal by Week", "Enrollment Goal Cumulative")

  colnames(rp) <- c("date", "year", "screen_by_week", "screening_cumulative",
                    "enrollment_by_week", "enrollment_cumulative")

  rp %<>%
    dplyr::as_tibble() %>%
    dplyr::mutate(date = all_dates, year = as.numeric(date - date[1]) %/% 365.25 + 1)

  if(length(recruited_by) != length(n)){
    r <- n %% length(recruited_by)
    n <- rep(n %/% length(recruited_by), length(recruited_by))
    n[length(n)] <- n[length(n)] + r
  }

  recruited_by_idx <- which(rp$date %in% recruited_by)
  rp[recruited_by_idx, "enrollment_cumulative"] <- cumsum(n)

  recruited_start_dates <- rp[recruited_by_idx, "date", drop = TRUE] - days_to_enroll
  recruited_start_idx <- which(rp$date %in% recruited_start_dates)
  rp[recruited_start_idx, "enrollment_cumulative"] <- cumsum(n) - n

  if(all(days_to_enroll > n)) {
    zeros <- days_to_enroll - n
    ebw <- lapply(1:length(zeros), function(x) c(rep(0, (zeros[x] + 1)), rep(1, n[x])))
  } else{
    ebw <- lapply(n %/% days_to_enroll, function(x) rep(x, days_to_enroll))
    l <- do.call("c", lapply(ebw, length))
    m <- l - (n - days_to_enroll)
    ebw <- lapply(1:length(ebw), function(x) c(0, ebw[[x]][1:(m[x]-1)], ebw[[x]][m[x]:l[x]] + 1))
    }

  for(i in 1:length(ebw)){
    rp[recruited_start_idx[i]:(recruited_by_idx[i]), "enrollment_by_week"] <- ebw[[i]]
  }

  rp$enrollment_by_week <- ifelse(is.na(rp$enrollment_by_week), 0, rp$enrollment_by_week)

  rp$screen_by_week <- rp$enrollment_by_week * screening_multiple

  rp %<>%
    dplyr::mutate(week = 1 + as.numeric(rp$date - dplyr::first(rp$date)) %/% 7, .after = "date") %>%
    dplyr::group_by(week, year) %>%
    dplyr::summarise(date = dplyr::first(date),
                     screen_by_week = sum(screen_by_week),
                     enrollment_by_week = sum(enrollment_by_week),
                     .groups = "drop") %>%
    dplyr::mutate(enrollment_cumulative = cumsum(enrollment_by_week),
                  screening_cumulative = cumsum(screen_by_week))

  p <-
    ggplot2::ggplot(rp) +
    ggplot2::geom_path(ggplot2::aes(x = date, y = screening_cumulative), linetype = "dashed", size = 1) +
    ggplot2::geom_path(ggplot2::aes(x = date, y = enrollment_cumulative), size = 1) +
    ggplot2::scale_y_continuous(breaks = seq(from = 0, to = max(rp$screening_cumulative), 50)) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(x = "Year", y = "Number of People Screened or Enrolled") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#ADD8E6"),
                   plot.background = ggplot2::element_rect(fill = "#ADD8E6"),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(color = "black", size = 1),
                   axis.text = ggplot2::element_text(size = 12, family = "Georgia"),
                   axis.title.x = ggplot2::element_text(
                     size = 14, face = "bold",
                     margin = ggplot2::margin(t = 15), family = "Georgia"),
                   axis.title.y = ggplot2::element_text(
                     size = 14, face = "bold",
                     margin = ggplot2::margin(r = 15), family = "Georgia"))

  return(list(p, rp))
}

