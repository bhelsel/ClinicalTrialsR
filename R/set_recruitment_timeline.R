
start_date = "2021-03-29"
end_date = "2026-03-28"
recruited_by = c("2022-03-29", "2023-03-29", "2024-03-29", "2025-03-29")
n = 150
cohort = FALSE
equal_distribution = TRUE
weeks_to_enroll <- 6
screening_multiple = 3

rp <- set_recruitment_timeline(start_date, end_date, recruited_by, n, cohort, screening_multiple, equal_distribution, weeks_to_enroll)

ggplot(rp) +
  geom_path(aes(x = date, y = enrollment_cumulative)) +
  geom_path(aes(x = date, y = screening_cumulative), linetype = "dashed") +
  theme_classic()



set_recruitment_timeline <- function(start_date, end_date, recruited_by, n, cohort = TRUE,
                                     screening_multiple = 3,
                                     equal_distribution = TRUE, weeks_to_enroll){
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  recruited_by <- as.Date(recruited_by)
  days_to_enroll <- weeks_to_enroll * 7
  
  all_dates <- seq(start_date, end_date, 1)
  l <- length(all_dates)
  
  rp <- matrix(nrow = l, ncol = 6)
  
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
  
  if(all(days_to_enroll > n)) zeros <- days_to_enroll - n
  
  ebw <- 
    1:length(zeros) %>%
    lapply(., function(x) {
      c(rep(0, (zeros[x] + 1)), rep(1, n[x]))
      })
  
  for(i in 1:length(ebw)){
    rp[recruited_start_idx[i]:(recruited_by_idx[i]), "enrollment_by_week"] <- ebw[[i]]
  }
  
  rp$enrollment_by_week <- ifelse(is.na(rp$enrollment_by_week), 0, rp$enrollment_by_week)
  
  rp$screen_by_week <- rp$enrollment_by_week * screening_multiple
  
  rp %<>%
    mutate(week = 1 + as.numeric(rp$date - first(rp$date)) %/% 7, .after = "date") %>%
    group_by(week, year) %>%
    summarise(date = first(date),
              screen_by_week = sum(screen_by_week),
              enrollment_by_week = sum(enrollment_by_week), 
              .groups = "drop") %>%
    mutate(enrollment_cumulative = cumsum(enrollment_by_week),
           screening_cumulative = cumsum(screen_by_week))
  
  return(rp)
}
