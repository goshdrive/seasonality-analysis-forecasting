analyse_dia_rates <- function(df_first_dia, sdate, edate) {
  
  # Extract useful years
  ys <- year(sdate)
  ye <- year(edate)
  yyyy_range = ys:ye
  
  # Filter to ensure all dia within input range
  df_filtered <- df_first_dia %>%
    filter(dia_date >= sdate & dia_date <= edate)
  
  # Compute the number of diagnoses per year
  yearly_count <- df_filtered %>%
    mutate(yyyy = year(dia_date)) %>%
    group_by(yyyy) %>%
    count() %>%
    ungroup() %>%
    complete(yyyy=yyyy_range, fill = list(n = 0))
  
  # Counts pre and post epic and covid
  days <- as.double(edate - sdate, unit='days')
  days_pre_epic <- as.double(epic_gosh_introduction - sdate, unit='days')
  days_post_epic <- as.double(edate - epic_gosh_introduction, unit='days')
  days_pre_covid <- as.double(national_covid_lockdowns[[1]][[1]] - sdate, unit='days')
  days_post_covid <- as.double(edate - national_covid_lockdowns[[1]][[1]], unit='days')
  n <- df_filtered %>% count() %>% .$n
  n_pre_epic <- df_filtered %>% filter(dia_date < epic_gosh_introduction) %>% count() %>% .$n
  n_post_epic <- df_filtered %>% filter(dia_date >= epic_gosh_introduction) %>% count() %>% .$n
  n_pre_covid <- df_filtered %>% filter(dia_date < national_covid_lockdowns[[1]][[1]]) %>% count() %>% .$n
  n_post_covid <- df_filtered %>% filter(dia_date >= national_covid_lockdowns[[1]][[1]]) %>% count() %>% .$n
  
  # Mean values
  output <- yearly_count %>%
    pivot_wider(names_from = yyyy, values_from = n) %>%
    mutate(
      mean = n / days * 365,
      mean_pre_epic = n_pre_epic / days_pre_epic * 365,
      mean_post_epic = n_post_epic / days_post_epic * 365,
      epic_ratio = mean_post_epic / mean_pre_epic,
      mean_pre_covid = n_pre_covid / days_pre_covid * 365,
      mean_post_covid = n_post_covid / days_post_covid * 365,
      covid_ratio = mean_post_covid / mean_pre_covid,
      median = median(yearly_count$n)
    )
  
  return(output)
  
}
