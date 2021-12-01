##' @param df_first_dia Data frames of first diagnoses
##' @param sdate Start date of period to study
##' @param edate End date of period to study
wrangle_dia_rate <- function(df_first_dia, sdate, edate, name) {
  
  name = factor(name)
  
  dia_rates <- df_first_dia %>%
    # Get the number of diagnoses per day in the dataset
    group_by(dia_date) %>%
    summarize(label = name, n = n()) %>%
    # Filter any outside of requested date range
    filter(dia_date >= sdate & dia_date <= edate) %>%
    # Complete into the full required date range so every day has a value
    complete(
      dia_date = as_date(sdate:edate),
      fill = list(label = name, n = 0)) %>%
    arrange() %>%
    # Add the 30 and 365 day moving averages
    mutate(
      across(
        n,
        list(
          "030da" = ~ zoo::rollmean(.x, k = 30, fill = NA, align = 'center'),
          "365da" = ~ zoo::rollmean(.x, k = 365, fill = NA, align = 'center')
    )))
  
  return(dia_rates)
}
