shift_log_const <- 0.1
shift_log <- function(x) {log(x + shift_log_const)}
inv_shift_log <- function(x) {exp(x) - shift_log_const}


##' @param df_dia_rates Diagnosis rate timeseries
##' @param mdl_sdate Start date of time period to build model with
##' @param mdl_edate End date of time period to build model with
analyse_dia_rate_seasonality <- function(
  df_dia_rates, mdl_sdate, mdl_edate, logadd = FALSE, mcmc = 0, pred_days = 730) {
  
  # If we want to use the log-additive instead of multiplicative model
  if (logadd) {
    df_dia_rates <- mutate(df_dia_rates, n_030da = shift_log(n_030da))
    seasonality.mode = 'additive'
  } else {
    seasonality.mode = 'multiplicative'
  }
  
  # Crop the data for modelling
  df_prophet <- df_dia_rates %>%
    # Ensure data only within the modelling range
    filter(dia_date >= mdl_sdate & dia_date < mdl_edate) %>%
    # Rename parameters for Prophet requirements
    rename(ds = dia_date, y = n_030da) %>%
    select(ds, y)
  
  # Fit the prophet model
  m <- prophet(df_prophet,
               seasonality.mode = seasonality.mode,
               interval.width = 0.95,
               yearly.seasonality = 5,
               weekly.seasonality = FALSE,
               daily.seasonality = FALSE,
               growth = 'linear',
               changepoint.prior.scale = 0.002,
               mcmc.samples = mcmc
               )
  
  # Predict from the model
  future <- make_future_dataframe(m, pred_days, 'day')
  forecast <- predict(m, future)
  
  # Undo logadd
  if (logadd) {
    forecast <- forecast %>% mutate(across(
      !ds,
      inv_shift_log
    ))
  } 
  
  return(list(
    'mdl' = m,
    'fcst' = forecast
  ))
}
