
analyse_residuals <- function(df_dia_rates, mdl_seasonality) {
  
  mdl <- mdl_seasonality$mdl
  fcst <- mdl_seasonality$fcst
  
  # Format input
  df_full <- df_dia_rates %>%
    select(ds = dia_date, y = n_030da, n) %>%
    full_join(fcst, by = 'ds') %>%
    mutate(
      logy = shift_log(y),
      quant = NA
    )
  
  mdl$uncertainty.samples = 10000
  df_samples <- predictive_samples(mdl, df_full)$yhat
  
  # Kernel density estimate quartile function
  kdequant <- function(x, xdat) {
    
    if (is.na(x)) return(NA)
    
    kde <- mean(pnorm((x-xdat), 0, density(xdat)$bw))
    
    # If the sample is so extreme the KDE doesn't cover it
    # then default to the normal approximation
    if (kde >= 1 | kde <= 0) kde <- pnorm(x, mean(xdat), sd(xdat))
    
    kde
  } 
  
  # Compute quantiles from KDE
  for (ii in 1:nrow(df_samples)) {
    df_full$quant[ii] <- kdequant(df_full$logy[ii], df_samples[ii,])
  }
  
  # Compute z-scores
  df_full <- df_full %>%
    mutate(zscore = qnorm(quant))
  
  return(df_full)
  
}