plot_full_analysis <- function(df, lbl) {
  
  # Check dates are Date rather than posixct
  df <- mutate(df, ds = as.Date(ds))
  
  # Rename values
  df_renamed <- df %>%
    transmute(
      ds = ds,
      # Diagnosis rate plot
      diag_yval = y, diag_ymid = yhat,
      diag_ymax = yhat_upper, diag_ymin = yhat_lower,
      # Z-score
      zscore_yval = zscore, zscore_ymid = 0,
      zscore_ymax = qnorm(0.975), zscore_ymin = qnorm(0.025),
      # Trend
      trend_yval = trend, trend_ymid = NA,
      trend_ymax = trend_upper, trend_ymin = trend_lower,
      # Seasonality
      season_yval = yearly, season_ymid = NA,
      season_ymax = yearly_upper, season_ymin = yearly_lower
    )
  
  df_long <- pivot_longer(
    df_renamed,
    !ds,
    names_pattern = '(.*)_(.*)',
    names_to = c("plt",".value")
  )
  
  
  plt_labels = list()
  plt_labels['diag'] <- 'Diagnosis frequency [diagnoses/day]'
  plt_labels['zscore'] <- 'Z-score (against-forecast)'
  plt_labels['trend'] <- 'Forecast Trend'
  plt_labels['season'] <- 'Forecast Seasonality'
  
  
  df_long <- df_long %>%
    mutate(plt = factor(plt_labels[plt], plt_labels))
  
  ggp_pipeline <- list(
    geom_ribbon(aes(ymin=ymin, ymax=ymax),
                fill = ribbon_colour,
                alpha = ribbon_alpha),
    geom_line(aes(ds, ymid), color = 'white', size = 0.5),
    geom_line(color = line_colour),
    facet_grid(plt ~ ., labeller = label_wrap_gen(), scales="free", switch="both"),
    ylab(NULL),
    common_style()
  )
  
  # Plot and save
  ggp <- df_long %>%
    filter(plt %in% plt_labels[1:2]) %>%
    ggplot(aes(ds, yval, group = plt)) +
    ggp_pipeline
    
  ggsave(
    paste0('./figures/forecast_', lbl,'.png'),
    ggp,
    height=12, width=18, units='cm'
  )
  
  ggp <- df_long %>%
    ggplot(aes(ds, yval, group = plt)) +
    ggp_pipeline
  
  ggsave(
    paste0('./figures/forecast_parts_', lbl,'.png'),
    ggp,
    height=18, width=18, units='cm'
  )
  
}