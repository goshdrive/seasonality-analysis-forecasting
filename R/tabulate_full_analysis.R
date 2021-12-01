tabulate_full_analysis <- function(df_full, df_codes) {
  
  selection <- list(
    'RSV',
    'Influenza',
    'Acute nasopharyngitis',
    'Acute bronchiolitis',
    'Diseases of the respiratory system',
    'Acute upper respiratory infections',
    'Influenza and pneumonia',
    'Other acute lower respiratory infections',
    'Other diseases of upper respiratory tract',
    'Other non-infectious diseases of the respiratory system',
    'Other diseases of the respiratory system'
  )
  
  df_out <- df_codes %>%
    rowwise() %>%
    mutate(
      label = ifelse(
        str_detect(label, '^.* \\(.*-.*\\)$'),
        str_match(label, '^(.*) \\(.*-.*\\)$')[2],
        label
      ),
      codes = codes2str(codes, excl_brackets=T)
    ) %>%
    ungroup() %>%
    cbind(bind_rows(lapply(df_full, full2results))) %>%
    as_tibble()
  
  indices <- as.numeric(
    sapply(selection, function(x){which(str_detect(df_out$label, x))[[1]]})
  )
  
  # Select only those we want to use
  df_out <- df_out[indices,]
  
  # Apply p-value correction for all
  df_out <- df_out %>%
    mutate(
      observed_mean_pval_intracovid = p.adjust(
        observed_mean_pval_intracovid,
        'bonferroni',
        length(observed_mean_pval_intracovid) * 2),
      observed_mean_pval_postcovid = p.adjust(
        observed_mean_pval_postcovid,
        'bonferroni',
        length(observed_mean_pval_postcovid) * 2)
    )
  
  date_fmt <- function(x) format(x, '%d %B %Y')
  ld1 <- national_covid_lockdowns[[1]][[1]]
  ds_valid <- df_full[[1]]$ds[!is.na(df_full[[1]]$n)]
  d0 <- as_date(min(ds_valid))
  d1 <- as_date(max(ds_valid))
  
  during_restrictions <- paste0(
    "During Restrictions\n(",
    date_fmt(ld1), ' to ', date_fmt(national_covid_release-1), ')'
  )
  
  pre_restrictions_yr <- paste0(
    "Pre Restrictions Year\n(",
    date_fmt(ld1-years(1)), ' to ', date_fmt(ld1-1), ')'
  )
  
  pre_restrictions <- paste0(
    "Pre Restrictions\n(",
    date_fmt(d0), ' to ', date_fmt(ld1-1), ')'
  )
  
  post_restrictions <- paste0(
    "Post Restrictions\n(",
    date_fmt(national_covid_release), ' to ', date_fmt(d1), ')'
  )
  
  
  tbl_out <- df_out %>%
    select(label, codes,
           forecast_amp_norm_precovid, seasonal_peak_precovid,
           forecast_n_intracovid, observed_n_intracovid, missing_n_intracovid,
           gain_perc_intracovid, zscore_min_intracovid,
           zscore_max_postcovid) %>%
    kbl(
      col.names = c(
        'Diagnosis',
        'ICD 10 Codes',
        'Seasonality Amplitude [normalised]',
        'Forecast Seasonal Peak',
        'Forecast Diagnoses [count]',
        'Observed Diagnoses [count]',
        'Unrealised Diagnoses [count]',
        'Difference in Diagnoses [%]',
        'Minimum Z-score',
        'Maximum Z-score'),
      digits = 2,
      align = 'l'
    ) %>%
    add_header_above(data.frame(
      x = c(' ', pre_restrictions, during_restrictions, post_restrictions),
      y = c(2, 2, 5, 1)
    )) %>%
    pack_rows("Seasonal Respiratory Infection Diagnosis Categories", 1,4) %>%
    pack_rows("ICD-10 Hierarchy Respiratory Categories", 5, length(indices)) %>%
    kable_styling()
  
  tbl_out %>% save_kable("./figures/results.png")
  
  
  format_obs <- function(mn, pv) {
    paste(
      round(mn, digits = 2) ,
      ifelse(
        pv < 0.001,
        '(p<0.001)',
        paste0('(p=', round(pv, digits = 3), ')')
      )
    )
  }
  
  
  tbl_maxmin <- df_out %>%
    mutate(
      observed_mean_vs_pre_intracovid_str = format_obs(observed_mean_vs_pre_intracovid, observed_mean_pval_intracovid),
      observed_mean_vs_pre_postcovid_str = format_obs(observed_mean_vs_pre_postcovid, observed_mean_pval_postcovid)
    ) %>%
    select(label, codes,
           observed_max_precovid, observed_min_precovid, observed_mean_precovid,
           observed_max_intracovid, observed_min_intracovid,
           observed_mean_intracovid, observed_mean_vs_pre_intracovid_str,
           observed_max_postcovid, observed_min_postcovid,
           observed_mean_postcovid, observed_mean_vs_pre_postcovid_str
    ) %>%
    kbl(
      col.names = c(
        'Diagnosis',
        'ICD 10 Codes',
        #
        'Max', 'Min', 'Mean',
        #
        'Max', 'Min', 'Mean',
        'Change in Mean [%] (p-value)',
        #
        'Max', 'Min', 'Mean',
        'Change in Mean [%] (p-value)'
        ),
      digits = 2,
      align = c('l')
    ) %>%
    add_header_above(data.frame(
      x = c(' ', pre_restrictions_yr, during_restrictions, post_restrictions),
      y = c(2, 3, 4, 4)
    )) %>%
    pack_rows("Seasonal Respiratory Infection Diagnosis Categories", 1,4) %>%
    pack_rows("ICD-10 Hierarchy Respiratory Categories", 5, length(indices)) %>%
    column_spec(c(9,13), width = '3.5cm') %>%
    kable_styling()
  
  tbl_maxmin %>% save_kable("./figures/maxmin.png")
  
  
  tbl_cohort <- df_out %>%
    mutate(total = rowSums(across(starts_with('observed_n_')))) %>%
    select(label, codes, total, observed_n_precovid, observed_n_intracovid, observed_n_postcovid) %>%
    kbl(
      col.names = c(
        'Diagnosis',
        'ICD 10 Codes',
        'Total Observed Diagnoses [count]',
        'Pre Restrictions Observed Diagnoses [count]',
        'During Restrictions Observed Diagnoses [count]',
        'Post Restrictions Observed Diagnoses [count]'),
      digits = 2,
      align = c('l')
    ) %>%
    pack_rows("Seasonal Respiratory Infection Diagnosis Categories", 1,4) %>%
    pack_rows("ICD-10 Hierarchy Respiratory Categories", 5, length(indices)) %>%
    kable_styling()
  
  tbl_cohort %>% save_kable("./figures/cohort.png")
  
  # Function to unify the data into one tidy dataset
  unify_dfs <- function(df, df_codes)
    bind_rows(
      mapply(
        function(a,b) mutate(a, ds = as.Date(ds), label = b),
        df,
        factor(df_codes$label, levels=df_codes$label),
        SIMPLIFY = F
  ))
  
  
  indices_seasonal = indices[1:4]
  indices_resp = indices[5:length(indices)]
  
  df_unified_seasonal <- unify_dfs(df_full[indices_seasonal], df_codes[indices_seasonal,])
  df_unified_resp <- unify_dfs(df_full[indices_resp], df_codes[indices_resp,])
  
  ggp_seasonal <- df_unified_seasonal %>%
    ggplot(aes(ds, y)) +
    geom_line(color = line_colour) +
    facet_grid(label ~ ., labeller = label_wrap_gen()) +
    ylab('Diagnosis frequency [diagnoses/day]') +
    common_style() +
    NULL
  
  ggsave(
    './figures/unified_seasonal.png',
    ggp_seasonal,
    height=20, width=18, units='cm'
  )
  
  
  ggp_resp <- df_unified_resp %>%
    ggplot(aes(ds, y)) +
    geom_line(color = line_colour) +
    facet_grid(label ~ ., scales="free", labeller = label_wrap_gen()) +
    ylab('Diagnosis frequency [diagnoses/day]') +
    common_style(panel_spacing = 0.5, strip_angle = 0, strip_font_size = 8) +
    NULL

  ggsave(
    './figures/unified_resp.png',
    ggp_resp,
    height=20, width=18, units='cm'
  )
  
  tbl_out
  
}


full2results <- function(df_full) {
  
  df_periods <- df_full %>%
    mutate(time_period = factor(ifelse(ds < national_covid_lockdowns[[1]][[1]], 'precovid',
                                       ifelse(ds >= national_covid_release, 'postcovid',
                                              'intracovid')))
    )
  
  # P-values
  df_periods_1yr <- df_periods %>% filter(ds >= (national_covid_lockdowns[[1]][[1]] - 365))
  pwt <- pairwise.wilcox.test(df_periods_1yr$y, df_periods_1yr$time_period, p.adjust.method = "bonferroni")
  
  res <- df_periods %>%
    group_by(time_period) %>%
    summarise(
      zscore_max = max(zscore, na.rm = T),
      zscore_min = min(zscore, na.rm = T),
      forecast_n = sum(yhat, na.rm = T),
      observed_n = sum(n, na.rm = T),
      missing_n = forecast_n - observed_n,
      gain_perc = -100.0 * missing_n / forecast_n,
      observed_min = min((if(length(y) > 730) tail(y, 365) else y), na.rm = T),
      observed_max = max((if(length(y) > 730) tail(y, 365) else y), na.rm = T),
      observed_mean = mean((if(length(y) > 730) tail(y, 365) else y), na.rm = T),
      observed_range = paste(round(observed_max, 2), '/', round(observed_min,2)),
      forecast_min = min(tail(yhat, 365)),
      forecast_max = max(tail(yhat, 365)),
      forecast_amp = forecast_max - forecast_min,
      forecast_amp_norm = forecast_amp / forecast_max,
      seasonal_peak = ifelse(forecast_amp_norm > 0.5, format(ds[which.max(yearly)], '%d %B'), NA)
    ) %>%
    pivot_wider(names_from = time_period, values_from = !time_period) %>%
    mutate(
      observed_mean_pval_intracovid = pwt$p.value['precovid','intracovid'],
      observed_mean_pval_postcovid = pwt$p.value['precovid','postcovid'],
      observed_mean_vs_pre_intracovid = 100 * (observed_mean_intracovid - observed_mean_precovid) / observed_mean_precovid,
      observed_mean_vs_pre_postcovid = 100 * (observed_mean_postcovid - observed_mean_precovid) / observed_mean_precovid
    )
  
  res
  
}
