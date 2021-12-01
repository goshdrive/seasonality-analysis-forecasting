# _targets.R
library(targets)
library(tarchetypes)

## Source all function files
source('globals.R')
lapply(list.files("./R", full.names = TRUE), source)

options(tidyverse.quiet = TRUE)

tar_option_set(
  packages = c(
    "tidyverse",
    "plotly",
    "scales",
    "lubridate",
    "jsonlite",
    "prophet",
    "RColorBrewer",
    "pryr",
    'kableExtra',
    'ggthemes',
    'grid',
    'gtable',
    'data.table'
  )
)

tar_plan(
  
  # Input files ################################################################
  tar_target_raw('raw_data_file',
                 './data/synthetic_patient_diagnoses.csv', 
                 format = "file"),
  
  tar_target_raw('resp_codes_file',
                 './data/df_resp.rds',
                 format = "file"),
  
  # Input parsing ##############################################################
  df_dia = read_csv(raw_data_file),
  df_resp = readRDS(resp_codes_file),
  df_resp_labels = df_resp$label,
  df_resp_codes = df_resp$codes,
  
  # Constant parameters ########################################################
  extract_sdate = as.Date('2010-01-01'),
  extract_edate = as.Date('2021-09-30'),
  mdl_sdate = extract_sdate,
  mdl_edate = national_covid_lockdowns[[1]][[1]],
  logadd = TRUE,
  mcmc_samples = 100,
  
  # Wrangling ##################################################################
  tar_target(
    "df_resp_first_dia",
    wrangle_first_dia(df_dia, df_resp_codes[[1]]),
    pattern = map(df_resp_codes),
    iteration = 'list'
  ),
  
  tar_target(
    "df_resp_dia_rates",
    wrangle_dia_rate(df_resp_first_dia, extract_sdate,
                     extract_edate, df_resp_labels),
    pattern = map(df_resp_first_dia, df_resp_labels),
    iteration = 'list'
  ),
  
  # Analysis ###################################################################
  tar_target(
    "mdl_resp_seasonality",
    analyse_dia_rate_seasonality(df_resp_dia_rates, mdl_sdate, mdl_edate,
                                 logadd, mcmc_samples),
    pattern = map(df_resp_dia_rates),
    iteration = 'list'
  ),
  
  tar_target(
    'df_resp_dia_analysis',
    analyse_dia_rates(df_resp_first_dia, mdl_sdate, extract_edate),
    pattern = map(df_resp_first_dia),
    iteration = 'list'
  ),

  tar_target(
    'df_resp_full',
    analyse_residuals(df_resp_dia_rates, mdl_resp_seasonality),
    pattern = map(df_resp_dia_rates, mdl_resp_seasonality),
    iteration = 'list'
  ),
  
  # Publication figures ########################################################
  tar_target(
    "plt_full_analysis",
    plot_full_analysis(df_resp_full, codes2str(df_resp_codes[[1]], TRUE)),
    pattern = map(df_resp_full, df_resp_codes),
    iteration = 'list'
  ),
  
  tbl_full_analysis = tabulate_full_analysis(df_resp_full, df_resp),
  
  NULL
)
