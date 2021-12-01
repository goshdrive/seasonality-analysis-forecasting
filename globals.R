assign(
  'national_covid_lockdowns',
  list(
    list(as.Date('2020-03-26'), as.Date('2020-06-01')), 
    list(as.Date('2020-11-05'), as.Date('2020-12-02')), 
    list(as.Date('2021-01-06'), as.Date('2021-03-08'))
  ),
  envir = .GlobalEnv)

assign(
  'national_covid_release',
  as.Date('2021-07-19'),
  envir = .GlobalEnv)

assign(
  'epic_gosh_introduction',
  as.Date('2019-04-30'),
  envir = .GlobalEnv)
