##' @param df_dia Data frame of diagnoses
##' @param df_codes Data frame of ICD 10 code categories to collect
wrangle_first_dia <- function(df_dia, df_codes) {
  
  # Create UNENCLOSED functions for each filter element (R defaults to closures)
  filter_functions <- list()
  for (code in df_codes) {
    if (length(code) == 2) {
      # Range of codes
      filter_functions <- append(filter_functions, unenclose(function(c) {
        (c >= code[[1]]) & (c < code[[2]])
      }))
      filter_functions <- append(filter_functions, unenclose(function(c) {
        str_detect(c, paste0('^',code[[2]]))
      }))
    } else if (length(code) == 1) {
      # Single code/root
      filter_functions <- append(filter_functions, unenclose(function(c) {
        str_detect(c, paste0('^',code[[1]]))
      }))
    } else {
      print(code)
      stop('Invalid ICD10 input')
    }
  }
  
  # Combine function list into a single or-d function
  filter_combined <- function(c) {
    Reduce('|', lapply(filter_functions, function(ff) ff(c)))
  }
  
  # Get the patient list of patients that match the query
  df_patient_list <- df_dia %>%
    filter(filter_combined(diag_local_code)) %>%
    distinct(project_id)
  
  # Get the relevant diagnoses from patients in the patient list
  df_patient_dia <- df_dia %>%
    select(project_id, start_datetime, diag_local_code) %>%
    drop_na() %>%
    filter(project_id %in% df_patient_list$project_id)
  
  # Function to extract the diagnosis periods (gathers repeats)
  extract_dia <- function(df) {
    
    df_out <- df %>%
      group_by(start_datetime) %>%
      summarize(found = sum(filter_combined(diag_local_code)) > 0) %>%
      filter(found & (is.na(lag(found)) | !lag(found))) %>%
      rename(dia_date = start_datetime)
    
    df_out
    
  }
  
  # Extract all diagnosis periods from the patient diagnosis list
  df_out <- df_patient_dia %>%
    group_by(project_id) %>%
    group_modify(~ extract_dia(.x)) %>%
    ungroup() %>%
    select(dia_date)
  
  df_out
}
