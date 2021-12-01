# Analysis Program for Seasonality in Diagnosis Rates


## Input data Structure
Data are required as a CSV of ICD-10 diagnoses in the `./data` directory.
The expected format of the CSV is as follows:

| project_id | start_datetime | diag_local_code |
|---|---|---|
| string ID for each patient | date of diagnosis | ICD-10 code for the diagnosis |
| e.g. | e.g. | e.g. |
| P0001 | 2010-01-01 | J121 |



## How to Run the Analysis
```r
targets::tar_make()
```
Output tables and figures will be produced in `./figures`

