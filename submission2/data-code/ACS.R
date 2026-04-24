# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, haven, tidycensus)

## Variables of interest from table B27010 (1-year ACS)
ins_vars <- c(
  all_18to34      = "B27010_018",
  employer_18to34 = "B27010_020",
  direct_18to34   = "B27010_021",
  medicare_18to34 = "B27010_022",
  medicaid_18to34 = "B27010_023",
  tricare_18to34  = "B27010_024",
  va_18to34       = "B27010_025",
  none_18to34     = "B27010_033",
  all_35to64      = "B27010_034",
  employer_35to64 = "B27010_036",
  direct_35to64   = "B27010_037",
  medicare_35to64 = "B27010_038",
  medicaid_35to64 = "B27010_039",
  tricare_35to64  = "B27010_040",
  va_35to64       = "B27010_041",
  none_35to64     = "B27010_050"
)


# Retrieve ACS data -------------------------------------------------------
insurance_list <- lapply(2012:2018, function(t) {
  get_acs(
    geography = "state",
    variables = ins_vars,
    year      = t,
    survey    = "acs1",
    output    = "wide"   # one row per state, one column per variable
  ) %>%
    # get_acs with output="wide" appends "E" (estimate) and "M" (margin of error)
    # to each variable name; keep only the estimate columns
    select(State = NAME, ends_with("E"), -GEOID) %>%
    rename_with(~ sub("E$", "", .x), ends_with("E")) %>%
    mutate(year = t)
})


# Tidy --------------------------------------------------------------------
final.insurance <- bind_rows(insurance_list) %>%
  mutate(adult_pop   = all_18to34      + all_35to64,
         ins_employer = employer_18to34 + employer_35to64,
         ins_direct   = direct_18to34   + direct_35to64,
         ins_medicare = medicare_18to34 + medicare_35to64,
         ins_medicaid = medicaid_18to34 + medicaid_35to64,
         uninsured    = none_18to34     + none_35to64) %>%
  select(State, year, adult_pop, ins_employer, ins_direct,
         ins_medicare, ins_medicaid, uninsured)

write_rds(final.insurance, 'data/output/insurance.rds')