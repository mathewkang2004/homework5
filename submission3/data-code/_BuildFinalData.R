# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)


# Load data ---------------------------------------------------------------
hcris.data <- read_tsv('data/output/HCRIS_Data.txt', show_col_types = FALSE)
kff.final <- read_rds('data/output/medicaid-kff.rds')

state_xwalk <- tibble(
  state_abb = state.abb,
  state_name = state.name
)

hcris.mcaid <- hcris.data %>%
  left_join(state_xwalk, by = c("state" = "state_abb")) %>%
  left_join(kff.final, by = c("state_name" = "State")) %>%
  filter(! state %in% c("DC", "GU", "PR", "MP", "VI"))

write_csv(hcris.mcaid, 'data/output/hcris-mcaid.csv')
