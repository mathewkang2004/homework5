# Preliminaries -----------------------------------------------------------
kff.dat <- read_csv('data/input/medicaid/KFF_medicaid_expansion_2019.csv')

# Clean KFF data -------------------------------------------------------

kff.final <- kff.dat %>%
  mutate(expanded = (`Expansion Status` == 'Adopted and Implemented'),
         date_text = str_replace_all(`Expansion Implementation Date`, c("\n"='', '"'=''))) %>%
  separate(date_text, sep=" ", into=c(NA, NA, NA, "date"), extra="drop", fill="right") %>%
  mutate(date_adopted = mdy(date)) %>%
  select(State, expanded, date_adopted)

write_rds(kff.final,'data/output/medicaid-kff.rds')