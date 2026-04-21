# Calculate mean uncomp_care by status ------------------------------------
q7 <- hcris.mcaid %>%
  mutate(expand_year = year(date_adopted),
         treated = (!is.na(expand_year) & year >= expand_year)) %>%
  feols(I(uncomp_care / 1e6) ~ treated | provider_number + year, data = ., cluster = ~state) %>%
  modelsummary(stars = TRUE,
               fmt = fmt_decimal(2),
               gof_map = c("nobs", "r.squared"))
