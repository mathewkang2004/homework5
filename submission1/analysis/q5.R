# Calculate mean uncomp_care by status ------------------------------------
q5 <- hcris.mcaid %>%
  mutate(expand_year = year(date_adopted),
         expand_group = case_when(
           expand_year == 2014 ~ TRUE,
           is.na(expand_year) ~ FALSE
         ),
         post = (year >= 2014)) %>%
  filter(!is.na(expand_group)) %>%
  lm(I(uncomp_care / 1e6) ~ expand_group * post, data = .) %>%
  modelsummary(stars = TRUE,
               fmt = fmt_decimal(2),
               gof_map = c("nobs", "r.squared"))
