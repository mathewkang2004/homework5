# Calculate mean uncomp_care by status ------------------------------------
q6 <- hcris.mcaid %>%
  mutate(expand_year = year(date_adopted),
         expand_group = case_when(
           expand_year == 2014 ~ TRUE,
           is.na(expand_year) ~ FALSE
         ),
         post = (year >= 2014)) %>%
  filter(!is.na(expand_group)) %>%
  feols(I(uncomp_care / 1e6) ~ i(post, expand_group, ref = FALSE) | provider_number + year, data = ., cluster = ~state) %>%
  modelsummary(stars = TRUE,
               fmt = fmt_decimal(2),
               gof_map = c("nobs", "r.squared"))
