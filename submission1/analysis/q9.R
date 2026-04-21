q9 <- hcris.mcaid %>%
  mutate(expand_year = year(date_adopted),
         time_to_treat = year - expand_year,
         time_to_treat = pmax(-4, pmin(time_to_treat, 4))) %>%
  feols(I(uncomp_care / 1e6) ~ i(time_to_treat, ref = -1) | provider_number + year, data = ., cluster = ~state) %>%
  iplot(main = "Event Study: Medicaid Expansion on Uncompensated Care",
        xlab = "Year",
        ylab = "Uncompensated Care ($m)")