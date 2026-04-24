q9 <- hcris.all %>%
  mutate(time_to_treat = ifelse(is.na(expand_year), -1, year - expand_year),
         time_to_treat = pmax(-4, pmin(time_to_treat, 4))) %>%
  feols(uncomp_care_mill ~ i(time_to_treat, ref = -1) | provider_number + year, data = ., cluster = ~state) %>%
  iplot(main = "Event Study: Medicaid Expansion on Uncompensated Care",
        xlab = "Year",
        ylab = "Uncompensated Care ($m)")