# Calculate mean uncomp_care by status ------------------------------------
q8 <- ate.data %>%
  mutate(expand_group = case_when(
           expand_year == 2014 ~ TRUE,
           is.na(expand_year) ~ FALSE)) %>%
  filter(!is.na(expand_group)) %>%
  feols(I(uncomp_care_mill) ~ i(year, expand_group, ref = 2013) | provider_number + year, data = ., cluster = ~state) %>%
  iplot(main = "Event Study: Medicaid Expansion on Uncompensated Care",
        xlab = "Year",
        ylab = "Uncompensated Care ($m)")