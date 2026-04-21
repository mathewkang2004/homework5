# 2x2 DD table ------------------------------------------------------------
q3 <- hcris.mcaid %>%
  mutate(expand_year = year(date_adopted),
         expand_group = case_when(
           expand_year == 2014 ~ "Expanded 2014",
           is.na(expand_year) ~ "Never Expanded"
         ),
         period = case_when(
           year == 2012 ~ "Pre",
           year == 2015 ~ "Post"
         )) %>%
  filter(!is.na(expand_group), !is.na(period)) %>%
  group_by(expand_group, period) %>%
  summarize(Mean = mean(uncomp_care, na.rm = TRUE) / 1e6, .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = Mean) %>%
  mutate(Diff = Post - Pre) %>%
  knitr::kable(
    caption = "Uncompensated Care in Millions of Dollars (2010-2018)",
    digits = 2
  )


