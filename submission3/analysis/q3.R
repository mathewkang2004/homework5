# 2x2 DD table ------------------------------------------------------------
q3 <- hcris.mcaid %>%
  mutate(period = case_when(
           year == 2012 ~ "Pre",
           year == 2015 ~ "Post"
         )) %>%
  filter(!is.na(period)) %>%
  group_by(expand_group, period) %>%
  summarize(Mean = mean(uncomp_care_mill, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = Mean) %>%
  mutate(Diff = Post - Pre) %>%
  knitr::kable(
    caption = "Uncompensated Care in Millions of Dollars (2010-2018)",
    col.names = c("Expansion Status", "Post (2015)", "Pre (2012)", "Difference"),
    digits = 2
  )


