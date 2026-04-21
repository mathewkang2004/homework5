# Calculate mean uncomp_care by status ------------------------------------
q2 <- hcris.mcaid %>%
  mutate(expand_year = year(date_adopted),
         expand_group = case_when(
           expand_year == 2014 ~ "Expanded 2014",
           is.na(expand_year) ~ "Never Expanded"
         )) %>%
  filter(!is.na(expand_group)) %>%
  group_by(year, expand_group) %>%
  summarize(mean_uncomp = mean(uncomp_care, na.rm =TRUE) / 1e6, .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_uncomp, color = expand_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2010:2018) +
  labs(
    title = "Mean Uncompensated Care By Expansion Status (2010-2018)",
    x = "Year",
    y = "Mean Uncompensated Care ($m)",
    color = "Expansion Status"
  ) +
  theme_minimal()
