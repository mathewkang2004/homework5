# Calculate mean uncomp_care by status ------------------------------------
q2 <- hcris.mcaid %>%
  group_by(year, expand_group) %>%
  summarize(mean_uncomp = mean(uncomp_care_mill, na.rm =TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_uncomp, color = expand_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2014, linetype="dashed", color = "red", linewidth=0.5) +
  annotate("text", x = 2016, y = 90, label = "Medicaid Expansion") +
  scale_x_continuous(breaks = 2010:2018) +
  labs(
    title = "Mean Uncompensated Care By Expansion Status (2010-2018)",
    x = "Year",
    y = "Mean Uncompensated Care ($m)",
    color = "Expansion Status"
  ) +
  theme_minimal()
