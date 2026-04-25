# Calculate mean uncompensated care ---------------------------------------
q1 <- hcris.mcaid %>%
  group_by(year) %>%
  summarize(
    Mean = mean(uncomp_care_mill, na.rm = TRUE),
    SD = sd(uncomp_care_mill, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  ) %>% 
  knitr::kable(
    caption = "Uncompensated Care in Millions of Dollars (2010-2018)",
    col.names = c("Year", "Mean", "SD", "N"),
    digits = 2
  )


