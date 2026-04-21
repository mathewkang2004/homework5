# Calculate mean uncompensated care ---------------------------------------
q1 <- hcris.mcaid %>%
  group_by(year) %>%
  summarize(
    Mean = mean(uncomp_care, na.rm = TRUE) / 1e6,
    SD = sd(uncomp_care, na.rm = TRUE) / 1e6,
    N = n(),
    .groups = "drop"
  ) %>% 
  knitr::kable(
    caption = "Uncompensated Care in Millions of Dollars (2010-2018)",
    digits = 2
  )


