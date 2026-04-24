# Calculate mean uncomp_care by status ------------------------------------
q5_model <- ate.data %>%
  mutate(expand_ever = !is.na(expand_year),
         treat = post * expand_ever) %>%
  lm(uncomp_care_mill ~ post + expand_ever + treat, data = .)

q5 <- q5_model %>%
  modelsummary(stars = TRUE,
               fmt = fmt_decimal(2),
               title = "Standard DD Regression: Medicaid Expansion on Uncompensated Care",
               coef_map = c("(Intercept)" = "Intercept",
                            "postTRUE" = "Post 2014",
                            "expand_everTRUE" = "Expanded 2014",
                            "treat" = "Post × Expanded (DD Estimate)"),
               gof_map = c("nobs", "r.squared"))