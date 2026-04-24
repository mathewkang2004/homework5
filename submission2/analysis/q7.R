# Calculate mean uncomp_care by status ------------------------------------
q7_model <- ate.data %>%
  mutate(treated = (!is.na(expand_year) & year >= expand_year)) %>%
  feols(uncomp_care_mill ~ treated | provider_number + year, data = ., cluster = ~state)
  
q7 <- modelsummary(list("DD" = q5_model, "DD + FE" = q6_model, "DD + FE (All States)" = q7_model),
                   stars = TRUE,
                   fmt = fmt_decimal(2),
                   coef_map = c("(Intercept)" = "Intercept",
                                "postTRUE" = "Post 2014",
                                "expand_everTRUE" = "Expanded 2014",
                                "treat" = "Post × Expanded",
                                "treatedTRUE" = "Post × Expanded (All States)"),
                   gof_map = c("nobs", "r.squared"))
