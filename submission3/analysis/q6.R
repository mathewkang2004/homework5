# Calculate mean uncomp_care by status ------------------------------------
q6_model <- ate.data %>%
  feols(uncomp_care_mill ~ treat | provider_number + year, data = ., cluster = ~state)

q6 <- modelsummary(list("DD" = q5_model, "DD + FE" = q6_model),
                   stars = TRUE,
                   fmt = fmt_decimal(2),
                   coef_map = c("treat" = "Post × Expand"),
                   gof_map = c("nobs", "r.squared"))
