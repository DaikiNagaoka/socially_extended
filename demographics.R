# Demographics ----
library(gtsummary)

# Generate demographic table by cluster
gt_tbl <- data %>%
  select(
    cluster, sex_at_birth, age_in_months, iq,
    child_has_any_siblings, education_mother, education_father,
    household_income, primary_caregiver, nationality
  ) %>%
  tbl_summary(by = cluster,
              type = c(age_in_months, iq) ~ "continuous",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              )
  ) %>%
  add_p(test = list(
    all_continuous() ~ "aov",          # ANOVA for continuous variables
    all_categorical() ~ "chisq.test"   # Chi-squared test for categorical variables
  ),
  pvalue_fun = function(x) x # Output raw p-value
  ) %>%
  add_overall()

# Display table
gt_tbl
