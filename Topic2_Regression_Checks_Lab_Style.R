# ============================================================================
# TOPIC 2: PAIN CATASTROPHIZING — INSTRUCTOR DATA CHECKS
# Fall 2026 | Run one numbered section at a time in RStudio.
# ============================================================================
# These data are simulated for teaching. This script checks the REVISED data.
# One row represents one participant; the file contains 600 participants.
#
# QUESTIONS WE WILL EXAMINE:
# 1. Do the distributions and missing values look reasonable?
# 2. Are the correlations close to the source-study benchmarks?
# 3. Is catastrophizing related to pain intensity before/after adding controls?
# 4. Does that relationship differ depending on fear of pain?
# 5. Are there problems with the regression models that deserve attention?
#
# Catastrophizing means thinking about pain in a persistently threatening or
# overwhelming way. A moderator is a variable that may change the relationship
# between a predictor and an outcome.
#
# This follows the explicit, step-by-step style of Lab 6. Plots use ggplot2 so
# that you do NOT need the separate load_plot_functions.R file to run this file.
#
# FILE SETUP:
# Download the REVISED pain_catastrophizing_F26.csv from Drive:
# https://drive.google.com/file/d/1mj-LVFEMaBfm_RkrQXMi6k6uPlPpeODX/view
# Put it in your RStudio project's data folder, or change data_file below.
# The GitHub Pages copy may be older; this script does not download it.

# 1. LOAD PACKAGES AND READ THE DATA -------------------------------------------

# Run the following line ONCE if these packages are not already installed:
# install.packages(c("tidyverse", "janitor", "GGally", "jtools", "performance"))

library(tidyverse)
library(janitor)
library(GGally)
library(jtools)
library(performance)

# getwd() shows the folder from which R is reading relative file paths.
getwd()
data_file <- file.path("data", "pain_catastrophizing_F26.csv")

# Also allow the CSV to be directly in the current working folder.
if (!file.exists(data_file) && file.exists("pain_catastrophizing_F26.csv")) {
  data_file <- "pain_catastrophizing_F26.csv"
}
if (!file.exists(data_file)) {
  stop("CSV not found. Put the revised CSV in your project's data folder, or edit data_file.")
}

pain_raw <- read_csv(file = data_file, na = c("", "NA"),
                     show_col_types = FALSE)
glimpse(pain_raw)

# Check that this is the revised 38-variable file rather than the old file.
stopifnot(nrow(pain_raw) == 600, ncol(pain_raw) == 38,
          !anyNA(pain_raw$participant_id),
          anyDuplicated(pain_raw$participant_id) == 0)

# 2. PREPARE CATEGORY VARIABLES -----------------------------------------------

# Factors tell R that these values are category labels, not amounts.
# Participant ID identifies rows; it is not a predictor.
# Relevel sets the comparison/reference group for a regression coefficient.

pain <- pain_raw |>
  mutate(
    across(where(is.character) & !all_of("participant_id"), as.factor),
    chronic_pain_group = factor(chronic_pain_group,
                                levels = c(0, 1),
                                labels = c("recent_pain", "chronic_pain")),
    pain_diagnosis_group = factor(pain_diagnosis_group,
                                  levels = c(1, 2, 3, 4),
                                  labels = c("back", "joint", "headache", "other")),
    gender = relevel(gender, ref = "man"),
    education = factor(education,
                       levels = c("high_school_or_less", "some_college",
                                  "bachelors", "postgraduate")),
    employment_status = relevel(employment_status, ref = "employed"),
    recruitment_source = relevel(recruitment_source, ref = "community"),
    analgesic_type = relevel(analgesic_type, ref = "none")
  )

glimpse(pain)
print(tabyl(pain, chronic_pain_group))
print(tabyl(pain, pain_diagnosis_group))
print(tabyl(pain, gender))

# IMPORTANT MEASUREMENT NOTES:
# - pain_catastrophizing_score_0_52 is a study-specific eight-item index.
#   Its 0–52 range does not make it the original 13-item PCS questionnaire.
# - Most other ratings range from 1 to 5. Higher sleep_quality means BETTER sleep.
# - Higher social_support and pain_self_efficacy mean MORE support/confidence.
# - injury_severity is an ordered self-report rating (1–5), not a clinical exam.
# - diagnosis codes are unordered categories. Do not correlate the 1–4 codes.
# - pain_locations is a count. comorbid_condition_count = 6 means six or more.
# - Blank cells are missing observations. They are never equivalent to zero.

# 3. CHECK MISSING VALUES, RANGES, AND BASIC SUMMARIES --------------------------

missing_summary <- pain |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
  mutate(percent_missing = 100 * n_missing / nrow(pain)) |>
  arrange(desc(percent_missing))
print(missing_summary, n = Inf)

# Numeric summaries: look for impossible values, very unusual spreads, or
# a mean near the boundary with no corresponding skew in the histogram.
numeric_summary <- pain |>
  select(where(is.numeric)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  summarise(
    n_observed = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    minimum = min(value, na.rm = TRUE),
    q1 = quantile(value, .25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q3 = quantile(value, .75, na.rm = TRUE),
    maximum = max(value, na.rm = TRUE),
    .groups = "drop"
  )
print(numeric_summary, n = Inf, width = Inf)

# Use the codebook's allowed ranges. These are validity checks, not statements
# that values must be uniformly distributed across the whole range.
range_rules <- tribble(
  ~variable, ~lower, ~upper,
  "pain_catastrophizing_score_0_52", 0, 52,
  "pain_intensity_0_10", 0, 10,
  "catastrophizing_rumination", 1, 5,
  "catastrophizing_magnification", 1, 5,
  "catastrophizing_helplessness", 1, 5,
  "fear_of_pain", 1, 5,
  "pain_vigilance", 1, 5,
  "pain_disability", 1, 5,
  "depressive_symptoms", 1, 5,
  "anxiety_symptoms", 1, 5,
  "sleep_quality", 1, 5,
  "social_support", 1, 5,
  "pain_self_efficacy", 1, 5,
  "work_limitation", 1, 5,
  "injury_severity", 1, 5,
  "age_years", 18, 80,
  "pain_duration_months", 0, 360,
  "pain_locations", 1, 6,
  "analgesic_use_days_week", 0, 7,
  "physical_activity_days_week", 0, 7,
  "pain_days_last_week", 1, 7,
  "comorbid_condition_count", 0, 6
)
range_checks <- range_rules |>
  rowwise() |>
  mutate(n_outside_range = sum(pain[[variable]] < lower |
                                pain[[variable]] > upper, na.rm = TRUE)) |>
  ungroup()
print(range_checks, n = Inf)
stopifnot(all(range_checks$n_outside_range == 0))

# BMI is a continuous body-size measure; inspect extremes rather than applying
# an arbitrary narrow 'normal' range. Chronic pain requires at least 3 months.
print(summary(pain$body_mass_index))
print(with(pain, table(chronic_pain_group, pain_duration_months >= 3)))

# 4. INSPECT DISTRIBUTIONS -----------------------------------------------------

# A histogram shows how common different score ranges are.
# The solid vertical line marks the mean; the dashed line marks the median.
cat_hist <- ggplot(pain, aes(x = pain_catastrophizing_score_0_52)) +
  geom_histogram(binwidth = 4, boundary = 0, fill = "lightblue", color = "black") +
  geom_vline(xintercept = mean(pain$pain_catastrophizing_score_0_52), linewidth = 1) +
  geom_vline(xintercept = median(pain$pain_catastrophizing_score_0_52),
             linetype = "dashed", linewidth = 1) +
  scale_x_continuous(limits = c(0, 52), breaks = seq(0, 52, 4)) +
  labs(title = "Distribution of pain catastrophizing",
       x = "Catastrophizing index (0–52)", y = "Number of participants") +
  theme_bw()
print(cat_hist)

pain_hist <- ggplot(pain, aes(x = pain_intensity_0_10)) +
  geom_histogram(binwidth = .5, boundary = 0, fill = "lightblue", color = "black") +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(title = "Distribution of pain intensity",
       x = "Pain intensity (0–10)", y = "Number of participants") +
  theme_bw()
print(pain_hist)

# Print manageable sets of numeric histograms so every numeric variable is seen.
# Each panel has its own axes because the measurement units differ.
numeric_names <- names(select(pain, where(is.numeric)))
for (start in seq(1, length(numeric_names), by = 6)) {
  current_names <- numeric_names[start:min(start + 5, length(numeric_names))]
  distribution_plot <- pain |>
    select(all_of(current_names)) |>
    pivot_longer(everything(), names_to = "variable", values_to = "value") |>
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15, fill = "lightblue", color = "black", na.rm = TRUE) +
    facet_wrap(~ variable, scales = "free", ncol = 2) +
    labs(title = "Check shape, spread, and unusual values",
         x = "Observed score", y = "Participants") +
    theme_bw()
  print(distribution_plot)
}

# Categories need frequency tables/bar charts, not means of category codes.
# The denominator below includes missing responses, which are shown explicitly.
category_counts <- pain |>
  select(where(is.factor)) |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "response") |>
  mutate(response = replace_na(response, "[Missing]")) |>
  count(variable, response, name = "n") |>
  group_by(variable) |>
  mutate(percent = 100 * n / sum(n)) |>
  ungroup()
print(category_counts, n = Inf)

for (current_variable in unique(category_counts$variable)) {
  category_plot <- category_counts |>
    filter(variable == current_variable) |>
    ggplot(aes(x = reorder(response, n), y = n)) +
    geom_col(fill = "lightblue", color = "black") +
    coord_flip() +
    labs(title = current_variable, x = "Response", y = "Participants") +
    theme_bw()
  print(category_plot)
}

# 5. EXAMINE CORRELATIONS ------------------------------------------------------

# Pearson's r describes a straight-line relationship from -1 to +1.
# A correlation does not establish that one variable causes the other.
# Use complete pairs: each correlation uses participants observed on BOTH
# variables, so sample size can differ from one pair to another.

focal_cor <- cor.test(x = pain$pain_catastrophizing_score_0_52,
                      y = pain$pain_intensity_0_10, method = "pearson")
print(focal_cor)

cor_variables <- pain |>
  select(pain_catastrophizing_score_0_52, fear_of_pain, pain_vigilance,
         anxiety_symptoms, depressive_symptoms, pain_intensity_0_10,
         pain_disability)
cor_matrix <- cor(cor_variables, use = "pairwise.complete.obs")
print(round(cor_matrix, 3))

# A smaller plot matrix is easier to read. For this plot only, use the same
# complete cases across all four variables; its correlations may therefore
# differ slightly from pairwise correlations printed above.
cor_plot_data <- pain |>
  select(pain_catastrophizing_score_0_52, fear_of_pain,
         pain_intensity_0_10, pain_disability) |>
  drop_na()
print(nrow(cor_plot_data))
print(ggpairs(cor_plot_data, mapping = aes(alpha = .15),
              lower = list(continuous = "smooth")))

# Compare all 12 correlations with Rogers & Farris (2022), Table 2.
# These are the pooled overall construct rows, not the PCS-only rows and not
# regression coefficients. The article reports results across many studies;
# our single teaching sample is not a replication of those individual studies.
# Sources: https://doi.org/10.1002/ejp.1994
#          https://doi.org/10.1097/00002508-200106000-00009

targets <- tribble(
  ~predictor, ~anxiety_symptoms, ~depressive_symptoms, ~pain_intensity_0_10, ~pain_disability,
  "pain_catastrophizing_score_0_52", .50, .51, .38, .45,
  "fear_of_pain", .34, .41, .27, .39,
  "pain_vigilance", .34, .28, .29, .34
) |>
  pivot_longer(-predictor, names_to = "outcome", values_to = "source_r")

correlation_checks <- targets |>
  rowwise() |>
  mutate(
    n_pairs = sum(complete.cases(pain[[predictor]], pain[[outcome]])),
    observed_r = cor(pain[[predictor]], pain[[outcome]], use = "complete.obs"),
    difference = observed_r - source_r,
    within_035 = abs(difference) <= .035
  ) |>
  ungroup()
print(correlation_checks, n = Inf, width = Inf)
# The .035 tolerance is an instructor check, not a statistical significance test.

# Variables in the same domain should share information, but need not be equal.
# Overlap among the thinking ratings is a reason not to put them together with
# the overall catastrophizing score in the same regression.
thinking_domain <- pain |>
  select(pain_catastrophizing_score_0_52, catastrophizing_rumination,
         catastrophizing_magnification, catastrophizing_helplessness)
print(round(cor(thinking_domain, use = "pairwise.complete.obs"), 3))
print(cor.test(pain$anxiety_symptoms, pain$depressive_symptoms))
print(cor.test(pain$pain_disability, pain$work_limitation))

# 6. FIT THE SIMPLE REGRESSION MODEL ------------------------------------------

# Question: Is catastrophizing related to pain intensity?
# Y = intercept + b * catastrophizing
# b is the expected difference in pain intensity for a ONE-point difference
# on the 0–52 catastrophizing index. It is not the correlation r.

reg_model1 <- lm(formula = pain_intensity_0_10 ~ pain_catastrophizing_score_0_52,
                 data = pain)
print(summ(reg_model1, digits = 3, model.fit = TRUE))
print(confint(reg_model1))

# In a simple regression with an intercept, R-squared equals Pearson r squared
# when both calculations use exactly the same observations.
print(c(r_squared = summary(reg_model1)$r.squared,
        correlation_squared = unname(focal_cor$estimate)^2))

reg_plot <- ggplot(pain, aes(x = pain_catastrophizing_score_0_52,
                             y = pain_intensity_0_10)) +
  geom_point(alpha = .3, color = "steelblue") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black") +
  labs(title = "Is catastrophizing related to pain intensity?",
       x = "Catastrophizing index (0–52)", y = "Pain intensity (0–10)") +
  theme_bw()
print(reg_plot)

# Predict pain at three observed levels of catastrophizing.
# These are confidence intervals for the AVERAGE predicted outcome, not
# prediction intervals for an individual person's pain score.
new_participants <- tibble(
  pain_catastrophizing_score_0_52 = as.numeric(
    quantile(pain$pain_catastrophizing_score_0_52, c(.25, .50, .75)))
)
print(bind_cols(new_participants,
                as_tibble(predict(reg_model1, newdata = new_participants,
                                  interval = "confidence"))))

# 7. ADD CONTROLS AND COMPARE MODELS ON THE SAME PARTICIPANTS ------------------

# Use one complete-case dataset for BOTH models. Otherwise a change in b could
# reflect a change in participants as well as the addition of controls.
# Duration is right-skewed. log1p(x) means log(1 + x), so zero is allowed.
# A coefficient for log duration is not a one-month effect.
# Injury severity is treated as a numeric rating here, assuming equally spaced
# steps. Its self-report meaning differs from the source's clinical impairment.

adjusted_data <- pain |>
  select(pain_intensity_0_10, pain_catastrophizing_score_0_52,
         chronic_pain_group, injury_severity, age_years,
         pain_duration_months, gender, pain_diagnosis_group) |>
  drop_na() |>
  mutate(log_pain_duration = log1p(pain_duration_months))
print(c(original_n = nrow(pain), analysis_n = nrow(adjusted_data)))

reg_model1_same_cases <- lm(
  formula = pain_intensity_0_10 ~ pain_catastrophizing_score_0_52,
  data = adjusted_data)

reg_model2 <- lm(
  formula = pain_intensity_0_10 ~ pain_catastrophizing_score_0_52 +
    chronic_pain_group + injury_severity + age_years + log_pain_duration +
    gender + pain_diagnosis_group,
  data = adjusted_data)

print(summ(reg_model1_same_cases, digits = 3, model.fit = TRUE))
print(summ(reg_model2, digits = 3, model.fit = TRUE))
print(confint(reg_model2))
print(anova(reg_model1_same_cases, reg_model2))

# Interpret the catastrophizing coefficient while holding listed controls fixed.
# The chronic_pain_group coefficient compares chronic with recent pain.
# Gender contrasts use 'man' as reference; diagnosis contrasts use 'back'.
# A category coefficient is not an inherent or causal group difference.
# The intercept at all numeric predictors = 0 is outside plausible age values;
# do not give it a substantive interpretation in this uncentered model.
# Ask: Is b positive? How large is its interval? How much did R-squared change?
# Do not expect every control to have a statistically significant coefficient.

print(effect_plot(reg_model2, pred = pain_catastrophizing_score_0_52,
                   interval = TRUE, plot.points = FALSE,
                   main.title = "Adjusted relationship with pain intensity"))

# Related outcomes: fit each separately, rather than treating every outcome as
# a control for every other outcome. These models use their own complete cases.
# Their coefficients are adjusted effects, NOT the Table 2 correlations.
reg_disability <- lm(
  formula = pain_disability ~ pain_catastrophizing_score_0_52 +
    chronic_pain_group + injury_severity + age_years + gender,
  data = pain)
reg_anxiety <- lm(
  formula = anxiety_symptoms ~ pain_catastrophizing_score_0_52 +
    chronic_pain_group + injury_severity + age_years + gender,
  data = pain)
reg_depression <- lm(
  formula = depressive_symptoms ~ pain_catastrophizing_score_0_52 +
    chronic_pain_group + injury_severity + age_years + gender,
  data = pain)
print(summ(reg_disability, digits = 3))
print(summ(reg_anxiety, digits = 3))
print(summ(reg_depression, digits = 3))
print(c(disability_n = nobs(reg_disability), anxiety_n = nobs(reg_anxiety),
        depression_n = nobs(reg_depression)))

# 8. TEST MODERATION: DOES FEAR CHANGE THE CATASTROPHIZING RELATIONSHIP? ---------

# Fear of pain is the designated teaching moderator. This interaction is an
# exploratory teaching feature; it is NOT an estimate reported in Table 2.
# The source's study-level comparisons are not participant-level interactions.
# We fit ordinary regression with independent rows, with no random effects.
#
# First use the same complete cases for models with and without the interaction.
moderation_data <- pain |>
  select(pain_intensity_0_10, pain_catastrophizing_score_0_52, fear_of_pain,
         pain_vigilance, chronic_pain_group, injury_severity, sleep_quality,
         social_support, pain_self_efficacy, physical_activity_days_week,
         age_years, gender, pain_diagnosis_group) |>
  drop_na()
print(c(original_n = nrow(pain), moderation_n = nrow(moderation_data)))

# Centering subtracts the analysis sample's mean. It changes the interpretation
# of the intercept/main effects; it does not create or remove an interaction.
cat_mean <- mean(moderation_data$pain_catastrophizing_score_0_52)
fear_mean <- mean(moderation_data$fear_of_pain)
moderation_data <- moderation_data |>
  mutate(cat_c = pain_catastrophizing_score_0_52 - cat_mean,
         fear_c = fear_of_pain - fear_mean)
print(c(cat_center = cat_mean, fear_center = fear_mean))

reg_no_interaction <- lm(
  formula = pain_intensity_0_10 ~ cat_c + fear_c + pain_vigilance +
    chronic_pain_group + injury_severity + sleep_quality + social_support +
    pain_self_efficacy + physical_activity_days_week + age_years + gender +
    pain_diagnosis_group,
  data = moderation_data)

# The * symbol includes cat_c, fear_c, AND their product cat_c:fear_c.
reg_moderation <- lm(
  formula = pain_intensity_0_10 ~ cat_c * fear_c + pain_vigilance +
    chronic_pain_group + injury_severity + sleep_quality + social_support +
    pain_self_efficacy + physical_activity_days_week + age_years + gender +
    pain_diagnosis_group,
  data = moderation_data)

print(summ(reg_no_interaction, digits = 3, model.fit = TRUE))
print(summ(reg_moderation, digits = 3, model.fit = TRUE))
print(confint(reg_moderation))
print(anova(reg_no_interaction, reg_moderation))
print(c(increase_in_R_squared = summary(reg_moderation)$r.squared -
          summary(reg_no_interaction)$r.squared))

# cat_c: catastrophizing slope at AVERAGE fear.
# fear_c: fear slope at AVERAGE catastrophizing.
# cat_c:fear_c: change in the catastrophizing slope per one-point higher fear.
# A nonsignificant fear main effect does not rule out moderation.
# Protective ratings (sleep, support, confidence) use higher = better coding.
# Examine estimates AND uncertainty; not every adjusted coefficient must match
# its simple correlation, and these associations do not establish causation.

# 9. CALCULATE SIMPLE SLOPES AND PLOT THE INTERACTION ---------------------------

# A simple slope is the catastrophizing slope at one specified fear score.
# Use the 25th, 50th, and 75th percentiles, rather than splitting people into
# artificial groups. Fear remains continuous in the fitted regression.
fear_values <- as.numeric(quantile(moderation_data$fear_of_pain, c(.25, .50, .75)))
simple_slopes <- tibble(
  fear_level = c("Lower fear (25th percentile)", "Median fear", "Higher fear (75th percentile)"),
  fear_of_pain = fear_values,
  fear_c = fear_values - fear_mean
)

# Slope = b_cat + b_interaction * centered fear.
# Its uncertainty must include the covariance between the two coefficients.
b <- coef(reg_moderation)
V <- vcov(reg_moderation)
t_critical <- qt(.975, df = df.residual(reg_moderation))
simple_slopes <- simple_slopes |>
  mutate(
    slope = b[["cat_c"]] + b[["cat_c:fear_c"]] * fear_c,
    se = sqrt(V["cat_c", "cat_c"] + fear_c^2 *
                V["cat_c:fear_c", "cat_c:fear_c"] +
                2 * fear_c * V["cat_c", "cat_c:fear_c"]),
    lower_95 = slope - t_critical * se,
    upper_95 = slope + t_critical * se,
    t_value = slope / se,
    p_value = 2 * pt(-abs(t_value), df = df.residual(reg_moderation))
  )
print(simple_slopes, width = Inf)
# Compare the slopes themselves. 'Significant in one group but not another'
# is not a valid substitute for testing the interaction coefficient.

# Predict across the middle 90% of observed catastrophizing scores.
# Other numeric controls are fixed at their sample means; categorical controls
# are fixed to chronic pain, man, and back diagnosis for this example profile.
# These lines are predictions for that profile, not separate fitted group lines.
cat_values <- seq(quantile(moderation_data$pain_catastrophizing_score_0_52, .05),
                  quantile(moderation_data$pain_catastrophizing_score_0_52, .95),
                  length.out = 60)
interaction_grid <- expand_grid(
  pain_catastrophizing_score_0_52 = cat_values,
  fear_of_pain = fear_values
) |>
  mutate(
    cat_c = pain_catastrophizing_score_0_52 - cat_mean,
    fear_c = fear_of_pain - fear_mean,
    pain_vigilance = mean(moderation_data$pain_vigilance),
    injury_severity = mean(moderation_data$injury_severity),
    sleep_quality = mean(moderation_data$sleep_quality),
    social_support = mean(moderation_data$social_support),
    pain_self_efficacy = mean(moderation_data$pain_self_efficacy),
    physical_activity_days_week = mean(moderation_data$physical_activity_days_week),
    age_years = mean(moderation_data$age_years),
    chronic_pain_group = factor("chronic_pain", levels = levels(pain$chronic_pain_group)),
    gender = factor("man", levels = levels(pain$gender)),
    pain_diagnosis_group = factor("back", levels = levels(pain$pain_diagnosis_group))
  )
interaction_predictions <- predict(reg_moderation, newdata = interaction_grid,
                                    interval = "confidence")
interaction_grid <- bind_cols(interaction_grid, as_tibble(interaction_predictions)) |>
  mutate(fear_level = factor(fear_of_pain, levels = fear_values,
                             labels = paste0(c("Lower", "Median", "Higher"),
                                             " fear (", round(fear_values, 2), ")")))

interaction_plot <- ggplot(interaction_grid,
                           aes(x = pain_catastrophizing_score_0_52, y = fit,
                               color = fear_level, fill = fear_level)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .12, color = NA) +
  geom_line(linewidth = 1) +
  labs(title = "Does fear of pain moderate the catastrophizing relationship?",
       subtitle = "Adjusted predictions; shaded bands are 95% confidence intervals",
       x = "Catastrophizing index (0–52)", y = "Predicted pain intensity (0–10)",
       color = "Fear of pain", fill = "Fear of pain") +
  theme_bw()
print(interaction_plot)

# 10. CHECK REGRESSION DIAGNOSTICS --------------------------------------------

# Residual = observed outcome minus predicted outcome.
# Residuals vs Fitted: look for curves or changing spread.
# Normal Q-Q: look for strong departures from the straight reference line.
# Scale-Location: look for changing residual spread across predictions.
# Residuals vs Leverage: look for observations with unusual predictor profiles
# that may have a large influence on the fitted model.
# A non-normal predictor histogram is NOT itself a violation of regression.
# These plots cannot verify independent observations; that depends on design.

old_par <- par(mfrow = c(2, 2))
plot(reg_model2, which = c(1, 2, 3, 5))
par(old_par)
old_par <- par(mfrow = c(2, 2))
plot(reg_moderation, which = c(1, 2, 3, 5))
par(old_par)

# Collinearity means predictors share information, making separate effects
# harder to estimate. Larger VIF values indicate more coefficient uncertainty.
print(check_collinearity(reg_model2))
print(check_collinearity(reg_moderation))
# Interaction terms can naturally be correlated with their component variables.
# Do not remove a required main effect simply to lower a VIF.

# Cook's distance identifies potentially influential observations.
# 4/n is a screening guide, not an automatic deletion rule.
cooks_values <- cooks.distance(reg_moderation)
influence_checks <- tibble(
  analysis_row = seq_along(cooks_values),
  cooks_distance = unname(cooks_values),
  flagged_for_review = cooks_values > 4 / nobs(reg_moderation)
) |>
  arrange(desc(cooks_distance))
print(head(influence_checks, 10))

# Linear predictions can sometimes go outside an outcome's possible range.
# Count them instead of silently clipping them to 0 or 10.
print(c(predictions_below_0 = sum(fitted(reg_moderation) < 0),
        predictions_above_10 = sum(fitted(reg_moderation) > 10)))

# Interpretation reminders:
# - Start with effect size, direction, confidence interval, and measurement units.
# - Missing-data deletion can change who is represented in a model.
# - Do not put every available variable into one regression automatically.
# - Medication use may follow worse pain; its association is not treatment harm.
# - Use the interaction test to assess moderation, not separate group p-values.
# - Fear is the planned moderator here; other interactions need a stated reason.
# - Ordinary regression deliberately omits the sources' group/random effects.

# 11. OPTIONAL: SAVE YOUR CHECKS ----------------------------------------------

# Change FALSE to TRUE to save tables and the two main plots in your project.
save_outputs <- FALSE
if (save_outputs) {
  dir.create("topic2_checks", showWarnings = FALSE)
  write_csv(missing_summary, "topic2_checks/missing_values.csv")
  write_csv(numeric_summary, "topic2_checks/numeric_distributions.csv")
  write_csv(category_counts, "topic2_checks/category_counts.csv")
  write_csv(correlation_checks, "topic2_checks/source_correlation_comparison.csv")
  write_csv(simple_slopes, "topic2_checks/fear_simple_slopes.csv")
  ggsave("topic2_checks/catastrophizing_regression.png", reg_plot,
         width = 8, height = 6, dpi = 300)
  ggsave("topic2_checks/fear_moderation.png", interaction_plot,
         width = 9, height = 6, dpi = 300)
  capture.output(summary(reg_model2), confint(reg_model2),
                 summary(reg_moderation), confint(reg_moderation),
                 anova(reg_no_interaction, reg_moderation),
                 file = "topic2_checks/regression_results.txt")
}

# Record package/R versions if you need to reproduce the analysis later.
sessionInfo()
