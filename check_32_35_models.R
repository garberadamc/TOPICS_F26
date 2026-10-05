# PSY150 Fall2026 | Topics 32-35 | INSTRUCTOR ONLY
# Open Topics32_35_F26.Rproj from the extracted package before running.
# install.packages(c("tidyverse", "here", "jtools"))
library(tidyverse)
library(here)
library(jtools)
options(contrasts = c("contr.treatment", "contr.poly"))
# Artificial p-value calibration is for teaching, not empirical inference.
# R was not installed in the authoring environment. Python checks independently
# used QR/SVD for OLS and Newton/trust-region likelihood optimization for logit.
# Topic 34 estimates, including the event rate, are HYPOTHETICAL, not medical evidence.
dir.create(here("qa"), showWarnings = FALSE, recursive = TRUE)
files <- c(
  t32 = "42053_hall_exercise_mood_F26.csv",
  t33 = "42053_torrescano_sleep_anxiety_F26.csv",
  t34 = "42053_walters_ketamine_dependence_F26.csv",
  t35 = "46306_gonzalez_exercise_wellbeing_F26.csv"
)
datasets <- map(files, function(file_name) {
  raw_data <- read_csv(here("data", file_name), show_col_types = FALSE)
  stopifnot(nrow(raw_data) >= 300, nrow(raw_data) <= 1000,
    !anyNA(raw_data), !anyDuplicated(raw_data$participant_id))
  raw_data
})
show_model <- function(model) {
  print(summ(model, digits = 3))
  tab <- coef(summary(model))
  print(tibble(term = rownames(tab), p_value = format.pval(tab[, 4], digits = 5, eps = 1e-9)))
}

# TOPIC 32: Regular exercise and mood ----
# P is Reed/Buck: mean corrected d=.57 across 105 studies, not an adjusted days/week slope. Match mean within-cohort change divided by enrollment SD to .57 as an illustrative magnitude only; this is NOT the meta-analytic estimator or a causal treatment-control effect. Lower starting affect predicts larger exercise-associated gains, consistent with moderator direction. The fitted days/week slope and all added covariate magnitudes are pedagogical assumptions. Acute visit fields are hypothetical extensions; the Crush design compared separate visits and does not establish the fabricated pre/post coefficients.
t32 <- datasets$t32 %>% mutate(
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("hispanic_latino", "white", "asian", "black", "multiracial_or_other")),
  first_generation = factor(first_generation, levels = c("no", "yes")),
  college_stage = factor(college_stage, levels = c("early", "advanced")),
  major_group = factor(major_group, levels = c("arts_social", "stem_business")),
  living_arrangement = factor(living_arrangement, levels = c("with_family", "away_from_family")),
  current_counseling = factor(current_counseling, levels = c("no", "yes")),
  chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
  organized_sport = factor(organized_sport, levels = c("no", "yes")),
  course_format = factor(course_format, levels = c("mainly_in_person", "mainly_online"))
)
m1_t32 <- lm(mood ~ exercise_frequency, data = t32)
show_model(m1_t32)
m2_t32 <- lm(
  mood ~ exercise_frequency +
    age +
    baseline_positive_affect +
    academic_stress +
    sleep_hours +
    social_support +
    baseline_negative_affect +
    exercise_self_efficacy +
    work_hours +
    financial_strain +
    physical_health +
    fitness_baseline +
    activity_enjoyment +
    time_pressure +
    screen_hours +
    sleep_quality +
    relationship_stress +
    coping_confidence +
    commute_minutes +
    neighborhood_access +
    course_credits +
    gender +
    race_ethnicity +
    first_generation +
    college_stage +
    major_group +
    living_arrangement +
    current_counseling +
    chronic_condition +
    organized_sport +
    course_format, data = t32)
show_model(m2_t32)
t32_centered <- t32 %>% mutate(
  exercise_frequency = exercise_frequency - mean(exercise_frequency),
  baseline_positive_affect = baseline_positive_affect - mean(baseline_positive_affect)
)
m3_t32 <- lm(
  mood ~ exercise_frequency +
    age +
    baseline_positive_affect +
    academic_stress +
    sleep_hours +
    social_support +
    baseline_negative_affect +
    exercise_self_efficacy +
    work_hours +
    financial_strain +
    physical_health +
    fitness_baseline +
    activity_enjoyment +
    time_pressure +
    screen_hours +
    sleep_quality +
    relationship_stress +
    coping_confidence +
    commute_minutes +
    neighborhood_access +
    course_credits +
    gender +
    race_ethnicity +
    first_generation +
    college_stage +
    major_group +
    living_arrangement +
    current_counseling +
    chronic_condition +
    organized_sport +
    course_format +
    baseline_positive_affect:exercise_frequency, data = t32_centered)
show_model(m3_t32)
covs_t32 <- c("age", "baseline_positive_affect", "academic_stress", "sleep_hours", "social_support", "baseline_negative_affect", "exercise_self_efficacy", "work_hours", "financial_strain", "physical_health", "fitness_baseline", "activity_enjoyment", "time_pressure", "screen_hours", "sleep_quality", "relationship_stress", "coping_confidence", "commute_minutes", "neighborhood_access", "course_credits", "gender", "race_ethnicity", "first_generation", "college_stage", "major_group", "living_arrangement", "current_counseling", "chronic_condition", "organized_sport", "course_format")

# TOPIC 33: Sleep duration and childhood anxiety ----
# P N=53 sequential rested/restricted sessions, 7 then 6 hours in bed; positive affect reduction t(52)=3.29, p=.002, negative affect p=.51, stronger positive-affect reduction with baseline anxiety. Preserve that pattern in companion fields, not as fabricated anxiety-onset evidence. User-requested 8 versus 5-6 hour parallel design and primary anxiety outcome are hypothetical. Baseline-anxiety moderation of the sleep/anxiety slope is a bidirectional-arousal teaching hypothesis, not the published interaction estimate. No exact adjusted source coefficient exists for the proposed outcome. Martin et al. provides additional school-age sleep-environment, routine and socioeconomic context; its observational sleep associations are not anxiety-outcome coefficients.
t33 <- datasets$t33 %>% mutate(
  gender = factor(gender, levels = c("girl", "boy", "self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("hispanic_latino", "white", "asian", "black", "multiracial_or_other")),
  school_stage = factor(school_stage, levels = c("primary_early", "primary_later")),
  shared_bedroom = factor(shared_bedroom, levels = c("no", "yes")),
  parent_shift_work = factor(parent_shift_work, levels = c("no", "yes")),
  prior_counseling = factor(prior_counseling, levels = c("no", "yes")),
  chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
  regular_bedtime_reading = factor(regular_bedtime_reading, levels = c("no", "yes")),
  household_structure = factor(household_structure, levels = c("one_caregiver", "multiple_caregivers")),
  session_day = factor(session_day, levels = c("school_day", "weekend"))
)
m1_t33 <- lm(anxiety ~ sleep_duration, data = t33)
show_model(m1_t33)
m2_t33 <- lm(
  anxiety ~ sleep_duration +
    age_months +
    baseline_anxiety +
    parent_anxiety +
    bedtime_resistance +
    family_stress +
    emotion_regulation +
    peer_support +
    usual_sleep_hours +
    evening_screen_minutes +
    school_pressure +
    bedtime_consistency +
    parent_support +
    baseline_fatigue +
    physical_activity_minutes +
    noise_at_night +
    housing_stability +
    recent_life_events +
    sensory_sensitivity +
    household_income_thousand +
    siblings_count +
    gender +
    race_ethnicity +
    school_stage +
    shared_bedroom +
    parent_shift_work +
    prior_counseling +
    chronic_condition +
    regular_bedtime_reading +
    household_structure +
    session_day, data = t33)
show_model(m2_t33)
t33_centered <- t33 %>% mutate(
  sleep_duration = sleep_duration - mean(sleep_duration),
  baseline_anxiety = baseline_anxiety - mean(baseline_anxiety)
)
m3_t33 <- lm(
  anxiety ~ sleep_duration +
    age_months +
    baseline_anxiety +
    parent_anxiety +
    bedtime_resistance +
    family_stress +
    emotion_regulation +
    peer_support +
    usual_sleep_hours +
    evening_screen_minutes +
    school_pressure +
    bedtime_consistency +
    parent_support +
    baseline_fatigue +
    physical_activity_minutes +
    noise_at_night +
    housing_stability +
    recent_life_events +
    sensory_sensitivity +
    household_income_thousand +
    siblings_count +
    gender +
    race_ethnicity +
    school_stage +
    shared_bedroom +
    parent_shift_work +
    prior_counseling +
    chronic_condition +
    regular_bedtime_reading +
    household_structure +
    session_day +
    baseline_anxiety:sleep_duration, data = t33_centered)
show_model(m3_t33)
covs_t33 <- c("age_months", "baseline_anxiety", "parent_anxiety", "bedtime_resistance", "family_stress", "emotion_regulation", "peer_support", "usual_sleep_hours", "evening_screen_minutes", "school_pressure", "bedtime_consistency", "parent_support", "baseline_fatigue", "physical_activity_minutes", "noise_at_night", "housing_stability", "recent_life_events", "sensory_sensitivity", "household_income_thousand", "siblings_count", "gender", "race_ethnicity", "school_stage", "shared_bedroom", "parent_shift_work", "prior_counseling", "chronic_condition", "regular_bedtime_reading", "household_structure", "session_day")

# TOPIC 34: Prior substance-use disorder and ketamine misuse/dependence ----
# CRITICAL: Hurst: 203 respondents, 52 received NIMH ketamine, no nonprescribed seeking or abuse symptoms, three mild cravings during clinical treatment. Zaki: 1148 selected monitored esketamine patients, not racemic IV ketamine and not a valid prior-SUD contrast. Consequently no empirical prior-SUD OR, dose-response or event rate can be replicated. The primary binary endpoint, nonzero event count, positive history contrast and protective monitoring interaction are explicitly hypothetical. A large N within the requested range supports a rare-event classroom illustration, but events per coefficient remain low; Wald inference and the engineered covariate significance pattern are not suitable for clinical prediction. Do not interpret absence of reported source events as zero risk.
t34 <- datasets$t34 %>% mutate(
  prior_substance_use_disorder = factor(prior_substance_use_disorder, levels = c("no", "yes")),
  condition_treated = factor(condition_treated, levels = c("depression", "depression_with_chronic_pain")),
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("hispanic_latino", "white", "asian", "black", "multiracial_or_other")),
  tobacco_use = factor(tobacco_use, levels = c("no", "yes")),
  concurrent_sedative_prescription = factor(concurrent_sedative_prescription, levels = c("no", "yes")),
  trauma_history = factor(trauma_history, levels = c("no", "yes")),
  family_sud_history = factor(family_sud_history, levels = c("no", "yes")),
  recovery_program = factor(recovery_program, levels = c("no", "yes")),
  stable_housing = factor(stable_housing, levels = c("no", "yes")),
  employment_status = factor(employment_status, levels = c("not_employed", "employed"))
)
m1_t34 <- glm(ketamine_misuse_or_dependence ~ prior_substance_use_disorder, data = t34, family = binomial(link = "logit"))
show_model(m1_t34)
m2_t34 <- glm(
  ketamine_misuse_or_dependence ~ prior_substance_use_disorder +
    age +
    ketamine_frequency +
    ketamine_dose +
    treatment_length +
    monitoring_intensity +
    baseline_craving +
    impulsivity +
    recovery_support +
    baseline_depression +
    baseline_anxiety +
    pain_interference +
    sleep_disruption +
    financial_strain +
    social_isolation +
    coping_skills +
    treatment_engagement +
    medication_literacy +
    household_stability +
    prior_psychiatric_treatments +
    travel_minutes +
    condition_treated +
    gender +
    race_ethnicity +
    tobacco_use +
    concurrent_sedative_prescription +
    trauma_history +
    family_sud_history +
    recovery_program +
    stable_housing +
    employment_status, data = t34, family = binomial(link = "logit"))
show_model(m2_t34)
t34_centered <- t34 %>% mutate(
  monitoring_intensity = monitoring_intensity - mean(monitoring_intensity)
)
m3_t34 <- glm(
  ketamine_misuse_or_dependence ~ prior_substance_use_disorder +
    age +
    ketamine_frequency +
    ketamine_dose +
    treatment_length +
    monitoring_intensity +
    baseline_craving +
    impulsivity +
    recovery_support +
    baseline_depression +
    baseline_anxiety +
    pain_interference +
    sleep_disruption +
    financial_strain +
    social_isolation +
    coping_skills +
    treatment_engagement +
    medication_literacy +
    household_stability +
    prior_psychiatric_treatments +
    travel_minutes +
    condition_treated +
    gender +
    race_ethnicity +
    tobacco_use +
    concurrent_sedative_prescription +
    trauma_history +
    family_sud_history +
    recovery_program +
    stable_housing +
    employment_status +
    monitoring_intensity:prior_substance_use_disorder, data = t34_centered, family = binomial(link = "logit"))
show_model(m3_t34)
covs_t34 <- c("age", "ketamine_frequency", "ketamine_dose", "treatment_length", "monitoring_intensity", "baseline_craving", "impulsivity", "recovery_support", "baseline_depression", "baseline_anxiety", "pain_interference", "sleep_disruption", "financial_strain", "social_isolation", "coping_skills", "treatment_engagement", "medication_literacy", "household_stability", "prior_psychiatric_treatments", "travel_minutes", "condition_treated", "gender", "race_ethnicity", "tobacco_use", "concurrent_sedative_prescription", "trauma_history", "family_sud_history", "recovery_program", "stable_housing", "employment_status")

# TOPIC 35: Exercise frequency and mental well-being ----
# P baseline N185: physical activity correlations positive affect .28/.30 and psychological wellbeing .31; anxiety -.27/-.26; these are observational correlations, not adjusted days/week slopes. Online pilot completers N74 and laboratory N32; effects differed by outcome. Eather N53 HIIT: stress p=.476, anxiety p=.709, fitness improved. Primary composite and academic-stress buffering interaction are teaching extensions, not published effect estimates. Companion anxiety is deliberately near-null after adjustment to illustrate outcome specificity; it does not reproduce an RCT. Target moderate positive observational direction while prioritizing requested adjusted SEs/p-values over exact source correlations.
t35 <- datasets$t35 %>% mutate(
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("hispanic_latino", "white", "asian", "black", "multiracial_or_other")),
  first_generation = factor(first_generation, levels = c("no", "yes")),
  college_stage = factor(college_stage, levels = c("early", "advanced")),
  major_group = factor(major_group, levels = c("arts_social", "stem_business")),
  living_arrangement = factor(living_arrangement, levels = c("with_family", "away_from_family")),
  current_counseling = factor(current_counseling, levels = c("no", "yes")),
  chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
  organized_sport = factor(organized_sport, levels = c("no", "yes")),
  course_format = factor(course_format, levels = c("mainly_in_person", "mainly_online"))
)
m1_t35 <- lm(mental_wellbeing ~ exercise_frequency, data = t35)
show_model(m1_t35)
m2_t35 <- lm(
  mental_wellbeing ~ exercise_frequency +
    age +
    academic_stress +
    work_responsibilities +
    relationship_stress +
    baseline_wellbeing +
    sleep_hours +
    social_support +
    financial_strain +
    exercise_enjoyment +
    activity_self_efficacy +
    perceived_fitness +
    screen_hours +
    time_pressure +
    coping_confidence +
    campus_belonging +
    sleep_quality +
    commute_minutes +
    course_credits +
    physical_health +
    neighborhood_access +
    gender +
    race_ethnicity +
    first_generation +
    college_stage +
    major_group +
    living_arrangement +
    current_counseling +
    chronic_condition +
    organized_sport +
    course_format, data = t35)
show_model(m2_t35)
t35_centered <- t35 %>% mutate(
  exercise_frequency = exercise_frequency - mean(exercise_frequency),
  academic_stress = academic_stress - mean(academic_stress)
)
m3_t35 <- lm(
  mental_wellbeing ~ exercise_frequency +
    age +
    academic_stress +
    work_responsibilities +
    relationship_stress +
    baseline_wellbeing +
    sleep_hours +
    social_support +
    financial_strain +
    exercise_enjoyment +
    activity_self_efficacy +
    perceived_fitness +
    screen_hours +
    time_pressure +
    coping_confidence +
    campus_belonging +
    sleep_quality +
    commute_minutes +
    course_credits +
    physical_health +
    neighborhood_access +
    gender +
    race_ethnicity +
    first_generation +
    college_stage +
    major_group +
    living_arrangement +
    current_counseling +
    chronic_condition +
    organized_sport +
    course_format +
    academic_stress:exercise_frequency, data = t35_centered)
show_model(m3_t35)
covs_t35 <- c("age", "academic_stress", "work_responsibilities", "relationship_stress", "baseline_wellbeing", "sleep_hours", "social_support", "financial_strain", "exercise_enjoyment", "activity_self_efficacy", "perceived_fitness", "screen_hours", "time_pressure", "coping_confidence", "campus_belonging", "sleep_quality", "commute_minutes", "course_credits", "physical_health", "neighborhood_access", "gender", "race_ethnicity", "first_generation", "college_stage", "major_group", "living_arrangement", "current_counseling", "chronic_condition", "organized_sport", "course_format")

# SOURCE-CONTEXT COMPANIONS (different estimands from the source papers) ----
print(t32 %>% summarise(standardized_mean_change = mean(mood_change) / sd(baseline_positive_affect)))
m4_t33_positive <- lm(positive_affect_change ~ positive_affect_rested +
  sleep_duration * baseline_anxiety, data = t33)
show_model(m4_t33_positive)
m5_t33_negative <- lm(I(negative_affect_followup - negative_affect_rested) ~
  sleep_duration + baseline_anxiety, data = t33)
show_model(m5_t33_negative)
m4_t35_anxiety <- update(m3_t35, anxiety_symptoms ~ .)
show_model(m4_t35_anxiety)
print(t34 %>% group_by(prior_substance_use_disorder) %>%
  summarise(n = n(), events = sum(ketamine_misuse_or_dependence),
    event_proportion = mean(ketamine_misuse_or_dependence), .groups = "drop"))
# Low events per coefficient means these logit Wald results are a teaching example,
# not a validated risk model. There is no reliable empirical prior-SUD OR in sources.
stopifnot(m1_t34$converged, m2_t34$converged, m3_t34$converged)

# NUMERIC CROSS-CHECK AGAINST SAVED PYTHON RESULTS ----
models_t32 <- list(focal_only = m1_t32, adjusted = m2_t32, interaction = m3_t32)
models_t33 <- list(focal_only = m1_t33, adjusted = m2_t33, interaction = m3_t33)
models_t34 <- list(focal_only = m1_t34, adjusted = m2_t34, interaction = m3_t34)
models_t35 <- list(focal_only = m1_t35, adjusted = m2_t35, interaction = m3_t35)
models <- list(`32` = models_t32, `33` = models_t33, `34` = models_t34, `35` = models_t35)
audit <- imap_dfr(models, function(batch, topic_id) {
  imap_dfr(batch, function(m, model_name) {
    tab <- coef(summary(m))
    tibble(topic = as.integer(topic_id), model = model_name, term = rownames(tab),
      estimate = tab[, 1], std_error = tab[, 2], p_value = tab[, 4])
  })
})
normalize_term <- function(term) {
  term <- if_else(term %in% c("Intercept", "(Intercept)"), "Intercept", term)
  term <- str_remove_all(term, "\\[|\\]")
  map_chr(str_split(term, ":"), ~ paste(sort(.x), collapse = ":"))
}
reference <- read_csv(here("qa", "topics32_35_regression_audit.csv"), show_col_types = FALSE)
comparison <- audit %>% mutate(key = normalize_term(term)) %>%
  inner_join(reference %>% mutate(key = normalize_term(term)),
    by = c("topic", "model", "key"), suffix = c("_R", "_Python"))
stopifnot(nrow(comparison) == nrow(audit),
  max(abs(comparison$estimate_R - comparison$estimate_Python)) < 1e-5,
  max(abs(comparison$std_error_R - comparison$std_error_Python)) < 1e-5,
  max(abs(comparison$p_value_R - comparison$p_value_Python)) < 1e-5)
write_csv(audit, here("qa", "topics32_35_R_regression_audit.csv"))

# Covariates are tested as whole terms, including multi-level factors.
# Wald F for lm; Wald chi-square for glm. Match the Python audit definitions.
block_tests <- function(m, variables, topic_id, model_name) {
  mm <- model.matrix(m)
  labels <- attr(terms(m), "term.labels")
  assignment <- attr(mm, "assign")
  map_dfr(variables, function(v) {
    ix <- which(assignment == match(v, labels))
    b <- coef(m)[ix]; V <- vcov(m)[ix, ix, drop = FALSE]
    W <- as.numeric(t(b) %*% solve(V, b)); k <- length(ix)
    p <- if (inherits(m, "glm")) pchisq(W, k, lower.tail = FALSE) else
      pf(W / k, k, df.residual(m), lower.tail = FALSE)
    tibble(topic = topic_id, model = model_name, covariate = v, p_value = p,
      significant = p < .05, near_threshold = p > .05 & p <= .10)
  })
}
checks <- bind_rows(
  block_tests(m2_t32, covs_t32, 32, "adjusted"),
  block_tests(m3_t32, covs_t32, 32, "interaction"),
  block_tests(m2_t33, covs_t33, 33, "adjusted"),
  block_tests(m3_t33, covs_t33, 33, "interaction"),
  block_tests(m2_t34, covs_t34, 34, "adjusted"),
  block_tests(m3_t34, covs_t34, 34, "interaction"),
  block_tests(m2_t35, covs_t35, 35, "adjusted"),
  block_tests(m3_t35, covs_t35, 35, "interaction")
)
check_summary <- checks %>% group_by(topic, model) %>%
  summarise(significant_covariates = sum(significant),
    nonsignificant_near_fraction = mean(near_threshold[!significant]), .groups = "drop")
print(check_summary)
stopifnot(all(check_summary$significant_covariates >= 5),
  all(check_summary$nonsignificant_near_fraction >= .70))
write_csv(checks, here("qa", "topics32_35_R_covariate_tests.csv"))
# Repeat all specified coefficient-direction checks using the R fits.
expected_directions <- read_csv(here("qa", "topics32_35_direction_audit.csv"), show_col_types = FALSE)
direction_check <- expected_directions %>% mutate(key = normalize_term(term)) %>%
  inner_join(audit %>% mutate(key = normalize_term(term)),
    by = c("topic", "model", "key"), suffix = c("_expected", "_R"))
stopifnot(nrow(direction_check) == nrow(expected_directions),
  all(direction_check$expected_sign == 0 |
      sign(direction_check$estimate_R) == direction_check$expected_sign))
write_csv(direction_check, here("qa", "topics32_35_R_direction_check.csv"))
# Conditional estimates at the observed endpoints and quantiles are provided in
# topics32_35_conditional_slopes.csv. Both the focal and moderator signs are checked.
