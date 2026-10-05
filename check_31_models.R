# PSY150 Fall2026: Topic 31 | INSTRUCTOR ONLY
# Open the extracted package as an RStudio project; data/ and qa/ are required.
# install.packages(c("tidyverse", "here", "jtools"))
library(tidyverse)
library(here)
library(jtools)
# Deliberately calibrated synthetic data, not estimates from real people.
# R was unavailable during authoring. Saved CSV OLS was independently checked
# with QR and SVD in Python. Run this script to verify the models in R.
# P trial: no one-month app benefit; K10 time-by-group p=.90, N=169.
# Companion ORIGINAL distress scale preserves means 27.92 -> 23.60 and p=.90.
# It is not K10 data. Duration and symptom/style measures are teaching extensions.
# No published duration-by-unhealthy-use symptom interaction is being replicated.
# Primary combined symptom outcome is 0-24, not a diagnosis; near-null effect at
# average unhealthy use, with a context-dependent duration slope.
# All added coefficient magnitudes and near-cutoff p-values are teaching choices.
dir.create(here("qa"), recursive = TRUE, showWarnings = FALSE)
files <- c(music_mental_health = "42053_lauer_music_mental_health_F26.csv")
datasets <- map(files, function(file_name) {
  raw_data <- read_csv(here("data", file_name), show_col_types = FALSE)
  stopifnot(nrow(raw_data) >= 300, !anyNA(raw_data), !anyDuplicated(raw_data$participant_id))
  raw_data
})
music <- datasets$music_mental_health %>% mutate(
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("hispanic_latino", "white", "asian", "black", "multiracial_or_other")),
  first_generation = factor(first_generation, levels = c("no", "yes")),
  app_access = factor(app_access, levels = c("delayed", "immediate")),
  music_genre = factor(music_genre, levels = c("pop_rock", "instrumental_classical", "hiphop_rnb", "other_mixed")),
  time_of_day = factor(time_of_day, levels = c("daytime", "evening")),
  cultural_music_context = factor(cultural_music_context, levels = c("mainly_familiar_local", "mixed_crosscultural")),
  current_counseling = factor(current_counseling, levels = c("no", "yes")),
  instrument_group = factor(instrument_group, levels = c("not_currently_active", "currently_active")),
  living_arrangement = factor(living_arrangement, levels = c("family_household", "other_household"))
)
show_model <- function(model) {
  print(summ(model, digits = 3))
  print(as.data.frame(coef(summary(model))) %>% rownames_to_column("term") %>%
    transmute(term, p_value = format.pval(`Pr(>|t|)`, digits = 4, eps = 1e-8)))
}

# FOCAL-ONLY MODEL ----
m1_music <- lm(depression_and_anxiety ~ music_listening_duration, data = music)
show_model(m1_music)

# ADJUSTED MODEL: 30 CONTEXT VARIABLES ----
m2_music <- lm(
  depression_and_anxiety ~ music_listening_duration +
    age +
    baseline_depression +
    baseline_anxiety +
    sleep_hours +
    sleep_quality +
    academic_stress +
    social_support +
    financial_strain +
    work_hours +
    physical_activity_minutes +
    screen_hours +
    rumination +
    healthy_music_use +
    unhealthy_music_use +
    emotion_regulation_skill +
    social_connection +
    music_importance +
    emotion_awareness +
    music_training_years +
    enrollment_units +
    gender +
    race_ethnicity +
    first_generation +
    app_access +
    music_genre +
    time_of_day +
    cultural_music_context +
    current_counseling +
    instrument_group +
    living_arrangement, data = music)
show_model(m2_music)

# INTERACTION MODEL: CENTER THE TWO INTERACTING VARIABLES ----
music_centered <- music %>% mutate(
  unhealthy_music_use = unhealthy_music_use - mean(unhealthy_music_use),
  music_listening_duration = music_listening_duration - mean(music_listening_duration))
m3_music <- lm(
  depression_and_anxiety ~ music_listening_duration +
    age +
    baseline_depression +
    baseline_anxiety +
    sleep_hours +
    sleep_quality +
    academic_stress +
    social_support +
    financial_strain +
    work_hours +
    physical_activity_minutes +
    screen_hours +
    rumination +
    healthy_music_use +
    unhealthy_music_use +
    emotion_regulation_skill +
    social_connection +
    music_importance +
    emotion_awareness +
    music_training_years +
    enrollment_units +
    gender +
    race_ethnicity +
    first_generation +
    app_access +
    music_genre +
    time_of_day +
    cultural_music_context +
    current_counseling +
    instrument_group +
    living_arrangement +
    unhealthy_music_use:music_listening_duration, data = music_centered)
show_model(m3_music)

# SOURCE-DESIGN COMPANION: NO DIFFERENTIAL ONE-MONTH APP BENEFIT ----
# With two occasions, the group coefficient in the baseline-adjusted change
# model equals the group coefficient in baseline-adjusted follow-up ANCOVA.
m4_source <- lm(general_distress_change ~ general_distress_baseline + app_access, data = music)
show_model(m4_source)
print(music %>% group_by(app_access) %>% summarise(
  n = n(), baseline_distress = mean(general_distress_baseline),
  month1_distress = mean(general_distress_month1), mean_change = mean(general_distress_change), .groups = "drop"))

models <- list(focal_only = m1_music, adjusted = m2_music, interaction = m3_music,
  source_design_change = m4_source)
audit <- imap_dfr(models, function(m, model_name) {
  as.data.frame(coef(summary(m))) %>% rownames_to_column("term") %>%
    transmute(topic = 31, model = model_name, term, estimate = Estimate,
      std_error = `Std. Error`, p_value = `Pr(>|t|)`)
})
normalize_term <- function(term) {
  term <- if_else(term %in% c("Intercept", "(Intercept)"), "Intercept", term)
  term <- str_remove_all(term, "\\[|\\]")
  map_chr(str_split(term, ":"), ~ paste(sort(.x), collapse = ":"))
}
# Python companion baseline is centered; align its intercept to the R model.
reference <- read_csv(here("qa", "topic31_regression_audit.csv"), show_col_types = FALSE)
source_baseline_b <- reference %>% filter(model == "source_design_change", term == "general_distress_baseline") %>% pull(estimate)
reference <- reference %>% mutate(estimate = if_else(model == "source_design_change" & term == "Intercept",
  estimate - source_baseline_b * mean(music$general_distress_baseline), estimate))
comparison <- audit %>% mutate(key = normalize_term(term)) %>%
  inner_join(reference %>% mutate(key = normalize_term(term)), by = c("topic", "model", "key"), suffix = c("_R", "_reference"))
# Centering changes only the companion intercept and its SE/p-value.
comparison_slopes <- comparison %>% filter(!(model == "source_design_change" & key == "Intercept"))
stopifnot(nrow(comparison) == nrow(audit),
  max(abs(comparison$estimate_R - comparison$estimate_reference)) < 1e-6,
  max(abs(comparison_slopes$std_error_R - comparison_slopes$std_error_reference)) < 1e-6,
  max(abs(comparison_slopes$p_value_R - comparison_slopes$p_value_reference)) < 1e-6)
write_csv(audit, here("qa", "topic31_R_regression_audit.csv"))

covariates <- c("age", "baseline_depression", "baseline_anxiety", "sleep_hours", "sleep_quality", "academic_stress", "social_support", "financial_strain", "work_hours", "physical_activity_minutes", "screen_hours", "rumination", "healthy_music_use", "unhealthy_music_use", "emotion_regulation_skill", "social_connection", "music_importance", "emotion_awareness", "music_training_years", "enrollment_units", "gender", "race_ethnicity", "first_generation", "app_access", "music_genre", "time_of_day", "cultural_music_context", "current_counseling", "instrument_group", "living_arrangement")
block_tests <- function(m, model_name) {
  mm <- model.matrix(m); labels <- attr(terms(m), "term.labels")
  map_dfr(covariates, function(v) {
    ix <- which(attr(mm, "assign") == match(v, labels))
    b <- coef(m)[ix]; V <- vcov(m)[ix, ix, drop = FALSE]
    f <- drop(t(b) %*% solve(V, b)) / length(ix)
    pv <- pf(f, length(ix), df.residual(m), lower.tail = FALSE)
    tibble(topic = 31, model = model_name, covariate = v, p_value = pv,
      significant = pv < .05, near_threshold = pv > .05 & pv <= .10)
  })
}
covariate_tests <- bind_rows(block_tests(m2_music, "adjusted"), block_tests(m3_music, "interaction"))
counts <- covariate_tests %>% group_by(model) %>% summarise(
  covariates = n(), significant_covariates = sum(significant),
  null_near_fraction = mean(near_threshold[!significant]), .groups = "drop")
print(counts)
stopifnot(all(counts$covariates == 30), all(counts$significant_covariates >= 5), all(counts$null_near_fraction >= .70))
write_csv(covariate_tests, here("qa", "topic31_R_covariate_block_tests.csv"))

directions <- read_csv(here("qa", "topic31_covariate_directions.csv"), show_col_types = FALSE)
direction_check <- audit %>% mutate(key = normalize_term(term)) %>%
  inner_join(directions %>% mutate(key = normalize_term(term)), by = c("topic", "model", "key"), suffix = c("_R", "_reference")) %>%
  mutate(direction_pass = expected == 0 | sign(estimate_R) == expected)
stopifnot(nrow(direction_check) == nrow(directions), all(direction_check$direction_pass))
write_csv(direction_check, here("qa", "topic31_R_direction_checks.csv"))

# CONDITIONAL EFFECTS AT THE OBSERVED ENDPOINTS AND 10/50/90TH PERCENTILES ----
conditional <- function(moderator, effect) {
  map_dfr(quantile(music[[moderator]], c(0, .1, .5, .9, 1)), function(value) {
    cc <- setNames(rep(0, length(coef(m3_music))), names(coef(m3_music)))
    cc[effect] <- 1
    interaction_name <- names(cc)[str_detect(names(cc), ":")]
    cc[interaction_name] <- value - mean(music[[moderator]])
    estimate <- sum(cc * coef(m3_music)); se <- sqrt(drop(t(cc) %*% vcov(m3_music) %*% cc))
    tibble(moderator, moderator_value = value, effect, estimate, std_error = se,
      p_value = 2 * pt(abs(estimate / se), df.residual(m3_music), lower.tail = FALSE))
  })
}
effects <- bind_rows(conditional("unhealthy_music_use", "music_listening_duration"),
  conditional("music_listening_duration", "unhealthy_music_use"))
print(effects)
stopifnot(all(effects$estimate[effects$effect == "unhealthy_music_use"] > 0))
write_csv(effects, here("qa", "topic31_R_conditional_effects.csv"))

influence <- map_dfr(covariates, function(v) {
  reduced <- lm(reformulate(c("music_listening_duration", setdiff(covariates, v)), "depression_and_anxiety"), data = music)
  tibble(covariate = v, full_estimate = coef(m2_music)[["music_listening_duration"]],
    without_covariate = coef(reduced)[["music_listening_duration"]], change_on_inclusion = full_estimate - without_covariate)
})
write_csv(influence, here("qa", "topic31_R_focal_covariate_influence.csv"))

# STRUCTURE AND SCALE CHECKS ----
stopifnot(all(table(music$app_access) == 200),
  max(abs(music$depression_and_anxiety - rowMeans(music[, c("depression_score", "anxiety_score")]))) < 1.1e-6,
  max(abs(music$symptom_change - (music$depression_and_anxiety - rowMeans(music[, c("baseline_depression", "baseline_anxiety")])))) < 1.6e-6,
  max(abs(music$general_distress_change - (music$general_distress_month1 - music$general_distress_baseline))) < 1.6e-6,
  all(music$depression_score >= 0 & music$depression_score <= 24),
  all(music$anxiety_score >= 0 & music$anxiety_score <= 24),
  all(music$general_distress_month1 >= 10 & music$general_distress_month1 <= 50),
  coef(summary(m4_source))["app_accessimmediate", "Pr(>|t|)"] > .5)
pdf(here("qa", "topic31_R_diagnostics.pdf"), width = 9, height = 7)
par(mfrow = c(2, 2)); plot(m3_music, which = c(1, 2, 3, 5))
dev.off()
capture.output(sessionInfo(), file = here("qa", "topic31_R_session_info.txt"))
cat("Saved-data coefficients, uncertainty, directions and structure passed.\n")
