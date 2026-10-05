# PSY150 - Topics 16-20 | instructor model checks
# Open DATA as an RStudio project. CSV files belong in data/; audits belong in qa/.
# install.packages(c("tidyverse", "here", "jtools"))
library(tidyverse)
library(here)
library(jtools)

# These are calibrated SYNTHETIC teaching datasets, not empirical study results.
# Main effects and conventional OLS standard errors are on their displayed scales.
# All numerical audits were independently computed from the saved CSVs in Python.
# R was unavailable in the authoring environment: this script has not been executed there.
# The updated SIM_SPECS supersede old all-coefficient p-value bounds.
# No promise is made about p-values in arbitrary alternative models.
dir.create(here("qa"), showWarnings = FALSE, recursive = TRUE)

files <- c(
  exercise = "42053_oroarty_exercise_mental_health_F26.csv",
  caffeine = "42053_burnett_caffeine_anxiety_F26.csv",
  diet = "42053_lynch_carnivore_mood_F26.csv",
  athlete = "42053_ferries_athlete_stress_F26.csv",
  work = "../qa/archive_bradley_batch16_20.csv"
)

datasets <- map(files, function(file_name) {
  raw_data <- read_csv(here("data", file_name), show_col_types = FALSE)
  stopifnot(nrow(raw_data) >= 300, !anyNA(raw_data),
    !anyDuplicated(raw_data$participant_id))
  raw_data
})

show_model <- function(model) {
  print(summ(model, digits = 3))
  # Retain small p-values that may round to 0.000 in three-decimal output.
  print(as.data.frame(coef(summary(model))) %>%
    rownames_to_column("term") %>%
    transmute(term, p_value = format.pval(`Pr(>|t|)`, digits = 4, eps = 1e-6)))
}

exercise <- datasets$exercise %>%
  mutate(
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    housing = factor(housing, levels = c("shared", "living_alone")),
    relationship_status = factor(relationship_status, levels = c("not_partnered", "partnered")),
    dependent_children = factor(dependent_children, levels = c("no", "yes")),
    smoking = factor(smoking, levels = c("no", "yes")),
    chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
    mental_health_history = factor(mental_health_history, levels = c("no", "yes")),
    current_counseling = factor(current_counseling, levels = c("no", "yes")),
    medication_use = factor(medication_use, levels = c("no", "yes")),
    recruitment_channel = factor(recruitment_channel, levels = c("community_notice", "online_notice"))
  )

caffeine <- datasets$caffeine %>%
  mutate(
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    housing = factor(housing, levels = c("shared", "living_alone")),
    relationship_status = factor(relationship_status, levels = c("not_partnered", "partnered")),
    dependent_children = factor(dependent_children, levels = c("no", "yes")),
    smoking = factor(smoking, levels = c("no", "yes")),
    chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
    mental_health_history = factor(mental_health_history, levels = c("no", "yes")),
    current_counseling = factor(current_counseling, levels = c("no", "yes")),
    medication_use = factor(medication_use, levels = c("no", "yes")),
    recruitment_channel = factor(recruitment_channel, levels = c("community_notice", "online_notice"))
  )

diet <- datasets$diet %>%
  mutate(
    diet_type = factor(diet_type, levels = c("standard_western", "carnivore")),
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    housing = factor(housing, levels = c("shared", "living_alone")),
    relationship_status = factor(relationship_status, levels = c("not_partnered", "partnered")),
    dependent_children = factor(dependent_children, levels = c("no", "yes")),
    smoking = factor(smoking, levels = c("no", "yes")),
    chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
    mental_health_history = factor(mental_health_history, levels = c("no", "yes")),
    current_counseling = factor(current_counseling, levels = c("no", "yes")),
    medication_use = factor(medication_use, levels = c("no", "yes")),
    recruitment_channel = factor(recruitment_channel, levels = c("community_notice", "online_notice"))
  )

athlete <- datasets$athlete %>%
  mutate(
    college_athlete_status = factor(college_athlete_status, levels = c("non_athlete", "athlete")),
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    housing = factor(housing, levels = c("shared", "living_alone")),
    relationship_status = factor(relationship_status, levels = c("not_partnered", "partnered")),
    dependent_children = factor(dependent_children, levels = c("no", "yes")),
    smoking = factor(smoking, levels = c("no", "yes")),
    chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
    mental_health_history = factor(mental_health_history, levels = c("no", "yes")),
    current_counseling = factor(current_counseling, levels = c("no", "yes")),
    medication_use = factor(medication_use, levels = c("no", "yes")),
    recruitment_channel = factor(recruitment_channel, levels = c("community_notice", "online_notice"))
  )

work <- datasets$work %>%
  mutate(
    part_time_work = factor(part_time_work, levels = c("not_working", "part_time")),
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    housing = factor(housing, levels = c("shared", "living_alone")),
    relationship_status = factor(relationship_status, levels = c("not_partnered", "partnered")),
    dependent_children = factor(dependent_children, levels = c("no", "yes")),
    smoking = factor(smoking, levels = c("no", "yes")),
    chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
    mental_health_history = factor(mental_health_history, levels = c("no", "yes")),
    current_counseling = factor(current_counseling, levels = c("no", "yes")),
    medication_use = factor(medication_use, levels = c("no", "yes")),
    reason_for_working = factor(reason_for_working, levels = c("experience_or_discretionary", "essential_expenses"))
  )


# 16. WEEKLY EXERCISE AND ANXIETY/DEPRESSION ----
# Priority source is a clinical letter, not an individual participant study or regression table. Negative exercise-distress direction is supported; N, score metric, partial coefficients and interaction magnitude are teaching assumptions. Positive daily-demands slopes, negative support slopes and a negative exercise slope are retained over observed ranges.

m1_exercise <- lm(anxiety_and_depression ~ weekly_exercise_time, data = exercise)
show_model(m1_exercise)

m2_exercise <- lm(
  anxiety_and_depression ~ weekly_exercise_time +
    age +
    sleep_hours +
    sleep_quality +
    social_support +
    financial_strain +
    daily_demands +
    chronic_pain +
    screen_hours +
    alcohol_days +
    loneliness +
    coping_confidence +
    time_management +
    commute_minutes +
    leisure_hours +
    physical_health +
    housing_stability +
    family_conflict +
    community_connection +
    household_size +
    exercise_self_efficacy +
    gender +
    housing +
    relationship_status +
    dependent_children +
    smoking +
    chronic_condition +
    mental_health_history +
    current_counseling +
    medication_use +
    recruitment_channel,
  data = exercise
)
show_model(m2_exercise)

exercise_centered <- exercise %>%
  mutate(
    social_support = social_support - mean(social_support),
    daily_demands = daily_demands - mean(daily_demands)
  )

m3_exercise <- lm(
  anxiety_and_depression ~ weekly_exercise_time +
    age +
    sleep_hours +
    sleep_quality +
    social_support +
    financial_strain +
    daily_demands +
    chronic_pain +
    screen_hours +
    alcohol_days +
    loneliness +
    coping_confidence +
    time_management +
    commute_minutes +
    leisure_hours +
    physical_health +
    housing_stability +
    family_conflict +
    community_connection +
    household_size +
    exercise_self_efficacy +
    gender +
    housing +
    relationship_status +
    dependent_children +
    smoking +
    chronic_condition +
    mental_health_history +
    current_counseling +
    medication_use +
    recruitment_channel +
    social_support:daily_demands,
  data = exercise_centered
)
show_model(m3_exercise)


# 17. DAILY CAFFEINE CONSUMPTION AND ANXIETY ----
# Priority N=520. Table 4 anxiety Kruskal-Wallis p=.557; the conclusion misorders anxiety/stress p-values, so the labeled anxiety table is authoritative. A weak positive partial slope with interaction-model p approximately .557 preserves the null conclusion; this is not a numerical replication of the rank test. Continuous doubled DASS-inspired score and sleep-by-academic-demand interaction are teaching adaptations.

m1_caffeine <- lm(anxiety ~ daily_caffeine_consumption, data = caffeine)
show_model(m1_caffeine)

m2_caffeine <- lm(
  anxiety ~ daily_caffeine_consumption +
    age +
    sleep_hours +
    sleep_quality +
    social_support +
    financial_strain +
    academic_stressors +
    chronic_pain +
    late_caffeine_days +
    alcohol_days +
    loneliness +
    coping_confidence +
    time_management +
    commute_minutes +
    leisure_hours +
    physical_health +
    housing_stability +
    family_conflict +
    community_connection +
    household_size +
    weekly_exercise_minutes +
    gender +
    housing +
    relationship_status +
    dependent_children +
    smoking +
    chronic_condition +
    mental_health_history +
    current_counseling +
    medication_use +
    recruitment_channel,
  data = caffeine
)
show_model(m2_caffeine)

caffeine_centered <- caffeine %>%
  mutate(
    sleep_quality = sleep_quality - mean(sleep_quality),
    academic_stressors = academic_stressors - mean(academic_stressors)
  )

m3_caffeine <- lm(
  anxiety ~ daily_caffeine_consumption +
    age +
    sleep_hours +
    sleep_quality +
    social_support +
    financial_strain +
    academic_stressors +
    chronic_pain +
    late_caffeine_days +
    alcohol_days +
    loneliness +
    coping_confidence +
    time_management +
    commute_minutes +
    leisure_hours +
    physical_health +
    housing_stability +
    family_conflict +
    community_connection +
    household_size +
    weekly_exercise_minutes +
    gender +
    housing +
    relationship_status +
    dependent_children +
    smoking +
    chronic_condition +
    mental_health_history +
    current_counseling +
    medication_use +
    recruitment_channel +
    sleep_quality:academic_stressors,
  data = caffeine_centered
)
show_model(m3_caffeine)


# 18. DIET PATTERN AND POSITIVE MOOD ----
# Uncontrolled priority survey N=2029, median age44,67%male; perceived wellbeing improved in66-91%. This does not identify a between-diet effect. Our N=2000 includes a newly invented Western comparison group and balanced recruitment, not the source population. Diet contrast is deliberately small/nonsignificant at average expectancy. Expectancy moderation is a selection/reporting hypothesis, not an empirically demonstrated carnivore treatment effect.

m1_diet <- lm(mood ~ diet_type, data = diet)
show_model(m1_diet)

m2_diet <- lm(
  mood ~ diet_type +
    age +
    sleep_hours +
    sleep_quality +
    social_support +
    financial_strain +
    daily_demands +
    chronic_pain +
    diet_expectancy +
    alcohol_days +
    loneliness +
    coping_confidence +
    time_management +
    commute_minutes +
    leisure_hours +
    physical_health +
    housing_stability +
    family_conflict +
    community_connection +
    household_size +
    diet_duration_months +
    gender +
    housing +
    relationship_status +
    dependent_children +
    smoking +
    chronic_condition +
    mental_health_history +
    current_counseling +
    medication_use +
    recruitment_channel,
  data = diet
)
show_model(m2_diet)

diet_centered <- diet %>%
  mutate(
    diet_expectancy = diet_expectancy - mean(diet_expectancy)
  )

m3_diet <- lm(
  mood ~ diet_type +
    age +
    sleep_hours +
    sleep_quality +
    social_support +
    financial_strain +
    daily_demands +
    chronic_pain +
    diet_expectancy +
    alcohol_days +
    loneliness +
    coping_confidence +
    time_management +
    commute_minutes +
    leisure_hours +
    physical_health +
    housing_stability +
    family_conflict +
    community_connection +
    household_size +
    diet_duration_months +
    gender +
    housing +
    relationship_status +
    dependent_children +
    smoking +
    chronic_condition +
    mental_health_history +
    current_counseling +
    medication_use +
    recruitment_channel +
    diet_expectancy:diet_type,
  data = diet_centered
)
show_model(m3_diet)


# 19. COLLEGE-ATHLETE STATUS AND STRESS ----
# Priority source reports item-specific t tests, e.g. extracurricular demands t(359)=8.81, financial burdens t(357)=3.27 favoring the opposite group. It does not identify the partial coefficient for our overall 0-40 inventory. A modest positive status contrast is a teaching synthesis, not a replicated total-score finding. Support buffering is a literature-motivated extension. Sport burden has structural zeros; conditioning on zero burden extrapolates outside the athlete observations.

m1_athlete <- lm(stress_level ~ college_athlete_status, data = athlete)
show_model(m1_athlete)

m2_athlete <- lm(
  stress_level ~ college_athlete_status +
    age +
    sleep_hours +
    sleep_quality +
    team_community_support +
    financial_strain +
    academic_stressors +
    chronic_pain +
    sport_stressors +
    alcohol_days +
    loneliness +
    coping_confidence +
    time_management +
    commute_minutes +
    leisure_hours +
    physical_health +
    housing_stability +
    family_conflict +
    community_connection +
    household_size +
    exercise_amount +
    gender +
    housing +
    relationship_status +
    dependent_children +
    smoking +
    chronic_condition +
    mental_health_history +
    current_counseling +
    medication_use +
    recruitment_channel,
  data = athlete
)
show_model(m2_athlete)

athlete_centered <- athlete %>%
  mutate(
    team_community_support = team_community_support - mean(team_community_support)
  )

m3_athlete <- lm(
  stress_level ~ college_athlete_status +
    age +
    sleep_hours +
    sleep_quality +
    team_community_support +
    financial_strain +
    academic_stressors +
    chronic_pain +
    sport_stressors +
    alcohol_days +
    loneliness +
    coping_confidence +
    time_management +
    commute_minutes +
    leisure_hours +
    physical_health +
    housing_stability +
    family_conflict +
    community_connection +
    household_size +
    exercise_amount +
    gender +
    housing +
    relationship_status +
    dependent_children +
    smoking +
    chronic_condition +
    mental_health_history +
    current_counseling +
    medication_use +
    recruitment_channel +
    team_community_support:college_athlete_status,
  data = athlete_centered
)
show_model(m3_athlete)


# 20. PART-TIME WORK AND ACADEMIC STRESS ----
# Supplied Mills source is four qualitative interviews with >=30h/week workers, not a part-time comparison or quantitative regression. Only publisher abstract was accessible. All numerical status/hour effects here are teaching assumptions. Peltz et al. motivate financial-strain moderation, but their sleep/depression model is not identical to our academic-stress model. Hours are zero in nonworkers; conditional status intercept extrapolates to zero hours for workers. Actual/anticipated reason avoids exact aliasing between employment status and a not-applicable dummy.

m1_work <- lm(academic_stress ~ part_time_work, data = work)
show_model(m1_work)

m2_work <- lm(
  academic_stress ~ part_time_work +
    age +
    sleep_hours +
    sleep_quality +
    social_support +
    financial_strain +
    daily_demands +
    chronic_pain +
    weekly_work_hours +
    alcohol_days +
    loneliness +
    coping_confidence +
    time_management +
    commute_minutes +
    leisure_hours +
    physical_health +
    housing_stability +
    family_conflict +
    community_connection +
    household_size +
    college_year +
    gender +
    housing +
    relationship_status +
    dependent_children +
    smoking +
    chronic_condition +
    mental_health_history +
    current_counseling +
    medication_use +
    reason_for_working,
  data = work
)
show_model(m2_work)

work_centered <- work %>%
  mutate(
    financial_strain = financial_strain - mean(financial_strain),
    weekly_work_hours = weekly_work_hours - mean(weekly_work_hours)
  )

m3_work <- lm(
  academic_stress ~ part_time_work +
    age +
    sleep_hours +
    sleep_quality +
    social_support +
    financial_strain +
    daily_demands +
    chronic_pain +
    weekly_work_hours +
    alcohol_days +
    loneliness +
    coping_confidence +
    time_management +
    commute_minutes +
    leisure_hours +
    physical_health +
    housing_stability +
    family_conflict +
    community_connection +
    household_size +
    college_year +
    gender +
    housing +
    relationship_status +
    dependent_children +
    smoking +
    chronic_condition +
    mental_health_history +
    current_counseling +
    medication_use +
    reason_for_working +
    financial_strain:weekly_work_hours,
  data = work_centered
)
show_model(m3_work)


# AUDIT ALL SAVED-DATA MODELS ----
tidy_lm <- function(model, topic, model_name) {
  sm <- coef(summary(model))
  as.data.frame(sm) %>% rownames_to_column("term") %>%
    transmute(topic = topic, model = model_name, term,
      estimate = Estimate, std_error = `Std. Error`, p_value = `Pr(>|t|)`)
}

models <- list(
  topic16 = list(focal_only = m1_exercise, adjusted = m2_exercise, interaction = m3_exercise),
  topic17 = list(focal_only = m1_caffeine, adjusted = m2_caffeine, interaction = m3_caffeine),
  topic18 = list(focal_only = m1_diet, adjusted = m2_diet, interaction = m3_diet),
  topic19 = list(focal_only = m1_athlete, adjusted = m2_athlete, interaction = m3_athlete),
  topic20 = list(focal_only = m1_work, adjusted = m2_work, interaction = m3_work)
)

audit <- imap_dfr(models, function(topic_models, topic_name) {
  imap_dfr(topic_models, function(model, model_name) {
    tidy_lm(model, as.integer(str_remove(topic_name, "topic")), model_name)
  })
})
write_csv(audit, here("qa", "topics16_20_R_regression_audit.csv"))

# Match coefficient names between Python treatment coding and R factor coding.
normalize_term <- function(term) {
  term <- ifelse(term %in% c("Intercept", "(Intercept)"), "Intercept", term)
  term <- str_remove_all(term, "\\[|\\]")
  map_chr(str_split(term, ":"), ~ paste(sort(.x), collapse = ":"))
}
expected <- read_csv(here("qa", "topics16_20_regression_audit.csv"), show_col_types = FALSE)
comparison <- audit %>% mutate(key = normalize_term(term)) %>%
  inner_join(expected %>% mutate(key = normalize_term(term)),
    by = c("topic", "model", "key"), suffix = c("_R", "_reference"))
stopifnot(nrow(comparison) == nrow(audit),
  max(abs(comparison$estimate_R - comparison$estimate_reference)) < 1e-6,
  max(abs(comparison$std_error_R - comparison$std_error_reference)) < 1e-6,
  max(abs(comparison$p_value_R - comparison$p_value_reference)) < 1e-6)

direction_reference <- read_csv(here("qa", "topics16_20_covariate_directions.csv"), show_col_types = FALSE)
direction_check <- audit %>% filter(model == "interaction") %>%
  mutate(key = normalize_term(term)) %>%
  inner_join(direction_reference %>% mutate(key = normalize_term(term)),
    by = c("topic", "key"), suffix = c("_R", "_reference")) %>%
  mutate(direction_pass = case_when(
    expected_direction == "positive" ~ estimate_R > 0,
    expected_direction == "negative" ~ estimate_R < 0,
    TRUE ~ TRUE))
stopifnot(all(direction_check$direction_pass))
write_csv(direction_check, here("qa", "topics16_20_R_direction_check.csv"))

# Block tests count each categorical covariate once. In the interaction model,
# a main-effect block is tested at centered values of its interacting variable.
block_tests <- function(model, covariates, topic, model_name) {
  mm <- model.matrix(model)
  labels <- attr(terms(model), "term.labels")
  map_dfr(covariates, function(v) {
    ix <- which(attr(mm, "assign") == match(v, labels))
    b <- coef(model)[ix]
    V <- vcov(model)[ix, ix, drop = FALSE]
    statistic <- drop(t(b) %*% solve(V, b)) / length(ix)
    p <- pf(statistic, length(ix), df.residual(model), lower.tail = FALSE)
    tibble(topic, model = model_name, covariate = v, p_value = p,
      significant = p < .05, near_threshold = p > .05 & p <= .10)
  })
}
covariate_tests <- imap_dfr(models, function(topic_models, topic_name) {
  covs <- setdiff(attr(terms(topic_models$adjusted), "term.labels"),
    attr(terms(topic_models$focal_only), "term.labels"))
  map_dfr(c("adjusted", "interaction"), function(m) {
    block_tests(topic_models[[m]], covs, topic_name, m)
  })
})
covariate_counts <- covariate_tests %>% group_by(topic, model) %>%
  summarise(covariates = n(), significant_covariates = sum(significant),
    null_near_fraction = mean(near_threshold[!significant]), .groups = "drop")
print(covariate_counts)
stopifnot(all(covariate_counts$covariates == 30),
  all(covariate_counts$significant_covariates >= 5),
  all(covariate_counts$null_near_fraction >= .70))
write_csv(covariate_tests, here("qa", "topics16_20_R_covariate_block_tests.csv"))

# Conditional effects at low, typical, and high moderator values.
simple_slopes <- function(model, effect, moderator, raw_moderator, topic) {
  nm <- names(coef(model))
  int_name <- nm[str_detect(nm, fixed(effect)) & str_detect(nm, fixed(moderator)) & str_detect(nm, ":")]
  stopifnot(length(int_name) == 1)
  values <- quantile(raw_moderator, c(.1, .5, .9))
  map_dfr(seq_along(values), function(i) {
    delta <- unname(values[i]) - mean(raw_moderator)
    contrast <- setNames(rep(0, length(nm)), nm)
    contrast[effect] <- 1
    contrast[int_name] <- delta
    estimate <- sum(contrast * coef(model))
    se <- sqrt(drop(t(contrast) %*% vcov(model) %*% contrast))
    tibble(topic, effect, moderator, percentile = c(10, 50, 90)[i],
      moderator_value = unname(values[i]), estimate, std_error = se,
      p_value = 2 * pt(abs(estimate / se), df.residual(model), lower.tail = FALSE))
  })
}

conditional_effects <- bind_rows(
  simple_slopes(m3_exercise, "daily_demands", "social_support", exercise$social_support, 16),
  simple_slopes(m3_caffeine, "academic_stressors", "sleep_quality", caffeine$sleep_quality, 17),
  simple_slopes(m3_diet, "diet_typecarnivore", "diet_expectancy", diet$diet_expectancy, 18),
  simple_slopes(m3_athlete, "college_athlete_statusathlete", "team_community_support", athlete$team_community_support, 19),
  simple_slopes(m3_work, "weekly_work_hours", "financial_strain", work$financial_strain, 20)
)
print(conditional_effects)
write_csv(conditional_effects, here("qa", "topics16_20_R_conditional_effects.csv"))


# Adjusted residual and quantile plots; assess OLS assumptions and unusual records.
pdf(here("qa", "topics16_20_R_diagnostics.pdf"), width = 9, height = 7)
for (topic_name in names(models)) {
  par(mfrow = c(2, 2))
  plot(models[[topic_name]]$interaction, which = c(1, 2, 3, 5), main = topic_name)
}
dev.off()
cat("All saved-data regression and direction checks passed.\n")
capture.output(sessionInfo(), file = here("qa", "topics16_20_R_session_info.txt"))
