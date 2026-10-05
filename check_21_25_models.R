# PSY150 - Topics 21-25 | instructor model checks
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
  work = "42053_bradley_work_academic_stress_F26.csv",
  music = "42053_altayyeb_music_academic_performance_F26.csv",
  body = "42053_mendoza_social_media_body_image_F26.csv",
  nature = "42053_kaiman_nature_anxiety_F26.csv",
  creativity = "42053_theios_creativity_wellbeing_F26.csv"
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

work <- datasets$work %>%
  mutate(
    part_time_work = factor(part_time_work, levels = c("not_working", "part_time")),
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "black", "asian", "multiracial_or_other")),
    first_generation = factor(first_generation, levels = c("no", "yes")),
    family_income_bracket = factor(family_income_bracket, levels = c("low", "middle", "high")),
    pell_grant = factor(pell_grant, levels = c("no", "yes")),
    dependent_care = factor(dependent_care, levels = c("no", "yes")),
    institution_control = factor(institution_control, levels = c("public", "private_nonprofit", "private_for_profit")),
    institution_selectivity = factor(institution_selectivity, levels = c("open_access", "minimally_selective", "moderately_selective", "highly_selective")),
    major_group = factor(major_group, levels = c("stem", "business", "health", "social_science", "arts_humanities", "other")),
    reason_for_working = factor(reason_for_working, levels = c("experience_or_discretionary", "essential_expenses"))
  )

music <- datasets$music %>%
  mutate(
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    college_year = factor(college_year, levels = c("first", "second", "third", "fourth_plus")),
    major_group = factor(major_group, levels = c("stem", "arts_humanities", "social_science", "other")),
    task_type = factor(task_type, levels = c("reading", "writing", "memorizing", "critical_thinking")),
    preferred_music = factor(preferred_music, levels = c("instrumental", "vocal")),
    headphone_preference = factor(headphone_preference, levels = c("no", "yes")),
    hearing_difficulty = factor(hearing_difficulty, levels = c("no", "yes")),
    study_location = factor(study_location, levels = c("home", "campus")),
    session_time = factor(session_time, levels = c("morning", "afternoon_evening")),
    first_generation = factor(first_generation, levels = c("no", "yes"))
  )

body <- datasets$body %>%
  mutate(
    gender = factor(gender, levels = c("girl", "boy", "nonbinary_or_self_described")),
    school_setting = factor(school_setting, levels = c("urban", "suburban", "rural")),
    organized_sport = factor(organized_sport, levels = c("no", "yes")),
    arts_club = factor(arts_club, levels = c("no", "yes")),
    prior_media_literacy = factor(prior_media_literacy, levels = c("no", "yes")),
    private_account = factor(private_account, levels = c("no", "yes")),
    main_platform = factor(main_platform, levels = c("image_video", "messaging_mixed")),
    caregiver_screen_rules = factor(caregiver_screen_rules, levels = c("no", "yes")),
    shared_bedroom = factor(shared_bedroom, levels = c("no", "yes")),
    recent_school_transition = factor(recent_school_transition, levels = c("no", "yes"))
  )

nature <- datasets$nature %>%
  mutate(
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "black", "asian", "multiracial_or_other")),
    college_year = factor(college_year, levels = c("first", "second", "third", "fourth_plus")),
    food_insecurity = factor(food_insecurity, levels = c("no", "yes")),
    in_person_classes = factor(in_person_classes, levels = c("no", "yes")),
    housing = factor(housing, levels = c("on_campus", "off_campus")),
    chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
    mental_health_history = factor(mental_health_history, levels = c("no", "yes")),
    current_counseling = factor(current_counseling, levels = c("no", "yes")),
    recruitment_channel = factor(recruitment_channel, levels = c("course_notice", "campus_notice"))
  )

creativity <- datasets$creativity %>%
  mutate(
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    creative_domain = factor(creative_domain, levels = c("visual_art", "writing", "music", "crafts_mixed")),
    employment_status = factor(employment_status, levels = c("not_employed", "employed", "retired")),
    relationship_status = factor(relationship_status, levels = c("not_partnered", "partnered")),
    dependent_care = factor(dependent_care, levels = c("no", "yes")),
    creative_group = factor(creative_group, levels = c("no", "yes")),
    formal_arts_training = factor(formal_arts_training, levels = c("no", "yes")),
    chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
    current_counseling = factor(current_counseling, levels = c("no", "yes")),
    housing = factor(housing, levels = c("shared", "living_alone"))
  )


# 21. OFF-CAMPUS WORK, ACADEMIC STRESS AND STUDENT PROGRESS ----
# Expands Summer topic2 (600rows,73columns) structurally, using its observed predictor distributions and seven item/index domains; all Fall records are newly generated. It is not a row append to the original. Includes nonworkers, restricts workers to1-29h, adds academic stress, college year and reasons. Logan Table3: >20h off campus GPA b=-.246,t=-2.304; lower-year high-work contrast -.552 versus upper-year .010 in a separate model. The secondary cumulative-GPA teaching model uses approximately -.55 lower-year/.01 upper-year contrasts. Academic-stress coefficients are explicit extensions because Logan measured GPA and Bui measured persistence. Their empirical effects are not claimed for the stress scale.

m1_work <- lm(academic_stress ~ part_time_work, data = work)
show_model(m1_work)

m2_work <- lm(
  academic_stress ~ part_time_work +
    age +
    weekly_work_hours +
    college_year +
    commute_minutes +
    tuition_fees_thousand +
    hs_gpa +
    english_proficiency +
    enrollment_units +
    study_hours_per_week +
    sleep_hours +
    financial_strain_index +
    academic_engagement_index +
    institution_support_index +
    self_efficacy_index +
    work_academic_conflict_index +
    work_career_value_index +
    mental_strain_index +
    schedule_variability +
    schedule_control +
    social_support +
    gender +
    race_ethnicity +
    first_generation +
    family_income_bracket +
    pell_grant +
    dependent_care +
    institution_control +
    institution_selectivity +
    major_group +
    reason_for_working,
  data = work
)
show_model(m2_work)

work_centered <- work %>%
  mutate(
    college_year = college_year - mean(college_year),
    weekly_work_hours = weekly_work_hours - mean(weekly_work_hours)
  )

m3_work <- lm(
  academic_stress ~ part_time_work +
    age +
    weekly_work_hours +
    college_year +
    commute_minutes +
    tuition_fees_thousand +
    hs_gpa +
    english_proficiency +
    enrollment_units +
    study_hours_per_week +
    sleep_hours +
    financial_strain_index +
    academic_engagement_index +
    institution_support_index +
    self_efficacy_index +
    work_academic_conflict_index +
    work_career_value_index +
    mental_strain_index +
    schedule_variability +
    schedule_control +
    social_support +
    gender +
    race_ethnicity +
    first_generation +
    family_income_bracket +
    pell_grant +
    dependent_care +
    institution_control +
    institution_selectivity +
    major_group +
    reason_for_working +
    college_year:weekly_work_hours,
  data = work_centered
)
show_model(m3_work)


# 22. MUSIC DURING STUDY AND CONCENTRATION/PERFORMANCE ----
# Priority paper is a self-selected habits survey (N140,age17-75); it does not supply a causal concentration/GPA effect. N300 college-student scenario adds a new task/composite outcome. Weak negative main association and a more negative association on harder tasks reflect the supplemental review direction, not a replicated numerical coefficient. Younger age and easier tasks are associated with more music exposure in generated predictors.

m1_music <- lm(concentration_and_academic_performance ~ music_while_studying, data = music)
show_model(m1_music)

m2_music <- lm(
  concentration_and_academic_performance ~ music_while_studying +
    age +
    sleep_hours +
    sleep_quality +
    task_difficulty +
    working_memory +
    prior_gpa +
    academic_motivation +
    test_anxiety +
    extraversion +
    music_training_years +
    music_proficiency +
    noise_sensitivity +
    study_environment_noise +
    study_session_minutes +
    weekly_study_hours +
    multitasking_frequency +
    music_preference_strength +
    caffeine_use +
    fatigue +
    time_management +
    gender +
    college_year +
    major_group +
    task_type +
    preferred_music +
    headphone_preference +
    hearing_difficulty +
    study_location +
    session_time +
    first_generation,
  data = music
)
show_model(m2_music)

music_centered <- music %>%
  mutate(
    task_difficulty = task_difficulty - mean(task_difficulty),
    music_while_studying = music_while_studying - mean(music_while_studying)
  )

m3_music <- lm(
  concentration_and_academic_performance ~ music_while_studying +
    age +
    sleep_hours +
    sleep_quality +
    task_difficulty +
    working_memory +
    prior_gpa +
    academic_motivation +
    test_anxiety +
    extraversion +
    music_training_years +
    music_proficiency +
    noise_sensitivity +
    study_environment_noise +
    study_session_minutes +
    weekly_study_hours +
    multitasking_frequency +
    music_preference_strength +
    caffeine_use +
    fatigue +
    time_management +
    gender +
    college_year +
    major_group +
    task_type +
    preferred_music +
    headphone_preference +
    hearing_difficulty +
    study_location +
    session_time +
    first_generation +
    task_difficulty:music_while_studying,
  data = music_centered
)
show_model(m3_music)


# 23. SOCIAL MEDIA AND ADOLESCENT BODY SATISFACTION ----
# Priority is a narrative review, not a source with one sample or transferable partial effect. Negative exposure-to-satisfaction direction and media-literacy buffering are literature-motivated assumptions. Outcome is satisfaction, so appearance pressure has negative signs and acceptance/support have positive signs. The age13-18 restriction comes from TOPICS. This is not a clinical eating-disorder or BMI model.

m1_body <- lm(body_image ~ social_media_exposure, data = body)
show_model(m1_body)

m2_body <- lm(
  body_image ~ social_media_exposure +
    age +
    sleep_hours +
    sleep_quality +
    appearance_comparison +
    appearance_ideal_internalization +
    appearance_teasing +
    family_body_acceptance +
    peer_support +
    media_literacy +
    self_compassion +
    academic_stress +
    family_conflict +
    physical_activity_minutes +
    appearance_content_percent +
    photo_editing_frequency +
    body_function_appreciation +
    offline_belonging +
    financial_strain +
    caregiver_education_years +
    siblings +
    gender +
    school_setting +
    organized_sport +
    arts_club +
    prior_media_literacy +
    private_account +
    main_platform +
    caregiver_screen_rules +
    shared_bedroom +
    recent_school_transition,
  data = body
)
show_model(m2_body)

body_centered <- body %>%
  mutate(
    media_literacy = media_literacy - mean(media_literacy),
    social_media_exposure = social_media_exposure - mean(social_media_exposure)
  )

m3_body <- lm(
  body_image ~ social_media_exposure +
    age +
    sleep_hours +
    sleep_quality +
    appearance_comparison +
    appearance_ideal_internalization +
    appearance_teasing +
    family_body_acceptance +
    peer_support +
    media_literacy +
    self_compassion +
    academic_stress +
    family_conflict +
    physical_activity_minutes +
    appearance_content_percent +
    photo_editing_frequency +
    body_function_appreciation +
    offline_belonging +
    financial_strain +
    caregiver_education_years +
    siblings +
    gender +
    school_setting +
    organized_sport +
    arts_club +
    prior_media_literacy +
    private_account +
    main_platform +
    caregiver_screen_rules +
    shared_bedroom +
    recent_school_transition +
    media_literacy:social_media_exposure,
  data = body_centered
)
show_model(m3_body)


# 24. NATURE TIME, SCREEN EXPOSURE AND ANXIETY ----
# Priority anxiety: nature-only adjusted b=-.79,p=.083; moderator model nature b=-.71,p=.12,screen b=.30,p=.002,positive interaction b=.23,p=.005. Keep the nonsignificant nature main association and POSITIVE interaction; do not impose conventional negative buffering. Source nature measurement is described inconsistently as1-5frequency and days; our hours/week plus0-21 teaching anxiety scale cannot replicate its T-score coefficients. Expanded30-covariate model prioritizes p-values and source direction. Source food-insecurity negative coefficient is coding-dependent; our yes=food insecurity convention explicitly predicts greater distress.

m1_nature <- lm(anxiety ~ weekly_time_in_nature, data = nature)
show_model(m1_nature)

m2_nature <- lm(
  anxiety ~ weekly_time_in_nature +
    screen_time +
    weekly_exercise +
    sleep_hours +
    age +
    sleep_quality +
    academic_stress +
    social_support +
    financial_strain +
    loneliness +
    baseline_worry +
    neighborhood_safety +
    green_space_distance +
    study_hours +
    work_hours +
    time_management +
    commute_minutes +
    chronic_pain +
    mindfulness +
    nature_connectedness +
    caffeine_mg +
    gender +
    race_ethnicity +
    college_year +
    food_insecurity +
    in_person_classes +
    housing +
    chronic_condition +
    mental_health_history +
    current_counseling +
    recruitment_channel,
  data = nature
)
show_model(m2_nature)

nature_centered <- nature %>%
  mutate(
    weekly_time_in_nature = weekly_time_in_nature - mean(weekly_time_in_nature),
    screen_time = screen_time - mean(screen_time)
  )

m3_nature <- lm(
  anxiety ~ weekly_time_in_nature +
    screen_time +
    weekly_exercise +
    sleep_hours +
    age +
    sleep_quality +
    academic_stress +
    social_support +
    financial_strain +
    loneliness +
    baseline_worry +
    neighborhood_safety +
    green_space_distance +
    study_hours +
    work_hours +
    time_management +
    commute_minutes +
    chronic_pain +
    mindfulness +
    nature_connectedness +
    caffeine_mg +
    gender +
    race_ethnicity +
    college_year +
    food_insecurity +
    in_person_classes +
    housing +
    chronic_condition +
    mental_health_history +
    current_counseling +
    recruitment_channel +
    weekly_time_in_nature:screen_time,
  data = nature_centered
)
show_model(m3_nature)


# 25. EVERYDAY CREATIVE ACTIVITY AND MENTAL WELL-BEING ----
# Priority is a narrative review without a single transferable effect size. Small positive activity-wellbeing association is plausible; no causal therapy benefit is claimed. Supplemental Silvia/Kimbrel explains generally<3% of creativity variance from anxiety/depression, which does not imply that mental illness improves creativity. Conner et al. study658 young adults over13days; this single-row-per-adult dataset is a cross-sectional extension. Social-support by demands interaction is theory-grounded and numerically calibrated, not an estimate reported by the priority review.

m1_creativity <- lm(mental_wellbeing ~ creative_activity, data = creativity)
show_model(m1_creativity)

m2_creativity <- lm(
  mental_wellbeing ~ creative_activity +
    age +
    sleep_hours +
    sleep_quality +
    social_support +
    daily_demands +
    financial_strain +
    physical_health +
    loneliness +
    creative_self_efficacy +
    creative_autonomy +
    openness +
    emotional_regulation +
    cognitive_flexibility +
    leisure_hours +
    exercise_minutes +
    work_hours +
    education_years +
    performance_pressure +
    community_belonging +
    access_to_materials +
    gender +
    creative_domain +
    employment_status +
    relationship_status +
    dependent_care +
    creative_group +
    formal_arts_training +
    chronic_condition +
    current_counseling +
    housing,
  data = creativity
)
show_model(m2_creativity)

creativity_centered <- creativity %>%
  mutate(
    social_support = social_support - mean(social_support),
    daily_demands = daily_demands - mean(daily_demands)
  )

m3_creativity <- lm(
  mental_wellbeing ~ creative_activity +
    age +
    sleep_hours +
    sleep_quality +
    social_support +
    daily_demands +
    financial_strain +
    physical_health +
    loneliness +
    creative_self_efficacy +
    creative_autonomy +
    openness +
    emotional_regulation +
    cognitive_flexibility +
    leisure_hours +
    exercise_minutes +
    work_hours +
    education_years +
    performance_pressure +
    community_belonging +
    access_to_materials +
    gender +
    creative_domain +
    employment_status +
    relationship_status +
    dependent_care +
    creative_group +
    formal_arts_training +
    chronic_condition +
    current_counseling +
    housing +
    social_support:daily_demands,
  data = creativity_centered
)
show_model(m3_creativity)


# AUDIT ALL SAVED-DATA MODELS ----
tidy_lm <- function(model, topic, model_name) {
  sm <- coef(summary(model))
  as.data.frame(sm) %>% rownames_to_column("term") %>%
    transmute(topic = topic, model = model_name, term,
      estimate = Estimate, std_error = `Std. Error`, p_value = `Pr(>|t|)`)
}

models <- list(
  topic21 = list(focal_only = m1_work, adjusted = m2_work, interaction = m3_work),
  topic22 = list(focal_only = m1_music, adjusted = m2_music, interaction = m3_music),
  topic23 = list(focal_only = m1_body, adjusted = m2_body, interaction = m3_body),
  topic24 = list(focal_only = m1_nature, adjusted = m2_nature, interaction = m3_nature),
  topic25 = list(focal_only = m1_creativity, adjusted = m2_creativity, interaction = m3_creativity)
)

audit <- imap_dfr(models, function(topic_models, topic_name) {
  imap_dfr(topic_models, function(model, model_name) {
    tidy_lm(model, as.integer(str_remove(topic_name, "topic")), model_name)
  })
})
write_csv(audit, here("qa", "topics21_25_R_regression_audit.csv"))

# Match coefficient names between Python treatment coding and R factor coding.
normalize_term <- function(term) {
  term <- ifelse(term %in% c("Intercept", "(Intercept)"), "Intercept", term)
  term <- str_remove_all(term, "\\[|\\]")
  map_chr(str_split(term, ":"), ~ paste(sort(.x), collapse = ":"))
}
expected <- read_csv(here("qa", "topics21_25_regression_audit.csv"), show_col_types = FALSE)
comparison <- audit %>% mutate(key = normalize_term(term)) %>%
  inner_join(expected %>% mutate(key = normalize_term(term)),
    by = c("topic", "model", "key"), suffix = c("_R", "_reference"))
stopifnot(nrow(comparison) == nrow(audit),
  max(abs(comparison$estimate_R - comparison$estimate_reference)) < 1e-6,
  max(abs(comparison$std_error_R - comparison$std_error_reference)) < 1e-6,
  max(abs(comparison$p_value_R - comparison$p_value_reference)) < 1e-6)

direction_reference <- read_csv(here("qa", "topics21_25_covariate_directions.csv"), show_col_types = FALSE)
direction_check <- audit %>% filter(model == "interaction") %>%
  mutate(key = normalize_term(term)) %>%
  inner_join(direction_reference %>% mutate(key = normalize_term(term)),
    by = c("topic", "key"), suffix = c("_R", "_reference")) %>%
  mutate(direction_pass = case_when(
    expected_direction == "positive" ~ estimate_R > 0,
    expected_direction == "negative" ~ estimate_R < 0,
    TRUE ~ TRUE))
stopifnot(all(direction_check$direction_pass))
write_csv(direction_check, here("qa", "topics21_25_R_direction_check.csv"))

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
write_csv(covariate_tests, here("qa", "topics21_25_R_covariate_block_tests.csv"))

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
  simple_slopes(m3_work, "weekly_work_hours", "college_year", work$college_year, 21),
  simple_slopes(m3_music, "music_while_studying", "task_difficulty", music$task_difficulty, 22),
  simple_slopes(m3_body, "social_media_exposure", "media_literacy", body$media_literacy, 23),
  simple_slopes(m3_nature, "screen_time", "weekly_time_in_nature", nature$weekly_time_in_nature, 24),
  simple_slopes(m3_creativity, "daily_demands", "social_support", creativity$social_support, 25)
)
print(conditional_effects)
write_csv(conditional_effects, here("qa", "topics21_25_R_conditional_effects.csv"))


# Adjusted residual and quantile plots; assess OLS assumptions and unusual records.
pdf(here("qa", "topics21_25_R_diagnostics.pdf"), width = 9, height = 7)
for (topic_name in names(models)) {
  par(mfrow = c(2, 2))
  plot(models[[topic_name]]$interaction, which = c(1, 2, 3, 5), main = topic_name)
}
dev.off()
cat("All saved-data regression and direction checks passed.\n")
capture.output(sessionInfo(), file = here("qa", "topics21_25_R_session_info.txt"))

# TOPIC21: SECONDARY GPA MODEL, INSPIRED BY LOGAN ET AL. ----
# This is a separate outcome. Do not interpret stress coefficients as GPA effects.
work <- work %>% mutate(college_stage = factor(college_stage, levels = c("lower", "upper")))
m4_work_gpa <- lm(
  cumulative_gpa ~ high_work_intensity * college_stage +
    age +
    weekly_work_hours +
    college_year +
    commute_minutes +
    tuition_fees_thousand +
    hs_gpa +
    english_proficiency +
    enrollment_units +
    study_hours_per_week +
    sleep_hours +
    financial_strain_index +
    academic_engagement_index +
    institution_support_index +
    self_efficacy_index +
    work_academic_conflict_index +
    work_career_value_index +
    mental_strain_index +
    schedule_variability +
    schedule_control +
    social_support +
    gender +
    race_ethnicity +
    first_generation +
    family_income_bracket +
    pell_grant +
    dependent_care +
    institution_control +
    institution_selectivity +
    major_group +
    reason_for_working, data = work)
show_model(m4_work_gpa)
gpa_reference <- read_csv(here("qa", "topic21_source_gpa_audit.csv"), show_col_types = FALSE)
gpa_check <- tidy_lm(m4_work_gpa, 21, "source_gpa") %>%
  mutate(key = normalize_term(term)) %>%
  inner_join(gpa_reference %>% mutate(key = normalize_term(term)),
    by = c("topic", "model", "key"), suffix = c("_R", "_reference"))
stopifnot(nrow(gpa_check) == length(coef(m4_work_gpa)),
  max(abs(gpa_check$estimate_R - gpa_check$estimate_reference)) < 1e-6,
  max(abs(gpa_check$std_error_R - gpa_check$std_error_reference)) < 1e-6)
b <- coef(m4_work_gpa)
gpa_contrasts <- tibble(college_stage = c("lower", "upper"),
  high_work_contrast = c(b["high_work_intensity"],
    b["high_work_intensity"] + b["high_work_intensity:college_stageupper"]))
print(gpa_contrasts)
stopifnot(abs(gpa_contrasts$high_work_contrast[1] + .552) < .002,
  abs(gpa_contrasts$high_work_contrast[2] - .010) < .002)
write_csv(gpa_contrasts, here("qa", "topic21_R_gpa_contrasts.csv"))

# Structural zeros and aliases must remain consistent.
stopifnot(all(work$weekly_work_hours[work$part_time_work == "not_working"] == 0),
  all(work$weekly_work_hours <= 29),
  all(work$off_campus_work_hours == work$weekly_work_hours),
  all(work$high_work_intensity == as.integer(work$weekly_work_hours > 20)),
  all(work$not_persisted == 1 - work$enrolled_next_year),
  max(abs(work$credits_earned - work$credits_attempted * work$course_completion_rate)) <= .00051)
stopifnot(max(abs(work$financial_strain_index - rowMeans(work[, c("fin_strain_pay_tuition", "fin_strain_living_costs", "fin_strain_food_housing", "fin_strain_need_work_income")]))) < 1e-8)
stopifnot(max(abs(work$academic_engagement_index - rowMeans(work[, c("engage_study_effort", "engage_class_attendance", "engage_class_participation", "engage_faculty_contact", "engage_peer_learning")]))) < 1e-8)
stopifnot(max(abs(work$institution_support_index - rowMeans(work[, c("support_advising_quality", "support_faculty_care", "support_belonging", "support_resource_use")]))) < 1e-8)
stopifnot(max(abs(work$self_efficacy_index - rowMeans(work[, c("efficacy_manage_time", "efficacy_complete_assignments", "efficacy_ask_for_help", "efficacy_recover_setbacks")]))) < 1e-8)
stopifnot(max(abs(work$work_academic_conflict_index - rowMeans(work[, c("work_conflict_missed_study", "work_conflict_schedule_overlap", "work_conflict_tired_in_class", "work_conflict_missed_campus")]))) < 1e-8)
stopifnot(max(abs(work$work_career_value_index - rowMeans(work[, c("work_value_applies_learning", "work_value_career_skills", "work_value_professional_network", "work_value_motivation")]))) < 1e-8)
stopifnot(max(abs(work$mental_strain_index - rowMeans(work[, c("wellbeing_stress", "wellbeing_exhaustion", "wellbeing_sleep_problems", "wellbeing_overwhelmed")]))) < 1e-8)
cat("Topic21 GPA and item checks passed.\n")
