# PSY150: check the five revised teaching datasets
# Open an RStudio project in DATA, with the CSV files inside data/.
# install.packages(c("tidyverse", "here", "jtools"))
library(tidyverse)
library(here)
library(jtools)

# Limits refer to coefficient tests for predictors and interactions in these
# named models. Intercept and overall F tests are not tuning criteria.
# Estimates and SEs print to 3 decimals; p-values print to 3 decimals.
# The intercept row is omitted from the display because it tests a zero baseline.
p_lower <- 0.0001
p_upper <- 0.85

specs <- list(
  topic_1 = list(
    file = "42053_armstrong_homesickness_F26.csv",
    outcome = "homesickness_score_1_5",
    primary = "childhood_adolescent_move_history",
    moderator = "perceived_social_support",
    interaction_predictor = "childhood_adolescent_move_history",
    outcome_limits = c(1, 5),
    covariates = c("number_of_moves",
      "age_at_first_move",
      "previous_transitions",
      "age_years",
      "first_generation_college",
      "living_on_campus",
      "distance_home_miles",
      "parental_attachment",
      "perceived_social_support",
      "friendship_quality",
      "campus_belonging",
      "ego_resiliency",
      "emotion_regulation",
      "coping_self_efficacy",
      "depressive_symptoms",
      "anxiety_symptoms",
      "loneliness",
      "academic_stress",
      "financial_stress",
      "parent_contact_days_week")
  ),
  topic_2 = list(
    file = "42053_kerr_pain_catastrophizing_F26.csv",
    outcome = "pain_intensity_0_10",
    primary = "pain_catastrophizing_score_0_52",
    moderator = "fear_of_pain",
    interaction_predictor = "pain_catastrophizing_score_0_52",
    outcome_limits = c(0, 10),
    covariates = c("chronic_pain_group",
      "pain_duration_months",
      "pain_locations",
      "catastrophizing_rumination",
      "catastrophizing_magnification",
      "catastrophizing_helplessness",
      "fear_of_pain",
      "pain_vigilance",
      "pain_disability",
      "depressive_symptoms",
      "anxiety_symptoms",
      "sleep_quality",
      "social_support",
      "pain_self_efficacy",
      "analgesic_use_days_week",
      "physical_activity_days_week",
      "injury_severity",
      "pain_diagnosis_group",
      "age_years",
      "work_limitation")
  ),
  topic_3 = list(
    file = "42053_reyes_gym_culture_body_dysmorphia_F26.csv",
    outcome = "body_appearance_concern_1_5",
    primary = "gym_content_hours_week",
    moderator = "appearance_comparison",
    interaction_predictor = "gym_content_hours_week",
    outcome_limits = c(1, 5),
    covariates = c("total_social_media_hours_day",
      "appearance_comparison",
      "internalized_appearance_ideal",
      "muscularity_ideal",
      "self_esteem",
      "depressive_symptoms",
      "anxiety_symptoms",
      "stress_level",
      "eating_concerns",
      "resistance_training_days_week",
      "exercise_minutes_week",
      "bmi",
      "age_years",
      "gender_woman",
      "peer_appearance_pressure",
      "family_appearance_pressure",
      "fitness_influencer_following",
      "photo_editing_frequency",
      "sleep_quality",
      "body_appreciation")
  ),
  topic_4 = list(
    file = "42053_munoz_procrastination_test_anxiety_F26.csv",
    outcome = "test_anxiety_score_1_5",
    primary = "academic_procrastination_score_0_100",
    moderator = "time_management",
    interaction_predictor = "academic_procrastination_score_0_100",
    outcome_limits = c(1, 5),
    covariates = c("time_management",
      "fear_of_failure",
      "academic_self_efficacy",
      "perfectionism",
      "study_hours_week",
      "work_hours_week",
      "course_load_units",
      "year_in_school",
      "age_years",
      "first_generation_college",
      "prior_exam_failure",
      "gpa_0_4",
      "test_preparation_hours_week",
      "sleep_quality",
      "general_anxiety",
      "depressive_symptoms",
      "academic_stress",
      "social_support",
      "attention_difficulties",
      "instructor_support")
  ),
  topic_5 = list(
    file = "42053_pedrini_music_concentration_F26.csv",
    outcome = "reading_comprehension_percent_0_100",
    primary = "music_condition",
    moderator = "study_music_days_week",
    interaction_predictor = "music_hip_hop",
    outcome_limits = c(0, 100),
    covariates = c("study_music_days_week",
      "reading_proficiency",
      "working_memory",
      "sustained_attention",
      "attention_difficulties",
      "music_training_years",
      "genre_preference",
      "lyrics_familiarity",
      "topic_familiarity",
      "perceived_task_difficulty",
      "sleep_hours",
      "fatigue",
      "stress_level",
      "caffeine_mg_day",
      "language_proficiency",
      "reading_time_minutes",
      "perceived_music_volume",
      "music_arousal",
      "age_years",
      "paid_work_hours_week")
  )
)

coefficient_table <- function(model) {
  as.data.frame(coef(summary(model))) %>%
    rownames_to_column("term") %>%
    rename(estimate = Estimate, std_error = `Std. Error`,
           statistic = `t value`, p_value = `Pr(>|t|)`) %>% as_tibble()
}

read_topic <- function(spec) {
  raw_data <- read_csv(here("data", spec$file), show_col_types = FALSE)
  stopifnot(nrow(raw_data) >= 300, !anyNA(raw_data),
    !anyDuplicated(raw_data$participant_id), length(spec$covariates) == 20,
    !any(c("INTERACTION", "INTERACTIONS", "project_id", "section") %in% names(raw_data)),
    all(raw_data[[spec$outcome]] >= spec$outcome_limits[1]),
    all(raw_data[[spec$outcome]] <= spec$outcome_limits[2]))

  if ("pain_diagnosis_group" %in% names(raw_data)) {
    # Diagnosis/location codes are categories, not a numeric trend.
    raw_data <- raw_data %>% mutate(pain_diagnosis_group = factor(pain_diagnosis_group,
      levels = c(1, 2, 3, 4)))
  }
  if (spec$primary == "music_condition") {
    raw_data <- raw_data %>% mutate(music_condition = factor(music_condition,
      levels = c("no_music", "light_classical", "hip_hop")))
  }
  if ("childhood_adolescent_move_history" %in% names(raw_data)) {
    stopifnot(all((raw_data$number_of_moves > 0) == (raw_data$childhood_adolescent_move_history == 1)),
      all((raw_data$age_at_first_move > 0) == (raw_data$childhood_adolescent_move_history == 1)))
  }

  raw_data
}

# Read the five data files together, then work through one project at a time.
datasets <- map(specs, read_topic)
homesickness <- datasets$topic_1
pain <- datasets$topic_2
body_image <- datasets$topic_3
procrastination <- datasets$topic_4
music <- datasets$topic_5

# Display helper only: model fitting is written out in each section below.
show_summ <- function(model) {
  output <- jtools::summ(model, digits = 3, confint = FALSE, pvals = TRUE,
    model.info = FALSE, model.fit = FALSE, which.cols = c("Est.", "S.E.", "p"))
  # Keep the model intercept, but omit its baseline p-value from this display.
  output$coeftable <- output$coeftable[
    rownames(output$coeftable) != "(Intercept)",
    c("Est.", "S.E.", "p"), drop = FALSE
  ]
  print(output)
  invisible(output)
}

# ------------------------------------------------------------------------------

# 1. CHILDHOOD MOVES AND HOMESICKNESS ----

# ------------------------------------------------------------------------------

# Model 1: Simple model
m1_homesick <- lm(
  homesickness_score_1_5 ~
    childhood_adolescent_move_history,
  data = homesickness
)
show_summ(m1_homesick)

# Model 2: Adjusted model
m2_homesick <- lm(
  homesickness_score_1_5 ~
    childhood_adolescent_move_history +
    number_of_moves +
    age_at_first_move +
    previous_transitions +
    age_years +
    first_generation_college +
    living_on_campus +
    distance_home_miles +
    parental_attachment +
    perceived_social_support +
    friendship_quality +
    campus_belonging +
    ego_resiliency +
    emotion_regulation +
    coping_self_efficacy +
    depressive_symptoms +
    anxiety_symptoms +
    loneliness +
    academic_stress +
    financial_stress +
    parent_contact_days_week,
  data = homesickness
)
show_summ(m2_homesick)

# Model 3: Interaction model: centered predictors
homesick_centered <- homesickness %>%
  mutate(
    childhood_adolescent_move_history = childhood_adolescent_move_history - mean(childhood_adolescent_move_history),
    perceived_social_support = perceived_social_support - mean(perceived_social_support)
  )

m3_homesick <- lm(
  homesickness_score_1_5 ~
    childhood_adolescent_move_history +
    number_of_moves +
    age_at_first_move +
    previous_transitions +
    age_years +
    first_generation_college +
    living_on_campus +
    distance_home_miles +
    parental_attachment +
    perceived_social_support +
    friendship_quality +
    campus_belonging +
    ego_resiliency +
    emotion_regulation +
    coping_self_efficacy +
    depressive_symptoms +
    anxiety_symptoms +
    loneliness +
    academic_stress +
    financial_stress +
    parent_contact_days_week +
    childhood_adolescent_move_history:perceived_social_support,
  data = homesick_centered
)
show_summ(m3_homesick)

# Model 4: Interaction model: original predictor scales
m4_homesick <- lm(
  homesickness_score_1_5 ~
    childhood_adolescent_move_history +
    number_of_moves +
    age_at_first_move +
    previous_transitions +
    age_years +
    first_generation_college +
    living_on_campus +
    distance_home_miles +
    parental_attachment +
    perceived_social_support +
    friendship_quality +
    campus_belonging +
    ego_resiliency +
    emotion_regulation +
    coping_self_efficacy +
    depressive_symptoms +
    anxiety_symptoms +
    loneliness +
    academic_stress +
    financial_stress +
    parent_contact_days_week +
    childhood_adolescent_move_history:perceived_social_support,
  data = homesickness
)
show_summ(m4_homesick)

# ------------------------------------------------------------------------------

# 2. PAIN CATASTROPHIZING AND PAIN INTENSITY ----

# ------------------------------------------------------------------------------

# Model 1: Simple model
m1_pain <- lm(
  pain_intensity_0_10 ~
    pain_catastrophizing_score_0_52,
  data = pain
)
show_summ(m1_pain)

# Model 2: Adjusted model
m2_pain <- lm(
  pain_intensity_0_10 ~
    pain_catastrophizing_score_0_52 +
    chronic_pain_group +
    pain_duration_months +
    pain_locations +
    catastrophizing_rumination +
    catastrophizing_magnification +
    catastrophizing_helplessness +
    fear_of_pain +
    pain_vigilance +
    pain_disability +
    depressive_symptoms +
    anxiety_symptoms +
    sleep_quality +
    social_support +
    pain_self_efficacy +
    analgesic_use_days_week +
    physical_activity_days_week +
    injury_severity +
    pain_diagnosis_group +
    age_years +
    work_limitation,
  data = pain
)
show_summ(m2_pain)

# Model 3: Interaction model: centered predictors
pain_centered <- pain %>%
  mutate(
    pain_catastrophizing_score_0_52 = pain_catastrophizing_score_0_52 - mean(pain_catastrophizing_score_0_52),
    fear_of_pain = fear_of_pain - mean(fear_of_pain)
  )
m3_pain <- lm(
  pain_intensity_0_10 ~
    pain_catastrophizing_score_0_52 +
    chronic_pain_group +
    pain_duration_months +
    pain_locations +
    catastrophizing_rumination +
    catastrophizing_magnification +
    catastrophizing_helplessness +
    fear_of_pain +
    pain_vigilance +
    pain_disability +
    depressive_symptoms +
    anxiety_symptoms +
    sleep_quality +
    social_support +
    pain_self_efficacy +
    analgesic_use_days_week +
    physical_activity_days_week +
    injury_severity +
    pain_diagnosis_group +
    age_years +
    work_limitation +
    pain_catastrophizing_score_0_52:fear_of_pain,
  data = pain_centered
)
show_summ(m3_pain)

# Model 4: Interaction model: original predictor scales
m4_pain <- lm(
  pain_intensity_0_10 ~
    pain_catastrophizing_score_0_52 +
    chronic_pain_group +
    pain_duration_months +
    pain_locations +
    catastrophizing_rumination +
    catastrophizing_magnification +
    catastrophizing_helplessness +
    fear_of_pain +
    pain_vigilance +
    pain_disability +
    depressive_symptoms +
    anxiety_symptoms +
    sleep_quality +
    social_support +
    pain_self_efficacy +
    analgesic_use_days_week +
    physical_activity_days_week +
    injury_severity +
    pain_diagnosis_group +
    age_years +
    work_limitation +
    pain_catastrophizing_score_0_52:fear_of_pain,
  data = pain
)
show_summ(m4_pain)

# ------------------------------------------------------------------------------

# 3. GYM-CULTURE CONTENT AND BODY-IMAGE CONCERNS ----

# ------------------------------------------------------------------------------

# Model 1: Simple model
m1_body_image <- lm(
  body_appearance_concern_1_5 ~
    gym_content_hours_week,
  data = body_image
)
show_summ(m1_body_image)

# Model 2: Adjusted model
m2_body_image <- lm(
  body_appearance_concern_1_5 ~
    gym_content_hours_week +
    total_social_media_hours_day +
    appearance_comparison +
    internalized_appearance_ideal +
    muscularity_ideal +
    self_esteem +
    depressive_symptoms +
    anxiety_symptoms +
    stress_level +
    eating_concerns +
    resistance_training_days_week +
    exercise_minutes_week +
    bmi +
    age_years +
    gender_woman +
    peer_appearance_pressure +
    family_appearance_pressure +
    fitness_influencer_following +
    photo_editing_frequency +
    sleep_quality +
    body_appreciation,
  data = body_image
)
show_summ(m2_body_image)

# Model 3: Interaction model: centered predictors
body_image_centered <- body_image %>%
  mutate(
    gym_content_hours_week = gym_content_hours_week - mean(gym_content_hours_week),
    appearance_comparison = appearance_comparison - mean(appearance_comparison)
  )
m3_body_image <- lm(
  body_appearance_concern_1_5 ~
    gym_content_hours_week +
    total_social_media_hours_day +
    appearance_comparison +
    internalized_appearance_ideal +
    muscularity_ideal +
    self_esteem +
    depressive_symptoms +
    anxiety_symptoms +
    stress_level +
    eating_concerns +
    resistance_training_days_week +
    exercise_minutes_week +
    bmi +
    age_years +
    gender_woman +
    peer_appearance_pressure +
    family_appearance_pressure +
    fitness_influencer_following +
    photo_editing_frequency +
    sleep_quality +
    body_appreciation +
    gym_content_hours_week:appearance_comparison,
  data = body_image_centered
)
show_summ(m3_body_image)

# Model 4: Interaction model: original predictor scales
m4_body_image <- lm(
  body_appearance_concern_1_5 ~
    gym_content_hours_week +
    total_social_media_hours_day +
    appearance_comparison +
    internalized_appearance_ideal +
    muscularity_ideal +
    self_esteem +
    depressive_symptoms +
    anxiety_symptoms +
    stress_level +
    eating_concerns +
    resistance_training_days_week +
    exercise_minutes_week +
    bmi +
    age_years +
    gender_woman +
    peer_appearance_pressure +
    family_appearance_pressure +
    fitness_influencer_following +
    photo_editing_frequency +
    sleep_quality +
    body_appreciation +
    gym_content_hours_week:appearance_comparison,
  data = body_image
)
show_summ(m4_body_image)

# ------------------------------------------------------------------------------

# 4. ACADEMIC PROCRASTINATION AND TEST ANXIETY ----

# ------------------------------------------------------------------------------

# Model 1: Simple model
m1_procrastination <- lm(
  test_anxiety_score_1_5 ~
    academic_procrastination_score_0_100,
  data = procrastination
)
show_summ(m1_procrastination)

# Model 2: Adjusted model
m2_procrastination <- lm(
  test_anxiety_score_1_5 ~
    academic_procrastination_score_0_100 +
    time_management +
    fear_of_failure +
    academic_self_efficacy +
    perfectionism +
    study_hours_week +
    work_hours_week +
    course_load_units +
    year_in_school +
    age_years +
    first_generation_college +
    prior_exam_failure +
    gpa_0_4 +
    test_preparation_hours_week +
    sleep_quality +
    general_anxiety +
    depressive_symptoms +
    academic_stress +
    social_support +
    attention_difficulties +
    instructor_support,
  data = procrastination
)
show_summ(m2_procrastination)

# Model 3: Interaction model: centered predictors
procrastination_centered <- procrastination %>%
  mutate(
    academic_procrastination_score_0_100 = academic_procrastination_score_0_100 - mean(academic_procrastination_score_0_100),
    time_management = time_management - mean(time_management)
  )
m3_procrastination <- lm(
  test_anxiety_score_1_5 ~
    academic_procrastination_score_0_100 +
    time_management +
    fear_of_failure +
    academic_self_efficacy +
    perfectionism +
    study_hours_week +
    work_hours_week +
    course_load_units +
    year_in_school +
    age_years +
    first_generation_college +
    prior_exam_failure +
    gpa_0_4 +
    test_preparation_hours_week +
    sleep_quality +
    general_anxiety +
    depressive_symptoms +
    academic_stress +
    social_support +
    attention_difficulties +
    instructor_support +
    academic_procrastination_score_0_100:time_management,
  data = procrastination_centered
)
show_summ(m3_procrastination)

# Model 4: Interaction model: original predictor scales
m4_procrastination <- lm(
  test_anxiety_score_1_5 ~
    academic_procrastination_score_0_100 +
    time_management +
    fear_of_failure +
    academic_self_efficacy +
    perfectionism +
    study_hours_week +
    work_hours_week +
    course_load_units +
    year_in_school +
    age_years +
    first_generation_college +
    prior_exam_failure +
    gpa_0_4 +
    test_preparation_hours_week +
    sleep_quality +
    general_anxiety +
    depressive_symptoms +
    academic_stress +
    social_support +
    attention_difficulties +
    instructor_support +
    academic_procrastination_score_0_100:time_management,
  data = procrastination
)
show_summ(m4_procrastination)

# ------------------------------------------------------------------------------

# 5. BACKGROUND MUSIC AND READING PERFORMANCE ----

# ------------------------------------------------------------------------------

# Model 1: Simple model
m1_music <- lm(
  reading_comprehension_percent_0_100 ~
    music_condition,
  data = music
)
show_summ(m1_music)

# Model 2: Adjusted model
m2_music <- lm(
  reading_comprehension_percent_0_100 ~
    music_condition +
    study_music_days_week +
    reading_proficiency +
    working_memory +
    sustained_attention +
    attention_difficulties +
    music_training_years +
    genre_preference +
    lyrics_familiarity +
    topic_familiarity +
    perceived_task_difficulty +
    sleep_hours +
    fatigue +
    stress_level +
    caffeine_mg_day +
    language_proficiency +
    reading_time_minutes +
    perceived_music_volume +
    music_arousal +
    age_years +
    paid_work_hours_week,
  data = music
)
show_summ(m2_music)

# Model 3: Interaction model: centered predictors
music_centered <- music %>%
  mutate(
    study_music_days_week = study_music_days_week - mean(study_music_days_week)
  )
# Only the hip-hop by study-music-days interaction is specified.
m3_music <- lm(
  reading_comprehension_percent_0_100 ~
    music_condition +
    study_music_days_week +
    reading_proficiency +
    working_memory +
    sustained_attention +
    attention_difficulties +
    music_training_years +
    genre_preference +
    lyrics_familiarity +
    topic_familiarity +
    perceived_task_difficulty +
    sleep_hours +
    fatigue +
    stress_level +
    caffeine_mg_day +
    language_proficiency +
    reading_time_minutes +
    perceived_music_volume +
    music_arousal +
    age_years +
    paid_work_hours_week +
    music_hip_hop:study_music_days_week,
  data = music_centered
)
show_summ(m3_music)

# Model 4: Interaction model: original predictor scales
m4_music <- lm(
  reading_comprehension_percent_0_100 ~
    music_condition +
    study_music_days_week +
    reading_proficiency +
    working_memory +
    sustained_attention +
    attention_difficulties +
    music_training_years +
    genre_preference +
    lyrics_familiarity +
    topic_familiarity +
    perceived_task_difficulty +
    sleep_hours +
    fatigue +
    stress_level +
    caffeine_mg_day +
    language_proficiency +
    reading_time_minutes +
    perceived_music_volume +
    music_arousal +
    age_years +
    paid_work_hours_week +
    music_hip_hop:study_music_days_week,
  data = music
)
show_summ(m4_music)

# Save the numerical checks after all five project sections. These do not
# add columns or extra output to the model summaries above.
all_models <- list(
  topic_1 = list(simple = m1_homesick, adjusted = m2_homesick,
    interaction = m3_homesick, raw_interaction = m4_homesick),
  topic_2 = list(simple = m1_pain, adjusted = m2_pain,
    interaction = m3_pain, raw_interaction = m4_pain),
  topic_3 = list(simple = m1_body_image, adjusted = m2_body_image,
    interaction = m3_body_image, raw_interaction = m4_body_image),
  topic_4 = list(simple = m1_procrastination, adjusted = m2_procrastination,
    interaction = m3_procrastination,
    raw_interaction = m4_procrastination),
  topic_5 = list(simple = m1_music, adjusted = m2_music,
    interaction = m3_music, raw_interaction = m4_music)
)

audit <- imap_dfr(all_models, function(models, topic) {
  imap_dfr(models, function(model, model_name) {
    stopifnot(model$rank == ncol(model.matrix(model)))
    coefficient_table(model) %>%
      filter(term != "(Intercept)") %>%
      mutate(dataset = specs[[topic]]$file, model = model_name, .before = 1)
  })
})
stopifnot(all(is.finite(audit$p_value)),
  all(audit$p_value >= p_lower & audit$p_value <= p_upper))
validation_summary <- audit %>% group_by(dataset, model) %>%
  summarise(tests = n(), min_p = min(p_value), max_p = max(p_value),
    passed = all(p_value >= p_lower & p_value <= p_upper), .groups = "drop")

dir.create(here("qa"), showWarnings = FALSE, recursive = TRUE)
write_csv(audit, here("qa", "R_regression_pvalue_audit.csv"))
write_csv(validation_summary, here("qa", "R_validation_summary.csv"))
# Residual-versus-fitted and normal Q-Q plots for each interaction model.
# These plots diagnose the generated data; they are not significance tests.
pdf(here("qa", "R_regression_diagnostics.pdf"), width = 10, height = 5)
walk(all_models, function(models) {
  par(mfrow = c(1, 2))
  plot(models$interaction, which = 1)
  plot(models$interaction, which = 2)
})
invisible(dev.off())
