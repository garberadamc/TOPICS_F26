# PSY 150: topics 6-10 model checks
# Open an RStudio project in DATA; put CSVs in data/ and validation files in qa/.
# install.packages(c("tidyverse", "here", "jtools"))
library(tidyverse)
library(here)
library(jtools)

# SYNTHETIC teaching data: estimates and p-values were deliberately calibrated.
# Limits apply to non-intercept coefficient tests in the models below.
# Changing a formula changes the tests; arbitrary alternative models are not calibrated.
# Conventional OLS SEs are used. Coefficients are unstandardized.
files <- c(swim = "topic06_F26.csv", burnout = "topic07_F26.csv", exercise = "topic08_F26.csv", horror = "topic09_F26.csv", sleep = "topic10_F26.csv")
datasets <- map(files, function(file_name) {
  raw_data <- read_csv(here("data", file_name), show_col_types = FALSE)
  stopifnot(nrow(raw_data) >= 300, !anyNA(raw_data),
    !anyDuplicated(raw_data$participant_id),
    !any(c("INTERACTION", "INTERACTIONS", "project_id", "section") %in% names(raw_data)))
  raw_data
})

swim <- datasets$swim %>%
  mutate(
    meditative_breathing = factor(meditative_breathing, levels = c("no_breathing", "breathing_5min")),
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    recent_injury = factor(recent_injury, levels = c("no", "yes")),
    taper_week = factor(taper_week, levels = c("no", "yes")),
    competition_level = factor(competition_level, levels = c("regional", "national")),
    usual_event = factor(usual_event, levels = c("sprint", "distance")),
    prior_mindfulness_course = factor(prior_mindfulness_course, levels = c("no", "yes")),
    strength_training = factor(strength_training, levels = c("no", "yes")),
    early_session = factor(early_session, levels = c("no", "yes")),
    recent_illness = factor(recent_illness, levels = c("no", "yes")),
    familiar_pool = factor(familiar_pool, levels = c("no", "yes"))
  )

burnout <- datasets$burnout %>%
  mutate(
    professional_role = factor(professional_role, levels = c("nursing", "medical", "allied_health", "support")),
    shift_pattern = factor(shift_pattern, levels = c("day", "night", "rotating")),
    employment_status = factor(employment_status, levels = c("full_time", "part_time")),
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    dependent_care = factor(dependent_care, levels = c("no", "yes")),
    postgraduate_training = factor(postgraduate_training, levels = c("no", "yes")),
    debrief_access = factor(debrief_access, levels = c("no", "yes")),
    contract_temporary = factor(contract_temporary, levels = c("no", "yes")),
    recent_violence = factor(recent_violence, levels = c("no", "yes")),
    peer_support_program = factor(peer_support_program, levels = c("no", "yes"))
  )

exercise <- datasets$exercise %>%
  mutate(
    sports_team_participation = factor(sports_team_participation, levels = c("no", "yes")),
    injuries = factor(injuries, levels = c("no", "yes")),
    exercise_type = factor(exercise_type, levels = c("mixed", "aerobic", "resistance")),
    exam_period = factor(exam_period, levels = c("ordinary", "exam")),
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    year_in_college = factor(year_in_college, levels = c("first", "later")),
    housing = factor(housing, levels = c("with_family", "away_from_family")),
    first_generation = factor(first_generation, levels = c("no", "yes")),
    caregiving = factor(caregiving, levels = c("no", "yes")),
    recreation_access = factor(recreation_access, levels = c("no", "yes"))
  )

horror <- datasets$horror %>%
  mutate(
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    viewing_company = factor(viewing_company, levels = c("alone", "with_peer")),
    session_time = factor(session_time, levels = c("day", "evening")),
    content_warning = factor(content_warning, levels = c("no", "yes")),
    prior_film_course = factor(prior_film_course, levels = c("no", "yes")),
    headphones = factor(headphones, levels = c("no", "yes")),
    subtitles = factor(subtitles, levels = c("no", "yes")),
    prior_adverse_reaction = factor(prior_adverse_reaction, levels = c("no", "yes")),
    viewing_setting = factor(viewing_setting, levels = c("lab", "screening_room")),
    relaxation_familiarity = factor(relaxation_familiarity, levels = c("no", "yes"))
  )

sleep <- datasets$sleep %>%
  mutate(
    setting = factor(setting, levels = c("academic", "occupational")),
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    schedule = factor(schedule, levels = c("day", "rotating_or_night")),
    caregiving = factor(caregiving, levels = c("no", "yes")),
    habitual_napper = factor(habitual_napper, levels = c("no", "yes")),
    alcohol_previous_evening = factor(alcohol_previous_evening, levels = c("no", "yes")),
    quiet_test_room = factor(quiet_test_room, levels = c("no", "yes")),
    breakfast = factor(breakfast, levels = c("no", "yes")),
    first_session = factor(first_session, levels = c("no", "yes")),
    sedating_medication = factor(sedating_medication, levels = c("no", "yes"))
  )

# DISPLAY ONLY: fitting is written explicitly in each section.
show_summ <- function(model) {
  output <- summ(model, digits = 3, confint = FALSE, pvals = TRUE,
    model.info = FALSE, model.fit = FALSE, which.cols = c("Est.", "S.E.", "p"))
  output$coeftable <- output$coeftable[
    rownames(output$coeftable) != "(Intercept)", c("Est.", "S.E.", "p"), drop = FALSE]
  print(output)
  invisible(output)
}

# 6. MEDITATIVE BREATHING AND SWIM-RACE PERFORMANCE ----
# The proposal's 15 swimmers per group is expanded to 180 per group. The focal effect is deliberately small and uncertain, respecting mixed performance evidence. All covariates are baseline measurements; post-race stroke mechanics are descriptive outcomes, not controls.

# Focal predictor only
m1_swim <- lm(
  swim_race_performance ~
    meditative_breathing,
  data = swim
)
show_summ(m1_swim)

# Adjusted for 30 contextual covariates
m2_swim <- lm(
  swim_race_performance ~
    meditative_breathing +
    baseline_speed +
    height +
    weight +
    age_years +
    training_hours_week +
    training_years +
    sleep_hours +
    pre_race_anxiety +
    attention_control +
    training_fatigue +
    recovery_readiness +
    coach_support +
    mindfulness_minutes_week +
    warmup_minutes +
    caffeine_mg +
    travel_minutes +
    hydration_readiness +
    baseline_stroke_count +
    baseline_turn_seconds +
    race_confidence +
    gender +
    recent_injury +
    taper_week +
    competition_level +
    usual_event +
    prior_mindfulness_course +
    strength_training +
    early_session +
    recent_illness +
    familiar_pool,
  data = swim
)
show_summ(m2_swim)

swim_centered <- swim %>%
  mutate(
    pre_race_anxiety = pre_race_anxiety - mean(pre_race_anxiety)
  )

# Interaction, with continuous focal/moderator variables centered
m3_swim <- lm(
  swim_race_performance ~
    meditative_breathing +
    baseline_speed +
    height +
    weight +
    age_years +
    training_hours_week +
    training_years +
    sleep_hours +
    pre_race_anxiety +
    attention_control +
    training_fatigue +
    recovery_readiness +
    coach_support +
    mindfulness_minutes_week +
    warmup_minutes +
    caffeine_mg +
    travel_minutes +
    hydration_readiness +
    baseline_stroke_count +
    baseline_turn_seconds +
    race_confidence +
    gender +
    recent_injury +
    taper_week +
    competition_level +
    usual_event +
    prior_mindfulness_course +
    strength_training +
    early_session +
    recent_illness +
    familiar_pool +
    meditative_breathing:pre_race_anxiety,
  data = swim_centered
)
show_summ(m3_swim)

# 7. EMERGENCY-DEPARTMENT STRESSORS AND BURNOUT ----
# Cross-sectional associations cannot establish whether stressors cause burnout. Burnout-related consequences are not recycled as covariates. The model separates environmental stressors from workload volume and includes baseline/contextual resources. The supplied studies do not provide a directly transferable adjusted coefficient on this classroom scale.

# Focal predictor only
m1_burnout <- lm(
  burnout ~
    emergency_department_work_stressors,
  data = burnout
)
show_summ(m1_burnout)

# Adjusted for 30 contextual covariates
m2_burnout <- lm(
  burnout ~
    emergency_department_work_stressors +
    workload +
    shift_hours +
    traumatic_exposure +
    work_life_balance +
    coping_strategies +
    supervisor_support +
    weekly_work_hours +
    sleep_hours +
    staffing_adequacy +
    job_control +
    team_support +
    workplace_conflict +
    infection_worry +
    commute_minutes +
    age_years +
    ed_experience_years +
    break_minutes +
    recovery_days_week +
    financial_strain +
    exercise_minutes_week +
    professional_role +
    shift_pattern +
    employment_status +
    gender +
    dependent_care +
    postgraduate_training +
    debrief_access +
    contract_temporary +
    recent_violence +
    peer_support_program,
  data = burnout
)
show_summ(m2_burnout)

burnout_centered <- burnout %>%
  mutate(
    emergency_department_work_stressors = emergency_department_work_stressors - mean(emergency_department_work_stressors),
    supervisor_support = supervisor_support - mean(supervisor_support)
  )

# Interaction, with continuous focal/moderator variables centered
m3_burnout <- lm(
  burnout ~
    emergency_department_work_stressors +
    workload +
    shift_hours +
    traumatic_exposure +
    work_life_balance +
    coping_strategies +
    supervisor_support +
    weekly_work_hours +
    sleep_hours +
    staffing_adequacy +
    job_control +
    team_support +
    workplace_conflict +
    infection_worry +
    commute_minutes +
    age_years +
    ed_experience_years +
    break_minutes +
    recovery_days_week +
    financial_strain +
    exercise_minutes_week +
    professional_role +
    shift_pattern +
    employment_status +
    gender +
    dependent_care +
    postgraduate_training +
    debrief_access +
    contract_temporary +
    recent_violence +
    peer_support_program +
    emergency_department_work_stressors:supervisor_support,
  data = burnout_centered
)
show_summ(m3_burnout)

# 8. EXERCISE DURATION AND STUDENT STRESS ----
# One-record-per-student cross-sectional adaptation. The original effect direction is retained, while classroom uncertainty takes priority over reproducing the exact coefficient. Exam-period moderation is a theory-informed extension; it was not tested in Teuber et al. Recovery and performance outcomes are not used as controls.

# Focal predictor only
m1_exercise <- lm(
  stress_level ~
    exercise_duration,
  data = exercise
)
show_summ(m1_exercise)

# Adjusted for 30 contextual covariates
m2_exercise <- lm(
  stress_level ~
    exercise_duration +
    sleep_quality +
    study_demands +
    financial_strain +
    social_support +
    time_management +
    sleep_hours +
    study_hours_week +
    paid_work_hours +
    course_load_units +
    study_breaks_day +
    longest_study_hours +
    sitting_hours_day +
    caffeine_mg_day +
    commute_minutes +
    baseline_worry +
    activity_self_efficacy +
    schedule_control +
    age_years +
    outdoor_hours_week +
    housing_disruption +
    sports_team_participation +
    injuries +
    exercise_type +
    exam_period +
    gender +
    year_in_college +
    housing +
    first_generation +
    caregiving +
    recreation_access,
  data = exercise
)
show_summ(m2_exercise)

exercise_centered <- exercise %>%
  mutate(
    exercise_duration = exercise_duration - mean(exercise_duration)
  )

# Interaction, with continuous focal/moderator variables centered
m3_exercise <- lm(
  stress_level ~
    exercise_duration +
    sleep_quality +
    study_demands +
    financial_strain +
    social_support +
    time_management +
    sleep_hours +
    study_hours_week +
    paid_work_hours +
    course_load_units +
    study_breaks_day +
    longest_study_hours +
    sitting_hours_day +
    caffeine_mg_day +
    commute_minutes +
    baseline_worry +
    activity_self_efficacy +
    schedule_control +
    age_years +
    outdoor_hours_week +
    housing_disruption +
    sports_team_participation +
    injuries +
    exercise_type +
    exam_period +
    gender +
    year_in_college +
    housing +
    first_generation +
    caregiving +
    recreation_access +
    exercise_duration:exam_period,
  data = exercise_centered
)
show_summ(m3_exercise)

# 9. HORROR-FILM EXPOSURE AND IMMEDIATE EMOTIONAL RESPONSE ----
# The proposal allows time or frequency exposure. Here the focal exposure is session minutes and usual monthly viewing is a contextual variable. Baseline preference is measured before exposure to avoid using post-viewing enjoyment as a control. An assigned exposure scenario supports a clear temporal ordering.

# Focal predictor only
m1_horror <- lm(
  anxiety_and_emotional_response ~
    horror_movie_exposure,
  data = horror
)
show_summ(m1_horror)

# Adjusted for 30 contextual covariates
m2_horror <- lm(
  anxiety_and_emotional_response ~
    horror_movie_exposure +
    horror_movie_enjoyment +
    baseline_anxiety +
    fearfulness +
    disgust_sensitivity +
    sensation_seeking +
    emotion_regulation +
    empathic_concern +
    horror_films_month +
    sleep_hours +
    current_stress +
    perceived_safety +
    age_years +
    caffeine_mg +
    film_familiarity +
    imagery_vividness +
    startle_sensitivity +
    social_support +
    threat_expectancy +
    coping_confidence +
    total_film_hours_week +
    gender +
    viewing_company +
    session_time +
    content_warning +
    prior_film_course +
    headphones +
    subtitles +
    prior_adverse_reaction +
    viewing_setting +
    relaxation_familiarity,
  data = horror
)
show_summ(m2_horror)

horror_centered <- horror %>%
  mutate(
    horror_movie_exposure = horror_movie_exposure - mean(horror_movie_exposure),
    horror_movie_enjoyment = horror_movie_enjoyment - mean(horror_movie_enjoyment)
  )

# Interaction, with continuous focal/moderator variables centered
m3_horror <- lm(
  anxiety_and_emotional_response ~
    horror_movie_exposure +
    horror_movie_enjoyment +
    baseline_anxiety +
    fearfulness +
    disgust_sensitivity +
    sensation_seeking +
    emotion_regulation +
    empathic_concern +
    horror_films_month +
    sleep_hours +
    current_stress +
    perceived_safety +
    age_years +
    caffeine_mg +
    film_familiarity +
    imagery_vividness +
    startle_sensitivity +
    social_support +
    threat_expectancy +
    coping_confidence +
    total_film_hours_week +
    gender +
    viewing_company +
    session_time +
    content_warning +
    prior_film_course +
    headphones +
    subtitles +
    prior_adverse_reaction +
    viewing_setting +
    relaxation_familiarity +
    horror_movie_exposure:horror_movie_enjoyment,
  data = horror_centered
)
show_summ(m3_horror)

# 10. SLEEP DEPRIVATION AND COGNITIVE PERFORMANCE ----
# Observed one-session adaptation with separate academic and occupational groups. Last-night deprivation and sleep hours are deterministic complements and must not both enter a model. Prior restricted nights refers to the six nights BEFORE last night. Emotional regulation is a secondary outcome, not an adjustment variable.

# Focal predictor only
m1_sleep <- lm(
  cognitive_performance ~
    sleep_deprivation,
  data = sleep
)
show_summ(m1_sleep)

# Adjusted for 30 contextual covariates
m2_sleep <- lm(
  cognitive_performance ~
    sleep_deprivation +
    prior_restricted_nights +
    baseline_task_score +
    sleep_quality +
    chronic_stress +
    schedule_irregularity_hours +
    task_motivation +
    age_years +
    education_years +
    caffeine_mg +
    exercise_minutes_week +
    screen_hours_evening +
    weekly_obligation_hours +
    commute_minutes +
    bedroom_noise +
    chronotype_eveningness +
    wake_to_test_hours +
    nap_minutes_prior_day +
    physical_discomfort +
    computer_familiarity +
    sleep_environment_quality +
    setting +
    gender +
    schedule +
    caregiving +
    habitual_napper +
    alcohol_previous_evening +
    quiet_test_room +
    breakfast +
    first_session +
    sedating_medication,
  data = sleep
)
show_summ(m2_sleep)

sleep_centered <- sleep %>%
  mutate(
    sleep_deprivation = sleep_deprivation - mean(sleep_deprivation),
    prior_restricted_nights = prior_restricted_nights - mean(prior_restricted_nights)
  )

# Interaction, with continuous focal/moderator variables centered
m3_sleep <- lm(
  cognitive_performance ~
    sleep_deprivation +
    prior_restricted_nights +
    baseline_task_score +
    sleep_quality +
    chronic_stress +
    schedule_irregularity_hours +
    task_motivation +
    age_years +
    education_years +
    caffeine_mg +
    exercise_minutes_week +
    screen_hours_evening +
    weekly_obligation_hours +
    commute_minutes +
    bedroom_noise +
    chronotype_eveningness +
    wake_to_test_hours +
    nap_minutes_prior_day +
    physical_discomfort +
    computer_familiarity +
    sleep_environment_quality +
    setting +
    gender +
    schedule +
    caregiving +
    habitual_napper +
    alcohol_previous_evening +
    quiet_test_room +
    breakfast +
    first_session +
    sedating_medication +
    sleep_deprivation:prior_restricted_nights,
  data = sleep_centered
)
show_summ(m3_sleep)

# Descriptive secondary outcome; higher = BETTER emotional regulation.
m4_sleep_emotion <- lm(emotional_regulation ~ sleep_deprivation, data = sleep)
show_summ(m4_sleep_emotion)

# OPTIONAL INSTRUCTOR CHECKS: these collect already fitted models. ----
# No model-fitting loops or generated formulas are used.
coefficient_table <- function(model, topic, model_name) {
  as.data.frame(coef(summary(model))) %>%
    rownames_to_column("term") %>%
    filter(term != "(Intercept)") %>%
    transmute(topic = topic, model = model_name, term, estimate = Estimate,
      std_error = `Std. Error`, p_value = `Pr(>|t|)`)
}
checks <- bind_rows(
  coefficient_table(m1_swim, 6, "simple"),
  coefficient_table(m2_swim, 6, "adjusted"),
  coefficient_table(m3_swim, 6, "interaction"),
  coefficient_table(m1_burnout, 7, "simple"),
  coefficient_table(m2_burnout, 7, "adjusted"),
  coefficient_table(m3_burnout, 7, "interaction"),
  coefficient_table(m1_exercise, 8, "simple"),
  coefficient_table(m2_exercise, 8, "adjusted"),
  coefficient_table(m3_exercise, 8, "interaction"),
  coefficient_table(m1_horror, 9, "simple"),
  coefficient_table(m2_horror, 9, "adjusted"),
  coefficient_table(m3_horror, 9, "interaction"),
  coefficient_table(m1_sleep, 10, "simple"),
  coefficient_table(m2_sleep, 10, "adjusted"),
  coefficient_table(m3_sleep, 10, "interaction"),
  coefficient_table(m4_sleep_emotion, 10, "secondary")
)
stopifnot(all(is.finite(checks$p_value)), all(checks$p_value > 0.0001),
  all(checks$p_value < 0.85))
# Match signs to the reviewed direction table and compare numerical results.
expected <- read_csv(here("qa", "batch6_10_coefficients.csv"), show_col_types = FALSE)
comparison <- checks %>% inner_join(expected %>%
  select(topic, model, term, expected_direction, expected_estimate = estimate, expected_p = p_value),
  by = c("topic", "model", "term"))
stopifnot(nrow(comparison) == nrow(checks),
  all(sign(comparison$estimate) == if_else(comparison$expected_direction == "positive", 1, -1)),
  max(abs(comparison$p_value - comparison$expected_p)) < 1e-5,
  max(abs(comparison$estimate - comparison$expected_estimate)) < 1e-5)
write_csv(checks, here("qa", "batch6_10_R_coefficients.csv"))
# Optional residual plots: uncomment one at a time after inspecting its summary.
# par(mfrow = c(2, 2)); plot(m2_swim)
# par(mfrow = c(2, 2)); plot(m2_burnout)
# par(mfrow = c(2, 2)); plot(m2_exercise)
# par(mfrow = c(2, 2)); plot(m2_horror)
# par(mfrow = c(2, 2)); plot(m2_sleep)
