# PSY150 Fall2026 | Topics 36-40 | INSTRUCTOR ONLY
# Open Topics36_40_F26.Rproj from the extracted ZIP.
# Required packages: tidyverse, here, jtools. No package installation is run here.
library(tidyverse)
library(here)
library(jtools)
options(contrasts = c("contr.treatment", "contr.poly"))
# All values are simulated and p-value targets deliberately calibrated.
# R was unavailable in the authoring runtime; this script was not executed there.
# Python QR and an independent SVD fit checked all 15 primary models.
files <- c(
  t36 = "46306_tavakoli_steps_gpa_F26.csv",
  t37 = "46306_bohlinger_keefe_notifications_attention_F26.csv",
  t38 = "46306_guevara_music_sleep_F26.csv",
  t39 = "46306_parish_discrimination_education_F26.csv",
  t40 = "46306_barrantes_sad_music_mood_F26.csv"
)
datasets <- map(files, function(file_name) {
  raw_data <- read_csv(here("data", file_name), na = "", show_col_types = FALSE)
  stopifnot(nrow(raw_data) >= 450, nrow(raw_data) <= 1500,
    !anyDuplicated(raw_data$participant_id),
    all(colMeans(is.na(raw_data)) <= .20))
  raw_data
})
show_model <- function(model) {
  print(summ(model, digits = 3))
  tab <- coef(summary(model))
  print(tibble(term = rownames(tab),
    p_value = format.pval(tab[, 4], digits = 5, eps = 1e-12)))
}
# Use the SAME complete cases for all three primary models within a topic.
# Complete-case results are conditional on observed response; no imputation.

# TOPIC 36: Daily steps and semester GPA
# Current linked P is Calestine et al., not the different study described in the older cell note. Its self-reported GPA correlations were VPA r=-.07,p=.14; MPA r=-.02,p=.62; fitness r=-.04,p=.36. Preserve a near-null adjusted focal effect; do not invent an exercise benefit. The simulated interaction main effect p=.62 is an illustrative null target, not an estimate of a phone-step study. Workload moderation (possible low-workload benefit and high-workload time competition) and all phone measures are explicit teaching hypotheses. Keating is background on activity measurement, not a GPA meta-analysis.
covs_t36 <- c("age", "prior_gpa", "study_hours", "sleep_hours", "work_hours", "academic_workload", "financial_strain", "academic_stress", "social_support", "study_self_efficacy", "physical_health", "sleep_quality", "course_credits", "screen_hours", "commute_minutes", "campus_belonging", "time_management", "phone_carrying_consistency", "household_income_thousand", "activity_enjoyment", "gender", "race_ethnicity", "first_generation", "college_stage", "major_group", "living_arrangement", "chronic_condition", "organized_sport", "course_format", "academic_support")
t36 <- datasets$t36 %>% drop_na(all_of(c("gpa", "average_daily_step_count", covs_t36)))
t36 <- t36 %>% mutate(
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  first_generation = factor(first_generation, levels = c("no", "yes")),
  college_stage = factor(college_stage, levels = c("early", "advanced")),
  major_group = factor(major_group, levels = c("arts_social", "stem_business")),
  living_arrangement = factor(living_arrangement, levels = c("with_family", "away_from_family")),
  chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
  organized_sport = factor(organized_sport, levels = c("no", "yes")),
  course_format = factor(course_format, levels = c("in_person", "online")),
  academic_support = factor(academic_support, levels = c("no", "yes"))
)
stopifnot(nrow(t36) == 945)
m1_t36 <- lm(gpa ~ average_daily_step_count, data = t36)
show_model(m1_t36)
m2_t36 <- lm(
  gpa ~ average_daily_step_count +
    age +
    prior_gpa +
    study_hours +
    sleep_hours +
    work_hours +
    academic_workload +
    financial_strain +
    academic_stress +
    social_support +
    study_self_efficacy +
    physical_health +
    sleep_quality +
    course_credits +
    screen_hours +
    commute_minutes +
    campus_belonging +
    time_management +
    phone_carrying_consistency +
    household_income_thousand +
    activity_enjoyment +
    gender +
    race_ethnicity +
    first_generation +
    college_stage +
    major_group +
    living_arrangement +
    chronic_condition +
    organized_sport +
    course_format +
    academic_support, data = t36)
show_model(m2_t36)
t36_centered <- t36 %>% mutate(academic_workload = academic_workload - mean(academic_workload),
  average_daily_step_count = average_daily_step_count - mean(average_daily_step_count))
m3_t36 <- lm(
  gpa ~ average_daily_step_count +
    age +
    prior_gpa +
    study_hours +
    sleep_hours +
    work_hours +
    academic_workload +
    financial_strain +
    academic_stress +
    social_support +
    study_self_efficacy +
    physical_health +
    sleep_quality +
    course_credits +
    screen_hours +
    commute_minutes +
    campus_belonging +
    time_management +
    phone_carrying_consistency +
    household_income_thousand +
    activity_enjoyment +
    gender +
    race_ethnicity +
    first_generation +
    college_stage +
    major_group +
    living_arrangement +
    chronic_condition +
    organized_sport +
    course_format +
    academic_support +
    academic_workload:average_daily_step_count, data = t36_centered)
show_model(m3_t36)
models_t36 <- list(focal_only = m1_t36, adjusted = m2_t36, interaction = m3_t36)

# TOPIC 37: Phone notifications and task attention
# P N=166 analyzed; notification-by-block error interaction p<.001. Its within-arm dz values .72(call), .54(text), .17(control) are NOT between-group standardized effects. This file uses aggregate accuracy with baseline adjustment and pools notification type; 0 vs 4 is a binary allocation, not evidence of a graded frequency curve. Upshaw found an approximately 3 ms overall RT difference and phone-proneness moderation on frequent-trial RT, not on accuracy; this accuracy interaction is an explicitly hypothetical extension. Age is centered within generation in all adjusted model matrices, preserving both requested fields without unstable raw-age/cohort collinearity. Baseline performance predicts follow-up strongly.
covs_t37 <- c("age", "baseline_attention", "sleep", "other_attention_factors", "smartphone_proneness", "fatigue", "task_motivation", "test_anxiety", "daily_phone_hours", "caffeine_mg", "mindfulness", "hearing_difficulty", "visual_discomfort", "education_years", "work_hours", "room_distraction", "recent_stress", "task_familiarity", "household_income_thousand", "self_control", "gender", "race_ethnicity", "generation", "phone_os", "session_time", "regular_gaming", "shift_work", "corrected_vision", "attention_medication", "employment_status")
t37 <- datasets$t37 %>% drop_na(all_of(c("attention_span", "notification_frequency", covs_t37)))
t37 <- t37 %>% mutate(
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  generation = factor(generation, levels = c("gen_z", "millennial", "gen_x", "boomer")),
  phone_os = factor(phone_os, levels = c("ios", "android")),
  session_time = factor(session_time, levels = c("morning", "afternoon")),
  regular_gaming = factor(regular_gaming, levels = c("no", "yes")),
  shift_work = factor(shift_work, levels = c("no", "yes")),
  corrected_vision = factor(corrected_vision, levels = c("no", "yes")),
  attention_medication = factor(attention_medication, levels = c("no", "yes")),
  employment_status = factor(employment_status, levels = c("not_employed", "employed"))
)
stopifnot(nrow(t37) == 758)
# Within-generation age deviation is an equivalent full-rank reparameterization.
age_centers <- c(gen_z = 23.5, millennial = 37.5, gen_x = 53.5, boomer = 68.5)
t37 <- t37 %>% mutate(age = age - unname(age_centers[as.character(generation)]))
m1_t37 <- lm(attention_span ~ notification_frequency, data = t37)
show_model(m1_t37)
m2_t37 <- lm(
  attention_span ~ notification_frequency +
    age +
    baseline_attention +
    sleep +
    other_attention_factors +
    smartphone_proneness +
    fatigue +
    task_motivation +
    test_anxiety +
    daily_phone_hours +
    caffeine_mg +
    mindfulness +
    hearing_difficulty +
    visual_discomfort +
    education_years +
    work_hours +
    room_distraction +
    recent_stress +
    task_familiarity +
    household_income_thousand +
    self_control +
    gender +
    race_ethnicity +
    generation +
    phone_os +
    session_time +
    regular_gaming +
    shift_work +
    corrected_vision +
    attention_medication +
    employment_status, data = t37)
show_model(m2_t37)
t37_centered <- t37 %>% mutate(smartphone_proneness = smartphone_proneness - mean(smartphone_proneness),
  notification_frequency = notification_frequency - mean(notification_frequency))
m3_t37 <- lm(
  attention_span ~ notification_frequency +
    age +
    baseline_attention +
    sleep +
    other_attention_factors +
    smartphone_proneness +
    fatigue +
    task_motivation +
    test_anxiety +
    daily_phone_hours +
    caffeine_mg +
    mindfulness +
    hearing_difficulty +
    visual_discomfort +
    education_years +
    work_hours +
    room_distraction +
    recent_stress +
    task_familiarity +
    household_income_thousand +
    self_control +
    gender +
    race_ethnicity +
    generation +
    phone_os +
    session_time +
    regular_gaming +
    shift_work +
    corrected_vision +
    attention_medication +
    employment_status +
    smartphone_proneness:notification_frequency, data = t37_centered)
show_model(m3_t37)
models_t37 <- list(focal_only = m1_t37, adjusted = m2_t37, interaction = m3_t37)

# TOPIC 38: Bedtime music and sleep quality
# P week-3 PSQI music 3.27 vs audiobook 5.17; contrast p=.0004, time-by-group p<.0001. This original higher-is-better score reverses the PSQI direction; no raw-unit conversion or exact effect replication is claimed. Scullin randomized 50 adults to instrumental vs lyrical familiar pop, finding worse sleep efficiency with instrumental music (p=.037), not a universal no-music comparison. The pooled music benefit, protective/lower effect at high earworm proneness, genre/tempo offsets and heart-rate fields are pedagogical extensions. Genre and tempo are ancillary randomized allocations within music; they are not baseline covariates in the 30-variable primary model.
covs_t38 <- c("age", "baseline_sleep_quality", "work_stress", "school_stress", "insomnia", "resting_heart_rate", "earworm_proneness", "bedtime_consistency", "usual_sleep_hours", "evening_screen_minutes", "caffeine_mg", "bedroom_noise", "bedroom_comfort", "baseline_anxiety", "physical_activity", "music_enjoyment", "relaxation_expectancy", "work_hours", "household_income_thousand", "social_support", "gender", "race_ethnicity", "parasomnia", "shift_work", "sleep_medication", "shared_bedroom", "preferred_genre", "preferred_tempo", "regular_bedtime_music", "student_status")
t38 <- datasets$t38 %>% drop_na(all_of(c("sleep_quality", "music_before_sleep", covs_t38)))
t38 <- t38 %>% mutate(
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  parasomnia = factor(parasomnia, levels = c("no", "yes")),
  shift_work = factor(shift_work, levels = c("no", "yes")),
  sleep_medication = factor(sleep_medication, levels = c("no", "yes")),
  shared_bedroom = factor(shared_bedroom, levels = c("no", "yes")),
  preferred_genre = factor(preferred_genre, levels = c("classical", "ambient", "familiar_pop")),
  preferred_tempo = factor(preferred_tempo, levels = c("slow", "moderate")),
  regular_bedtime_music = factor(regular_bedtime_music, levels = c("no", "yes")),
  student_status = factor(student_status, levels = c("student", "not_student"))
)
stopifnot(nrow(t38) == 662)
m1_t38 <- lm(sleep_quality ~ music_before_sleep, data = t38)
show_model(m1_t38)
m2_t38 <- lm(
  sleep_quality ~ music_before_sleep +
    age +
    baseline_sleep_quality +
    work_stress +
    school_stress +
    insomnia +
    resting_heart_rate +
    earworm_proneness +
    bedtime_consistency +
    usual_sleep_hours +
    evening_screen_minutes +
    caffeine_mg +
    bedroom_noise +
    bedroom_comfort +
    baseline_anxiety +
    physical_activity +
    music_enjoyment +
    relaxation_expectancy +
    work_hours +
    household_income_thousand +
    social_support +
    gender +
    race_ethnicity +
    parasomnia +
    shift_work +
    sleep_medication +
    shared_bedroom +
    preferred_genre +
    preferred_tempo +
    regular_bedtime_music +
    student_status, data = t38)
show_model(m2_t38)
t38_centered <- t38 %>% mutate(earworm_proneness = earworm_proneness - mean(earworm_proneness),
  music_before_sleep = music_before_sleep - mean(music_before_sleep))
m3_t38 <- lm(
  sleep_quality ~ music_before_sleep +
    age +
    baseline_sleep_quality +
    work_stress +
    school_stress +
    insomnia +
    resting_heart_rate +
    earworm_proneness +
    bedtime_consistency +
    usual_sleep_hours +
    evening_screen_minutes +
    caffeine_mg +
    bedroom_noise +
    bedroom_comfort +
    baseline_anxiety +
    physical_activity +
    music_enjoyment +
    relaxation_expectancy +
    work_hours +
    household_income_thousand +
    social_support +
    gender +
    race_ethnicity +
    parasomnia +
    shift_work +
    sleep_medication +
    shared_bedroom +
    preferred_genre +
    preferred_tempo +
    regular_bedtime_music +
    student_status +
    earworm_proneness:music_before_sleep, data = t38_centered)
show_model(m3_t38)
models_t38 <- list(focal_only = m1_t38, adjusted = m2_t38, interaction = m3_t38)

# TOPIC 39: Perceived discrimination and educational pathways
# P N=290 Swedish pre-service teachers rated an L2 guise MORE favorably on most outcomes; this is not evidence for a negative Irish attainment effect. Smyth examined 12 Irish schools and higher-level subject uptake, not college attendance. Therefore the negative perceived-discrimination slope, school-support buffering interaction, college probabilities and academic index are all hypothetical. Neither source identifies these causal effects. The dataset uses one pupil per distinct fictional school to avoid pretending independent observations from clustered source schools. DEIS/private categories are imperfect operationalizations of the student proposal; school social mix, patronage and location remain separate fields.
covs_t39 <- c("age", "prior_achievement", "socioeconomic_status", "school_support", "family_support", "study_hours", "financial_strain", "parent_education_years", "baseline_attendance", "academic_self_efficacy", "school_belonging", "teacher_expectations", "transport_difficulty", "home_study_space", "peer_support", "language_confidence", "baseline_stress", "paid_work_hours", "college_information", "health_limitations", "gender", "school_location", "religious_denomination", "school_type", "first_generation_college", "home_language", "learning_support", "broadband_access", "care_responsibilities", "school_year")
t39 <- datasets$t39 %>% drop_na(all_of(c("educational_outcomes_and_college_attendance", "linguistic_and_class_discrimination", covs_t39)))
t39 <- t39 %>% mutate(
  gender = factor(gender, levels = c("girl", "boy", "nonbinary_or_self_described")),
  school_location = factor(school_location, levels = c("dublin", "other_urban", "rural")),
  religious_denomination = factor(religious_denomination, levels = c("catholic", "other_faith", "multidenominational_or_none")),
  school_type = factor(school_type, levels = c("deis", "other_public", "fee_paying")),
  first_generation_college = factor(first_generation_college, levels = c("no", "yes")),
  home_language = factor(home_language, levels = c("english", "irish_or_other")),
  learning_support = factor(learning_support, levels = c("no", "yes")),
  broadband_access = factor(broadband_access, levels = c("no", "yes")),
  care_responsibilities = factor(care_responsibilities, levels = c("no", "yes")),
  school_year = factor(school_year, levels = c("penultimate", "final"))
)
stopifnot(nrow(t39) == 547)
m1_t39 <- lm(educational_outcomes_and_college_attendance ~ linguistic_and_class_discrimination, data = t39)
show_model(m1_t39)
m2_t39 <- lm(
  educational_outcomes_and_college_attendance ~ linguistic_and_class_discrimination +
    age +
    prior_achievement +
    socioeconomic_status +
    school_support +
    family_support +
    study_hours +
    financial_strain +
    parent_education_years +
    baseline_attendance +
    academic_self_efficacy +
    school_belonging +
    teacher_expectations +
    transport_difficulty +
    home_study_space +
    peer_support +
    language_confidence +
    baseline_stress +
    paid_work_hours +
    college_information +
    health_limitations +
    gender +
    school_location +
    religious_denomination +
    school_type +
    first_generation_college +
    home_language +
    learning_support +
    broadband_access +
    care_responsibilities +
    school_year, data = t39)
show_model(m2_t39)
t39_centered <- t39 %>% mutate(school_support = school_support - mean(school_support),
  linguistic_and_class_discrimination = linguistic_and_class_discrimination - mean(linguistic_and_class_discrimination))
m3_t39 <- lm(
  educational_outcomes_and_college_attendance ~ linguistic_and_class_discrimination +
    age +
    prior_achievement +
    socioeconomic_status +
    school_support +
    family_support +
    study_hours +
    financial_strain +
    parent_education_years +
    baseline_attendance +
    academic_self_efficacy +
    school_belonging +
    teacher_expectations +
    transport_difficulty +
    home_study_space +
    peer_support +
    language_confidence +
    baseline_stress +
    paid_work_hours +
    college_information +
    health_limitations +
    gender +
    school_location +
    religious_denomination +
    school_type +
    first_generation_college +
    home_language +
    learning_support +
    broadband_access +
    care_responsibilities +
    school_year +
    school_support:linguistic_and_class_discrimination, data = t39_centered)
show_model(m3_t39)
models_t39 <- list(focal_only = m1_t39, adjusted = m2_t39, interaction = m3_t39)

# TOPIC 40: Sad music and mood across listening occasions
# P N=177: immediate happy music time effect p=.001, sad p=.24; immediate rumination interactions happy p=.62, sad p=.47. Diary sad-music impact varied with rumination across weeks (multivariate p=.01; final diary p=.002). Whole-month mood changes were not significant. Primary final-session moderation is a teaching adaptation of the later diary pattern, not a claim that the first-session POMS interaction was significant. First-session companion fields preserve happy improvement and no detectable sad improvement; global change is near-null. Taruffi is a self-selected N=772 survey, not a randomized mood-treatment trial. Added context magnitudes and all original scales are hypothetical.
covs_t40 <- c("age", "baseline_mood", "frequency_of_sad_music_use", "rumination", "emotion_regulation", "social_support", "loneliness", "aesthetic_engagement", "empathy", "nostalgia_proneness", "music_enjoyment", "baseline_stress", "sleep_hours", "music_familiarity", "improvement_expectancy", "musical_training", "recent_loss", "daily_music_minutes", "household_income_thousand", "coping_confidence", "gender", "race_ethnicity", "student_status", "current_counseling", "prior_music_therapy", "listening_device", "session_time", "living_arrangement", "preferred_genre", "regular_group_music")
t40 <- datasets$t40 %>% drop_na(all_of(c("mood_after_listening", "sad_music_listening", covs_t40)))
t40 <- t40 %>% mutate(
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  student_status = factor(student_status, levels = c("no", "yes")),
  current_counseling = factor(current_counseling, levels = c("no", "yes")),
  prior_music_therapy = factor(prior_music_therapy, levels = c("no", "yes")),
  listening_device = factor(listening_device, levels = c("headphones", "speakers")),
  session_time = factor(session_time, levels = c("daytime", "evening")),
  living_arrangement = factor(living_arrangement, levels = c("with_others", "alone")),
  preferred_genre = factor(preferred_genre, levels = c("pop", "classical", "other")),
  regular_group_music = factor(regular_group_music, levels = c("no", "yes"))
)
stopifnot(nrow(t40) == 1152)
m1_t40 <- lm(mood_after_listening ~ sad_music_listening, data = t40)
show_model(m1_t40)
m2_t40 <- lm(
  mood_after_listening ~ sad_music_listening +
    age +
    baseline_mood +
    frequency_of_sad_music_use +
    rumination +
    emotion_regulation +
    social_support +
    loneliness +
    aesthetic_engagement +
    empathy +
    nostalgia_proneness +
    music_enjoyment +
    baseline_stress +
    sleep_hours +
    music_familiarity +
    improvement_expectancy +
    musical_training +
    recent_loss +
    daily_music_minutes +
    household_income_thousand +
    coping_confidence +
    gender +
    race_ethnicity +
    student_status +
    current_counseling +
    prior_music_therapy +
    listening_device +
    session_time +
    living_arrangement +
    preferred_genre +
    regular_group_music, data = t40)
show_model(m2_t40)
t40_centered <- t40 %>% mutate(rumination = rumination - mean(rumination),
  sad_music_listening = sad_music_listening - mean(sad_music_listening))
m3_t40 <- lm(
  mood_after_listening ~ sad_music_listening +
    age +
    baseline_mood +
    frequency_of_sad_music_use +
    rumination +
    emotion_regulation +
    social_support +
    loneliness +
    aesthetic_engagement +
    empathy +
    nostalgia_proneness +
    music_enjoyment +
    baseline_stress +
    sleep_hours +
    music_familiarity +
    improvement_expectancy +
    musical_training +
    recent_loss +
    daily_music_minutes +
    household_income_thousand +
    coping_confidence +
    gender +
    race_ethnicity +
    student_status +
    current_counseling +
    prior_music_therapy +
    listening_device +
    session_time +
    living_arrangement +
    preferred_genre +
    regular_group_music +
    rumination:sad_music_listening, data = t40_centered)
show_model(m3_t40)
models_t40 <- list(focal_only = m1_t40, adjusted = m2_t40, interaction = m3_t40)
models <- list(`36` = models_t36, `37` = models_t37, `38` = models_t38, `39` = models_t39, `40` = models_t40)

# NUMERIC CHECKS AGAINST SAVED PYTHON FITS
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
reference <- read_csv(here("qa", "topics36_40_regression_audit.csv"), show_col_types = FALSE)
comparison <- audit %>% mutate(key = normalize_term(term)) %>%
  inner_join(reference %>% mutate(key = normalize_term(term)),
    by = c("topic", "model", "key"), suffix = c("_R", "_Python"))
stopifnot(nrow(comparison) == nrow(audit),
  max(abs(comparison$estimate_R - comparison$estimate_Python)) < 1e-5,
  max(abs(comparison$std_error_R - comparison$std_error_Python)) < 1e-5,
  max(abs(comparison$p_value_R - comparison$p_value_Python)) < 1e-5)
write_csv(audit, here("qa", "topics36_40_R_regression_audit.csv"))
block_tests <- function(m, variables, topic_id, model_name) {
  labels <- attr(terms(m), "term.labels")
  assignment <- attr(model.matrix(m), "assign")
  map_dfr(variables, function(v) {
    ix <- which(assignment == match(v, labels))
    b <- coef(m)[ix]; V <- vcov(m)[ix, ix, drop = FALSE]
    W <- as.numeric(t(b) %*% solve(V, b)); k <- length(ix)
    pv <- pf(W / k, k, df.residual(m), lower.tail = FALSE)
    tibble(topic = topic_id, model = model_name, covariate = v, p_value = pv,
      significant = pv < .05, near_threshold = pv > .05 & pv <= .10)
  })
}
checks <- bind_rows(
  block_tests(m2_t36, covs_t36, 36, "adjusted"),
  block_tests(m3_t36, covs_t36, 36, "interaction"),
  block_tests(m2_t37, covs_t37, 37, "adjusted"),
  block_tests(m3_t37, covs_t37, 37, "interaction"),
  block_tests(m2_t38, covs_t38, 38, "adjusted"),
  block_tests(m3_t38, covs_t38, 38, "interaction"),
  block_tests(m2_t39, covs_t39, 39, "adjusted"),
  block_tests(m3_t39, covs_t39, 39, "interaction"),
  block_tests(m2_t40, covs_t40, 40, "adjusted"),
  block_tests(m3_t40, covs_t40, 40, "interaction")
)
check_summary <- checks %>% group_by(topic, model) %>%
  summarise(significant_covariates = sum(significant),
    nonsignificant_near_fraction = mean(near_threshold[!significant]), .groups = "drop")
print(check_summary)
stopifnot(all(check_summary$significant_covariates >= 5),
  all(check_summary$nonsignificant_near_fraction >= .70))
write_csv(checks, here("qa", "topics36_40_R_covariate_tests.csv"))
expected <- read_csv(here("qa", "topics36_40_direction_audit.csv"), show_col_types = FALSE)
directions <- expected %>% mutate(key = normalize_term(term)) %>%
  inner_join(audit %>% mutate(key = normalize_term(term)),
    by = c("topic", "model", "key"), suffix = c("_expected", "_R"))
stopifnot(nrow(directions) == nrow(expected),
  all(directions$expected_sign == 0 | sign(directions$estimate_R) == directions$expected_sign))
# Missingness remains in raw datasets; never replace blanks with zero.
missingness <- imap_dfr(datasets, function(d, id) {
  tibble(topic = id, variable = names(d), missing = colSums(is.na(d)),
    missing_rate = colMeans(is.na(d)))
})
write_csv(missingness, here("qa", "topics36_40_R_missingness.csv"))

# COMPANION OUTCOMES: different questions, outside the calibrated primary models.
# Topic 38: randomized genre/tempo comparisons apply only within the music arm.
t38_music <- datasets$t38 %>% filter(music_before_sleep == 1) %>%
  mutate(playlist_genre = factor(playlist_genre, levels = c("classical", "ambient", "familiar_pop")))
m4_t38 <- lm(sleep_quality ~ baseline_sleep_quality + playlist_genre +
  playlist_tempo_bpm + earworm_proneness, data = t38_music)
show_model(m4_t38)
# Topic 39: the separate binary college endpoint is not the continuous main score.
m4_t39 <- glm(college_attendance ~ linguistic_and_class_discrimination +
  prior_achievement + socioeconomic_status + school_support,
  family = binomial(), data = datasets$t39)
show_model(m4_t39)
# Topic 40: first-session and whole-month outcomes must not be conflated.
first_changes <- datasets$t40 %>% mutate(change = first_session_mood_after - first_session_mood_before)
global_changes <- datasets$t40 %>% mutate(change = global_mood_week4 - global_mood_baseline)
print(t.test(first_changes$change[first_changes$sad_music_listening == 0], mu = 0))
print(t.test(first_changes$change[first_changes$sad_music_listening == 1], mu = 0))
print(t.test(global_changes$change[global_changes$sad_music_listening == 0], mu = 0))
print(t.test(global_changes$change[global_changes$sad_music_listening == 1], mu = 0))
print(datasets$t40 %>% count(playlist_condition, mood_change_category, .drop = FALSE))
