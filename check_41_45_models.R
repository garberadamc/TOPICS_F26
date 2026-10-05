# PSY150 Fall2026 | Topics 41-45 | INSTRUCTOR ONLY
# Extract the ZIP and open Topics41_45_F26.Rproj.
# Requires tidyverse, here, jtools; no installation is run by this script.
library(tidyverse)
library(here)
library(jtools)
options(contrasts = c("contr.treatment", "contr.poly"))
# Artificial classroom calibration, not empirical discoveries.
# R was unavailable in the authoring runtime; this script was not executed there.
# All 15 main models were independently checked using Python QR and SVD.
files <- c(
  t41 = "46306_mora_team_sports_belonging_F26.csv",
  t42 = "46306_withrington_notifications_performance_F26.csv",
  t43 = "46306_zicari_language_cognition_F26.csv",
  t44 = "46306_aslanian_concussion_cognition_F26.csv",
  t45 = "46306_balleza_pornography_self_image_F26.csv"
)
datasets <- map(files, function(file_name) {
  raw_data <- read_csv(here("data", file_name), na = "", show_col_types = FALSE)
  stopifnot(nrow(raw_data) >= 300, nrow(raw_data) <= 800,
    !anyDuplicated(raw_data$participant_id), all(colMeans(is.na(raw_data)) <= .10))
  raw_data
})
show_model <- function(model) {
  print(summ(model, digits = 3))
  tab <- coef(summary(model))
  print(tibble(term = rownames(tab), p_value = format.pval(tab[,4], digits=5, eps=1e-12)))
}
# The same complete cases are used for all three models within each topic.

# TOPIC 41: Team sports, confidence and belonging
# P: observational Add Health N=10,500, adjusted sport beta=.03,p<.01; boys=.04,p<.01, girls=-.004,p=.74. P excluded no-sport respondents and included individual sport. Binary team/no-team comparison and original confidence/belonging measures are extensions. Small positive focal target p=.048 is illustrative, not a replicated coefficient. Reinboth/Duda N=265 male adolescent athletes: toxicity/ego-climate association with self-esteem more negative at low perceived ability; climate-by-ability b=.08,t=2.0. The positive covariate-by-covariate interaction follows that buffering pattern. Neither study warrants causal team-sport claims.
covs_t41 <- c("age", "baseline_confidence", "baseline_belonging", "team_environment_toxicity", "perceived_sport_ability", "peer_acceptance", "mastery_climate", "family_support", "academic_stress", "body_confidence", "social_anxiety", "coach_accessibility", "school_safety", "bullying_exposure", "physical_health", "friendship_quality", "emotion_regulation", "sleep_hours", "other_activity_hours", "household_income_thousand", "gender", "race_ethnicity", "school_stage", "school_sector", "household_structure", "urbanicity", "disability_support", "prior_team_sport", "club_membership", "transport_access")
t41 <- datasets$t41 %>% drop_na(all_of(c("self_confidence_and_belonging", "team_sports_participation", covs_t41)))
t41 <- t41 %>% mutate(
  gender = factor(gender, levels = c("girl_or_woman", "boy_or_man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  school_stage = factor(school_stage, levels = c("middle", "high")),
  school_sector = factor(school_sector, levels = c("public", "independent")),
  household_structure = factor(household_structure, levels = c("two_caregivers", "other_arrangement")),
  urbanicity = factor(urbanicity, levels = c("urban", "suburban", "rural")),
  disability_support = factor(disability_support, levels = c("no", "yes")),
  prior_team_sport = factor(prior_team_sport, levels = c("no", "yes")),
  club_membership = factor(club_membership, levels = c("no", "yes")),
  transport_access = factor(transport_access, levels = c("no", "yes"))
)
stopifnot(nrow(t41) == 439)
m1_t41 <- lm(self_confidence_and_belonging ~ team_sports_participation, data = t41)
show_model(m1_t41)
m2_t41 <- lm(
  self_confidence_and_belonging ~ team_sports_participation +
    age +
    baseline_confidence +
    baseline_belonging +
    team_environment_toxicity +
    perceived_sport_ability +
    peer_acceptance +
    mastery_climate +
    family_support +
    academic_stress +
    body_confidence +
    social_anxiety +
    coach_accessibility +
    school_safety +
    bullying_exposure +
    physical_health +
    friendship_quality +
    emotion_regulation +
    sleep_hours +
    other_activity_hours +
    household_income_thousand +
    gender +
    race_ethnicity +
    school_stage +
    school_sector +
    household_structure +
    urbanicity +
    disability_support +
    prior_team_sport +
    club_membership +
    transport_access, data = t41)
show_model(m2_t41)
t41_centered <- t41 %>% mutate(perceived_sport_ability = perceived_sport_ability - mean(perceived_sport_ability),
  team_environment_toxicity = team_environment_toxicity - mean(team_environment_toxicity))
m3_t41 <- lm(
  self_confidence_and_belonging ~ team_sports_participation +
    age +
    baseline_confidence +
    baseline_belonging +
    team_environment_toxicity +
    perceived_sport_ability +
    peer_acceptance +
    mastery_climate +
    family_support +
    academic_stress +
    body_confidence +
    social_anxiety +
    coach_accessibility +
    school_safety +
    bullying_exposure +
    physical_health +
    friendship_quality +
    emotion_regulation +
    sleep_hours +
    other_activity_hours +
    household_income_thousand +
    gender +
    race_ethnicity +
    school_stage +
    school_sector +
    household_structure +
    urbanicity +
    disability_support +
    prior_team_sport +
    club_membership +
    transport_access +
    perceived_sport_ability:team_environment_toxicity, data = t41_centered)
show_model(m3_t41)
models_t41 <- list(focal_only = m1_t41, adjusted = m2_t41, interaction = m3_t41)

# TOPIC 42: Daily notifications, performance and fatigue
# P N=247, 44.5% employees; self-report interruption->performance b=-.23,SE=.08,p=.01; interruption->strain b=.13,p=.04. Direct assignment effect on performance b=-.12,p=.41. FOMO moderation b=+.21,p=.05, with weaker negative interruption slopes at higher FOMO, not stronger harm. This simulation uses logged delivered counts, objective accuracy and fatigue: scale and measurement extensions, not a coefficient conversion. Focal p=.009 and positive moderation p=.048 illustrate the reported pattern; extreme-context estimates may be uncertain. Setting main effect p=.41. Disorder association is a teaching assumption; medication sign is unconstrained because of indication confounding.
covs_t42 <- c("age", "baseline_accuracy", "baseline_fatigue", "sleep_hours", "work_hours", "screen_hours", "caffeine_mg", "household_income_thousand", "fear_of_missing_out", "telepressure", "workload", "task_interdependence", "task_motivation", "attention_control", "work_stress", "phone_checking_habit", "workspace_quiet", "task_familiarity", "visual_discomfort", "autonomy", "gender", "race_ethnicity", "attention_disorder_status", "medication_status", "notification_setting", "phone_os", "work_location", "job_sector", "shift_work", "education_level")
t42 <- datasets$t42 %>% drop_na(all_of(c("task_performance_and_cognitive_fatigue", "daily_notification_frequency", covs_t42)))
t42 <- t42 %>% mutate(
  gender = factor(gender, levels = c("girl_or_woman", "boy_or_man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  attention_disorder_status = factor(attention_disorder_status, levels = c("no", "yes")),
  medication_status = factor(medication_status, levels = c("none", "current")),
  notification_setting = factor(notification_setting, levels = c("usual", "disabled")),
  phone_os = factor(phone_os, levels = c("ios", "android")),
  work_location = factor(work_location, levels = c("onsite", "remote")),
  job_sector = factor(job_sector, levels = c("service", "office", "technical")),
  shift_work = factor(shift_work, levels = c("no", "yes")),
  education_level = factor(education_level, levels = c("secondary", "college", "postgraduate"))
)
stopifnot(nrow(t42) == 558)
m1_t42 <- lm(task_performance_and_cognitive_fatigue ~ daily_notification_frequency, data = t42)
show_model(m1_t42)
m2_t42 <- lm(
  task_performance_and_cognitive_fatigue ~ daily_notification_frequency +
    age +
    baseline_accuracy +
    baseline_fatigue +
    sleep_hours +
    work_hours +
    screen_hours +
    caffeine_mg +
    household_income_thousand +
    fear_of_missing_out +
    telepressure +
    workload +
    task_interdependence +
    task_motivation +
    attention_control +
    work_stress +
    phone_checking_habit +
    workspace_quiet +
    task_familiarity +
    visual_discomfort +
    autonomy +
    gender +
    race_ethnicity +
    attention_disorder_status +
    medication_status +
    notification_setting +
    phone_os +
    work_location +
    job_sector +
    shift_work +
    education_level, data = t42)
show_model(m2_t42)
t42_centered <- t42 %>% mutate(fear_of_missing_out = fear_of_missing_out - mean(fear_of_missing_out),
  daily_notification_frequency = daily_notification_frequency - mean(daily_notification_frequency))
m3_t42 <- lm(
  task_performance_and_cognitive_fatigue ~ daily_notification_frequency +
    age +
    baseline_accuracy +
    baseline_fatigue +
    sleep_hours +
    work_hours +
    screen_hours +
    caffeine_mg +
    household_income_thousand +
    fear_of_missing_out +
    telepressure +
    workload +
    task_interdependence +
    task_motivation +
    attention_control +
    work_stress +
    phone_checking_habit +
    workspace_quiet +
    task_familiarity +
    visual_discomfort +
    autonomy +
    gender +
    race_ethnicity +
    attention_disorder_status +
    medication_status +
    notification_setting +
    phone_os +
    work_location +
    job_sector +
    shift_work +
    education_level +
    fear_of_missing_out:daily_notification_frequency, data = t42_centered)
show_model(m3_t42)
models_t42 <- list(focal_only = m1_t42, adjusted = m2_t42, interaction = m3_t42)

# TOPIC 43: Language and judgments of duration
# P medium stimuli: Swedish>Spanish interference for lines (p=.001), Spanish>Swedish for containers (p=.009); language differences absent at extremes and without verbal prompts. Main language effect averaged over displays is near-null by design; display-by-language interaction p=.003 produces the crossover. This is a simplified between-person teaching design; not the exact repeated-trial experiment. No grammatical-gender hypothesis, general intelligence claim or invented Indigenous participant records. Negative timing-precision/working-memory associations are plausible measurement-control assumptions, not estimates published in P.
covs_t43 <- c("age", "stimulus_type", "baseline_timing_precision", "working_memory", "attention_control", "education_years", "sleep_hours", "daily_second_language_hours", "response_latency_ms", "household_income_thousand", "linguistic_automaticity", "task_familiarity", "test_anxiety", "fatigue", "visual_discomfort", "music_training", "spatial_navigation", "numeracy_confidence", "task_motivation", "distraction_sensitivity", "gender", "race_ethnicity", "prompt_order", "urbanicity", "bilingual_use", "corrected_vision", "testing_time", "employment_status", "household_structure", "site_region")
t43 <- datasets$t43 %>% drop_na(all_of(c("cognitive_function_and_time_perception", "language_structure", covs_t43)))
t43 <- t43 %>% mutate(
  gender = factor(gender, levels = c("girl_or_woman", "boy_or_man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  prompt_order = factor(prompt_order, levels = c("verbal_first", "nonverbal_first")),
  urbanicity = factor(urbanicity, levels = c("urban", "suburban", "rural")),
  bilingual_use = factor(bilingual_use, levels = c("no", "yes")),
  corrected_vision = factor(corrected_vision, levels = c("no", "yes")),
  testing_time = factor(testing_time, levels = c("morning", "afternoon")),
  employment_status = factor(employment_status, levels = c("student", "employed", "other")),
  household_structure = factor(household_structure, levels = c("alone", "with_others")),
  site_region = factor(site_region, levels = c("region_a", "region_b"))
)
stopifnot(nrow(t43) == 327)
m1_t43 <- lm(cognitive_function_and_time_perception ~ language_structure, data = t43)
show_model(m1_t43)
m2_t43 <- lm(
  cognitive_function_and_time_perception ~ language_structure +
    age +
    stimulus_type +
    baseline_timing_precision +
    working_memory +
    attention_control +
    education_years +
    sleep_hours +
    daily_second_language_hours +
    response_latency_ms +
    household_income_thousand +
    linguistic_automaticity +
    task_familiarity +
    test_anxiety +
    fatigue +
    visual_discomfort +
    music_training +
    spatial_navigation +
    numeracy_confidence +
    task_motivation +
    distraction_sensitivity +
    gender +
    race_ethnicity +
    prompt_order +
    urbanicity +
    bilingual_use +
    corrected_vision +
    testing_time +
    employment_status +
    household_structure +
    site_region, data = t43)
show_model(m2_t43)
t43_centered <- t43 %>% mutate(stimulus_type = stimulus_type - mean(stimulus_type),
  language_structure = language_structure - mean(language_structure))
m3_t43 <- lm(
  cognitive_function_and_time_perception ~ language_structure +
    age +
    stimulus_type +
    baseline_timing_precision +
    working_memory +
    attention_control +
    education_years +
    sleep_hours +
    daily_second_language_hours +
    response_latency_ms +
    household_income_thousand +
    linguistic_automaticity +
    task_familiarity +
    test_anxiety +
    fatigue +
    visual_discomfort +
    music_training +
    spatial_navigation +
    numeracy_confidence +
    task_motivation +
    distraction_sensitivity +
    gender +
    race_ethnicity +
    prompt_order +
    urbanicity +
    bilingual_use +
    corrected_vision +
    testing_time +
    employment_status +
    household_structure +
    site_region +
    stimulus_type:language_structure, data = t43_centered)
show_model(m3_t43)
models_t43 <- list(focal_only = m1_t43, adjusted = m2_t43, interaction = m3_t43)

# TOPIC 44: Childhood concussion and cognitive outcomes
# P N=866, ages 8-16.99, concussion versus orthopedic injury; no clinically meaningful IQ deficit. Prior concussion history, symptoms and acute features were not associated with IQ (p>.09); group interactions with age, sex and SES were not significant. Preserve small, nonsignificant focal p=.32 and age interaction p=.45; do not tune an IQ deficit into significance. SES and pre-injury achievement are positive. Added SES reference supports background socioeconomic/developmental context, not injury causation. Domain-specific attention/learning variations are explicitly hypothetical, not claims that the review establishes a universal deficit.
covs_t44 <- c("age", "socioeconomic_status", "prior_academic_score", "parent_education_years", "sleep_hours", "assessment_delay_days", "school_absence_days", "screen_hours", "household_income_thousand", "learning_enrichment", "testing_engagement", "family_support", "school_support", "baseline_attention_difficulty", "baseline_anxiety", "baseline_fatigue", "reading_habit", "physical_health", "test_familiarity", "caregiver_stress", "gender", "race_ethnicity", "study_setting", "school_sector", "urbanicity", "learning_support", "attention_diagnosis", "handedness", "household_structure", "injury_activity")
t44 <- datasets$t44 %>% drop_na(all_of(c("cognitive_functioning", "childhood_concussion_history", covs_t44)))
t44 <- t44 %>% mutate(
  gender = factor(gender, levels = c("girl_or_woman", "boy_or_man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  study_setting = factor(study_setting, levels = c("setting_a", "setting_b")),
  school_sector = factor(school_sector, levels = c("public", "independent")),
  urbanicity = factor(urbanicity, levels = c("urban", "suburban", "rural")),
  learning_support = factor(learning_support, levels = c("no", "yes")),
  attention_diagnosis = factor(attention_diagnosis, levels = c("no", "yes")),
  handedness = factor(handedness, levels = c("right", "left_or_mixed")),
  household_structure = factor(household_structure, levels = c("two_caregivers", "other_arrangement")),
  injury_activity = factor(injury_activity, levels = c("sport", "play_or_fall", "other"))
)
stopifnot(nrow(t44) == 669)
m1_t44 <- lm(cognitive_functioning ~ childhood_concussion_history, data = t44)
show_model(m1_t44)
m2_t44 <- lm(
  cognitive_functioning ~ childhood_concussion_history +
    age +
    socioeconomic_status +
    prior_academic_score +
    parent_education_years +
    sleep_hours +
    assessment_delay_days +
    school_absence_days +
    screen_hours +
    household_income_thousand +
    learning_enrichment +
    testing_engagement +
    family_support +
    school_support +
    baseline_attention_difficulty +
    baseline_anxiety +
    baseline_fatigue +
    reading_habit +
    physical_health +
    test_familiarity +
    caregiver_stress +
    gender +
    race_ethnicity +
    study_setting +
    school_sector +
    urbanicity +
    learning_support +
    attention_diagnosis +
    handedness +
    household_structure +
    injury_activity, data = t44)
show_model(m2_t44)
t44_centered <- t44 %>% mutate(age = age - mean(age),
  childhood_concussion_history = childhood_concussion_history - mean(childhood_concussion_history))
m3_t44 <- lm(
  cognitive_functioning ~ childhood_concussion_history +
    age +
    socioeconomic_status +
    prior_academic_score +
    parent_education_years +
    sleep_hours +
    assessment_delay_days +
    school_absence_days +
    screen_hours +
    household_income_thousand +
    learning_enrichment +
    testing_engagement +
    family_support +
    school_support +
    baseline_attention_difficulty +
    baseline_anxiety +
    baseline_fatigue +
    reading_habit +
    physical_health +
    test_familiarity +
    caregiver_stress +
    gender +
    race_ethnicity +
    study_setting +
    school_sector +
    urbanicity +
    learning_support +
    attention_diagnosis +
    handedness +
    household_structure +
    injury_activity +
    age:childhood_concussion_history, data = t44_centered)
show_model(m3_t44)
models_t44 <- list(focal_only = m1_t44, adjusted = m2_t44, interaction = m3_t44)

# TOPIC 45: Media exposure and body appreciation over time
# P baseline N=2904, three annual waves (24 months elapsed), matched all-wave N=1407. Within-group baseline correlations were negative for heterosexual cisgender girls and SGM boys; no longitudinal coupling in any modeled group. Do not force universal harm or interpret positive pooled correlation as benefit. Follow-up focal target p=.70; identity-by-exposure joint p=.52, all small slopes. Counts 0-30 replace source ordinal frequency and original items replace the BAS; this is not replication. Gender-diverse oversampling and 0-10% missingness are teaching extensions; source attrition was larger. Age first exposure uses 0 for never and is centered at 12 only among ever-exposed, with lifetime_exposure separately adjusted to avoid treating zero as infancy.
covs_t45 <- c("age", "age_at_first_exposure", "baseline_self_image", "sleep_hours", "screen_hours", "pubertal_development", "household_income_thousand", "family_support", "peer_support", "body_comparison_pressure", "bullying_exposure", "self_compassion", "media_literacy", "appearance_ideal_internalization", "social_media_comparison", "general_stress", "physical_health", "relationship_security", "sensation_seeking", "privacy_comfort", "identity_group", "lifetime_exposure", "race_ethnicity", "urbanicity", "school_sector", "household_structure", "media_education", "private_device", "relationship_status", "followup_mode")
t45 <- datasets$t45 %>% drop_na(all_of(c("self_image", "pornography_exposure", covs_t45)))
t45 <- t45 %>% mutate(
  identity_group = factor(identity_group, levels = c("hc_boys", "hc_girls", "sgm_boys", "sgm_girls", "gender_diverse")),
  lifetime_exposure = factor(lifetime_exposure, levels = c("never", "ever")),
  race_ethnicity = factor(race_ethnicity, levels = c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  urbanicity = factor(urbanicity, levels = c("urban", "suburban", "rural")),
  school_sector = factor(school_sector, levels = c("public", "independent")),
  household_structure = factor(household_structure, levels = c("two_caregivers", "other_arrangement")),
  media_education = factor(media_education, levels = c("no", "yes")),
  private_device = factor(private_device, levels = c("no", "yes")),
  relationship_status = factor(relationship_status, levels = c("not_partnered", "partnered")),
  followup_mode = factor(followup_mode, levels = c("online", "in_person"))
)
stopifnot(nrow(t45) == 706)
# Zero means confirmed never exposed, not infancy. Lifetime group is also adjusted.
t45 <- t45 %>% mutate(age_at_first_exposure = if_else(age_at_first_exposure == 0, 0, age_at_first_exposure - 12))
m1_t45 <- lm(self_image ~ pornography_exposure, data = t45)
show_model(m1_t45)
m2_t45 <- lm(
  self_image ~ pornography_exposure +
    age +
    age_at_first_exposure +
    baseline_self_image +
    sleep_hours +
    screen_hours +
    pubertal_development +
    household_income_thousand +
    family_support +
    peer_support +
    body_comparison_pressure +
    bullying_exposure +
    self_compassion +
    media_literacy +
    appearance_ideal_internalization +
    social_media_comparison +
    general_stress +
    physical_health +
    relationship_security +
    sensation_seeking +
    privacy_comfort +
    identity_group +
    lifetime_exposure +
    race_ethnicity +
    urbanicity +
    school_sector +
    household_structure +
    media_education +
    private_device +
    relationship_status +
    followup_mode, data = t45)
show_model(m2_t45)
t45_centered <- t45 %>% mutate(pornography_exposure = pornography_exposure - mean(pornography_exposure))
m3_t45 <- lm(
  self_image ~ pornography_exposure +
    age +
    age_at_first_exposure +
    baseline_self_image +
    sleep_hours +
    screen_hours +
    pubertal_development +
    household_income_thousand +
    family_support +
    peer_support +
    body_comparison_pressure +
    bullying_exposure +
    self_compassion +
    media_literacy +
    appearance_ideal_internalization +
    social_media_comparison +
    general_stress +
    physical_health +
    relationship_security +
    sensation_seeking +
    privacy_comfort +
    identity_group +
    lifetime_exposure +
    race_ethnicity +
    urbanicity +
    school_sector +
    household_structure +
    media_education +
    private_device +
    relationship_status +
    followup_mode +
    identity_group:pornography_exposure, data = t45_centered)
show_model(m3_t45)
models_t45 <- list(focal_only = m1_t45, adjusted = m2_t45, interaction = m3_t45)
models <- list(`41` = models_t41, `42` = models_t42, `43` = models_t43, `44` = models_t44, `45` = models_t45)

audit <- imap_dfr(models, function(batch, topic_id) {
  imap_dfr(batch, function(m, model_name) {
    tab <- coef(summary(m))
    tibble(topic=as.integer(topic_id), model=model_name, term=rownames(tab),
      estimate=tab[,1], std_error=tab[,2], p_value=tab[,4])
  })
})
normalize_term <- function(term) {
  term <- if_else(term %in% c("Intercept", "(Intercept)"), "Intercept", term)
  term <- str_remove_all(term, "\\[|\\]")
  map_chr(str_split(term, ":"), ~ paste(sort(.x), collapse=":"))
}
reference <- read_csv(here("qa", "topics41_45_regression_audit.csv"), show_col_types=FALSE)
comparison <- audit %>% mutate(key=normalize_term(term)) %>%
  inner_join(reference %>% mutate(key=normalize_term(term)),
    by=c("topic", "model", "key"), suffix=c("_R", "_Python"))
stopifnot(nrow(comparison)==nrow(audit),
  max(abs(comparison$estimate_R-comparison$estimate_Python))<1e-5,
  max(abs(comparison$std_error_R-comparison$std_error_Python))<1e-5,
  max(abs(comparison$p_value_R-comparison$p_value_Python))<1e-5)
write_csv(audit, here("qa", "topics41_45_R_regression_audit.csv"))
block_tests <- function(m, variables, topic_id, model_name) {
  labels <- attr(terms(m), "term.labels"); assignment <- attr(model.matrix(m), "assign")
  map_dfr(variables, function(v) {
    ix <- which(assignment==match(normalize_term(v), normalize_term(labels))); b <- coef(m)[ix]; V <- vcov(m)[ix,ix,drop=FALSE]
    W <- as.numeric(t(b) %*% solve(V,b)); k <- length(ix)
    pv <- pf(W/k, k, df.residual(m), lower.tail=FALSE)
    tibble(topic=topic_id, model=model_name, covariate=v, p_value=pv,
      significant=pv<.05, near_threshold=pv>.05 & pv<=.10)
  })
}
checks <- bind_rows(
  block_tests(m2_t41, covs_t41, 41, "adjusted"),
  block_tests(m3_t41, covs_t41, 41, "interaction"),
  block_tests(m2_t42, covs_t42, 42, "adjusted"),
  block_tests(m3_t42, covs_t42, 42, "interaction"),
  block_tests(m2_t43, covs_t43, 43, "adjusted"),
  block_tests(m3_t43, covs_t43, 43, "interaction"),
  block_tests(m2_t44, covs_t44, 44, "adjusted"),
  block_tests(m3_t44, covs_t44, 44, "interaction"),
  block_tests(m2_t45, covs_t45, 45, "adjusted"),
  block_tests(m3_t45, covs_t45, 45, "interaction")
)
check_summary <- checks %>% group_by(topic, model) %>%
  summarise(significant_covariates=sum(significant),
    nonsignificant_near_fraction=mean(near_threshold[!significant]), .groups="drop")
print(check_summary)
stopifnot(all(check_summary$significant_covariates>=5), all(check_summary$nonsignificant_near_fraction>=.70))
write_csv(checks, here("qa", "topics41_45_R_covariate_tests.csv"))
expected <- read_csv(here("qa", "topics41_45_direction_audit.csv"), show_col_types=FALSE)
directions <- expected %>% mutate(key=normalize_term(term)) %>%
  inner_join(audit %>% mutate(key=normalize_term(term)), by=c("topic", "model", "key"), suffix=c("_expected","_R"))
stopifnot(nrow(directions)==nrow(expected), all(directions$expected_sign==0 | sign(directions$estimate_R)==directions$expected_sign))
# Whole interaction terms: topic45 is a four-df test, not one contrast's p-value.
interaction_tests <- bind_rows(
  block_tests(m3_t41,"perceived_sport_ability:team_environment_toxicity",41,"interaction"),
  block_tests(m3_t42,"daily_notification_frequency:fear_of_missing_out",42,"interaction"),
  block_tests(m3_t43,"language_structure:stimulus_type",43,"interaction"),
  block_tests(m3_t44,"childhood_concussion_history:age",44,"interaction"),
  block_tests(m3_t45,"pornography_exposure:identity_group",45,"interaction")
)
print(interaction_tests)
# Companion outcomes use their own available-case samples.
m4_t42 <- lm(cognitive_fatigue ~ daily_notification_frequency + baseline_fatigue + workload,
  data=datasets$t42)
show_model(m4_t42)
m4_t43 <- lm(nonverbal_interference ~ language_structure * stimulus_type, data=datasets$t43)
show_model(m4_t43)
m5_t43 <- lm(extreme_stimulus_interference ~ language_structure * stimulus_type, data=datasets$t43)
show_model(m5_t43)
# Raw longitudinal association can reflect baseline differences; compare m1_t45 with m2_t45.
# Do not interpret the near-null adjusted slopes as proof of exact equivalence.
print(datasets$t45 %>% group_by(identity_group) %>% summarise(
  available_pairs=sum(complete.cases(pornography_exposure,baseline_self_image)),
  baseline_correlation=cor(pornography_exposure,baseline_self_image,use="complete.obs"), .groups="drop"))
