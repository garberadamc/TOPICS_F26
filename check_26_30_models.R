# PSY150 Fall2026: Topics26-30 | INSTRUCTOR ONLY
# Run from DATA as an RStudio project, with CSVs in data/ and audits in qa/.
# install.packages(c("tidyverse", "here", "jtools"))
library(tidyverse)
library(here)
library(jtools)
# Calibrated synthetic teaching data, not estimates from real participants.
# R was unavailable during authoring. Saved-CSV OLS was checked independently
# with QR and SVD in Python; running this script verifies the same results in R.
# Near threshold means .05 < p <= .10, tested per covariate (joint factor test).
# No guarantee is made for arbitrary subsets, robust-SE models, or other outcomes.
dir.create(here("qa"), recursive = TRUE, showWarnings = FALSE)

files <- c(
  portraits = "42053_jimenez_portrait_first_impressions_F26.csv",
  ai = "42053_neuman_ai_critical_thinking_F26.csv",
  outdoors = "42053_faircloth_outdoors_mental_health_F26.csv",
  music = "42053_sullivan_music_stress_F26.csv",
  confidence = "42053_padilla_confidence_performance_F26.csv"
)
datasets <- map(files, function(file_name) {
  raw_data <- read_csv(here("data", file_name), show_col_types = FALSE)
  stopifnot(nrow(raw_data) >= 300, !anyNA(raw_data), !anyDuplicated(raw_data$participant_id))
  raw_data
})
show_model <- function(model) {
  print(summ(model, digits = 3))
  print(as.data.frame(coef(summary(model))) %>% rownames_to_column("term") %>%
    transmute(term, p_value = format.pval(`Pr(>|t|)`, digits = 4, eps = 1e-8)))
}

portraits <- datasets$portraits %>% mutate(
  portrait_style = factor(portrait_style, levels = c("casual_phone", "professional")),
  clothing_color = factor(clothing_color, levels = c("white", "black")),
  judge_gender = factor(judge_gender, levels = c("woman", "man")),
  portrait_gender = factor(portrait_gender, levels = c("woman", "man")),
  race_ethnicity = factor(race_ethnicity, levels = c("hispanic_latino", "white", "asian", "black", "multiracial_or_other")),
  first_generation = factor(first_generation, levels = c("no", "yes")),
  major_group = factor(major_group, levels = c("arts_social", "stem_business")),
  employment = factor(employment, levels = c("no", "yes")),
  photography_training = factor(photography_training, levels = c("no", "yes")),
  hiring_experience = factor(hiring_experience, levels = c("no", "yes")),
  vision_corrected = factor(vision_corrected, levels = c("no", "yes"))
)
ai <- datasets$ai %>% mutate(
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("hispanic_latino", "white", "asian", "black", "multiracial_or_other")),
  first_generation = factor(first_generation, levels = c("no", "yes")),
  major_group = factor(major_group, levels = c("arts_social", "stem_business")),
  college_stage = factor(college_stage, levels = c("early", "advanced")),
  formal_ai_training = factor(formal_ai_training, levels = c("no", "yes")),
  research_methods_course = factor(research_methods_course, levels = c("no", "yes")),
  english_additional_language = factor(english_additional_language, levels = c("no", "yes")),
  reliable_internet = factor(reliable_internet, levels = c("no", "yes")),
  course_format = factor(course_format, levels = c("mainly_in_person", "mainly_online"))
)
outdoors <- datasets$outdoors %>% mutate(
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("hispanic_latino", "white", "asian", "black", "multiracial_or_other")),
  income_band = factor(income_band, levels = c("lower", "middle", "higher")),
  long_term_condition = factor(long_term_condition, levels = c("no", "yes")),
  employment_status = factor(employment_status, levels = c("not_employed", "employed")),
  residential_setting = factor(residential_setting, levels = c("urban", "rural")),
  has_children = factor(has_children, levels = c("no", "yes")),
  dog_ownership = factor(dog_ownership, levels = c("no", "yes")),
  rain_day = factor(rain_day, levels = c("no", "yes")),
  day_type = factor(day_type, levels = c("weekday", "weekend"))
)
music <- datasets$music %>% mutate(
  music_exposure_and_genre = factor(music_exposure_and_genre, levels = c("silence", "classical", "self_selected", "heavy_metal")),
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("hispanic_latino", "white", "asian", "black", "multiracial_or_other")),
  first_generation = factor(first_generation, levels = c("no", "yes")),
  college_stage = factor(college_stage, levels = c("early", "advanced")),
  employment = factor(employment, levels = c("no", "yes")),
  hearing_difficulty = factor(hearing_difficulty, levels = c("no", "yes")),
  prior_relaxation_training = factor(prior_relaxation_training, levels = c("no", "yes")),
  current_counseling = factor(current_counseling, levels = c("no", "yes")),
  session_time = factor(session_time, levels = c("morning", "afternoon")),
  exam_week = factor(exam_week, levels = c("no", "yes"))
)
confidence <- datasets$confidence %>% mutate(
  gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity = factor(race_ethnicity, levels = c("hispanic_latino", "white", "asian", "black", "multiracial_or_other")),
  sport_type = factor(sport_type, levels = c("individual", "team")),
  competition_level = factor(competition_level, levels = c("recreational", "competitive")),
  coach_present = factor(coach_present, levels = c("no", "yes")),
  recent_injury = factor(recent_injury, levels = c("no", "yes")),
  mental_skills_training = factor(mental_skills_training, levels = c("no", "yes")),
  session_time = factor(session_time, levels = c("morning", "afternoon")),
  venue = factor(venue, levels = c("familiar", "unfamiliar")),
  equipment_familiar = factor(equipment_familiar, levels = c("no", "yes"))
)

# TOPIC 26: PORTRAIT PRESENTATION AND FIRST IMPRESSIONS ----
# Neither supplied file is marked P. Howlett is used as the focal source: confidence suit-type partial eta squared .04; Damhorst/Reed color-by-judge interaction about .06 explained variance. Professional-vs-phone, composite outcomes, and extension to male/female portraits are hypothetical, not exact replications. Clothing black effect is small/slightly negative among women and positive among men, reflecting the historical potency pattern without claiming gender universality. Unique identity per rater prevents pseudoreplication; facial expression fixed neutral. Added rater-context effects are assumptions motivated by the cultural-learning review, not 30 published causal estimates.
m1_portraits <- lm(first_impression_ratings ~ portrait_style, data = portraits)
show_model(m1_portraits)
m2_portraits <- lm(
  first_impression_ratings ~ portrait_style +
    age +
    rater_income_thousand +
    college_year +
    sleep_hours +
    positive_mood +
    negative_mood +
    generalized_trust +
    social_cynicism +
    clothing_involvement +
    professional_norm_endorsement +
    appearance_skepticism +
    need_for_cognition +
    social_anxiety +
    extraversion +
    agreeableness +
    cultural_familiarity +
    portrait_experience_years +
    social_media_hours +
    current_fatigue +
    general_response_leniency +
    clothing_color +
    judge_gender +
    portrait_gender +
    race_ethnicity +
    first_generation +
    major_group +
    employment +
    photography_training +
    hiring_experience +
    vision_corrected,
  data = portraits)
show_model(m2_portraits)
portraits_centered <- portraits
m3_portraits <- lm(
  first_impression_ratings ~ portrait_style +
    age +
    rater_income_thousand +
    college_year +
    sleep_hours +
    positive_mood +
    negative_mood +
    generalized_trust +
    social_cynicism +
    clothing_involvement +
    professional_norm_endorsement +
    appearance_skepticism +
    need_for_cognition +
    social_anxiety +
    extraversion +
    agreeableness +
    cultural_familiarity +
    portrait_experience_years +
    social_media_hours +
    current_fatigue +
    general_response_leniency +
    clothing_color +
    judge_gender +
    portrait_gender +
    race_ethnicity +
    first_generation +
    major_group +
    employment +
    photography_training +
    hiring_experience +
    vision_corrected +
    judge_gender:clothing_color,
  data = portraits_centered)
show_model(m3_portraits)

# TOPIC 27: AI USE, CRITICAL THINKING AND ACADEMIC PROGRESS ----
# Priority Gerlich supplied corrected PDF is internally inconsistent: Table 5 reports AI-critical thinking r=-.49 while Table 6 reports -.68 (offloading also differs .89 vs .72). These are correlations, not adjusted causal effects. Use negative direction only; do not claim exact numerical replication. Original population is broader than college students. Composite academic progression and original assessment are extensions. Verification moderation is a theory-grounded teaching assumption drawn from the review, not an estimated interaction in Gerlich. Cognitive offloading is a potential mediator and is not adjusted for in the primary model. All 30 added coefficient magnitudes are calibration choices.
m1_ai <- lm(critical_thinking_and_academic_progression ~ ai_use_frequency, data = ai)
show_model(m1_ai)
m2_ai <- lm(
  critical_thinking_and_academic_progression ~ ai_use_frequency +
    age +
    prior_distance_education_exposure +
    prior_gpa +
    study_hours +
    sleep_hours +
    academic_stress +
    baseline_reasoning +
    information_literacy +
    ai_verification +
    need_for_cognition +
    self_regulation +
    academic_motivation +
    reading_engagement +
    digital_skill +
    distraction +
    work_hours +
    financial_strain +
    instructor_support +
    peer_discussion +
    enrollment_units +
    gender +
    race_ethnicity +
    first_generation +
    major_group +
    college_stage +
    formal_ai_training +
    research_methods_course +
    english_additional_language +
    reliable_internet +
    course_format,
  data = ai)
show_model(m2_ai)
ai_centered <- ai %>% mutate(ai_verification = ai_verification - mean(ai_verification), ai_use_frequency = ai_use_frequency - mean(ai_use_frequency))
m3_ai <- lm(
  critical_thinking_and_academic_progression ~ ai_use_frequency +
    age +
    prior_distance_education_exposure +
    prior_gpa +
    study_hours +
    sleep_hours +
    academic_stress +
    baseline_reasoning +
    information_literacy +
    ai_verification +
    need_for_cognition +
    self_regulation +
    academic_motivation +
    reading_engagement +
    digital_skill +
    distraction +
    work_hours +
    financial_strain +
    instructor_support +
    peer_discussion +
    enrollment_units +
    gender +
    race_ethnicity +
    first_generation +
    major_group +
    college_stage +
    formal_ai_training +
    research_methods_course +
    english_additional_language +
    reliable_internet +
    course_format +
    ai_verification:ai_use_frequency,
  data = ai_centered)
show_model(m3_ai)

# TOPIC 28: OUTDOOR TIME AND SAME-DAY MOOD ----
# White P: N=19,806; 120-179 minutes vs zero OR1.59 health and OR1.23 high wellbeing; plateau around200-300minutes. Daily continuous mood is not the source outcome. Main OLS is a same-day teaching extension, with modest positive direction and stress moderation; numeric OLS coefficients do not reproduce the weekly OR. Companion binary outcomes use those ORs as probability-generating targets for120-179, with a plateau thereafter; realized fitted ORs can differ because N800 and sampling error. Weekly exposure includes the diary day. No causal recommendation or sharp biological threshold is implied.
m1_outdoors <- lm(daily_mood_and_mental_health ~ time_spent_outdoors, data = outdoors)
show_model(m1_outdoors)
m2_outdoors <- lm(
  daily_mood_and_mental_health ~ time_spent_outdoors +
    age +
    baseline_stress +
    baseline_mood +
    sleep_hours +
    sleep_quality +
    social_support +
    physical_activity_minutes +
    work_hours_week +
    screen_hours +
    financial_strain +
    nature_connectedness +
    neighborhood_safety +
    greenspace_percent +
    park_distance_km +
    temperature_c +
    daylight_hours +
    baseline_anxiety +
    physical_function +
    time_pressure +
    loneliness +
    gender +
    race_ethnicity +
    income_band +
    long_term_condition +
    employment_status +
    residential_setting +
    has_children +
    dog_ownership +
    rain_day +
    day_type,
  data = outdoors)
show_model(m2_outdoors)
outdoors_centered <- outdoors %>% mutate(baseline_stress = baseline_stress - mean(baseline_stress), time_spent_outdoors = time_spent_outdoors - mean(time_spent_outdoors))
m3_outdoors <- lm(
  daily_mood_and_mental_health ~ time_spent_outdoors +
    age +
    baseline_stress +
    baseline_mood +
    sleep_hours +
    sleep_quality +
    social_support +
    physical_activity_minutes +
    work_hours_week +
    screen_hours +
    financial_strain +
    nature_connectedness +
    neighborhood_safety +
    greenspace_percent +
    park_distance_km +
    temperature_c +
    daylight_hours +
    baseline_anxiety +
    physical_function +
    time_pressure +
    loneliness +
    gender +
    race_ethnicity +
    income_band +
    long_term_condition +
    employment_status +
    residential_setting +
    has_children +
    dog_ownership +
    rain_day +
    day_type +
    baseline_stress:time_spent_outdoors,
  data = outdoors_centered)
show_model(m3_outdoors)

# TOPIC 29: MUSIC GENRE AND RECOVERY FROM STRESS ----
# P has56 participants; enlarged to360 for30 controls and realistic residual df. Classical and self-selected lower post-stress, heavy metal near-null/slightly higher at average preference. Source scales differ from this0-100 original teaching scale; source directions, not scale coefficients, are retained. Preference-by-genre is one interaction term with3 contrasts; p values differ by contrast. Higher preference makes music contrasts more favorable, including heavy metal for fans. Meta-analysis pooled g=.15 CI[-.21,.52] cautions against universal benefit. All listening-duration measurements are baseline habits; treatment duration fixed10min. Preference comparator in silence is general liking, an explicit measurement limitation.
m1_music <- lm(stress_level ~ music_exposure_and_genre, data = music)
show_model(m1_music)
m2_music <- lm(
  stress_level ~ music_exposure_and_genre +
    age +
    music_listening_duration +
    music_preference +
    baseline_stress +
    sleep_hours +
    sleep_quality +
    trait_anxiety +
    academic_stress +
    financial_strain +
    social_support +
    rumination +
    emotion_regulation +
    musical_engagement +
    training_years +
    caffeine_mg +
    physical_activity_days +
    work_hours +
    noise_sensitivity +
    baseline_fatigue +
    mindfulness +
    gender +
    race_ethnicity +
    first_generation +
    college_stage +
    employment +
    hearing_difficulty +
    prior_relaxation_training +
    current_counseling +
    session_time +
    exam_week,
  data = music)
show_model(m2_music)
music_centered <- music %>% mutate(music_preference = music_preference - mean(music_preference))
m3_music <- lm(
  stress_level ~ music_exposure_and_genre +
    age +
    music_listening_duration +
    music_preference +
    baseline_stress +
    sleep_hours +
    sleep_quality +
    trait_anxiety +
    academic_stress +
    financial_strain +
    social_support +
    rumination +
    emotion_regulation +
    musical_engagement +
    training_years +
    caffeine_mg +
    physical_activity_days +
    work_hours +
    noise_sensitivity +
    baseline_fatigue +
    mindfulness +
    gender +
    race_ethnicity +
    first_generation +
    college_stage +
    employment +
    hearing_difficulty +
    prior_relaxation_training +
    current_counseling +
    session_time +
    exam_week +
    music_preference:music_exposure_and_genre,
  data = music_centered)
show_model(m3_music)

# TOPIC 30: SELF-CONFIDENCE AND ATHLETIC TASK PERFORMANCE ----
# P experiment N28 expert skippers; confidence-adjusted group means66.13vs75.80 and performance group-by-trial eta squared.21. That is not a continuous confidence slope or mastery interaction. Preserve negative conditional confidence-performance direction in high-mastery athletes while adding a justified-but-hypothetical mastery interaction and an observational broader-athlete scenario. Low-mastery conditional slope can be positive, consistent with the review's broader positive association. N360,30controls and near-cutoff inference take priority over exact effect-size replication. No causal interpretation of confidence coefficient; objective performance standardized to one task, not mixed sport scores.
m1_confidence <- lm(athletic_performance ~ self_confidence, data = confidence)
show_model(m1_confidence)
m2_confidence <- lm(
  athletic_performance ~ self_confidence +
    age +
    task_mastery +
    baseline_performance +
    training_years +
    training_hours_week +
    sleep_hours +
    sleep_quality +
    recovery_readiness +
    fatigue +
    injury_discomfort +
    competitive_anxiety +
    attentional_control +
    coach_support +
    achievement_motivation +
    goal_clarity +
    mental_preparation +
    perceived_pressure +
    warmup_quality +
    hydration_ml +
    recent_training_load +
    gender +
    race_ethnicity +
    sport_type +
    competition_level +
    coach_present +
    recent_injury +
    mental_skills_training +
    session_time +
    venue +
    equipment_familiar,
  data = confidence)
show_model(m2_confidence)
confidence_centered <- confidence %>% mutate(task_mastery = task_mastery - mean(task_mastery), self_confidence = self_confidence - mean(self_confidence))
m3_confidence <- lm(
  athletic_performance ~ self_confidence +
    age +
    task_mastery +
    baseline_performance +
    training_years +
    training_hours_week +
    sleep_hours +
    sleep_quality +
    recovery_readiness +
    fatigue +
    injury_discomfort +
    competitive_anxiety +
    attentional_control +
    coach_support +
    achievement_motivation +
    goal_clarity +
    mental_preparation +
    perceived_pressure +
    warmup_quality +
    hydration_ml +
    recent_training_load +
    gender +
    race_ethnicity +
    sport_type +
    competition_level +
    coach_present +
    recent_injury +
    mental_skills_training +
    session_time +
    venue +
    equipment_familiar +
    task_mastery:self_confidence,
  data = confidence_centered)
show_model(m3_confidence)
models <- list(
  topic26 = list(focal_only = m1_portraits, adjusted = m2_portraits, interaction = m3_portraits),
  topic27 = list(focal_only = m1_ai, adjusted = m2_ai, interaction = m3_ai),
  topic28 = list(focal_only = m1_outdoors, adjusted = m2_outdoors, interaction = m3_outdoors),
  topic29 = list(focal_only = m1_music, adjusted = m2_music, interaction = m3_music),
  topic30 = list(focal_only = m1_confidence, adjusted = m2_confidence, interaction = m3_confidence)
)

# REPRODUCE THE SAVED-DATA AUDIT ----
audit <- imap_dfr(models, function(mm, topic_name) {
  imap_dfr(mm, function(m, model_name) {
    as.data.frame(coef(summary(m))) %>% rownames_to_column("term") %>%
      transmute(topic = as.integer(str_remove(topic_name, "topic")), model = model_name,
        term, estimate = Estimate, std_error = `Std. Error`, p_value = `Pr(>|t|)`)
  })
})
normalize_term <- function(term) {
  term <- ifelse(term %in% c("Intercept", "(Intercept)"), "Intercept", term)
  term <- str_remove_all(term, "\\[|\\]")
  map_chr(str_split(term, ":"), ~ paste(sort(.x), collapse = ":"))
}
reference <- read_csv(here("qa", "topics26_30_regression_audit.csv"), show_col_types = FALSE)
comparison <- audit %>% mutate(key = normalize_term(term)) %>%
  inner_join(reference %>% mutate(key = normalize_term(term)),
    by = c("topic", "model", "key"), suffix = c("_R", "_reference"))
stopifnot(nrow(comparison) == nrow(audit),
  max(abs(comparison$estimate_R - comparison$estimate_reference)) < 1e-6,
  max(abs(comparison$std_error_R - comparison$std_error_reference)) < 1e-6,
  max(abs(comparison$p_value_R - comparison$p_value_reference)) < 1e-6)
write_csv(audit, here("qa", "topics26_30_R_regression_audit.csv"))
directions <- read_csv(here("qa", "topics26_30_covariate_directions.csv"), show_col_types = FALSE)
direction_check <- audit %>% mutate(key = normalize_term(term)) %>%
  inner_join(directions %>% mutate(key = normalize_term(term)),
    by = c("topic", "model", "key"), suffix = c("_R", "_reference")) %>%
  mutate(direction_pass = expected == 0 | sign(estimate_R) == expected)
stopifnot(nrow(direction_check) == nrow(directions), all(direction_check$direction_pass))
write_csv(direction_check, here("qa", "topics26_30_R_direction_checks.csv"))

block_tests <- function(m, covs, topic, model_name) {
  mm <- model.matrix(m); labels <- attr(terms(m), "term.labels")
  map_dfr(covs, function(v) {
    ix <- which(attr(mm, "assign") == match(v, labels))
    b <- coef(m)[ix]; V <- vcov(m)[ix, ix, drop = FALSE]
    f <- drop(t(b) %*% solve(V, b)) / length(ix)
    pv <- pf(f, length(ix), df.residual(m), lower.tail = FALSE)
    tibble(topic, model = model_name, covariate = v, p_value = pv,
      significant = pv < .05, near_threshold = pv > .05 & pv <= .10)
  })
}
covariate_tests <- imap_dfr(models, function(mm, topic_name) {
  covs <- setdiff(attr(terms(mm$adjusted), "term.labels"), attr(terms(mm$focal_only), "term.labels"))
  map_dfr(c("adjusted", "interaction"), ~ block_tests(mm[[.x]], covs, topic_name, .x))
})
counts <- covariate_tests %>% group_by(topic, model) %>% summarise(
  covariates = n(), significant_covariates = sum(significant),
  null_near_fraction = mean(near_threshold[!significant]), .groups = "drop")
print(counts)
stopifnot(all(counts$covariates == 30), all(counts$significant_covariates >= 5),
  all(counts$null_near_fraction >= .70))
write_csv(covariate_tests, here("qa", "topics26_30_R_covariate_block_tests.csv"))

# CONDITIONAL EFFECTS; CONTRASTS ACCOUNT FOR FACTOR CODING AND CENTERING ----
conditional <- function(m, original, moderator, effect, topic) {
  factor_m <- is.factor(original[[moderator]])
  vals <- if (factor_m) levels(original[[moderator]]) else quantile(original[[moderator]], c(0, .1, .5, .9, 1))
  effect_levels <- if (is.factor(original[[effect]])) levels(original[[effect]])[-1] else "per_unit"
  map_dfr(vals, function(value) {
    map_dfr(effect_levels, function(level) {
      low <- high <- model.frame(m)[1, , drop = FALSE]
      if (factor_m) {
        low[[moderator]] <- high[[moderator]] <- factor(value, levels = levels(original[[moderator]]))
      } else {
        low[[moderator]] <- high[[moderator]] <- as.numeric(value) - mean(original[[moderator]])
      }
      if (is.factor(original[[effect]])) {
        low[[effect]] <- factor(levels(original[[effect]])[1], levels = levels(original[[effect]]))
        high[[effect]] <- factor(level, levels = levels(original[[effect]]))
      } else { low[[effect]] <- 0; high[[effect]] <- 1 }
      tt <- delete.response(terms(m))
      contrast <- model.matrix(tt, high, contrasts.arg = m$contrasts, xlev = m$xlevels) -
        model.matrix(tt, low, contrasts.arg = m$contrasts, xlev = m$xlevels)
      estimate <- drop(contrast %*% coef(m))
      se <- sqrt(drop(contrast %*% vcov(m) %*% t(contrast)))
      tibble(topic, moderator, moderator_value = as.character(value), effect, level,
        estimate, std_error = se,
        p_value = 2 * pt(abs(estimate/se), df.residual(m), lower.tail = FALSE))
    })
  })
}

conditional_effects <- bind_rows(
  conditional(m3_portraits, portraits, "judge_gender", "clothing_color", 26),
  conditional(m3_ai, ai, "ai_verification", "ai_use_frequency", 27),
  conditional(m3_outdoors, outdoors, "baseline_stress", "time_spent_outdoors", 28),
  conditional(m3_music, music, "music_preference", "music_exposure_and_genre", 29),
  conditional(m3_confidence, confidence, "task_mastery", "self_confidence", 30)
)
print(conditional_effects)
write_csv(conditional_effects, here("qa", "topics26_30_R_conditional_effects.csv"))

# MULTIPLE OUTCOMES AND STRUCTURAL CHECKS ----
stopifnot(max(abs(portraits$first_impression_ratings - rowMeans(portraits[, c("confidence_rating", "approachability_rating", "authenticity_rating")]))) < 1e-6,
  max(abs(ai$critical_thinking_and_academic_progression - rowMeans(ai[, c("critical_thinking_score", "academic_progression_score")]))) < 1e-6,
  all(ai$academic_progression_score <= 100), all(ai$credits_earned <= ai$credits_attempted + 1e-6),
  all(outdoors$weekly_nature_minutes >= outdoors$time_spent_outdoors - .011),
  max(abs(music$stress_change - (music$stress_level - music$baseline_stress))) < 1e-6,
  max(abs(confidence$performance_change - (confidence$athletic_performance - confidence$baseline_performance))) < 1e-6)
# Companion weekly binary outcomes are NOT the same-day primary outcome.
# Published ORs are generation targets, not guaranteed fitted values in this sample.
outdoors <- outdoors %>% mutate(weekly_nature_group = factor(weekly_nature_group,
  levels = c("0", "1_to_119", "120_to_179", "180_to_299", "300_plus")))
health_model <- glm(I(good_health == "yes") ~ weekly_nature_group + long_term_condition + physical_function,
  family = binomial(), data = outdoors)
wellbeing_model <- glm(I(high_wellbeing == "yes") ~ weekly_nature_group + long_term_condition + physical_function,
  family = binomial(), data = outdoors)
print(summ(health_model, exp = TRUE, digits = 3))
print(summ(wellbeing_model, exp = TRUE, digits = 3))

pdf(here("qa", "topics26_30_R_diagnostics.pdf"), width = 9, height = 7)
for (topic_name in names(models)) {
  par(mfrow = c(2, 2)); plot(models[[topic_name]]$interaction, which = c(1, 2, 3, 5), main = topic_name)
}
dev.off()
capture.output(sessionInfo(), file = here("qa", "topics26_30_R_session_info.txt"))
cat("Saved-data coefficient, uncertainty, direction and structural checks passed.\n")
