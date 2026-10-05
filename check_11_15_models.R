# PSY150 - Topics 11-15 | instructor model checks
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
  dolls = "42053_tien_dolls_body_satisfaction_F26.csv",
  household = "42053_coaquira_household_relationships_F26.csv",
  exercise = "42053_hernandez_exercise_stress_F26.csv",
  bilingual = "42053_soto_bilingualism_self_esteem_F26.csv",
  income = "42053_gorospe_income_happiness_F26.csv"
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

dolls <- datasets$dolls %>%
  mutate(
    unrealistic_doll_exposure = factor(unrealistic_doll_exposure, levels = c("neutral_toy", "unrealistic_doll")),
    school_setting = factor(school_setting, levels = c("suburban", "urban", "rural")),
    caregiver_relationship = factor(caregiver_relationship, levels = c("parent", "other_guardian")),
    older_sister = factor(older_sister, levels = c("no", "yes")),
    doll_ownership = factor(doll_ownership, levels = c("no", "yes")),
    organized_sport = factor(organized_sport, levels = c("no", "yes")),
    arts_club = factor(arts_club, levels = c("no", "yes")),
    shared_bedroom = factor(shared_bedroom, levels = c("no", "yes")),
    caregiver_fulltime_work = factor(caregiver_fulltime_work, levels = c("no", "yes")),
    prior_media_lesson = factor(prior_media_lesson, levels = c("no", "yes")),
    session_time = factor(session_time, levels = c("morning", "afternoon"))
  )

household <- datasets$household %>%
  mutate(
    childhood_household_structure = factor(childhood_household_structure, levels = c("two_parent", "one_parent")),
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    current_relationship = factor(current_relationship, levels = c("not_partnered", "partnered")),
    education = factor(education, levels = c("secondary", "college", "graduate")),
    childhood_area = factor(childhood_area, levels = c("urban", "suburban", "rural")),
    current_housing = factor(current_housing, levels = c("with_family", "independent")),
    first_generation_college = factor(first_generation_college, levels = c("no", "yes")),
    childhood_mentor = factor(childhood_mentor, levels = c("no", "yes")),
    school_transition = factor(school_transition, levels = c("no", "yes")),
    caregiving_responsibility = factor(caregiving_responsibility, levels = c("no", "yes")),
    community_group = factor(community_group, levels = c("no", "yes"))
  )

exercise <- datasets$exercise %>%
  mutate(
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    college_year = factor(college_year, levels = c("first", "later")),
    housing = factor(housing, levels = c("with_family", "away_from_family")),
    first_generation = factor(first_generation, levels = c("no", "yes")),
    caregiving = factor(caregiving, levels = c("no", "yes")),
    recent_injury = factor(recent_injury, levels = c("no", "yes")),
    exercise_type = factor(exercise_type, levels = c("mixed", "running", "other_aerobic", "resistance")),
    physically_active_coping = factor(physically_active_coping, levels = c("no", "yes")),
    recreation_access = factor(recreation_access, levels = c("no", "yes")),
    exam_period = factor(exam_period, levels = c("no", "yes"))
  )

bilingual <- datasets$bilingual %>%
  mutate(
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    generation = factor(generation, levels = c("first", "second", "third_or_later")),
    college_year = factor(college_year, levels = c("first", "later")),
    survey_language = factor(survey_language, levels = c("English", "Spanish")),
    home_language = factor(home_language, levels = c("Spanish", "both", "English")),
    first_generation_college = factor(first_generation_college, levels = c("no", "yes")),
    living_arrangement = factor(living_arrangement, levels = c("with_family", "away_from_family")),
    language_brokering = factor(language_brokering, levels = c("no", "yes")),
    cultural_club = factor(cultural_club, levels = c("no", "yes")),
    financial_aid = factor(financial_aid, levels = c("no", "yes"))
  )

income <- datasets$income %>%
  mutate(
    gender = factor(gender, levels = c("woman", "man", "nonbinary_or_self_described")),
    marital_status = factor(marital_status, levels = c("not_partnered", "partnered")),
    housing_tenure = factor(housing_tenure, levels = c("rent_or_other", "own")),
    employment_status = factor(employment_status, levels = c("not_employed", "employed", "retired")),
    skin_color = factor(skin_color, levels = c("white", "pardo", "black_or_other")),
    smoking = factor(smoking, levels = c("no", "yes")),
    excess_alcohol = factor(excess_alcohol, levels = c("no", "yes")),
    health_insurance = factor(health_insurance, levels = c("no", "yes")),
    recent_physician_visit = factor(recent_physician_visit, levels = c("no", "yes")),
    dependent_children = factor(dependent_children, levels = c("no", "yes")),
    log2_income = log2(income)
  )

# ------------------------------------------------------------------------------

# 11. DOLL IMAGES AND BODY SATISFACTION ----
# Dittmar Table 2: Barbie body esteem M=14.45, SD=3.05; neutral control M=14.96, SD=2.63; raw difference=-0.51 (approximately d=-0.18). The larger multivariate Barbie-versus-other contrast (eta squared=.05) is NOT the same two-group body-esteem effect. Age interaction eta squared=.08; adverse effects absent in oldest group. Our two-arm classroom scale and expanded N are adaptations.

# ------------------------------------------------------------------------------

m1_dolls <- lm(body_satisfaction ~ unrealistic_doll_exposure, data = dolls)
show_model(m1_dolls)

m2_dolls <- lm(
  body_satisfaction ~ unrealistic_doll_exposure +
    age +
    usual_doll_play_frequency +
    cultural_beauty_standards +
    bmi_for_age_z +
    appearance_internalization +
    appearance_focus +
    autonomy +
    caregiver_body_acceptance +
    peer_acceptance +
    appearance_teasing +
    screen_hours_day +
    sleep_hours +
    outdoor_play_minutes +
    family_warmth +
    peer_appearance_talk +
    siblings +
    caregiver_education_years +
    household_size +
    media_literacy +
    general_worry +
    school_setting +
    caregiver_relationship +
    older_sister +
    doll_ownership +
    organized_sport +
    arts_club +
    shared_bedroom +
    caregiver_fulltime_work +
    prior_media_lesson +
    session_time,
  data = dolls
)
show_model(m2_dolls)

dolls_centered <- dolls %>%
  mutate(
    age = age - mean(age)
  )

m3_dolls <- lm(
  body_satisfaction ~ unrealistic_doll_exposure +
    age +
    usual_doll_play_frequency +
    cultural_beauty_standards +
    bmi_for_age_z +
    appearance_internalization +
    appearance_focus +
    autonomy +
    caregiver_body_acceptance +
    peer_acceptance +
    appearance_teasing +
    screen_hours_day +
    sleep_hours +
    outdoor_play_minutes +
    family_warmth +
    peer_appearance_talk +
    siblings +
    caregiver_education_years +
    household_size +
    media_literacy +
    general_worry +
    school_setting +
    caregiver_relationship +
    older_sister +
    doll_ownership +
    organized_sport +
    arts_club +
    shared_bedroom +
    caregiver_fulltime_work +
    prior_media_lesson +
    session_time +
    age:unrealistic_doll_exposure,
  data = dolls_centered
)
show_model(m3_dolls)

# ------------------------------------------------------------------------------

# 12. CHILDHOOD HOUSEHOLDS AND LATER RELATIONSHIPS ----
# Rani N=200 ages 12-16: total EQ 118.2 vs 124.5 (single vs nuclear; SD15.8 vs12.3), but social skills 30.1 vs28.1 (single higher; reported p=.049). Instrument described ambiguously as SSEIT OR MEII; totals do not fully reconcile with subscale means. No adult relationship-functioning adjusted coefficient is reported. Weak positive focal association follows the social-skills result, not a claim of overall family-type superiority. Secondary emotional_intelligence_index preserves the opposite total-EQ direction. Support interaction is theoretical, not an estimated source interaction.

# ------------------------------------------------------------------------------

m1_household <- lm(relationship_functioning ~ childhood_household_structure, data = household)
show_model(m1_household)

m2_household <- lm(
  relationship_functioning ~ childhood_household_structure +
    trust +
    communication +
    social_confidence +
    conflict_management +
    parental_emotional_support +
    childhood_financial_strain +
    childhood_conflict +
    caregiving_consistency +
    extended_family_support +
    peer_support +
    age +
    residential_moves +
    siblings +
    caregiver_education_years +
    neighborhood_safety +
    emotion_regulation +
    attachment_security +
    sleep_hours +
    work_hours +
    current_financial_strain +
    gender +
    current_relationship +
    education +
    childhood_area +
    current_housing +
    first_generation_college +
    childhood_mentor +
    school_transition +
    caregiving_responsibility +
    community_group,
  data = household
)
show_model(m2_household)

household_centered <- household %>%
  mutate(
    parental_emotional_support = parental_emotional_support - mean(parental_emotional_support)
  )

m3_household <- lm(
  relationship_functioning ~ childhood_household_structure +
    trust +
    communication +
    social_confidence +
    conflict_management +
    parental_emotional_support +
    childhood_financial_strain +
    childhood_conflict +
    caregiving_consistency +
    extended_family_support +
    peer_support +
    age +
    residential_moves +
    siblings +
    caregiver_education_years +
    neighborhood_safety +
    emotion_regulation +
    attachment_security +
    sleep_hours +
    work_hours +
    current_financial_strain +
    gender +
    current_relationship +
    education +
    childhood_area +
    current_housing +
    first_generation_college +
    childhood_mentor +
    school_transition +
    caregiving_responsibility +
    community_group +
    parental_emotional_support:childhood_household_structure,
  data = household_centered
)
show_model(m3_household)

# emotional_intelligence_index is a secondary outcome; never include it as a control for relationship_functioning.


# 13. EXERCISE FREQUENCY AND PERCEIVED STRESS ----
# Lepping N=405: no significant stress difference by MVPA guideline status. Physically active coping: stress means13.1 vs15.5, p=.003. Weak inverse but nonsignificant overall exercise-days association is intentional. Academic-demand interaction is a plausible extension of Wunsch stress-buffering findings, not a coefficient from Lepping. Original classroom stress wording replaces the copyrighted scale; not a validated PSS administration.

m1_exercise <- lm(stress_level ~ exercise_frequency, data = exercise)
show_model(m1_exercise)

m2_exercise <- lm(
  stress_level ~ exercise_frequency +
    age +
    sleep_hours +
    study_hours +
    work_hours +
    academic_demands +
    financial_strain +
    social_support +
    sleep_quality +
    coping_confidence +
    time_management +
    commute_minutes +
    sedentary_hours +
    caffeine_mg +
    screen_hours +
    bmi +
    course_credits +
    care_hours +
    neighborhood_safety +
    exercise_enjoyment +
    health_limitations +
    gender +
    college_year +
    housing +
    first_generation +
    caregiving +
    recent_injury +
    exercise_type +
    physically_active_coping +
    recreation_access +
    exam_period,
  data = exercise
)
show_model(m2_exercise)

exercise_centered <- exercise %>%
  mutate(
    academic_demands = academic_demands - mean(academic_demands),
    exercise_frequency = exercise_frequency - mean(exercise_frequency)
  )

m3_exercise <- lm(
  stress_level ~ exercise_frequency +
    age +
    sleep_hours +
    study_hours +
    work_hours +
    academic_demands +
    financial_strain +
    social_support +
    sleep_quality +
    coping_confidence +
    time_management +
    commute_minutes +
    sedentary_hours +
    caffeine_mg +
    screen_hours +
    bmi +
    course_credits +
    care_hours +
    neighborhood_safety +
    exercise_enjoyment +
    health_limitations +
    gender +
    college_year +
    housing +
    first_generation +
    caregiving +
    recent_injury +
    exercise_type +
    physically_active_coping +
    recreation_access +
    exam_period +
    academic_demands:exercise_frequency,
  data = exercise_centered
)
show_model(m3_exercise)


# 14. BILINGUAL LANGUAGE USE, IDENTITY, AND SELF-ESTEEM ----
# Devos N=128: implicit family identification x attitude B=.39, SE=.19, beta=.18, p<.05; R2=.08 for their three-term model. Survey language did not affect implicit self-esteem (p>.80). Preserve positive identification-by-attitude interaction and do not manufacture a language-status advantage. Our explicit 1-5 ratings and focal use/identity composite cannot reproduce the IAT coefficient; focal p near .05 is a teaching assumption, not a source estimate.

m1_bilingual <- lm(self_esteem ~ bilingual_language_use_and_identity, data = bilingual)
show_model(m1_bilingual)

m2_bilingual <- lm(
  self_esteem ~ bilingual_language_use_and_identity +
    language_proficiency +
    cultural_identity_strength +
    family_attitude +
    family_identification +
    campus_belonging +
    family_support +
    perceived_discrimination +
    family_conflict +
    financial_strain +
    gpa +
    age +
    sleep_hours +
    study_hours +
    work_hours +
    bilingual_use_days +
    spanish_identification +
    english_identification +
    bicultural_harmony +
    peer_support +
    academic_confidence +
    gender +
    generation +
    college_year +
    survey_language +
    home_language +
    first_generation_college +
    living_arrangement +
    language_brokering +
    cultural_club +
    financial_aid,
  data = bilingual
)
show_model(m2_bilingual)

bilingual_centered <- bilingual %>%
  mutate(
    family_attitude = family_attitude - mean(family_attitude),
    family_identification = family_identification - mean(family_identification)
  )

m3_bilingual <- lm(
  self_esteem ~ bilingual_language_use_and_identity +
    language_proficiency +
    cultural_identity_strength +
    family_attitude +
    family_identification +
    campus_belonging +
    family_support +
    perceived_discrimination +
    family_conflict +
    financial_strain +
    gpa +
    age +
    sleep_hours +
    study_hours +
    work_hours +
    bilingual_use_days +
    spanish_identification +
    english_identification +
    bicultural_harmony +
    peer_support +
    academic_confidence +
    gender +
    generation +
    college_year +
    survey_language +
    home_language +
    first_generation_college +
    living_arrangement +
    language_brokering +
    cultural_club +
    financial_aid +
    family_attitude:family_identification,
  data = bilingual_centered
)
show_model(m3_bilingual)


# 15. INCOME, DAILY STRESS, AND HAPPINESS ----
# Demenech N=1168, median monthly income $700 (IQR440-1240), stress M23.6 SD7.4; 78.9% happy. Highest-stress poorest vs richest happiness34% vs76%; low-stress income trend p=.820. Source used binary happiness and Poisson regression, while this classroom file uses a continuous rating and OLS; no transferable OLS effect is claimed. Income enters OLS as log2(income): one unit is a doubling. Positive income-by-stress interaction means income is more associated with happiness at high stress; stress itself remains negative.

m1_income <- lm(happiness ~ log2_income, data = income)
show_model(m1_income)

m2_income <- lm(
  happiness ~ log2_income +
    daily_stress +
    spending +
    age +
    education_years +
    household_size +
    sleep_hours +
    sleep_quality +
    social_support +
    financial_strain +
    work_hours +
    leisure_hours +
    exercise_days +
    chronic_conditions +
    back_pain_days +
    commute_minutes +
    fruit_veg_servings +
    bmi +
    neighborhood_safety +
    job_security +
    community_connection +
    gender +
    marital_status +
    housing_tenure +
    employment_status +
    skin_color +
    smoking +
    excess_alcohol +
    health_insurance +
    recent_physician_visit +
    dependent_children,
  data = income
)
show_model(m2_income)

income_centered <- income %>%
  mutate(
    daily_stress = daily_stress - mean(daily_stress),
    log2_income = log2_income - mean(log2_income)
  )

m3_income <- lm(
  happiness ~ log2_income +
    daily_stress +
    spending +
    age +
    education_years +
    household_size +
    sleep_hours +
    sleep_quality +
    social_support +
    financial_strain +
    work_hours +
    leisure_hours +
    exercise_days +
    chronic_conditions +
    back_pain_days +
    commute_minutes +
    fruit_veg_servings +
    bmi +
    neighborhood_safety +
    job_security +
    community_connection +
    gender +
    marital_status +
    housing_tenure +
    employment_status +
    skin_color +
    smoking +
    excess_alcohol +
    health_insurance +
    recent_physician_visit +
    dependent_children +
    daily_stress:log2_income,
  data = income_centered
)
show_model(m3_income)

# Doubling income raises log2_income by one. Do not include income_group or happy_group as controls.


# AUDIT ALL SAVED-DATA MODELS ----
tidy_lm <- function(model, topic, model_name) {
  sm <- coef(summary(model))
  as.data.frame(sm) %>% rownames_to_column("term") %>%
    transmute(topic = topic, model = model_name, term,
      estimate = Estimate, std_error = `Std. Error`, p_value = `Pr(>|t|)`)
}

models <- list(
  topic11 = list(focal_only = m1_dolls, adjusted = m2_dolls, interaction = m3_dolls),
  topic12 = list(focal_only = m1_household, adjusted = m2_household, interaction = m3_household),
  topic13 = list(focal_only = m1_exercise, adjusted = m2_exercise, interaction = m3_exercise),
  topic14 = list(focal_only = m1_bilingual, adjusted = m2_bilingual, interaction = m3_bilingual),
  topic15 = list(focal_only = m1_income, adjusted = m2_income, interaction = m3_income)
)

audit <- imap_dfr(models, function(topic_models, topic_name) {
  imap_dfr(topic_models, function(model, model_name) {
    tidy_lm(model, as.integer(str_remove(topic_name, "topic")), model_name)
  })
})
write_csv(audit, here("qa", "topics11_15_R_regression_audit.csv"))

# Match coefficient names between Python treatment coding and R factor coding.
normalize_term <- function(term) {
  term <- ifelse(term %in% c("Intercept", "(Intercept)"), "Intercept", term)
  term <- str_remove_all(term, "\\[|\\]")
  map_chr(str_split(term, ":"), ~ paste(sort(.x), collapse = ":"))
}
expected <- read_csv(here("qa", "topics11_15_regression_audit.csv"), show_col_types = FALSE)
comparison <- audit %>% mutate(key = normalize_term(term)) %>%
  inner_join(expected %>% mutate(key = normalize_term(term)),
    by = c("topic", "model", "key"), suffix = c("_R", "_reference"))
stopifnot(nrow(comparison) == nrow(audit),
  max(abs(comparison$estimate_R - comparison$estimate_reference)) < 1e-6,
  max(abs(comparison$std_error_R - comparison$std_error_reference)) < 1e-6,
  max(abs(comparison$p_value_R - comparison$p_value_reference)) < 1e-6)

direction_reference <- read_csv(here("qa", "topics11_15_covariate_directions.csv"), show_col_types = FALSE)
direction_check <- audit %>% filter(model == "interaction") %>%
  mutate(key = normalize_term(term)) %>%
  inner_join(direction_reference %>% mutate(key = normalize_term(term)),
    by = c("topic", "key"), suffix = c("_R", "_reference")) %>%
  mutate(direction_pass = case_when(
    expected_direction == "positive" ~ estimate_R > 0,
    expected_direction == "negative" ~ estimate_R < 0,
    TRUE ~ TRUE))
stopifnot(all(direction_check$direction_pass))
write_csv(direction_check, here("qa", "topics11_15_R_direction_check.csv"))

# Joint term tests count categorical variables once, even when they have multiple levels.
covariate_counts <- imap_dfr(models, function(topic_models, topic_name) {
  tests <- drop1(topic_models$adjusted, test = "F") %>% as.data.frame() %>%
    rownames_to_column("term")
  focal <- attr(terms(topic_models$focal_only), "term.labels")
  tests <- tests %>% filter(term != "<none>", !term %in% focal)
  tibble(topic = topic_name, covariates = nrow(tests),
    significant_covariates = sum(tests$`Pr(>F)` < .05, na.rm = TRUE))
})
print(covariate_counts)
stopifnot(all(covariate_counts$covariates == 30), all(covariate_counts$significant_covariates >= 5))

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
  simple_slopes(m3_dolls, "unrealistic_doll_exposureunrealistic_doll", "age", dolls$age, 11),
  simple_slopes(m3_household, "childhood_household_structureone_parent", "parental_emotional_support", household$parental_emotional_support, 12),
  simple_slopes(m3_exercise, "exercise_frequency", "academic_demands", exercise$academic_demands, 13),
  simple_slopes(m3_bilingual, "family_identification", "family_attitude", bilingual$family_attitude, 14),
  simple_slopes(m3_income, "log2_income", "daily_stress", income$daily_stress, 15)
)
print(conditional_effects)
write_csv(conditional_effects, here("qa", "topics11_15_R_conditional_effects.csv"))


# Adjusted residual and quantile plots; assess OLS assumptions and unusual records.
pdf(here("qa", "topics11_15_R_diagnostics.pdf"), width = 9, height = 7)
for (topic_name in names(models)) {
  par(mfrow = c(2, 2))
  plot(models[[topic_name]]$interaction, which = c(1, 2, 3, 5), main = topic_name)
}
dev.off()
cat("All saved-data regression and direction checks passed.\n")
capture.output(sessionInfo(), file = here("qa", "topics11_15_R_session_info.txt"))
