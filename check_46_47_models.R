# PSY150 Fall2026 | Topics 46-47 | INSTRUCTOR ONLY
# Extract ZIP and open Topics46_47_F26.Rproj.
# Required packages: tidyverse, here, jtools. No installation is run here.
library(tidyverse)
library(here)
library(jtools)
options(contrasts = c("contr.treatment", "contr.poly"))
# Fully simulated data, deliberately calibrated for a classroom exercise.
# R was not available in the authoring runtime; this script was not executed there.
# All six linear models were independently checked using Python QR and SVD.
files <- c(
  t46 = "46306_hernandez_bautista_technology_performance_F26.csv",
  t47 = "46306_maida_sleep_headaches_F26.csv"
)
datasets <- map(files, function(file_name) {
  raw_data <- read_csv(here("data", file_name), na="", show_col_types=FALSE)
  missing_rates <- colMeans(is.na(raw_data))
  stopifnot(nrow(raw_data)>=300, nrow(raw_data)<=600,
    !anyDuplicated(raw_data$participant_id), all(missing_rates<=.10),
    mean(missing_rates==0)>=.45, mean(missing_rates==0)<=.55)
  raw_data
})
show_model <- function(model) {
  print(summ(model, digits=3))
  tab <- coef(summary(model))
  print(tibble(term=rownames(tab), p_value=format.pval(tab[,4],digits=5,eps=1e-12)))
}
# Main model comparisons use the SAME complete-case sample within a topic.

# TOPIC 46: Technology use and academic performance
# Priority study Experiment 1: N=40, multitasking 55% vs no multitasking 66%, F=10.2,p=.003. Both arms used laptops for notes, so this batch's paper comparator is an explicit extension. Experiment 2 peer exposure reduced scores 73% to 56%, but peer interference is not included in this independent-booth design. Question-difficulty interactions were nonsignificant; do not substitute an invented significant difficulty effect. The academic-context interaction is a teaching synthesis with Escueta's review of purposeful/adaptive technology, not an interaction tested by Sana. Lecture device slope is negative; guided-practice slope is smaller and may be weakly positive. Context and protocol are bundled, so the interaction cannot separate activity from software mechanism. technology_activity_type is a pretreatment habit to avoid perfectly aliasing no-device assignment with actual activity=none. Actual protocol is descriptive only. Raw effects differ from the published 11-point contrast because the requested larger N and threshold-oriented SEs take precedence. The test is an integer 40-item score, not a continuous pseudo-percentage.
covs_t46 <- c("age", "prior_gpa", "baseline_knowledge", "study_hours", "work_hours", "sleep_hours", "course_load", "daily_screen_hours", "household_income_thousand", "attention_control", "test_anxiety", "task_motivation", "subject_interest", "time_management", "digital_literacy", "baseline_fatigue", "reading_confidence", "notification_habit", "academic_stress", "note_taking_skill", "gender", "race_ethnicity", "academic_activity_type", "technology_activity_type", "college_stage", "major_group", "first_generation", "english_background", "regular_laptop_use", "session_time")
t46 <- datasets$t46 %>% drop_na(all_of(c("academic_performance", "technology_use_during_academic_tasks", covs_t46)))
t46 <- t46 %>% mutate(
  gender=factor(gender,levels=c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity=factor(race_ethnicity,levels=c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  academic_activity_type=factor(academic_activity_type,levels=c("lecture", "guided_practice")),
  technology_activity_type=factor(technology_activity_type,levels=c("academic_only", "mixed_offtask")),
  college_stage=factor(college_stage,levels=c("early", "advanced")),
  major_group=factor(major_group,levels=c("arts_social", "stem_business")),
  first_generation=factor(first_generation,levels=c("no", "yes")),
  english_background=factor(english_background,levels=c("primary_language", "additional_language")),
  regular_laptop_use=factor(regular_laptop_use,levels=c("no", "yes")),
  session_time=factor(session_time,levels=c("morning", "afternoon"))
)
stopifnot(nrow(t46)==378)

# ------------------------------------------------------------------------------

m1_t46 <- lm(academic_performance ~ technology_use_during_academic_tasks,data=t46)
show_model(m1_t46)

m2_t46 <- lm(
  academic_performance ~ technology_use_during_academic_tasks +
    age +
    prior_gpa +
    baseline_knowledge +
    study_hours +
    work_hours +
    sleep_hours +
    course_load +
    daily_screen_hours +
    household_income_thousand +
    attention_control +
    test_anxiety +
    task_motivation +
    subject_interest +
    time_management +
    digital_literacy +
    baseline_fatigue +
    reading_confidence +
    notification_habit +
    academic_stress +
    note_taking_skill +
    gender +
    race_ethnicity +
    academic_activity_type +
    technology_activity_type +
    college_stage +
    major_group +
    first_generation +
    english_background +
    regular_laptop_use +
    session_time,data=t46)
show_model(m2_t46)

t46_centered <- t46 %>% mutate(technology_use_during_academic_tasks=technology_use_during_academic_tasks-mean(technology_use_during_academic_tasks))

m3_t46 <- lm(
  academic_performance ~ technology_use_during_academic_tasks +
    age +
    prior_gpa +
    baseline_knowledge +
    study_hours +
    work_hours +
    sleep_hours +
    course_load +
    daily_screen_hours +
    household_income_thousand +
    attention_control +
    test_anxiety +
    task_motivation +
    subject_interest +
    time_management +
    digital_literacy +
    baseline_fatigue +
    reading_confidence +
    notification_habit +
    academic_stress +
    note_taking_skill +
    gender +
    race_ethnicity +
    academic_activity_type +
    technology_activity_type +
    college_stage +
    major_group +
    first_generation +
    english_background +
    regular_laptop_use +
    session_time +
    academic_activity_type:technology_use_during_academic_tasks,data=t46_centered)
show_model(m3_t46)
models_t46 <- list(focal_only=m1_t46,adjusted=m2_t46,interaction=m3_t46)

# ------------------------------------------------------------------------------

# TOPIC 47: Sleep duration and headache frequency
# Priority study N=98 adults with episodic migraine, 4406 days, 870 headaches: duration <=6.5h was not associated with migraine on day 0/day 1. Low diary efficiency predicted day-1 migraine OR=1.39 (CI1.06-1.81); exploratory association stronger in migraine WITHOUT aura, OR=1.87 (CI1.31-2.68). Device findings differed; low actigraphic efficiency was inversely associated on day0. Preserve near-null duration and negative diary-efficiency slopes, with a stronger efficiency slope in the without-aura group. This subgroup interaction is exploratory and extended to a college cohort, not established causal heterogeneity. Monthly between-person headache-day counts are not the original within-person daily migraine-onset estimand; the added other-headache group and academic covariates are explicit teaching extensions. Smitherman N=31 chronic-migraine/insomnia trial: follow-up OR=.40,p=.028 did NOT survive Bonferroni alpha=.025; no treatment efficacy is imposed. The additional college review supports routine/context selection, not headache effect coefficients. Primary linear models use integer 0-28 counts as mean comparisons; a quasipoisson sensitivity model is supplied for the count endpoint. No severity-duration effect is substituted for a frequency-duration effect.

covs_t47 <- c("age", "class_load", "study_hours", "work_hours", "social_activity", "time_management", "baseline_headache_days", "sleep_efficiency", "perceived_stress", "bedtime_variability", "caffeine_servings", "alcohol_servings_week", "physical_activity_hours", "meal_irregularity", "hydration_consistency", "neck_tension", "anxiety_symptoms", "depressive_symptoms", "evening_screen_hours", "social_support", "gender", "race_ethnicity", "headache_history", "college_stage", "living_arrangement", "class_schedule", "work_schedule", "family_headache_history", "preventive_medication", "assessment_period")

t47 <- datasets$t47 %>% drop_na(all_of(c("headache_frequency", "sleep_duration", covs_t47)))

t47 <- t47 %>% mutate(
  gender=factor(gender,levels=c("woman", "man", "nonbinary_or_self_described")),
  race_ethnicity=factor(race_ethnicity,levels=c("white", "hispanic_latino", "asian", "black", "multiracial_or_other")),
  headache_history=factor(headache_history,levels=c("other_headache", "migraine_without_aura", "migraine_with_aura")),
  college_stage=factor(college_stage,levels=c("early", "advanced")),
  living_arrangement=factor(living_arrangement,levels=c("with_family", "campus_or_shared", "alone")),
  class_schedule=factor(class_schedule,levels=c("mostly_daytime", "early_or_evening")),
  work_schedule=factor(work_schedule,levels=c("not_employed", "daytime", "evening_or_variable")),
  family_headache_history=factor(family_headache_history,levels=c("no", "yes")),
  preventive_medication=factor(preventive_medication,levels=c("no", "yes")),
  assessment_period=factor(assessment_period,levels=c("regular_term", "exam_period"))
)
stopifnot(nrow(t47)==516)

# ------------------------------------------------------------------------------

m1_t47 <- lm(headache_frequency ~ sleep_duration,data=t47)
show_model(m1_t47)
m2_t47 <- lm(
  headache_frequency ~ sleep_duration +
    age +
    class_load +
    study_hours +
    work_hours +
    social_activity +
    time_management +
    baseline_headache_days +
    sleep_efficiency +
    perceived_stress +
    bedtime_variability +
    caffeine_servings +
    alcohol_servings_week +
    physical_activity_hours +
    meal_irregularity +
    hydration_consistency +
    neck_tension +
    anxiety_symptoms +
    depressive_symptoms +
    evening_screen_hours +
    social_support +
    gender +
    race_ethnicity +
    headache_history +
    college_stage +
    living_arrangement +
    class_schedule +
    work_schedule +
    family_headache_history +
    preventive_medication +
    assessment_period,data=t47)
show_model(m2_t47)

t47_centered <- t47 %>% mutate(sleep_efficiency=sleep_efficiency-mean(sleep_efficiency))

m3_t47 <- lm(
  headache_frequency ~ sleep_duration +
    age +
    class_load +
    study_hours +
    work_hours +
    social_activity +
    time_management +
    baseline_headache_days +
    sleep_efficiency +
    perceived_stress +
    bedtime_variability +
    caffeine_servings +
    alcohol_servings_week +
    physical_activity_hours +
    meal_irregularity +
    hydration_consistency +
    neck_tension +
    anxiety_symptoms +
    depressive_symptoms +
    evening_screen_hours +
    social_support +
    gender +
    race_ethnicity +
    headache_history +
    college_stage +
    living_arrangement +
    class_schedule +
    work_schedule +
    family_headache_history +
    preventive_medication +
    assessment_period +
    headache_history:sleep_efficiency,data=t47_centered)
show_model(m3_t47)
models_t47 <- list(focal_only=m1_t47,adjusted=m2_t47,interaction=m3_t47)
models <- list(`46`=models_t46, `47`=models_t47)

# Match every coefficient, SE and p-value to the saved Python audit.
audit <- imap_dfr(models,function(batch,topic_id) {
  imap_dfr(batch,function(m,model_name) {
    tab <- coef(summary(m))
    tibble(topic=as.integer(topic_id),model=model_name,term=rownames(tab),
      estimate=tab[,1],std_error=tab[,2],p_value=tab[,4])
  })
})
normalize_term <- function(term) {
  term <- if_else(term %in% c("Intercept","(Intercept)"),"Intercept",term)
  term <- str_remove_all(term,"\\[|\\]")
  map_chr(str_split(term,":"),~paste(sort(.x),collapse=":"))
}
reference <- read_csv(here("qa","topics46_47_regression_audit.csv"),show_col_types=FALSE)
comparison <- audit %>% mutate(key=normalize_term(term)) %>%
  inner_join(reference %>% mutate(key=normalize_term(term)),
    by=c("topic","model","key"),suffix=c("_R","_Python"))
stopifnot(nrow(comparison)==nrow(audit),
  max(abs(comparison$estimate_R-comparison$estimate_Python))<1e-5,
  max(abs(comparison$std_error_R-comparison$std_error_Python))<1e-5,
  max(abs(comparison$p_value_R-comparison$p_value_Python))<1e-5)
write_csv(audit,here("qa","topics46_47_R_regression_audit.csv"))
block_tests <- function(m,variables,topic_id,model_name) {
  labels <- attr(terms(m),"term.labels"); assignment <- attr(model.matrix(m),"assign")
  map_dfr(variables,function(v) {
    ix <- which(assignment==match(normalize_term(v),normalize_term(labels)))
    stopifnot(length(ix)>0)
    b <- coef(m)[ix]; V <- vcov(m)[ix,ix,drop=FALSE]
    W <- as.numeric(t(b)%*%solve(V,b)); k <- length(ix)
    pv <- pf(W/k,k,df.residual(m),lower.tail=FALSE)
    tibble(topic=topic_id,model=model_name,covariate=v,p_value=pv,
      significant=pv<.05,near_threshold=pv>.05 & pv<=.10)
  })
}
checks <- bind_rows(
  block_tests(m2_t46,covs_t46,46,"adjusted"),
  block_tests(m3_t46,covs_t46,46,"interaction"),
  block_tests(m2_t47,covs_t47,47,"adjusted"),
  block_tests(m3_t47,covs_t47,47,"interaction")
)
check_summary <- checks %>% group_by(topic,model) %>%
  summarise(significant_covariates=sum(significant),
    nonsignificant_near_fraction=mean(near_threshold[!significant]),.groups="drop")
print(check_summary)
stopifnot(all(check_summary$significant_covariates>=5),all(check_summary$nonsignificant_near_fraction>=.70))
write_csv(checks,here("qa","topics46_47_R_covariate_tests.csv"))
expected <- read_csv(here("qa","topics46_47_direction_audit.csv"),show_col_types=FALSE)
directions <- expected %>% mutate(key=normalize_term(term)) %>%
  inner_join(audit %>% mutate(key=normalize_term(term)),
    by=c("topic","model","key"),suffix=c("_expected","_R"))
stopifnot(nrow(directions)==nrow(expected),
  all(directions$expected_sign==0 | sign(directions$estimate_R)==directions$expected_sign))
print(block_tests(m3_t46,"academic_activity_type:technology_use_during_academic_tasks",46,"interaction"))
print(block_tests(m3_t47,"headache_history:sleep_efficiency",47,"interaction"))
# Topic47 is a two-df interaction, not one dummy contrast's p-value.
# CHECK COUNTS, IDENTITIES AND MISSINGNESS
stopifnot(all(t46$quiz_correct %in% 0:40),
  all(t46$academic_performance==100*t46$quiz_correct/40),
  all(t47$headache_frequency %in% 0:28),
  all(t47$migraine_days<=t47$headache_frequency))
# Count sensitivity: same RHS, cases and contrasts as the linear models.
q2_t47 <- glm(formula(m2_t47),family=quasipoisson(link="log"),data=t47)
show_model(q2_t47)
q3_t47 <- glm(formula(m3_t47),family=quasipoisson(link="log"),data=t47_centered)
show_model(q3_t47)
quasi_models <- list(adjusted=q2_t47,interaction=q3_t47)
quasi_audit <- imap_dfr(quasi_models,function(m,name) {
  tab <- coef(summary(m))
  tibble(model=name,term=rownames(tab),estimate=tab[,1],std_error=tab[,2],p_value=tab[,4])
})
quasi_ref <- read_csv(here("qa","topics46_47_count_sensitivity.csv"),show_col_types=FALSE)
quasi_check <- quasi_audit %>% mutate(key=normalize_term(term)) %>%
  inner_join(quasi_ref %>% mutate(key=normalize_term(term)),
    by=c("model","key"),suffix=c("_R","_Python"))
stopifnot(nrow(quasi_check)==nrow(quasi_audit),
  max(abs(quasi_check$estimate_R-quasi_check$estimate_Python))<1e-4,
  max(abs(quasi_check$std_error_R-quasi_check$std_error_Python))<1e-4,
  max(abs(quasi_check$p_value_R-quasi_check$p_value_Python))<1e-4)
write_csv(quasi_audit,here("qa","topics46_47_R_count_sensitivity.csv"))
# Quasi-model p-values are sensitivity results, not independently tuned targets.
# Linear models compare mean headache-day counts; quasi models compare log means.
# The raw sleep-duration association need not equal its adjusted association.
