# =============================================================================
# grade_mc_responses.R
# Grades LLM responses against answer keys for CAOS, ACTM, and GE exams
# NA and NC responses are treated as incorrect (scored 0)
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------------

raw <- read_excel("../CorrectAnswers.xlsx", sheet = "Correct Answers")

# Quick sanity check: print dimensions and column names
cat("Dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n")
cat("Column names:\n")
print(colnames(raw))

model_cols_present <- setdiff(colnames(raw), c("Question", "Options", "Exam", "Truth", "Image","Cross_Reference"))

cat("\nModel columns detected:\n")
print(model_cols_present)

# Verify image and cross_reference coding
raw %>%
  group_by(Exam) %>%
  summarise(
    N_total = n(),
    N_image = sum(Image == TRUE, na.rm = TRUE),
    N_crossref = sum(Cross_Reference == TRUE, na.rm = TRUE)
  )


# -----------------------------------------------------------------------------
# 2. Clean response values
#
# - Trim whitespace (catches cases like " d" in ACTM row 1)
# - Uppercase for consistent comparison
# - Recode NA and NC as incorrect sentinel "WRONG"
# -----------------------------------------------------------------------------

clean_response <- function(x) {
  x <- str_trim(x)           # remove leading/trailing whitespace
  x <- str_to_upper(x)       # uppercase
  x[is.na(x)] <- "MISSING"   # true NAs (empty cells) -> MISSING
  x[x == "NC"] <- "NC"       # keep NC label for tracking
  x[x == "NA"] <- "NA_RESP"  # keep NA label for tracking (avoid R's NA)
  return(x)
}

truth_clean <- str_to_upper(str_trim(raw$Truth))

# -----------------------------------------------------------------------------
# 3. Score each model
#
# Scoring logic:
#   correct answer          -> 1
#   NC (no conclusion)      -> 0  (model declined due to missing image)
#   NA (no response)        -> 0  (missing/refused)
#   MISSING (empty cell)    -> NA (not yet graded — excluded from analysis)
#   wrong answer            -> 0
# -----------------------------------------------------------------------------

score_model <- function(responses, truth) {
  resp_clean <- clean_response(responses)
  score <- case_when(
    resp_clean == "MISSING" ~ NA_real_,          # not yet graded
    resp_clean %in% c("NC", "NA_RESP") ~ 0,      # NC and NA = incorrect
    resp_clean == str_to_upper(str_trim(truth))  ~ 1,  # correct
    TRUE ~ 0                                      # wrong answer
  )
  return(score)
}

# -----------------------------------------------------------------------------
# 4. Build scored data frame
# -----------------------------------------------------------------------------

scored <- raw %>%
  select(Question, Exam, Truth, all_of(model_cols_present))

# Add score columns (named score_<model>)
for (model in model_cols_present) {
  score_col <- paste0("score_", model)
  scored[[score_col]] <- score_model(scored[[model]], scored$Truth)
}

# -----------------------------------------------------------------------------
# 5. Accuracy summary: by model and exam
# -----------------------------------------------------------------------------

score_cols <- paste0("score_", model_cols_present)

# Long format for easy summarisation
scored_long <- scored %>%
  select(Question, Exam, Truth, all_of(model_cols_present), all_of(score_cols)) %>%
  pivot_longer(
    cols = all_of(model_cols_present),
    names_to = "Model",
    values_to = "Response"
  ) %>%
  mutate(
    Response_clean = clean_response(Response),
    score_col = paste0("score_", Model)
  ) %>%
  rowwise() %>%
  mutate(Score = scored[[score_col]][
    scored$Question == Question & scored$Exam == Exam][1]) %>%
  ungroup()

# Overall accuracy by model
accuracy_by_model <- scored_long %>%
  filter(!is.na(Score)) %>%           # exclude ungraded cells
  group_by(Model) %>%
  summarise(
    N_questions  = n(),
    N_correct    = sum(Score),
    Accuracy     = round(mean(Score), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(Accuracy))

cat("\n=== Overall accuracy by model ===\n")
print(accuracy_by_model, n = Inf)

# Accuracy by model AND exam
accuracy_by_model_exam <- scored_long %>%
  filter(!is.na(Score)) %>%
  group_by(Model, Exam) %>%
  summarise(
    N_questions = n(),
    N_correct   = sum(Score),
    Accuracy    = round(mean(Score), 3),
    .groups = "drop"
  ) %>%
  arrange(Exam, desc(Accuracy))

cat("\n=== Accuracy by model and exam ===\n")
print(accuracy_by_model_exam, n = Inf)

# -----------------------------------------------------------------------------
# 6. NC and NA frequency tracking
#
# Track how often each model produces NC or NA by exam — this feeds into
# the analytical decision about whether NC decreases in more powerful models
# -----------------------------------------------------------------------------

nc_na_summary <- scored_long %>%
  mutate(
    Is_NC      = Response_clean == "NC",
    Is_NA      = Response_clean == "NA_RESP",
    Is_Missing = Response_clean == "MISSING"
  ) %>%
  filter(!Is_Missing) %>%
  group_by(Model, Exam) %>%
  summarise(
    N_total  = n(),
    N_NC     = sum(Is_NC),
    N_NA     = sum(Is_NA),
    Pct_NC   = round(100 * mean(Is_NC), 1),
    Pct_NA   = round(100 * mean(Is_NA), 1),
    .groups  = "drop"
  )

## Calculate accuracy with questions attempted as the denominator

attempted_accuracy <- scored_long %>%
  filter(
    !is.na(Score),
    !Response_clean %in% c("NC", "NA_RESP", "MISSING")
  ) %>%
  group_by(Model, Exam) %>%
  summarise(
    N_attempted = n(),
    N_correct_attempted = sum(Score),
    Attempted_Accuracy = round(mean(Score), 3),
    .groups = "drop"
  )
cat("\n=== Attempted Accuracy (excluding NC and NA from Denominator) ===\n")
print(attempted_accuracy, n = Inf)


# Overall NC/NA/Missing by model (collapsed across exams)
nc_na_by_model <- nc_na_summary %>%
  group_by(Model) %>%
  summarise(
    N_total   = sum(N_total),
    N_NC      = sum(N_NC),
    N_NA      = sum(N_NA),
    Pct_NC    = round(100 * N_NC / N_total, 1),
    Pct_NA    = round(100 * N_NA / N_total, 1),
    .groups   = "drop"
  ) %>%
  arrange(desc(Pct_NC), desc(Pct_NA))

cat("\n=== NC, NA, and missing frequency by model (all exams) ===\n")
print(nc_na_by_model, n = Inf)

# -----------------------------------------------------------------------------
# 7. Flag questions where multiple models gave the same wrong answer
#
# These are candidates for key review — either the key is wrong,
# or the question is genuinely ambiguous
# -----------------------------------------------------------------------------

# For each question, count how many models got it wrong (excluding ungraded)
question_difficulty <- scored_long %>%
  filter(!is.na(Score)) %>%
  group_by(Question, Exam) %>%
  summarise(
    N_models_graded  = n(),
    N_correct        = sum(Score),
    N_wrong          = sum(Score == 0),
    Pct_correct      = round(100 * mean(Score), 1),
    .groups = "drop"
  ) %>%
  arrange(Pct_correct)

cat("\n=== Questions where all or most models answered incorrectly ===\n")
cat("(Possible key errors or genuinely ambiguous items)\n")
print(
  filter(question_difficulty, Pct_correct <= 20),
  n = Inf
)

# -----------------------------------------------------------------------------
# 8. Export results
# -----------------------------------------------------------------------------

write.csv(accuracy_by_model,       "accuracy_by_model.csv",       row.names = FALSE)
write.csv(accuracy_by_model_exam,  "accuracy_by_model_exam.csv",  row.names = FALSE)
write.csv(nc_na_summary,           "nc_na_by_model_exam.csv",     row.names = FALSE)
write.csv(nc_na_by_model,          "nc_na_by_model.csv",          row.names = FALSE)
write.csv(question_difficulty,     "question_difficulty.csv",     row.names = FALSE)
write.csv(scored,                  "scored_responses.csv",        row.names = FALSE)
write.csv(scored_long,             "scored_responses_long.csv",   row.names = FALSE)

cat("\nAll output files written to working directory.\n")
cat("scored_responses.csv contains the full data with score columns for each model.\n")