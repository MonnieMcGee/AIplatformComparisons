# =============================================================================
# Read DOCX explanations for one model folder
# =============================================================================

library(dplyr)
library(stringr)
library(readtext)
library(purrr)
library(tibble)

# -----------------------------------------------------------------------------
# 1. Set path to one model folder
# -----------------------------------------------------------------------------

model_dir <- "../Data/claude-3-7-sonnet"

# -----------------------------------------------------------------------------
# 2. Find all DOCX files recursively
# -----------------------------------------------------------------------------

docx_files <- list.files(
  path = model_dir,
  pattern = "\\.docx$",
  recursive = TRUE,
  full.names = TRUE
)

length(docx_files)
head(docx_files)

# -----------------------------------------------------------------------------
# 3. Parse metadata from file path and file name
# -----------------------------------------------------------------------------

parse_docx_path <- function(path) {
  
  file_name <- basename(path)
  subfolder <- basename(dirname(path))
  model_folder <- basename(dirname(dirname(path)))
  
  # Example file:
  # P13_MC_Yes_claude_claude-3-7-sonnet-20250219.docx
  
  problem <- str_extract(file_name, "^P\\d+")
  question_number <- as.integer(str_remove(problem, "^P"))
  
  qtype <- str_match(file_name, "^P\\d+_(MC|SA)_")[, 2]
  image <- str_match(file_name, "^P\\d+_(MC|SA)_(Yes|No)_")[, 3]
  
  # Infer exam from subfolder name
  exam <- case_when(
    str_starts(subfolder, "ACTM_MC_Exam") ~ "ACTM",
    str_starts(subfolder, "ACTM_SA_Exam") ~ "ACTM",
    str_starts(subfolder, "APExam") ~ "AP",
    str_starts(subfolder, "CAOSExam") ~ "CAOS",
    str_starts(subfolder, "Exam1fall2022") ~ "GE",
    TRUE ~ NA_character_
  )
  
  tibble(
    file_path = path,
    file_name = file_name,
    model_folder = model_folder,
    subfolder = subfolder,
    Exam = exam,
    Question = question_number,
    qtype = qtype,
    image = image == "Yes"
  )
}

# Read the meta data
docx_meta <- map_dfr(docx_files, parse_docx_path)

docx_meta %>%
  count(Exam, qtype, image)

# -----------------------------------------------------------------------------
# 4. Read DOCX text
# -----------------------------------------------------------------------------

docx_text <- readtext(docx_files)

docx_text_clean <- docx_text %>%
  as_tibble() %>%
  rename(
    readtext_doc_id = doc_id,
    explanation_text = text
  ) %>%
  mutate(
    subfolder = str_extract(readtext_doc_id, "^[^/]+"),
    file_name = str_extract(
      readtext_doc_id,
      "P\\d+_(MC|SA)_(Yes|No)_[^/]+\\.docx"
    )
  )
# -----------------------------------------------------------------------------
# 5. Combine metadata with text
# -----------------------------------------------------------------------------

claude37_explanations <- docx_meta %>%
  left_join(
    docx_text_clean,
    by = c("subfolder", "file_name")
  ) %>%
  mutate(
    Model = "Claude 3.7",
    qtype = factor(qtype, levels = c("MC", "SA")),
    Exam = factor(Exam, levels = c("ACTM", "AP", "CAOS", "GE")),
    image = factor(image, levels = c(FALSE, TRUE),
                   labels = c("No image", "Image"))
  ) %>%
  arrange(Exam, qtype, Question)

glimpse(claude37_explanations)

# -----------------------------------------------------------------------------
# 6. Sanity Checks
# -----------------------------------------------------------------------------

# Number of documents by exam and question type
claude37_explanations %>%
  count(Exam, qtype)

# Check for missing text
claude37_explanations %>%
  summarise(
    n_docs = n(),
    n_missing_text = sum(is.na(explanation_text)),
    n_empty_text = sum(str_squish(explanation_text) == "", na.rm = TRUE)
  )

# Preview one document
claude37_explanations %>%
  select(Exam, Question, qtype, image, file_name, explanation_text) %>%
  slice(1) %>%
  pull(explanation_text)