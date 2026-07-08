# =============================================================================
# Read DOCX explanations for all models
# Reads in short answer explanations as well as MC explanations
# File organization Data > 15 Model Folders > 6 exam folders 
# .docx files (one for each question) in each exam folder.
# =============================================================================

library(dplyr)
library(stringr)
library(readtext)
library(purrr)
library(tibble)

# -----------------------------------------------------------------------------
# 1. Set path to folders
# -----------------------------------------------------------------------------
model_dirs <- list.dirs("../Data", recursive = FALSE, full.names = TRUE)

model_dirs
length(model_dirs)

# -----------------------------------------------------------------------------
# 2. Wrapper function to read in all 15 models
# -----------------------------------------------------------------------------

read_model_explanations <- function(model_dir, model_label = NULL) {
  
  docx_files <- list.files(
    path = model_dir,
    pattern = "\\.docx$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  docx_meta <- map_dfr(docx_files, parse_docx_path)
  
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
  
  out <- docx_meta %>%
    left_join(
      docx_text_clean,
      by = c("subfolder", "file_name")
    ) %>%
    mutate(
      Model = ifelse(is.null(model_label), model_folder, model_label),
      qtype = factor(qtype, levels = c("MC", "SA")),
      Exam = factor(Exam, levels = c("ACTM", "AP", "CAOS", "GE")),
      image = factor(
        image,
        levels = c(FALSE, TRUE),
        labels = c("No image", "Image")
      )
    ) %>%
    arrange(Model, Exam, qtype, Question)
  
  out
}

# -----------------------------------------------------------------------------
# 3. Apply wrapper function to read in all 15 models with checks
# -----------------------------------------------------------------------------

all_explanations <- map_dfr(model_dirs, read_model_explanations)

all_explanations %>%
  count(model_folder)

all_explanations %>%
  count(Exam, qtype)

all_explanations %>%
  summarise(
    n_docs = n(),
    n_missing_text = sum(is.na(explanation_text)),
    n_empty_text = sum(str_squish(explanation_text) == "", na.rm = TRUE)
  )

# -----------------------------------------------------------------------------
# 4. Save to .csv file
# -----------------------------------------------------------------------------

write.csv(all_explanations, "all_explanations_long.csv", row.names = FALSE)

