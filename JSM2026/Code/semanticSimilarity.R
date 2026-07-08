# =============================================================================
# Semantic / lexical similarity of LLM explanations
# TF-IDF cosine similarity approach
# Assumes .rds objects below were created by stmCorpusBuild.R
# =============================================================================

## Load the data

stm_meta           <- readRDS("stm_meta.rds")
stm20              <- readRDS("stm20.rds")
topic_labels       <- readRDS("topic_labels.rds")
topic_labels_short <- readRDS("topic_labels_short.rds")

# Load libraries

library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(ggplot2)
library(quanteda)
library(Matrix)

# -----------------------------------------------------------------------------
# 1. Prepare document metadata
# -----------------------------------------------------------------------------

sim_meta <- stm_meta %>%
  mutate(
    doc_index = row_number(),
    QuestionID = paste(Exam, qtype, Question, sep = "_"),
    Model = as.character(Model),
    text_for_similarity = explanation_clean
  )
stopifnot(nrow(sim_meta) == 1350)

# -----------------------------------------------------------------------------
# 2. Create TF-IDF document-feature matrix
# -----------------------------------------------------------------------------

corp <- corpus(
  sim_meta,
  text_field = "text_for_similarity"
)

toks <- tokens(
  corp,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en"))

dfm_raw <- dfm(toks)

dfm_trimmed <- dfm_trim(
  dfm_raw,
  min_termfreq = 5
)

dfm_tfidf_mat <- dfm_tfidf(dfm_trimmed)

# Convert to sparse matrix
X <- as(dfm_tfidf_mat, "dgCMatrix")

# -----------------------------------------------------------------------------
# 3. L2-normalize rows and compute cosine similarity
# -----------------------------------------------------------------------------

row_norms <- sqrt(rowSums(X^2))
row_norms[row_norms == 0] <- 1

X_norm <- X / row_norms

# Full document-by-document cosine similarity matrix
doc_similarity <- as.matrix(tcrossprod(X_norm))

dim(doc_similarity)

# -----------------------------------------------------------------------------
# 4. Pairwise model similarities within each question
# -----------------------------------------------------------------------------

get_question_pairs <- function(df_question) {
  
  if (nrow(df_question) < 2) return(NULL)
  
  idx <- df_question$doc_index
  model_names <- df_question$Model
  
  pairs <- combn(seq_along(idx), 2)
  
  tibble(
    Exam = df_question$Exam[1],
    Question = df_question$Question[1],
    QuestionID = df_question$QuestionID[1],
    Model_1 = model_names[pairs[1, ]],
    Model_2 = model_names[pairs[2, ]],
    Similarity = doc_similarity[cbind(idx[pairs[1, ]], idx[pairs[2, ]])]
  )
}

pairwise_by_question <- sim_meta %>%
  group_by(QuestionID) %>%
  group_split() %>%
  map_dfr(get_question_pairs)

pairwise_by_question %>%
  filter(Model_1 == Model_2)

pairwise_by_question %>%
  count(QuestionID) %>%
  count(n)

glimpse(pairwise_by_question)

# -----------------------------------------------------------------------------
# 5. Average similarity within each question
# -----------------------------------------------------------------------------

question_similarity_summary <- pairwise_by_question %>%
  group_by(Exam, Question, QuestionID) %>%
  summarise(
    Mean_Similarity = mean(Similarity),
    Median_Similarity = median(Similarity),
    Min_Similarity = min(Similarity),
    Max_Similarity = max(Similarity),
    SD_Similarity = sd(Similarity),
    N_pairs = n(),
    .groups = "drop"
  ) %>%
  arrange(Mean_Similarity)

print(question_similarity_summary, n = 20)

# -----------------------------------------------------------------------------
# 6. Model-by-model average similarity matrix
# -----------------------------------------------------------------------------

model_pair_similarity <- pairwise_by_question %>%
  mutate(
    Model_A = pmin(Model_1, Model_2),
    Model_B = pmax(Model_1, Model_2)
  ) %>%
  group_by(Model_A, Model_B) %>%
  summarise(
    Mean_Similarity = mean(Similarity),
    Median_Similarity = median(Similarity),
    N_pairs = n(),
    .groups = "drop"
  )

models <- sort(unique(sim_meta$Model))
# model_pair_similarity = read.csv(file="model_pair_similarity.csv", header=TRUE)
similarity_matrix <- matrix(
  NA_real_,
  nrow = length(models),
  ncol = length(models),
  dimnames = list(models, models)
)

diag(similarity_matrix) <- 1

for (i in seq_len(nrow(model_pair_similarity))) {
  m1 <- model_pair_similarity$Model_A[i]
  m2 <- model_pair_similarity$Model_B[i]
  val <- model_pair_similarity$Mean_Similarity[i]
  
  similarity_matrix[m1, m2] <- val
  similarity_matrix[m2, m1] <- val
}

round(similarity_matrix, 3)

# -----------------------------------------------------------------------------
# 7. Heatmap of model-by-model similarity
# -----------------------------------------------------------------------------

similarity_long <- as.data.frame(as.table(similarity_matrix)) %>%
  rename(
    Model_1 = Var1,
    Model_2 = Var2,
    Similarity = Freq
  )

ggplot(similarity_long, aes(x = Model_1, y = Model_2, fill = Similarity)) +
  geom_tile() +
  scale_fill_viridis_c(
    option = "C",
    limits = c(min(similarity_long$Similarity, na.rm = TRUE), 1),
    name = "Cosine\nsimilarity"
  ) +
  coord_equal() +
  labs(
    x = "Model",
    y = "Model",
    title = "Average pairwise similarity of explanations by model"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  "figure_model_similarity_heatmap.pdf",
  width = 7,
  height = 6
)

# -----------------------------------------------------------------------------
# 8. Hierarchical clustering of models
# -----------------------------------------------------------------------------

model_distance <- as.dist(1 - similarity_matrix)

hc_models <- hclust(model_distance, method = "average")

pdf("figure_model_similarity_dendrogram.pdf", width = 7, height = 5)
plot(
  hc_models,
  main = "Hierarchical clustering of LLM explanations",
  xlab = "",
  sub = "",
  ylab = "Distance = 1 - cosine similarity"
)
dev.off()

# -----------------------------------------------------------------------------
# 9. MDS plot of model similarities
# -----------------------------------------------------------------------------

mds <- cmdscale(model_distance, k = 2)

mds_df <- as.data.frame(mds) %>%
  rename(
    Dim1 = V1,
    Dim2 = V2
  ) %>%
  mutate(
    Model = rownames(mds),
    Company = case_when(
      str_detect(Model, "claude") ~ "Anthropic",
      str_detect(Model, "gemini") ~ "Google",
      str_detect(Model, "gpt") ~ "OpenAI",
      str_detect(Model, "grok") ~ "xAI",
      TRUE ~ "Other"
    )
  )

model_labels <- c(
  "claude-3-7-sonnet"             = "Claude 3.7",
  "claude-sonnet-4"               = "Claude 4",
  "claude-sonnet-4-6"             = "Claude 4.6",
  "gemini-2.0-flash-lite"         = "Gemini 2.0",
  "gemini-2.5-flash-lite"         = "Gemini 2.5",
  "gemini-3.1-flash-lite-preview" = "Gemini 3.1",
  "gpt-4o"                        = "GPT-4o",
  "gpt-5"                         = "GPT-5",
  "gpt-5-4"                       = "GPT-5.4",
  "gpt-5.1"                       = "GPT-5.1",
  "gpt-5.4-mini"                  = "GPT-5.4 mini",
  "gpt-5.4-nano"                  = "GPT-5.4 nano",
  "grok-2-vision"                 = "Grok 2",
  "grok-4-1"                      = "Grok 4.1",
  "grok-4.3"                      = "Grok 4.3"
)

mds_df <- mds_df %>%
  mutate(
    Model_short = recode(Model, !!!model_labels)
  )

xpad <- 0.03
ypad <- 0.03

ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Model_short)) +
  geom_point(aes(shape = Company), size = 3) +
  geom_text(vjust = -0.7, size = 3.3) +
  labs(
    x = "MDS dimension 1",
    y = "MDS dimension 2",
    shape = "Company"
  ) +
coord_cartesian(
  xlim = range(mds_df$Dim1) + c(-xpad, xpad),
  ylim = range(mds_df$Dim2) + c(-ypad, ypad)
) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

ggsave(
  "figure_model_similarity_mds.pdf",
  width = 7,
  height = 5,
  units = "in"
)

# ----------------------------------------------------------
# 10. Vendor by vendor similarity display
# ------------------------------------------------------------
library(knitr)
library(kableExtra)

# Add vendor labels to model-pair similarity table
get_vendor <- function(model) {
  case_when(
    str_detect(model, "claude") ~ "Anthropic",
    str_detect(model, "gemini") ~ "Google",
    str_detect(model, "gpt") ~ "OpenAI",
    str_detect(model, "grok") ~ "xAI",
    TRUE ~ "Other"
  )
}

model_pair_similarity_vendor <- model_pair_similarity %>%
  mutate(
    Vendor_A = get_vendor(Model_A),
    Vendor_B = get_vendor(Model_B),
    Comparison_Type = if_else(
      Vendor_A == Vendor_B,
      "Within vendor",
      "Between vendors"
    )
  )

# ------------------------------------------------------------
# Overall within-vendor vs between-vendor similarity
# ------------------------------------------------------------

vendor_similarity_summary <- model_pair_similarity_vendor %>%
  group_by(Comparison_Type) %>%
  summarise(
    N_model_pairs = n(),
    Mean_similarity = mean(Mean_Similarity),
    Median_similarity = median(Mean_Similarity),
    SD_similarity = sd(Mean_Similarity),
    Min_similarity = min(Mean_Similarity),
    Max_similarity = max(Mean_Similarity),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(Mean_similarity, Median_similarity, SD_similarity,
        Min_similarity, Max_similarity),
      ~ round(.x, 3)
    )
  )

vendor_similarity_summary

# Similarity by vendor pairs

vendor_pair_similarity <- model_pair_similarity_vendor %>%
  mutate(
    Vendor_Pair = if_else(
      Vendor_A <= Vendor_B,
      paste(Vendor_A, Vendor_B, sep = "–"),
      paste(Vendor_B, Vendor_A, sep = "–")
    )
  ) %>%
  group_by(Vendor_Pair) %>%
  summarise(
    N_model_pairs = n(),
    Mean_similarity = mean(Mean_Similarity),
    Median_similarity = median(Mean_Similarity),
    SD_similarity = sd(Mean_Similarity),
    Min_similarity = min(Mean_Similarity),
    Max_similarity = max(Mean_Similarity),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(Mean_similarity, Median_similarity,
        SD_similarity, Min_similarity, Max_similarity),
      ~ round(.x, 3)
    )
  ) %>%
  arrange(desc(Mean_similarity))

vendor_pair_similarity

# -----------------------------------------------------------------------------
# 11. Save Outputs
# -----------------------------------------------------------------------------

write.csv(
  pairwise_by_question,
  "pairwise_similarity_by_question.csv",
  row.names = FALSE
)

write.csv(
  question_similarity_summary,
  "question_similarity_summary.csv",
  row.names = FALSE
)

write.csv(
  model_pair_similarity,
  "model_pair_similarity.csv",
  row.names = FALSE
)

write.csv(
  similarity_matrix,
  "model_similarity_matrix.csv"
)

write.csv(
  vendor_similarity_summary,
  "vendor_similarity_summary.csv",
  row.names = FALSE
)

write.csv(
  vendor_similarity_summary,
  "vendor_similarity_summary.csv",
  row.names = FALSE
)

write.csv(
  vendor_pair_similarity,
  "vendor_pair_similarity.csv",
  row.names = FALSE
)

## Latex code for similarity table

vendor_similarity_summary %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 3,
    caption = "Average pairwise TF-IDF cosine similarity of model explanations within and between vendors.",
    label = "tab:vendor_similarity",
    col.names = c(
      "Comparison",
      "Model pairs",
      "Mean",
      "Median",
      "SD",
      "Minimum",
      "Maximum"
    )
  ) %>%
  kable_styling(latex_options = c("hold_position"))

vendor_pair_similarity %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 3,
    caption = "Average TF--IDF cosine similarity between explanations produced by models from different AI developers. Values are averaged over all model pairs within each vendor pairing.",
    label = "tab:vendor_pairs",
    col.names = c(
      "Vendor Pair",
      "Model Pairs",
      "Mean",
      "Median",
      "SD",
      "Min",
      "Max"
    )
  ) %>%
  kable_styling(latex_options = c("hold_position"))