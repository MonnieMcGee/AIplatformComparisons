# =============================================================================
# Bootstrap confidence intervals for s
# semantic / lexical similarity of LLM explanations
# Assumes pairwise_by_question file contains pairs of cosine similarity measures
# this file is created by semanticSimilarity.R
# =============================================================================

library(dplyr)
library(stringr)
library(purrr)

pairwise_by_question <- read.csv(file="pairwise_similarity_by_question.csv", header = TRUE)

get_vendor <- function(model) {
  case_when(
    str_detect(model, "claude")  ~ "Anthropic",
    str_detect(model, "gemini")  ~ "Google",
    str_detect(model, "gpt")     ~ "OpenAI",
    str_detect(model, "grok")    ~ "xAI",
    TRUE                         ~ "Other"
  )
}

pairwise_boot <- pairwise_by_question %>%
  mutate(
    Vendor_1 = get_vendor(Model_1),
    Vendor_2 = get_vendor(Model_2),
    Comparison =
      if_else(Vendor_1 == Vendor_2,
              "Within vendor",
              "Between vendors")
  )

set.seed(12345)
# Question-level mean similarity by comparison type
question_vendor_similarity <- pairwise_boot %>%
  group_by(QuestionID, Comparison) %>%
  summarise(
    Mean_similarity = mean(Similarity),
    .groups = "drop"
  )

# Check that question-level means vary
question_vendor_similarity %>%
  group_by(Comparison) %>%
  summarise(
    n = n(),
    mean = mean(Mean_similarity),
    sd = sd(Mean_similarity),
    min = min(Mean_similarity),
    max = max(Mean_similarity),
    .groups = "drop"
  )

set.seed(2026)

question_ids <- unique(question_vendor_similarity$QuestionID)
B <- 5000

boot_results <- map_dfr(seq_len(B), function(b) {
  
  sampled_ids <- sample(
    question_ids,
    size = length(question_ids),
    replace = TRUE
  )
  
  boot_dat <- tibble(QuestionID = sampled_ids) %>%
    left_join(question_vendor_similarity, by = "QuestionID")
  
  boot_dat %>%
    group_by(Comparison) %>%
    summarise(
      Mean_similarity = mean(Mean_similarity),
      .groups = "drop"
    ) %>%
    mutate(Boot = b)
})

boot_ci <- boot_results %>%
  group_by(Comparison) %>%
  summarise(
    Mean = mean(Mean_similarity),
    Lower = quantile(Mean_similarity, 0.025),
    Upper = quantile(Mean_similarity, 0.975),
    .groups = "drop"
  )

boot_ci

boot_diff <- boot_results %>%
  select(Boot, Comparison, Mean_similarity) %>%
  tidyr::pivot_wider(
    names_from = Comparison,
    values_from = Mean_similarity
  ) %>%
  mutate(
    Difference = `Within vendor` - `Between vendors`
  )

diff_ci <- boot_diff %>%
  summarise(
    Mean_difference = mean(Difference),
    Lower = quantile(Difference, 0.025),
    Upper = quantile(Difference, 0.975)
  )

diff_ci

ggplot(boot_ci, aes(x = Mean, y = Comparison)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper), height = 0.15) +
  labs(
    x = "Mean TF-IDF cosine similarity",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "figure_vendor_similarity_bootstrap_ci.pdf",
  width = 6,
  height = 3.5
)

