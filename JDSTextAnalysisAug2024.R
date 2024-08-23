## Text analysis with R for JDS submission
## Using ChatGPT exam answers
# Sources
# https://www.tidytextmining.com/sentiment
# https://library.virginia.edu/data/articles/a-beginners-guide-to-text-analysis-with-quanteda
# https://guides.library.upenn.edu/penntdm/r

#install.packages("quanteda")
#install.packages("quanteda.textplots")
#install.packages("readtext")
library(quanteda)
library(readtext)
library(tidyverse)
library(readxl)
library(data.table)
setwd("/Users/monniemcgee/Dropbox/2024Fall/JDSSubmissionAugust2024")

# Bar chart for graduate exam. 
scores <- c(92,28,70,82,46,72,96,41,82,74)
xbar <- mean(scores[1:7])
students <- c("S1","S2","S3","S4","S5","S6","S7","GPT3.5","GPT4","GPT4om")
bp <- barplot(scores, names.arg=students, col="gray80",cex.names =.75, main="Exam Scores for Students and ChatGPT Versions", cex.main=.9, ylim=range(0,102))
text(x= bp, y=scores,pos = 3, label = scores,cex=1.2, col = "darkblue")
abline(h=xbar,col="firebrick", lty=2, lwd=3)

### Reading level analysis
readingLevel <- read_excel("Data/GPTExamScores.xlsx", sheet="Raw Data", range="A1:K49")
readingLevel$Version <- as.factor(readingLevel$Version)
fkSum <- tapply(readingLevel$FK_Level, readingLevel$Version, summary)
smogSum <- tapply(readingLevel$SMOG, readingLevel$Version, summary)
fkSD <- tapply(readingLevel$FK_Level, readingLevel$Version, sd)
smogSD <- tapply(readingLevel$SMOG, readingLevel$Version, sd)
# There are 16 problems on the grad exam
tCrit <- qt(.025,15)
# Calculate confidence intervals
library(ggplot2)
fkMean1 <- fkSum$GPT3.5[4]
fkMean2 <- fkSum$GPT4.0[4]
fkMean3 <- fkSum$GPT4omini[4]
fkSD1 <- fkSD[1]
fkSD2 <- fkSD[2]
fkSD3 <- fkSD[3]
smogMean1 <- smogSum$GPT3.5[4]
smogMean2 <- smogSum$GPT4.0[4]
smogMean3 <- smogSum$GPT4omini[4]
smogSD1 <- smogSD[1]
smogSD2 <- smogSD[2]
smogSD3 <- smogSD[3]

confInt <- function(mean, sd, alpha=.05, n=16){
  tcrit <- qt(alpha/2, n-1)
  ciLow <- mean + tcrit*sd/sqrt(n) # tcrit will be negative
  ciHi <- mean - tcrit*sd/sqrt(n)
  return(c(ciLow, ciHi))
}
  
confInt(fkMean1, fkSD1)
# 12.80083 15.10417 
confInt(fkMean2, fkSD2)
# 12.10500 15.43375 
confInt(fkMean3, fkSD3)
# 9.314726 22.030274 
confInt(smogMean1, smogSD1)
# 14.65050 16.42325 
confInt(smogMean2, smogSD2)
# 13.4766 16.1084  
confInt(smogMean3, smogSD3)
# 14.05944 17.38056 

## Violin plots
library(dplyr)
library(hrbrthemes)
library(viridis)
rlLong <- readingLevel %>% pivot_longer(cols = c(FK_Level,SMOG), names_to = "Method", values_to = "Grade")
sample_size = rlLong %>% group_by(Version) %>% summarize(num=n())
rlLong %>%
  ggplot(aes(fill=Version, y=Grade, x=Method)) + 
  geom_violin(position="dodge", alpha=0.5) +
  geom_boxplot(width=0.1, position=position_dodge(.9),color="white", alpha=0.2) +
  scale_fill_manual(values=c("#35b779","#31688e","#440154","#35b779","#31688e","#440154")) +
  theme_ipsum()  +
  theme(
    legend.position.inside = c(0.8, 0.8),
    plot.title = element_text(size=11)
  ) +
  ggtitle("FK and SMOG Reading Level Scores") +
  xlab("") +
  ylab("Score")

############## Serious Text Analysis ######################
# Read in 3 files: GPT35Answers.docx and GPT4Answers.docx
gpt4ans <- readtext("Data/GPT4GradExamAnswers/P*.docx",
                      docvarsfrom = "filenames",
                      docvarnames = c("Problem","Type","Image","Version"),
                      dvsep = "_",
                      encoding = "UTF-8")
gpt3ans <- readtext("Data/GPT35GradExamAnswers/P*.docx",
                    docvarsfrom = "filenames",
                    docvarnames = c("Problem","Type","Image","Version"),
                    dvsep = "_",
                    encoding = "UTF-8")
gpt4oans <- readtext("Data/GPT4oMiniAnswers/P*.docx",
                     docvarsfrom = "filenames",
                     docvarnames = c("Problem","Type","Image","Version"),
                     dvsep = "_",
                     encoding = "UTF-8")
## Create a corpus for each platform. 
# Tokenize and remove stopwords
# Move all words to lower case
# Remove tokens with length 1

gpt4corpus <- corpus(gpt4ans)
gpt3corpus <- corpus(gpt3ans)
gpt4ocorpus <- corpus(gpt4oans)

# Mean types, tokens, and sentences by version
gptDF <- data.frame(summary(c(gpt4corpus,gpt3corpus,gpt4ocorpus)))
gptStats <- gptDF %>% group_by(Version) %>% summarise_at(.vars= vars(Types, Tokens, Sentences),
                                             .fun = c(mean="mean",
                                                      median = "median",
                                                      sd = "sd",
                                                      min = "min",
                                                      max = "max"))
# Version Types_mean Tokens_mean Sentences_mean
# 1 GPT35         90.1        185.           8.4
# 2 GPT4          65.1        106.           4.5
# 3 GPT4o        180.         593.          20.8

gpt4tokens <- tokens(gpt4corpus,remove_punct = TRUE, 
                     remove_symbols = TRUE, 
                     remove_numbers=TRUE,
                     remove_separators = TRUE)
gpt4tokens <- tokens_select(gpt4tokens, stopwords('english'),selection='remove')
gpt4tokens <- tokens_tolower(gpt4tokens)
gpt4tokens <- tokens_select(gpt4tokens, pattern = "^.{2,}$", valuetype = "regex")

gpt3tokens <- tokens(gpt3corpus,remove_punct = TRUE, 
                     remove_symbols = TRUE, 
                     remove_numbers=TRUE,
                     remove_separators = TRUE)
gpt3tokens <- tokens_select(gpt3tokens, stopwords('english'),selection='remove')
gpt3tokens <- tokens_tolower(gpt3tokens)
gpt3tokens <- tokens_select(gpt3tokens, pattern = "^.{2,}$", valuetype = "regex")

gpt4otokens <- tokens(gpt4ocorpus,remove_punct = TRUE, 
                     remove_symbols = TRUE, 
                     remove_numbers=TRUE,
                     remove_separators = TRUE)
gpt4otokens <- tokens_select(gpt4otokens, stopwords('english'),selection='remove')
gpt4otokens <- tokens_tolower(gpt4otokens)
gpt4otokens <- tokens_select(gpt4otokens, pattern = "^.{2,}$", valuetype = "regex")

# Convert tokens classes into a document feature matrix (DFM)
# Assign group labels to these documents
gpt3_dfm <- dfm(gpt3tokens)
gpt4_dfm <- dfm(gpt4tokens)
gpt4o_dfm <- dfm(gpt4otokens)
docvars(gpt3_dfm, "Version") <- "GPT3.5"
docvars(gpt4_dfm, "Version") <- "GPT4"
docvars(gpt4o_dfm, "Version") <- "GPT4o-mini"
# Combine the DFM's 
gptDFM <- rbind(gpt3_dfm, gpt4_dfm, gpt4o_dfm)
# Use dfm_group to aggregate the features within each group.

topfeatures(gptDFM, 5, groups=Version)
#$GPT3.5
#data        can    p-value hypothesis        two 
# 22         17         16         16         16 

#$GPT4
#data      error       mean hypothesis    p-value 
# 11         10         10          9          8 

#$GPT4o
#data         mean         test   hypothesis distribution 
# 90           60           52           47           42 

## Plot a word frequency plot
library(quanteda.textstats)
tstat_freq_gpt <- textstat_frequency(gptDFM, groups = Version,n = 20)
ggplot(tstat_freq_gpt, aes(x = frequency, y = reorder(feature, frequency), color=group)) +
  geom_point(aes(shape = group), size=3) +
  scale_shape_manual(values = c(16, 17, 19)) + theme_bw() + scale_color_grey() +
  labs(x = "", y = "") + theme(legend.position = c(0.8, 0.2))

# Word cloud, Comparison cloud, and other statistics
library(wordcloud)
#dfmat_gpt <- as.matrix(gptDFM)
#dfmat_gpt <- t(dfmat_gpt)
#colnames(dfmat_gpt) <- c("GPT-3.5", "GPT-4","GPT-4o-min")
#par(mar = c(5, 4, 4, 2) + 0.1)
#comparison.cloud(dfmat_gpt[,1:2], random.order=FALSE, colors = c("indianred3","lightsteelblue3"),
#                 title.size=1.5, max.words=50)
#par(mar = c(3, 3, 3, 1) + 0.1)
#commonality.cloud(dfmat_gpt, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=400)

# comparison cloud for versions
# Does not seem to work in R 4.4.1. Worked in R 4.3.2.
gptDFM |>
  dfm_group(groups = Version) |>
  dfm_trim(min_termfreq = 10, verbose = FALSE) |>
  textplot_wordcloud(comparison = TRUE)

### Topic Modeling
library(seededlda)
## GPT3.5
tmod3 <- textmodel_lda(gpt3_dfm, k = 3)
terms(tmod3, 20)
head(topics(tmod3), 20)
# assign topic as a new document-level variable
gpt3_dfm$topic <- topics(tmod3)
# cross-table of the topic frequency
table(gpt3_dfm$topic)
knitr::kable(terms(tmod3), format="latex")

## GPT4
tmod4 <- textmodel_lda(gpt4_dfm, k = 3)
terms(tmod4, 20)
head(topics(tmod4), 20)
# assign topic as a new document-level variable
gpt4_dfm$topic <- topics(tmod4)
# cross-table of the topic frequency
table(gpt4_dfm$topic)
knitr::kable(terms(tmod4), format="latex")

## GPT4o
tmod4o <- textmodel_lda(gpt4o_dfm, k = 3)
terms(tmod4o, 20)
head(topics(tmod4o), 20)
# assign topic as a new document-level variable
gpt4o_dfm$topic <- topics(tmod4o)
# cross-table of the topic frequency
table(gpt4o_dfm$topic)
knitr::kable(terms(tmod4o), format="latex")


####### Sentiment analysis
# install.packages("remotes")
# uses code found in https://rdrr.io/github/quanteda/quanteda.sentiment/f/vignettes/sentiment_analysis.Rmd
# remotes::install_github("quanteda/quanteda.sentiment")
# library(remotes)
library(quanteda.sentiment)
afinn <- read.delim(system.file("extdata/afinn/AFINN-111.txt", 
                                package = "quanteda.sentiment"),
                    header = FALSE, col.names = c("word", "valence"))
head(afinn)
data_dictionary_afinn <- dictionary(list(afinn = afinn$word))
valence(data_dictionary_afinn) <- list(afinn = afinn$valence)
data_dictionary_afinn

## Sentiment for GPT3.5
gpt3val <- textstat_valence(gpt3tokens, data_dictionary_afinn)
gpt4val <- textstat_valence(gpt4tokens, data_dictionary_afinn)
gpt4oval <- textstat_valence(gpt4otokens, data_dictionary_afinn)

# Get summaries and create a line chart
gptValDF <- data.frame(gpt4val$doc_id,gpt3val$sentiment,gpt4val$sentiment,gpt4oval$sentiment)
names(gptValDF) <- c("Document","GPT3.5","GPT4","GPT4o-mini")
summary(gptValDF)
#Doc                 GPT3              GPT4             GPT4o        
#Length:16          Min.   :-1.5556   Min.   :-2.0000   Min.   :-0.7857  
#Class :character   1st Qu.: 0.3413   1st Qu.: 0.0000   1st Qu.: 0.5929  
#Mode  :character   Median : 0.8333   Median : 0.6667   Median : 1.1167  
#                   Mean   : 0.8616   Mean   : 0.5436   Mean   : 0.9234   
#                   3rd Qu.: 1.5000   3rd Qu.: 1.0500   3rd Qu.: 1.3596  
#                   Max.   : 2.4545   Max.   : 2.0000   Max.   : 2.2000  

gptValDFlong <- pivot_longer(gptValDF,cols=2:4,names_to="Version",values_to="Valence")

# Friedman and Quade tests
library(coin)
gptValDFlong$Version <- as.factor(gptValDFlong$Version)
gptValDFlong$Doc <- as.factor(gptValDFlong$Doc)
friedman_test(Valence~Version | Doc, data=gptValDFlong)
quade_test(Valence~Version | Doc, data=gptValDFlong)

library(viridis)
library(hrbrthemes)
labels <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16")
gptValDFlong %>% ggplot( aes(x=Document, y=Valence, group=Version, color=Version)) +
  geom_line(size=1.5, aes(linetype=Version)) +
  scale_color_manual(values=c("#69b3a2", "purple", "black")) +
  ggtitle("Sentiment for Answers from GPT Version") +
  theme_ipsum() + scale_x_discrete(label = labels) +
  ylab("Sentiment from AFINN Dictionary") + theme(legend.position = c(0.2, 0.2)) 


