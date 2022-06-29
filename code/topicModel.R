# 
# code: Develop topic models per time period, export results
# 
# author: Kelly Claborn, clabornkelly@gmail.com; Masha Monakhova, mmonakho@asu.edu
# date: November 2021
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 0: SOURCE SCRIPTS, DEFINE OPTIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


options(stringsAsFactors = FALSE)

source('code/metadataExtract.R')

ntopics <- 10


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: DEFINE FUNCTION FOR TOPIC MODELLING BY TIME PERIOD ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


topicModelByPeriod <- function(dat = NULL, years = NULL, ntopics = NULL) {
  
  # define year period name, for naming function outputs
  periodName <- paste(as.character(years)[1], as.character(years)[2], sep = "-")
  
  # filter data and turn into corpus
  dat <- dat %>% filter(year%in%years)
  corp <- corpus(dat)
  
  # tokenize
  corpTokens <- corp %>% 
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
    tokens_remove(pattern = stopwords_extended, padding = T)
  
  # identify candidate collocations
  collocations <- textstat_collocations(corpTokens, min_count = 5)
  collocations <- collocations[1:250, ]
  
  corpTokens <- tokens_compound(corpTokens, collocations)
  
  # create DTM, remove terms that appear in less than 1% of documents
  DTM <- corpTokens %>% 
    tokens_remove("") %>%
    dfm() %>% 
    dfm_trim(min_docfreq = 0.01, max_docfreq = 1, docfreq_type = "prop")
  
  sel_idx <- rowSums(DTM) > 0
  
  DTM <- DTM[sel_idx, ]
  dat <- dat[sel_idx, ]
  
  # compute LDA model, inference via n iterations of Gibbs sampling
  topicModel <- LDA(DTM, ntopics, method = "Gibbs", control = list(iter = 500, seed = 1, verbose = 25))
  terms(topicModel, 10)
  
  # create data frames for top five terms per topic, and all give terms combined into string
  topFiveTerms <- terms(topicModel, 5)
  topicNames <- apply(topFiveTerms, 2, paste, collapse=" ")
  
  # create data frames for posterior probabilities of the topics for each document 
  # and of the terms for each topic for the fitted topic model
  tmResult <- posterior(topicModel)
  
  resultTerms <- tmResult$terms
  resultTheta <- tmResult$topics
  
  
  return(list(DTM = DTM, top5 = topFiveTerms, names = topicNames, terms = resultTerms, theta = resultTheta))
  
}


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: RUN TOPIC MODELLING FUNCTION PER TIME PERIOD ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- NOTE: Have decided to only run the topic modelling function a single time across the full corpus ----
# To compare topics temporally, we simply disaggregate by time period when looking at posterior probabilities.  


# ---- 2.1 Full Corpus (1995-2021) ----

topicModel_full <- topicModelByPeriod(dat = ADN, years = c(1995:2021), ntopics = ntopics)

# Examine topic outputs
topicModel_full$top5
topicModel_full$names

# Calculate posterior probability for the top 40 terms within each topic
termProbabilities_full <- data.frame(NA)

for(i in 1:ntopics) {
  termProbabilities_full <- 
    cbind.data.frame(termProbabilities_full,
                     sort(topicModel_full$terms[i,], decreasing = TRUE)[1:40] %>%
                       data.frame(terms = names(.), prob = .) %>%
                       rename_with(~ paste0(., "_", i, sep = "")))
  
}

termProbabilities_full <- termProbabilities_full %>% select(-`NA.`)


# Calculate aggregated probability for each topic across the corpus
topicProbs_total_full <- colSums(topicModel_full$theta) / nrow(topicModel_full$DTM)

# Calculate posterior probability for each topic within each document
topicProbs_byDoc_full <- 
  cbind.data.frame(filename = paste("text", topicModel_full$DTM$docid, sep = ""), topicModel_full$theta)

# Calculate aggregated probability for each topic by year
topicProbs_byYear_full <- 
  cbind.data.frame(docid = topicModel_full$DTM$docid, topicModel_full$theta) %>%
  left_join(ADN[,c("docid","year")], by = "docid") %>%
  group_by(year) %>%
  mutate(ndocs = length(docid)) %>%
  ungroup() %>%
  group_by(year, ndocs) %>%
  summarise_at(vars(4:ncol(.)-2), mean)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: EXPORT OUTPUTS WITH TODAY'S DATE ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 3.1 Create output directory with today's date ----
# This helps with version controlling topic modeling outputs, since  output can vary due to the probabilistic nature of the model

dir.create(paste("data/outputs/topicmodel/", format(Sys.Date(), "%Y%m%d"), sep = ""))
output.dir <- paste("data/outputs/topicmodel/", format(Sys.Date(), "%Y%m%d"), sep = "")


# ---- 3.2 Export topic names and terms ----

# Full Corpus
export(topicModel_full$top5, paste(output.dir, "top5_full.csv", sep = "/"))
export(as.data.frame(topicModel_full$names), paste(output.dir, "names_full.csv", sep = "/"))


# ---- 3.3 Export term probabilities per topic ----

export(termProbabilities_full, paste(output.dir, "termProbabilities_full.csv", sep = "/"))


# ---- 3.4 Export topic probabilities by document, and year ----

# Full Corpus
export(as.data.frame(topicProbs_total_full), paste(output.dir, "topicProbs_total_full.csv", sep = "/"))
export(topicProbs_byDoc_full, paste(output.dir, "topicProbs_byDoc_full.csv", sep = "/"))
export(topicProbs_byYear_full, paste(output.dir, "topicProbs_byYear_full.csv", sep = "/"))
