# 
# code: Topic Modelling
# 
# author: Masha Monakhova, mmonakho@asu.edu.com
# date: October 2021
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: LOAD THE DATA & PACKAGES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


options(stringsAsFactors = FALSE)

pacman::p_load(topicmodels, quanteda, quanteda.textstats)
source('code/metadataExtract.R')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: CREATE A DTM AND COMPUTE THE LDA MODEL ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 1.1 Create DTM ----

corpus_tokens <- ADNcorpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)

ADNcollocations <- textstat_collocations(corpus_tokens, min_count = 25)
ADNcollocations <- ADNcollocations[1:250]

corpus_tokens <- tokens_compound(corpus_tokens, sotu_collocations)

# ---- 1.2 Remove terms which occur in less than 1% of all documents
DTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 1, docfreq_type = "prop")

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
ADN <- ADN[sel_idx, ]

require(topicmodels)

# ---- 1.3 Choose the number of topics
K <- 15

# ---- 1.4 Compute the LDA model, inference via n iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, seed = 1, verbose = 25))

terms(topicModel, 10)