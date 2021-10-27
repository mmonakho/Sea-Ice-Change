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

pacman::p_load(topicmodels, quanteda, quanteda.textstats, wordcloud2)
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

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: DATA VISUALIZATION ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 1.1 Create a word cloud ----

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

topicToViz <- 11 # change for your own topic of interest
topicToViz <- grep('ice', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
tmResult <- posterior(topicModel)
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilities of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
wordcloud2(data.frame(words, probabilities), shuffle = FALSE, size = 0.8)