# 
# code: Topic Modelling 2
# 
# author: Masha Monakhova, mmonakho@asu.edu.com
# date: October 2021
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 0: LOAD THE DATA & PACKAGES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


options(stringsAsFactors = FALSE)

pacman::p_load(topicmodels, quanteda, quanteda.textstats, wordcloud2,
               reshape2, ggplot2, pals)
source('code/metadataExtract.R')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: TOPIC MODELLING FOR YEARS 1995:2003 ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- 1.1 Create DTM ----

ADN_pd1 <- ADN %>%
  # mutate(text = stringr::str_replace_all(text, stringr::regex("sea ice", ignore_case = T), "sea-ice"),
  #        text = stringr::str_replace_all(text, stringr::regex("climate change", ignore_case = T), "climate-change")) %>%
  filter(year%in%c(1995:2003))

ADNcorpus_pd1 <- corpus(ADN_pd1)

corpus_tokens_pd1 <- ADNcorpus_pd1 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)

ADNcollocations_pd1 <- textstat_collocations(corpus_tokens_pd1, min_count = 5)
ADNcollocations_pd1 <- ADNcollocations_pd1[1:250,]

corpus_tokens_pd1 <- tokens_compound(corpus_tokens_pd1, ADNcollocations_pd1)

# ---- 1.2 Remove terms which occur in less than 1% of all documents

DTM_pd1 <- corpus_tokens_pd1 %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 1, docfreq_type = "prop")

sel_idx_pd1 <- rowSums(DTM_pd1) > 0
DTM_pd1 <- DTM_pd1[sel_idx_pd1, ]
ADN_pd1 <- ADN_pd1[sel_idx_pd1, ]


# ---- 1.3 Choose the number of topics

K <- 8

# ---- 1.4 Compute the LDA model, inference via n iterations of Gibbs sampling
topicModel_pd1 <- LDA(DTM_pd1, K, method="Gibbs", control=list(iter = 500, seed = 1, verbose = 25))
terms(topicModel_pd1, 10)

# ---- 1.5 Create a word cloud ----

top5termsPerTopic_pd1 <- terms(topicModel_pd1, 5)
topicNames_pd1 <- apply(top5termsPerTopic_pd1, 2, paste, collapse=" ")

# Choose topic number (1-10)
topicToViz_pd1 <- 1 # change for your own topic of interest

# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
tmResult_pd1 <- posterior(topicModel_pd1)
top40terms_pd1 <- sort(tmResult_pd1$terms[topicToViz_pd1,], decreasing=TRUE)[1:40]
words_pd1 <- names(top40terms_pd1)
# extract the probabilities of each of the 40 terms
probabilities_pd1 <- sort(tmResult_pd1$terms[topicToViz_pd1,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
wordcloud2(data.frame(words_pd1, probabilities_pd1), shuffle = FALSE, size = 0.8)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: TOPIC MODELLING FOR YEARS 2004:2012 ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ---- 2.1 Create DTM ----

ADN_pd2 <- ADN %>%
  filter(year%in%c(2004:2012))

ADNcorpus_pd2 <- corpus(ADN_pd2)

corpus_tokens_pd2 <- ADNcorpus_pd2 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)

ADNcollocations_pd2 <- textstat_collocations(corpus_tokens_pd2, min_count = 5)
ADNcollocations_pd2 <- ADNcollocations_pd2[1:250, ]

corpus_tokens_pd2 <- tokens_compound(corpus_tokens_pd2, ADNcollocations_pd2)

# ---- 2.2 Remove terms which occur in less than 1% of all documents

DTM_pd2 <- corpus_tokens_pd2 %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 1, docfreq_type = "prop")

sel_idx_pd2 <- rowSums(DTM_pd2) > 0
DTM_pd2 <- DTM_pd2[sel_idx_pd2, ]
ADN_pd2 <- ADN_pd2[sel_idx_pd2, ]


# ---- 2.3 Choose the number of topics

K <- 8

# ---- 2.4 Compute the LDA model, inference via n iterations of Gibbs sampling
topicModel_pd2 <- LDA(DTM_pd2, K, method="Gibbs", control=list(iter = 500, seed = 1, verbose = 25))

terms(topicModel_pd2, 10)

# ---- 2.5 Create a word cloud ----

top5termsPerTopic_pd2 <- terms(topicModel_pd2, 5)
topicNames_pd2 <- apply(top5termsPerTopic_pd2, 2, paste, collapse=" ")

# Change to each topic number and re-run
topicToViz_pd2 <- 2 # change for your own topic of interest

# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
tmResult_pd2 <- posterior(topicModel_pd2)
top40terms_pd2 <- sort(tmResult_pd2$terms[topicToViz_pd2,], decreasing=TRUE)[1:40]
words_pd2 <- names(top40terms_pd2)
# extract the probabilities of each of the 40 terms
probabilities_pd2 <- sort(tmResult_pd2$terms[topicToViz_pd2,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
wordcloud2(data.frame(words_pd2, probabilities_pd2), shuffle = FALSE, size = 0.8)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: TOPIC MODELLING FOR YEARS 2004:2012 ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ---- 3.1 Create DTM ----

ADN_pd3 <- ADN %>%
  filter(year%in%c(2013:2021))

ADNcorpus_pd3 <- corpus(ADN_pd3)

corpus_tokens_pd3 <- ADNcorpus_pd3 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)

ADNcollocations_pd3 <- textstat_collocations(corpus_tokens_pd3, min_count = 5)
ADNcollocations_pd3 <- ADNcollocations_pd3[1:250, ]

corpus_tokens_pd3 <- tokens_compound(corpus_tokens_pd3, ADNcollocations_pd3)

# ---- 3.2 Remove terms which occur in less than 1% of all documents

DTM_pd3 <- corpus_tokens_pd3 %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 1, docfreq_type = "prop")

sel_idx_pd3 <- rowSums(DTM_pd3) > 0
DTM_pd3 <- DTM_pd3[sel_idx_pd3, ]
ADN_pd3 <- ADN_pd3[sel_idx_pd3, ]


# ---- 3.3 Choose the number of topics

K <- 8

# ---- 3.4 Compute the LDA model, inference via n iterations of Gibbs sampling
topicModel_pd3 <- LDA(DTM_pd3, K, method="Gibbs", control=list(iter = 500, seed = 1, verbose = 25))

terms(topicModel_pd3, 10)

# ---- 3.5 Create a word cloud ----

top5termsPerTopic_pd3 <- terms(topicModel_pd3, 5)
topicNames_pd3 <- apply(top5termsPerTopic_pd3, 2, paste, collapse=" ")

# Change to each topic number and re-run
topicToViz_pd3 <- 2 # change for your own topic of interest

# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
tmResult_pd3 <- posterior(topicModel_pd3)
top40terms_pd3 <- sort(tmResult_pd3$terms[topicToViz_pd3,], decreasing=TRUE)[1:40]
words_pd3 <- names(top40terms_pd3)
# extract the probabilities of each of the 40 terms
probabilities_pd3 <- sort(tmResult_pd3$terms[topicToViz_pd3,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
wordcloud2(data.frame(words_pd3, probabilities_pd3), shuffle = FALSE, size = 0.8)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: TOPIC DISTRIBUTIONS FOR DIFFERENT PERIODS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ---- 4.1 Visualize the topic distributions within the documents ----

exampleIds <- c(2, 100, 200)
cat(ADNcorpus_pd1)
cat(ADNcorpus_pd2)
cat(ADNcorpus_pd3)

theta <- tmResult$topics 
dim(theta) 

N <- length(exampleIds)
# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

# Now we change the Alpha-parameter (make it lower) of the model
# (higher alpha results in an even distribution of topics within
# a document)

topicModel2 <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25, seed = 1, alpha = 0.2))
tmResult <- posterior(topicModel2)
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel2, 5), 2, paste, collapse = " ")  # reset topic names
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), 
                           document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)



#still needs work


# ---- 4.2 Rank topics and sort them based on their probability within the corpus ----

topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
topicProportions <- colSums(theta) / nrow(DTM)  # mean probabilities over all paragraphs
names(topicProportions) <- topicNames     # assign the topic names we created before
sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order


# ---- 2.8 Visualize topics in the data over time ----


# append decade information for aggregation
ADN$decade <- paste0(substr(ADN$year, 0, 3), "0")
# get mean topic proportions per decade
topic_proportion_per_decade <- aggregate(theta, by = list(decade = ADN$decade), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames

# reshape data frame
vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")

# plot topic proportions per decade as bar plot


require(pals)
ggplot(vizDataFrame, aes(x=decade, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


