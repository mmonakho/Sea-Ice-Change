# 
# code: Topic Modelling
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
  mutate(text = stringr::str_replace_all(stringr::regex("sea ice", ignore_case = T), "sea-ice"),
         text = stringr::str_replace_all(stringr::regex("climate change", ignore_case = T), "climate-change")) %>%
  filter(year%in%c(1995:2003))

ADNcorpus_pd1 <- corpus(ADN_pd1)

corpus_tokens_pd1 <- ADNcorpus_pd1 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)

ADNcollocations_pd1 <- textstat_collocations(corpus_tokens_pd1, min_count = 25)
ADNcollocations-pd1 <- ADNcollocations_pd1[1:250]

corpus_tokens_pd1 <- tokens_compound(corpus_tokens_pd1, sotu_collocations)

# ---- 1.2 Remove terms which occur in less than 1% of all documents

DTM_pd1 <- corpus_tokens_pd1 %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 1, docfreq_type = "prop")

sel_idx_pd1 <- rowSums(DTM_pd1) > 0
DTM_pd1 <- DTM_pd1[sel_idx, ]
ADN_pd1 <- ADN_pd1[sel_idx, ]

require(topicmodels)

# ---- 1.3 Choose the number of topics

K <- 15

# ---- 1.4 Compute the LDA model, inference via n iterations of Gibbs sampling
topicModel_pd1 <- LDA(DTM_pd1, K, method="Gibbs", control=list(iter = 500, seed = 1, verbose = 25))

terms(topicModel_pd1, 10)

# ---- 1.5 Create a word cloud ----

top5termsPerTopic_pd1 <- terms(topicModel_pd1, 5)
topicNames_pd1 <- apply(top5termsPerTopic_pd1, 2, paste, collapse=" ")

topicToViz_pd1 <- 2 # change for your own topic of interest
topicToViz_pd1 <- grep('ice', topicNames_pd1)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
tmResult_pd1 <- posterior(topicModel_pd1)
top40terms_pd1 <- sort(tmResult$terms[topicToViz_pd1,], decreasing=TRUE)[1:40]
words_pd1 <- names(top40terms_pd1)
# extract the probabilities of each of the 40 terms
probabilities_pd1 <- sort(tmResult$terms[topicToViz_pd1,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
wordcloud2(data.frame(words_pd1, probabilities_pd1), shuffle = FALSE, size = 0.8)


# ---- 1.6 Visualize the topic distributions within the documents ----

exampleIds_pd1 <- c(2, 100, 200)
cat(ADNcorpus_pd1[exampleIds_pd1[1]])
cat(ADNcorpus_pd1[exampleIds_pd1[2]])
cat(ADNcorpus_pd1[exampleIds_pd1[3]])

theta_pd1 <- tmResult_pd1$topics 
dim(theta_pd1) 

N_pd1 <- length(exampleIds_pd1)
# get topic proportions form example documents
topicProportionExamples_pd1 <- theta[exampleIds_pd1,]
colnames(topicProportionExamples_pd1) <- topicNames_pd1
vizDataFrame_pd1 <- melt(cbind(data.frame(topicProportionExamples_pd1), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame_pd1, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

# Now we change the Alpha-parameter (make it lower) of the model
# (higher alpha results in an even distribution of topics within
# a document)

topicModel2_pd1 <- ADN(DTM_pd1, K, method="Gibbs", control=list(iter = 500, verbose = 25, seed = 1, alpha = 0.2))
tmResult_pd1 <- posterior(topicModel2_pd1)
theta_pd1 <- tmResult_pd1$topics
beta_pd1 <- tmResult_pd1$terms
topicNames_pd1 <- apply(terms(topicModel2_pd1, 5), 2, paste, collapse = " ")  # reset topicnames
topicProportionExamples_pd1 <- theta[exampleIds_pd1,]
colnames(topicProportionExamples_pd1) <- topicNames_pd1
vizDataFrame_pd1 <- melt(cbind(data.frame(topicProportionExamples_pd1), 
                           document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame_pd1, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)


# ---- 2.7 Rank topics and sort them based on their probability within the corpus ----

topicNames_pd1 <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
topicProportions_pd1 <- colSums(theta_pd1) / nrow(DTM_pd1)  # mean probabilities over all paragraphs
names(topicProportions_pd1) <- topicNames_pd1     # assign the topic names we created before
sort(topicProportions_pd1, decreasing = TRUE) # show summed proportions in decreased order


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

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: TOPIC MODELLING FOR YEARS 1995:2003 ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++