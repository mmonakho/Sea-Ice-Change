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

pacman::p_load(topicmodels, quanteda, quanteda.textstats, wordcloud2,
               reshape2, ggplot2, pals)
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

corpus_tokens <- tokens_compound(corpus_tokens, ADNcollocations)

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

# ---- 2.1 Create a word cloud ----

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


# ---- 2.2 Visualize the topic distributions within the documents ----

exampleIds <- c(2, 100, 200)
cat(ADNcorpus[exampleIds[1]])
cat(ADNcorpus[exampleIds[2]])
cat(ADNcorpus[exampleIds[3]])

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
topicNames <- apply(terms(topicModel2, 5), 2, paste, collapse = " ")  # reset topicnames
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), 
                           document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)


# ---- 2.3 Rank topics and sort them based on their probability within the corpus ----

topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
topicProportions <- colSums(theta) / nrow(DTM)  # mean probablities over all paragraphs
names(topicProportions) <- topicNames     # assign the topic names we created before
sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order


# ---- 2.4 Visualize topics in the data over time ----


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