# 
# code: Reading PDF files into R for text mining
# 
# author: Krista Lawless, kllawles@asu.edu
# date: October 2021
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT LIBRARIES, DEFINE FILE PATHS & FUNCTIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Load packages ----
pacman::p_load(tidyverse, pdftools, tm, SnowballC)

# ---- 1.2 Define file paths for pre-processed PDFs ----


articles <- paste('data/corpus/pre-processed/AlaskaDispatchNews_1/', 
                  list.files('data/corpus/pre-processed/AlaskaDispatchNews_1'),
                  sep = "")

corp <- Corpus(URISource(articles),
               readerControl = list(reader = readPDF))

corp <- tm_map(corp, removePunctuation, ucp = TRUE)

# ---- 1.3 Identify list of folders & files you need to access for processing PDFs ----

length(articles)

lapply(articles, length)

# ---- 1.4 Define function for converting pdf to text ----
articles.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf)))) 

findFreqTerms(articles.tdm, lowfreq = 100, highfreq = Inf)

ft <- findFreqTerms(articles.tdm, lowfreq = 100, highfreq = Inf)
as.matrix(articles.tdm[ft,])

ft.tdm <- as.matrix(articles.tdm[ft,])
sort(apply(ft.tdm, 1, sum), decreasing = TRUE)

