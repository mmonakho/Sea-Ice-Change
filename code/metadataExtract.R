# 
# code: Metadata extract & corpus development
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: October 2021
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT TEXT FILES & EXTRACT METADATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


pacman::p_load(dplyr)


# List files from Alaska Dispatch News (ADN) to be included in corpus (n = 1429)
ADNfiles <- 
  paste('data/corpus/post-processed/', list.files('data/corpus/post-processed'), sep = "")

# Create empty vectors to append full text and metadata to
fullstring <- c()
year <- c()
intro <- c()

# Run loop to import full text and metadata for each file listed in ADNfiles
for(i in 1:length(ADNfiles)) {
  fullstring <- append(fullstring, readLines(ADNfiles[i]))
  year <- append(year, sub(".*Copyright (\\d{4}).*", "\\1", fullstring[i]))
  intro <- append(intro, sub(" Copyright.*", "\\1", fullstring[i]))
}

# Bind the vectors back together into a new data frame
ADN <- cbind.data.frame(doc_id = c(1:length(fullstring)),
                        source = "Alaska Dispatch News",
                        year = year,
                        intro = intro,
                        text = fullstring)

# Create corpus using quanteda package
ADNcorpus <- quanteda::corpus(ADN)

  
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: IMPORT LEMMAS AND STOPWORDS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# Lemma data file 
lemma_data <- read.csv('data/resources/baseform_en.tsv', encoding = 'UTF-8', sep = "\t", header = F) %>%
  rename("inflected_form" = "V1",
         "lemma" = "V2") %>%
  na.omit()

# Stopwords file
stopwords_extended <- readLines('data/resources/stopwords_en.txt', encoding = 'UTF-8')

