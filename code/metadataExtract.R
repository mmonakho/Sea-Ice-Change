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


pacman::p_load(dplyr, quanteda, quanteda.textstats, ggthemes, grid, gridExtra, ggplot2)


# List files from Alaska Dispatch News (ADN) to be included in corpus (n = 1429)
ADNfiles <- 
  paste('data/corpus/post-processed/', list.files('data/corpus/post-processed'), sep = "")

# Create empty data frame to append full text and metadata to
ADN <- data.frame(docid = NA,
                  text = NA,
                  title = NA,
                  year = NA)


# Run loop to import full text, post-process, and extract metadata for each file listed in ADNfiles
for(i in 1:length(ADNfiles)) {
  file <- readLines(ADNfiles[i])
  year <- sub(".*Copyright (\\d{4}).*", "\\1", file)
  
  title1 <- stringr::str_replace(file, " Anchorage Daily News.*.Copyright.*", "")
  title2 <- stringr::str_replace(file, " Alaska Dispatch News.*.Copyright.*", "")
  
  title <- ifelse(title1==file, title2, title1)
  
  aftertitle <- stringr::str_replace(file, title, "") %>%
    stringr::str_replace_all(";", "; ") %>%
    stringr::str_replace_all(title, " ") %>% # some text files repeat the title throughout the text, as the article was going onto new pages 
    # (need to remove duplicated titles, so that word counts are not artificially inflated)
    stringr::str_replace_all("Anchorage Daily News.*.Body", "") %>%
    stringr::str_replace_all("Alaska Dispatch News.*.Body", "") %>%
    stringr::str_replace_all("Anchorage Daily News", "") %>%
    stringr::str_replace_all("Alaska Dispatch News", "") %>%
    stringr::str_replace_all(stringr::regex("Associated Press", ignore_case = T), "") %>%
    stringr::str_replace_all(stringr::regex("Photo by", ignore_case = T), "") %>%
    stringr::str_replace_all(stringr::regex("Illustrated by", ignore_case = T), "") %>%
    stringr::str_replace_all(stringr::regex("Associated Press Graphic", ignore_case = T), "") %>%
    stringr::str_replace_all(stringr::regex("Graphic image link", ignore_case = T), "") %>%
    stringr::str_replace_all(stringr::regex("image link", ignore_case = T), "") %>%
    stringr::str_replace_all("Load-Date:", "") %>%
    stringr::str_replace_all("End of Document", "")
  
  clean <- paste(title, aftertitle)
  
  output <- data.frame(docid = i,
                       text = clean,
                       title = title, 
                       year = year)
  
  ADN <- rbind.data.frame(ADN, output)
  
}

ADN <- ADN %>% filter(!is.na(docid))

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

