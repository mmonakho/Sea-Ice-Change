# 
# code: data mining - 1st try

# 
# author: Masha Monakhova, mmonakho@asu.edu
# date: October 2021
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: ??? ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ---- 1.1 Set options and load packages ----

options(stringsAsFactors = F)
options(scipen = 999)
options(max.print=1000)

pacman::p_load (cluster, corpus, FactoMineR, factoextra,
                flextable, GGally, ggdendro, igraph, network,
                Matrix, quanteda, sna, tidyverse, tm, tokenizers, 
                tidytext)

txtfiles <- paste('data/corpus/post-processed/', 
                  list.files('data/corpus/post-processed/'),
                  sep = '')

# ---- 1.2 Bi-grams ----

# read in text
newspapers <- base::readLines(txtfiles[1]) %>%
  paste0(collapse = " ") %>%
  stringr::str_squish() %>%
  stringr::str_remove_all("- ")
# further processing
newspapers_split <- newspapers %>% 
  as_tibble() %>%
  tidytext::unnest_tokens(words, value)
# create data frame
newspapers_words <- newspapers_split %>%
  dplyr::rename(word1 = words) %>%
  dplyr::mutate(word2 = c(word1[2:length(word1)], NA)) %>%
  na.omit()