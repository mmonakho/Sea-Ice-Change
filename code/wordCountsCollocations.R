# 
# code: Initial word counts
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: October 2021
# 
# 

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE FILES AND PRE-PROCESS DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- Source metadataExtract.R ----

source('code/metaDataExtract.R')


# ---- Tokenize corpus ----

ADNcorpus_tokens <- ADNcorpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = TRUE)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: NUMBER DOCUMENTS PER YEAR ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

pacman::p_load(ggplot2)


num_docs_plot <- 
  ggplot(ADN, aes(x = year)) +
  geom_hist()


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: EXTRACTING WORD COUNTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# TM4SS TUTORIAL: https://tm4ss.github.io/docs/Tutorial_3_Frequency.html


# ---- 3.1 Find single word counts across entire corpus ----

# ADN_DTM <- ADNcorpus_tokens %>%
#   dfm()
# 
# ADN_DTM_reduced <- ADN_DTM[, c("climate", "sea")]


# ---- 3.2 Find single word counts per year for top XX words ----

counts_per_year <- aggregate(DTM_reduced, by = list(year = ADN$year), sum)


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: EXTRACTING BIGRAMS AND TRIGRAMS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 4.1 Find bi-grams and tri-grams across entire corpus ----

ADN_bigrams <- textstat_collocations(ADNcorpus_tokens, size = 2, min_count = 25) %>%
  filter(!grepl("anchorage daily|image link|reserve section|main pg|word|@card@", collocation))

ADN_trigrams <- textstat_collocations(ADNcorpus_tokens, size = 3, min_count = 25) %>%
  filter(!grepl("anchorage daily|image link|reserve section|main pg|word|@card@", collocation))

head(ADN_bigrams, 25)


# ---- 4.2 Find bi-grams per year ----

year_groups <- list("1995-1999" = c("1995", "1996", "1997", "1998", "1999"),
                    "2000-2004" = c("2000", "2001", "2002", "2003", "2004"))

sort(unique(ADNcorpus$year))

for(i in names(year_groups)) {
  subset_corpus <- 
    quanteda::corpus(ADN %>% filter(year%in%year_groups[i][1])) 
  
  token_subset <- 
    subset_corpus %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
    tokens_remove(pattern = stopwords_extended, padding = TRUE)
  
  newcols <- paste0(c("count", "lambda", "z"), i, sep = "_")

  assign(paste("bigrams_", i, sep = ""),
         textstat_collocations(token_subset, size = 2, min_count = 10) %>%
           filter(!grepl("anchorage daily|image link|reserve section|main pg|word|@card@", collocation)) %>%
           rename_with(~ paste0(., "_", i, sep = ""), !starts_with("collocation")))
    
}

list_bigram_df <- paste0("bigrams_", sort(unique(ADNcorpus$year)), sep = "")
bigrams_byyear <- bigrams_1995

for(i in list_bigram_df[-1]) {
  bigrams_byyear <- full_join(bigrams_byyear, get(i), by = "collocation")
}

nrow(ADN %>% filter(year%in%year_groups[1][1]))
