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

pacman::p_load(ggplot2)
citation("quanteda")

# ---- Source metadataExtract.R ----

source('code/metadataExtract.R')


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
  geom_histogram(stat = "count", fill = "#332288") +
  scale_x_discrete(name = "",
                   breaks = seq(1995, 2021, by = 3)) +
  scale_y_continuous(name = "", 
                     expand = c(0,0),
                     limits = c(0, 160)) +
  labs(title = "Number documents containing 'sea ice', per year",
       subtitle = "Alaska Dispatch News") +
  theme(plot.title = element_text(size = rel(1),
                                  colour = "#303030",
                                  face = "bold"),
        plot.subtitle = element_text(size = rel(0.75),
                                     colour = "#303030", 
                                     face = "italic"),
        axis.ticks.x = element_line(colour = "#C0C0C0"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "#909090"),
        panel.border = element_rect(fill = NA,
                                    size = 0.25,
                                    colour = "#C0C0C0"),
        panel.grid.major.y = element_line(colour = "#C0C0C0",
                                          size = 0.35,
                                          linetype = 3),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5, unit = "pt"),
        axis.title = element_text(size = rel(0.9),
                                  angle = 0,
                                  face = "bold",
                                  colour = "#303030"),
        axis.text = element_text(size = rel(0.9),
                                 angle = 0,
                                 colour = "#303030",
                                 lineheight = 0.7))


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

year_groups <- list("1995-1997" = c("1995", "1996", "1997"),
                    "1998-2000" = c("1998", "1999", "2000"),
                    "2001-2003" = c("2001", "2002", "2003"))

sort(unique(ADNcorpus$year))

for(i in names(year_groups)) {
  subset_corpus <- 
    quanteda::corpus(ADN %>% filter(year%in%year_groups[i][[1]])) 
  
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

nrow(ADN %>% filter(year%in%year_groups["1995-1997"][[1]]))
