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

source('code/metadataExtract.R')
source('code/plotThemes.R')


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
  seaice.plot.theme



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

ADN_bigrams <- textstat_collocations(ADNcorpus_tokens, size = 2, min_count = 25)

ADN_trigrams <- textstat_collocations(ADNcorpus_tokens, size = 3, min_count = 25)


# Subset a list of most frequent bigrams across entire corpus
ADN_bigrams_mostfrequent <- head(ADN_bigrams, 5)


# ---- 4.2 Find bi-grams per year ----

# Identify year groups to cluster data by
year_groups <- list("1995-1997" = c("1995", "1996", "1997"),
                    "1998-2000" = c("1998", "1999", "2000"),
                    "2001-2003" = c("2001", "2002", "2003"),
                    "2004-2006" = c("2004", "2005", "2006"),
                    "2007-2009" = c("2007", "2008", "2009"),
                    "2010-2012" = c("2010", "2011", "2012"),
                    "2013-2015" = c("2013", "2014", "2015"),
                    "2016-2018" = c("2016", "2017", "2018"),
                    "2019-2021" = c("2019", "2020", "2021"))

# Calculate bi-grams per year group
for(i in names(year_groups)) {
  subset_corpus <- 
    quanteda::corpus(ADN %>% filter(year%in%year_groups[i][[1]])) 
  
  token_subset <- 
    subset_corpus %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
    tokens_remove(pattern = stopwords_extended, padding = TRUE)

  assign(paste("bigrams_", i, sep = ""),
         textstat_collocations(token_subset, size = 2, min_count = 3) %>%
           rename_with(~ paste0(., "_", i, sep = ""), !starts_with("collocation")))
    
}

# Join bi-grams together across all year groups
list_bigram_df <- paste0("bigrams_", names(year_groups), sep = "")
bigrams_byperiod <- `bigrams_1995-1997`

for(i in list_bigram_df[-1]) {
  bigrams_byperiod <- full_join(bigrams_byperiod, get(i), by = "collocation")
}



# Define a substring function to pull XX number of digits from the right of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Number of docs per year period from ADN corpus
ADN <- ADN %>% mutate(period = NA)

for(i in names(year_groups)) {
  ADN <- ADN %>%
  mutate(period = ifelse(year%in%year_groups[i][[1]], i, period)) 
}

ADN_ndocs_byperiod <- ADN %>%
  group_by(period) %>%
  summarise(ndocs = length(docid))

# Wrangle top 5 bi-grams (across full corpus) by year period
top5_bigrams_byperiod <- 
  as.data.frame(bigrams_byperiod) %>% 
  filter(collocation%in%ADN_bigrams_mostfrequent$collocation) %>%
  select(-contains("count_nested") & -contains("length")) %>%
  tidyr::pivot_longer(cols = `count_1995-1997`:`z_2019-2021`) %>%
  mutate(period = substrRight(name, 9),
         variable = sub("_.*", "\\1", name)) %>%
  select(-name) %>%
  left_join(ADN_ndocs_byperiod, by = "period") %>%
  mutate(value_scaled = value / ndocs)

  

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: VISUALIZING BIGRAMS AND TRIGRAMS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


plot_bigrams_byyear <- 
  ggplot(data = top5_bigrams_byperiod %>% filter(variable=="count")) +
  geom_line(aes(x = period, y = value_scaled, 
                group = collocation, color = collocation),
            size = 1.5) +
  scale_color_ptol(name = "Collocation\n(stemmed)") +
  scale_y_continuous(name = "Count / number documents",
                     expand = c(0,0)) +
  scale_x_discrete(name = "",
                   expand = c(0,0)) +
  seaice.plot.theme +
  theme(axis.text.x = element_text(angle = 330, 
                                   vjust = 1,
                                   hjust = 0))
  