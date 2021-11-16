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


# ---- Identify year groups to cluster data by ----

year_groups <- list("1995-1997" = c("1995", "1996", "1997"),
                    "1998-2000" = c("1998", "1999", "2000"),
                    "2001-2003" = c("2001", "2002", "2003"),
                    "2004-2006" = c("2004", "2005", "2006"),
                    "2007-2009" = c("2007", "2008", "2009"),
                    "2010-2012" = c("2010", "2011", "2012"),
                    "2013-2015" = c("2013", "2014", "2015"),
                    "2016-2018" = c("2016", "2017", "2018"),
                    "2019-2021" = c("2019", "2020", "2021"))


# Define a substring function to pull XX number of digits from the right of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: NUMBER DOCUMENTS PER YEAR ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# Number of docs per year period from ADN corpus
ADN <- ADN %>% mutate(period = NA)

for(i in names(year_groups)) {
  ADN <- ADN %>%
    mutate(period = ifelse(year%in%year_groups[i][[1]], i, period)) 
}

ADN_ndocs_byperiod <- ADN %>%
  group_by(period) %>%
  summarise(ndocs = length(docid))


# Number docs per year
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

num_docs_peryear <- ADN %>% group_by(year) %>% summarise(n = length(docid))

mean(num_docs_peryear$n)


# Number docs per 3-year period
num_docs_byperiod_plot <- 
  ggplot(ADN, aes(x = period)) +
  geom_histogram(stat = "count", fill = "#332288") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", 
                     expand = c(0,0)) +
  labs(title = "Number documents containing 'sea ice', per 3-year period",
       subtitle = "Alaska Dispatch News") +
  seaice.plot.theme +
  theme(axis.text.x = element_text(angle = 330, 
                                   vjust = 1,
                                   hjust = 0))

# Number docs per 8-year period
num_docs_8yr_period <- 
  ADN %>%
  mutate(period = ifelse(year%in%c(1995:2003), "1", ifelse(year%in%c(2004:2012), "2", "3"))) %>%
  group_by(period) %>%
  summarise(count = length(docid))


# Export plots
png("data/outputs/NumDocs_peryear.png",
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(num_docs_plot)
dev.off()


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

seaice_trigrams <-
  ADN_trigrams %>% 
  filter(collocation%in%grep("sea ice", collocation, value = TRUE)) %>%
  mutate(collocation=stringr::str_replace_all(collocation, "sea ice", ""),
         collocation=stringr::str_trim(collocation, side = "both")) %>%
  group_by(collocation) %>%
  summarise(value = sum(count, na.rm = TRUE)) %>%
  arrange(desc(value))

# Subset a list of most frequent bigrams across entire corpus
ADN_bigrams_mostfrequent <- head(ADN_bigrams, 5)
ADN_trigrams_seaice_mostfrequent <- head(seaice_trigrams, 10)

# ---- 4.2 Find bi-grams per year ----

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


# ---- 4.2 Find sea ice-related tri-grams per year ----

# Calculate sea ice tri-grams per year group
for(i in names(year_groups)) {
  subset_corpus <- 
    quanteda::corpus(ADN %>% filter(year%in%year_groups[i][[1]])) 
  
  token_subset <- 
    subset_corpus %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
    tokens_remove(pattern = stopwords_extended, padding = TRUE)
  
  assign(paste("trigrams_", i, sep = ""),
         textstat_collocations(token_subset, size = 3, min_count = 1) %>%
           rename_with(~ paste0(., "_", i, sep = ""), !starts_with("collocation")) %>%
           filter(collocation%in%grep("sea ice", collocation, value = TRUE)))
  
}

# Join bi-grams together across all year groups
list_trigram_df <- paste0("trigrams_", names(year_groups), sep = "")
trigrams_byperiod <- `trigrams_1995-1997`

for(i in list_trigram_df[-1]) {
  trigrams_byperiod <- full_join(trigrams_byperiod, get(i), by = "collocation")
}



# Wrangle top 5 bi-grams (across full corpus) by year period
top10_trigrams_byperiod <- 
  as.data.frame(trigrams_byperiod) %>% 
  select(-contains("count_nested") & -contains("length") & -contains("lambda") & -contains("z")) %>%
  tidyr::pivot_longer(cols = `count_1995-1997`:`count_2019-2021`) %>%
  mutate(period = substrRight(name, 9),
         variable = sub("_.*", "\\1", name),
         collocation=stringr::str_replace_all(collocation, "sea ice", ""),
         collocation=stringr::str_trim(collocation, side = "both")) %>%
  group_by(collocation, period, variable) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(collocation%in%ADN_trigrams_seaice_mostfrequent$collocation) %>%
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
  scale_color_ptol(name = "Collocation") +
  scale_y_continuous(name = "# / total documents\n",
                     expand = c(0,0),
                     limits = c(0, 4.5)) +
  scale_x_discrete(name = "",
                   expand = c(0,0)) +
  seaice.plot.theme +
  theme(axis.text.x = element_text(angle = 330, 
                                   vjust = 1,
                                   hjust = 0)) +
  labs(title = "Most Frequent Bigrams")


plot_seaice_trigrams_byyear <- 
  ggplot(data = top10_trigrams_byperiod) +
  geom_line(aes(x = period, y = value_scaled, 
                group = collocation, color = collocation),
            size = 1.5) +
  scale_color_ptol(name = "Collocation") +
  scale_y_continuous(name = "# / total documents\n",
                     expand = c(0,0),
                     limits = c(0, 0.35)) +
  scale_x_discrete(name = "",
                   expand = c(0,0)) +
  seaice.plot.theme +
  theme(axis.text.x = element_text(angle = 330, 
                                   vjust = 1,
                                   hjust = 0)) +
  labs(title = "Top 10 Words Collocated with 'Sea Ice'")
  


# Export plots
png("data/outputs/Bigrams_peryearperiod.png",
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(plot_bigrams_byyear)
dev.off()

png("data/outputs/Seaice_trigrams_peryearperiod.png",
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(plot_seaice_trigrams_byyear)
dev.off()
