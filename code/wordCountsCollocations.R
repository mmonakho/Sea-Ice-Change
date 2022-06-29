# 
# code: Initial analysis - number of docs, bigrams, trigrams
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: October 2021; modified: June 2022
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE FILES AND PRE-PROCESS DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Source metadataExtract.R ----

source('code/metadataExtract.R')
source('code/plotThemes.R')


# ---- 1.2 Tokenize corpus ----

ADNcorpus_tokens <- ADNcorpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = TRUE)


# ---- 1.3 Identify year groups to cluster data by ----

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


# ---- 2.1 Number of docs per year period from ADN corpus ----

ADN <- ADN %>% mutate(period = NA)

for(i in names(year_groups)) {
  ADN <- ADN %>%
    mutate(period = ifelse(year%in%year_groups[i][[1]], i, period)) 
}

ADN_ndocs_byperiod <- ADN %>%
  group_by(period) %>%
  summarise(ndocs = length(docid))


# ---- 2.2 Plot number docs per year ----

num_docs_plot <- 
  ggplot(ADN, aes(x = year)) +
  geom_histogram(stat = "count", fill = "#332288") +
  geom_text(aes(x = 13, y = 111, label = "*")) +
  geom_text(aes(x = 18, y = 102, label = "*")) +
  geom_text(aes(x = 21, y = 158, label = "*")) +
  scale_x_discrete(name = "",
                   breaks = seq(1995, 2021, by = 3)) +
  scale_y_continuous(name = "", 
                     expand = c(0,0),
                     limits = c(0, 166)) +
  labs(title = "Number documents containing 'sea ice', per year",
       subtitle = "Alaska Dispatch News") +
  seaice.plot.theme

num_docs_plot_arranged <-
  grid.arrange(num_docs_plot, 
               bottom = grid.text(label = "*Year of record-breaking loss in sea ice extent",
                                  x = unit(45, "pt"),
                                  just = "left"))

num_docs_peryear <- ADN %>% group_by(year) %>% summarise(n = length(docid))

# export(num_docs_peryear, 'data/outputs/num_docs_per_year.csv')


# ---- 2.3 Plot number docs per 3-year period ----

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


# ---- 2.4 Calcaulte number docs per 9-year period ----

num_docs_9yr_period <- 
  ADN %>%
  mutate(period = ifelse(year%in%c(1995:2003), "1", ifelse(year%in%c(2004:2012), "2", "3"))) %>%
  group_by(period) %>%
  summarise(count = length(docid))


# ---- 2.5 Export number of docs per year plots ----

png("data/outputs/NumDocs_peryear.png",
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(num_docs_plot_arranged)
dev.off()



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: EXTRACTING BIGRAMS AND TRIGRAMS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Find bi-grams and tri-grams across entire corpus ----

ADN_bigrams <- textstat_collocations(ADNcorpus_tokens, size = 2, min_count = 25) %>%
  arrange(desc(count))

ADN_trigrams <- textstat_collocations(ADNcorpus_tokens, size = 3, min_count = 25)

seaice_trigrams <-
  ADN_trigrams %>% 
  filter(collocation%in%grep("sea ice", collocation, value = TRUE)) %>%
  mutate(collocation=stringr::str_replace_all(collocation, "sea ice", ""),
         collocation=stringr::str_trim(collocation, side = "both")) %>%
  group_by(collocation) %>%
  summarise(value = sum(count, na.rm = TRUE)) %>%
  arrange(desc(value))
  
# Look for sea ice trigrams with lower threshold for inclusion (minimum count of 10, instead of 25)
ADN_trigrams_min10 <- textstat_collocations(ADNcorpus_tokens, size = 3, min_count = 10)

seaice_trigrams_min10 <-
  ADN_trigrams_min10 %>% 
  filter(collocation%in%grep("sea ice", collocation, value = TRUE)) %>%
  mutate(collocation=stringr::str_replace_all(collocation, "sea ice", ""),
         collocation=stringr::str_trim(collocation, side = "both")) %>%
  group_by(collocation) %>%
  summarise(value = sum(count, na.rm = TRUE)) %>%
  arrange(desc(value))


# Subset a list of most frequent bigrams across entire corpus
ADN_bigrams_mostfrequent <- head(ADN_bigrams, 6)
ADN_trigrams_seaice_mostfrequent <- head(seaice_trigrams, 10)


# ---- 3.2 Find bi-grams per year ----

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



# Wrangle top 6 bi-grams (across full corpus) by year period
top6_bigrams_byperiod <- 
  as.data.frame(bigrams_byperiod) %>% 
  filter(collocation%in%ADN_bigrams_mostfrequent$collocation) %>%
  select(-contains("count_nested") & -contains("length")) %>%
  tidyr::pivot_longer(cols = `count_1995-1997`:`z_2019-2021`) %>%
  mutate(period = substrRight(name, 9),
         variable = sub("_.*", "\\1", name)) %>%
  select(-name) %>%
  left_join(ADN_ndocs_byperiod, by = "period") %>%
  mutate(value_scaled = value / ndocs,
         collocation = factor(collocation, levels = ADN_bigrams_mostfrequent$collocation, ordered = T))


# ---- 3.2 Find sea ice-related tri-grams per year ----

# Calculate "sea ice" tri-grams per year group
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



# Wrangle top 10 tri-grams containing "sea ice" (across full corpus), by year period
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
  mutate(value_scaled = value / ndocs,
         collocation = factor(collocation, levels = ADN_trigrams_seaice_mostfrequent$collocation, ordered = T))


# ---- 3.3 Look for "ice"-only bi-grams to compare to "sea ice" trigrams ----

ADN_ice_bigrams <- 
  ADN %>% 
  mutate(text = stringr::str_replace_all(text, "sea ice", "seaice"),
         text = stringr::str_replace_all(text, "SEA ICE", "seaice"),
         text = stringr::str_replace_all(text, "Sea ice", "seaice"),
         title = stringr::str_replace_all(title, "sea ice", "seaice"),
         title = stringr::str_replace_all(title, "SEA ICE", "seaice"),
         title = stringr::str_replace_all(title, "Sea ice", "seaice")) %>%
  quanteda::corpus() %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = TRUE) %>%
  textstat_collocations(., size = 2, min_count = 25)

ice_bigrams <-
  ADN_ice_bigrams %>% 
  filter(collocation%in%grep("ice", collocation, value = TRUE)) %>%
  filter(!collocation%in%grep("service|police|officer|price|seaice", collocation, value = TRUE)) %>%
  mutate(collocation=stringr::str_replace_all(collocation, "ice", ""),
         collocation=stringr::str_trim(collocation, side = "both")) %>%
  group_by(collocation) %>%
  summarise(value = sum(count, na.rm = TRUE),
            term = "ice") %>%
  arrange(desc(value))

ice_seaice_bigrams <-
  seaice_trigrams %>% mutate(term = "sea ice") %>%
  rbind.data.frame(ice_bigrams)

# export(ice_seaice_bigrams, "data/outputs/ice_seaice_bigrams.csv")


# Calculate "ice" bi-grams per year group
for(i in names(year_groups)) {
  subset_corpus <- 
    quanteda::corpus(ADN %>% 
                       mutate(text = stringr::str_replace_all(text, "sea ice", "seaice"),
                              text = stringr::str_replace_all(text, "SEA ICE", "seaice"),
                              text = stringr::str_replace_all(text, "Sea ice", "seaice"),
                              title = stringr::str_replace_all(title, "sea ice", "seaice"),
                              title = stringr::str_replace_all(title, "SEA ICE", "seaice"),
                              title = stringr::str_replace_all(title, "Sea ice", "seaice")) %>% 
                       filter(year%in%year_groups[i][[1]])) 
  
  token_subset <- 
    subset_corpus %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
    tokens_remove(pattern = stopwords_extended, padding = TRUE)
  
  assign(paste("ice_bigrams_", i, sep = ""),
         textstat_collocations(token_subset, size = 2, min_count = 1) %>%
           rename_with(~ paste0(., "_", i, sep = ""), !starts_with("collocation")) %>%
           filter(collocation%in%grep("ice", collocation, value = TRUE)) %>%
           filter(!collocation%in%grep("service|police|officer|price|seaice", collocation, value = TRUE)))
  
}

# Join bi-grams together across all year groups
list_ice_bigram_df <- paste0("ice_bigrams_", names(year_groups), sep = "")
ice_bigrams_byperiod <- `ice_bigrams_1995-1997`

for(i in list_ice_bigram_df[-1]) {
  ice_bigrams_byperiod <- full_join(ice_bigrams_byperiod, get(i), by = "collocation")
}



# Wrangle top 10 bi-grams containing "ice" (across full corpus), by year period
top10_ice_bigrams_byperiod <- 
  as.data.frame(ice_bigrams_byperiod) %>% 
  select(-contains("count_nested") & -contains("length") & -contains("lambda") & -contains("z")) %>%
  tidyr::pivot_longer(cols = `count_1995-1997`:`count_2019-2021`) %>%
  mutate(period = substrRight(name, 9),
         variable = sub("_.*", "\\1", name),
         collocation=stringr::str_replace_all(collocation, "ice", ""),
         collocation=stringr::str_trim(collocation, side = "both")) %>%
  group_by(collocation, period, variable) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(collocation%in%ice_bigrams$collocation[1:10]) %>%
  left_join(ADN_ndocs_byperiod, by = "period") %>%
  mutate(value_scaled = value / ndocs,
         collocation = factor(collocation, levels = ice_bigrams$collocation[1:10], ordered = T))


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: VISUALIZING BIGRAMS AND TRIGRAMS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 4.1 Plot bigrams by 3-year period ----

plot_bigrams_byyear <- 
  ggplot(data = top6_bigrams_byperiod %>% filter(variable=="count")) +
  geom_vline(aes(xintercept = 3.5), linetype = 1, color = "#C0C0C0", size = 0.35) +
  geom_vline(aes(xintercept = 6.5), linetype = 1, color = "#C0C0C0", size = 0.35) +
  geom_text(aes(x = 2.25, y = 4.35, label = "1995 - 2003"),
            color = "#909090", size = 2.5) +
  geom_text(aes(x = 5, y = 4.35, label = "2004 - 2012"),
            color = "#909090", size = 2.5) +
  geom_text(aes(x = 7.75, y = 4.35, label = "2013 - 2021"),
            color = "#909090", size = 2.5) +
  geom_line(aes(x = period, y = value_scaled, 
                group = collocation, color = collocation),
            size = 1.5) +
  scale_color_ptol(name = "") +
  scale_y_continuous(name = "",
                     expand = c(0,0),
                     limits = c(0, 4.5)) +
  scale_x_discrete(name = "",
                   expand = c(0,0)) +
  seaice.plot.theme +
  theme(axis.text.x = element_text(angle = 330, 
                                   vjust = 1,
                                   hjust = 0)) +
  labs(title = "Most Frequent Bigrams", subtitle = "Relative frequency per time period (count per # of documents)")


# ---- 4.2 Plot "sea ice" trigrams by 3-year period ----

plot_seaice_trigrams_byyear <- 
  ggplot(data = top10_trigrams_byperiod) +
  geom_vline(aes(xintercept = 3.5), linetype = 1, color = "#C0C0C0", size = 0.35) +
  geom_vline(aes(xintercept = 6.5), linetype = 1, color = "#C0C0C0", size = 0.35) +
  geom_text(aes(x = 2.25, y = 0.34, label = "1995 - 2003"),
            color = "#909090", size = 2.5) +
  geom_text(aes(x = 5, y = 0.34, label = "2004 - 2012"),
            color = "#909090", size = 2.5) +
  geom_text(aes(x = 7.75, y = 0.34, label = "2013 - 2021"),
            color = "#909090", size = 2.5) +
  geom_line(aes(x = period, y = value_scaled, 
                group = collocation, color = collocation),
            size = 1.5) +
  scale_color_ptol(name = "") +
  scale_y_continuous(name = "",
                     expand = c(0,0),
                     limits = c(0, 0.35)) +
  scale_x_discrete(name = "",
                   expand = c(0,0)) +
  seaice.plot.theme +
  theme(axis.text.x = element_text(angle = 330, 
                                   vjust = 1,
                                   hjust = 0)) +
  labs(title = "Top 10 Words Collocated with 'Sea Ice'", subtitle = "Relative frequency per time period (count per # of documents)")
  

# ---- 4.3 Plot "ice"-only bigrams by 3-year period ----

plot_ice_bigrams_byyear <- 
  ggplot(data = top10_ice_bigrams_byperiod) +
  geom_line(aes(x = period, y = value_scaled, 
                group = collocation, color = collocation),
            size = 1.5) +
  scale_color_ptol(name = "") +
  scale_y_continuous(name = "",
                     expand = c(0,0),
                     limits = c(0, 0.4)) +
  scale_x_discrete(name = "",
                   expand = c(0,0)) +
  seaice.plot.theme +
  theme(axis.text.x = element_text(angle = 330, 
                                   vjust = 1,
                                   hjust = 0)) +
  labs(title = "Top 10 Words Collocated with 'Ice'", subtitle = "Relative frequency per time period (count per # of documents)")



# ---- 4.4 Export plots ----

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

png("data/outputs/Ice_bigrams_peryearperiod.png",
    units = "in", height = 6, width = 8, res = 400)
grid.newpage()
grid.draw(plot_ice_bigrams_byyear)
dev.off()
