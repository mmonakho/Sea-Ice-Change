# 
# code: Check for duplicated articles (requires outputs from Julia function)
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: April 2022
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE LIBARIES, CREATE OUTPUT DATA FRAMES, IMPORT DATA FROM JULIA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 1.1 Load library ----

pacman::p_load(stringdist)


# ---- 1.2 Create empty duplicates matrix ----

checkduplicates_to10 <- matrix(NA, nrow = length(ADN$text), ncol = length(ADN$text[1:10]),
                          dimnames = list(paste("text", ADN$docid, sep = ""), paste("text", ADN$docid[1:10], sep = "")))



# --- 1.3 Pull in checkduplicates csv files from Julia ----

checkduplicates_1_100 <- import('data/corpus/checkduplicates_1-100.csv', header = F) %>%
  mutate(V3 = NA, V4 = NA)

checkduplicates_101_500 <- import('data/corpus/checkduplicates_101-500.csv', header = F)
checkduplicates_501_1423 <- import('data/corpus/checkduplicates_501-1423.csv', header = F)


checkduplicates <- rbind.data.frame(checkduplicates_1_100, checkduplicates_101_500, checkduplicates_501_1423) %>%
  select(-V1) %>%
  filter(!is.na(V3)) %>%
  group_by(V2, V3, V4) %>%
  summarise(n = length(V2)) %>%
  ungroup() %>%
  mutate(id = seq(1:length(V2))) %>%
  pivot_longer(c(V2, V3, V4), names_to = "Match", values_to = "row") %>%
  na.omit() %>%
  left_join(docid_lkp, by = "row")

export(checkduplicates, 'data/corpus/manualcheck_duplicates.csv')



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: MANUALLY EXPLORE DUPLICATE DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Check ADN for "FINAL EDITION" amongst duplicates ----
# (only keep FINAL EDITIONS when there are duplicates) 

ADN_check <- data.frame(docid = NA,
                        text = NA,
                        year = NA)


# ---- 2.2 Run loop to import full text, post-process, and extract metadata for each file listed in ADNfiles ----

for(i in 1:length(ADNfiles)) {
  file <- readLines(ADNfiles[i])
  year <- sub(".*Copyright (\\d{4}).*", "\\1", file)
  
  output <- data.frame(docid = stringr::str_replace_all(ADNfiles[i], "data/corpus/post-processed/text", "") %>%
                         stringr::str_replace_all(".txt", "") %>%
                         as.numeric(),
                       text = file,
                       year = year)
  
  ADN_check<- rbind.data.frame(ADN_check, output)
  
}


# ---- 2.3 Identify true duplicates, and decide which to remove ----

identifyduplicate <- 
  checkduplicates %>%
  left_join(ADN_check %>% mutate(filename = paste("text", docid, sep = "")), by = "filename") %>%
  mutate(finaledition = ifelse(grepl("FINAL EDITION", text, ignore.case = F)==T, "Y", "N"))

duplicates_toremove <- 
  identifyduplicate %>%
  filter(finaledition=="N")
