# 
# code: Initial word counts
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: October 2021
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT LIBRARIES & CORPUS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

pacman::p_load()


ADNfiles <- 
  paste('data/corpus/post-processed/', list.files('data/corpus/post-processed'), sep = "")
  

  
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: EXTRACT METADATA FROM TXT FILES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


fullstring <- c()
year <- c()
intro <- c()


for(i in 1:length(ADNfiles)) {
  fullstring <- append(fullstring, readLines(ADNfiles[i]))
  year <- append(year, sub(".*Copyright (\\d{4}).*", "\\1", fullstring[i]))
  intro <- append(intro, sub(" Copyright.*", "\\1", fullstring[i]))
}

ADNcorpus <- cbind.data.frame(source = "Alaska Dispatch News",
                              year = year,
                              intro = intro,
                              fullstring = fullstring)
