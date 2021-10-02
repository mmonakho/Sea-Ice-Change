# 
# code: Corpus pre-processing, from Nexis Lexis sources
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# date: October 2021
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT LIRBARIES, DEFINE FILE PATHS & FUNCTIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Load packages ----

pacman::p_load(rio, tidyverse, stringr, quanteda, tidytext, tm, DT)


# ---- 1.2 Define file paths for pre-processed PDFs & post-processed txt files ----

# -- Create directories, if they aren't already created 
#     (OK to run these lines of script even if you have already defined these directories,  
#      will just get a friendly warning)
dir.create("data/corpus/pre-processed")
dir.create("data/corpus/post-processed")


pre_processed <- "data/corpus/pre-processed"
post_processed <- "data/corpus/post-processed"


# ---- 1.3 Identify list of folders & files you need to access for processing PDFs ----

folders_toprocess <- paste0("AlaskaDispatchNews_", seq(1:16), sep = "")

files_toprocess <- c()

for(i in 1:length(folders_toprocess)) {
  folder <- folders_toprocess[i]
  files <- paste(pre_processed, folder, list.files(paste(pre_processed, folder, sep = "/")), sep = "/")
  files_toprocess <- append(files_toprocess, files, length(files_toprocess))
}


# ---- 1.4 Define function for converting pdf to text ----

convertpdf2txt <- function(filename){
  x <- sapply(filename, function(x){
    x <- pdftools::pdf_text(x) %>%
      paste(sep = " ") %>%
      stringr::str_replace_all(fixed("\n"), " ") %>%
      stringr::str_replace_all(fixed("\r"), " ") %>%
      stringr::str_replace_all(fixed("\t"), " ") %>%
      stringr::str_replace_all(fixed("\""), " ") %>%
      paste(sep = " ", collapse = " ") %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("- ", "") 
    return(x)
  })
}


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: CONVERT PDFS TO TEXT AND EXPORT ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 Run function for files_toprocess vector ----

txt <- convertpdf2txt(files_toprocess)

# Add names to txt files
names(txt) <- paste("text", 1:length(txt), sep = "")


# ---- 2.2 Export text documents to directory ----

# Save result to working directory
lapply(seq_along(txt), 
       function(i)writeLines(text = unlist(txt[i]),
                             con = paste(post_processed, "/", names(txt)[i],".txt", sep = "")))


