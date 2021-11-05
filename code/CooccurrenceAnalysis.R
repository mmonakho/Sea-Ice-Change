# 
# code: Co-occurrence Analysis (co-location within sentences)
# 
# author: Krista Lawless, kllawles@asu.edu; Kelly Claborn, clabornkelly@gmail.com
# date: October 2021
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: IMPORT LIBRARIES, DEFINE FILE PATHS & FUNCTIONS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Load packages ----

options(stringsAsFactors = FALSE)

pacman::p_load(tidyverse, quanteda)


# ---- 1.2 Source metadataExtract.R for post-processed corpus ----

source('code/metadataExtract.R')


# ---- 1.3 Source functions for significance tests ----

source('code/functions/calculateCoocStatistics.R')


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: TOKENIZE CORPUS, COUNT CO-OCCURRENCES----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Tokenize ADNcorpus by sentence ----

# check original corpus length and its first document
ndoc(ADNcorpus)
substr(as.character(ADNcorpus)[1], 0, 200)


ADNcorpus_sentences <- corpus_reshape(ADNcorpus, to = "sentences")

# check number of sentences, and manually ensure tokenizing looks right
ndoc(ADNcorpus_sentences)
as.character(ADNcorpus_sentences)[1]
as.character(ADNcorpus_sentences)[2]


# ---- 2.2 Process the tokenized sentences -----

ADNcorpus_sentence_tokens <- ADNcorpus_sentences %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)



# ---- 2.3 Calculate multi-word unit candidates to add to co-occurrence analysis ----

ADN_collocations <- textstat_collocations(ADNcorpus_sentence_tokens, min_count = 25)
ADN_collocations <- ADN_collocations[1:250, ]

# Add collocations back into tokenized sentence corpus
ADNcorpus_sentence_tokens <- tokens_compound(ADNcorpus_sentence_tokens, ADN_collocations)

# ---- 2.4 Create DTM for analysis ----

# Identify minimum number of docs token must appear in to be included in analysis
minimumFrequency <- 10

# Create DTM, prune vocabulary and set binary values for presence/absence of types
binDTM <- ADNcorpus_sentence_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = minimumFrequency, max_docfreq = 100000) %>% 
  dfm_weight("boolean")


# ---- 2.5 Counting co-occurrences ----

# Matrix multiplication for co-occurrence counts
coocCounts <- t(binDTM) %*% binDTM

as.matrix(coocCounts[500:510,500:510])


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: STATISTICAL TESTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Manual calculations of mutual information, Dice, and Log-Likelihood measures ----

# This manual calculation allows for easier learning of how the various significance tests are conducted
# However, calculateCoocStatistics.R wraps up all of the following tests into a single function


# k - Number of all context units in the corpus
# ki - Number of occurrences of coocTerm
# kj - Number of occurrences of comparison term j
# kij - Number of joint occurrences of coocTerm and j

coocTerm <- "sea_ice"
k <- nrow(binDTM)
ki <- sum(binDTM[, coocTerm])
kj <- colSums(binDTM)
names(kj) <- colnames(binDTM)
kij <- coocCounts[coocTerm, ]


# -- MI: log(k*kij / (ki * kj)
mutualInformationSig <- log((k * kij) / (ki * kj))
mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]

# -- DICE: 2 X&Y / X + Y 
dicesig <- 2 * kij / (ki + kj)
dicesig <- dicesig[order(dicesig, decreasing = TRUE)]

# -- Log Likelihood
logsig <- 2 * ((k * log(k)) - 
               (ki * log(ki)) - 
               (kj * log(kj)) + 
               (kij * log(kij)) +
               (k - ki - kj + kij) * log(k - ki - kj + kij) +
               (ki - kij) * log(ki - kij)  +
               (kj - kij) * log(kj - kij)  -
               (k - ki) * log(k - ki) - 
               (k - kj) * log(k - kj))
logsig <- logsig[order(logsig, decreasing=T)]


# Put all significance statistics in one dataframe
resultOverView <- data.frame(
  names(sort(kij, decreasing=T)[1:15]), sort(kij, decreasing=T)[1:15],
  names(mutualInformationSig[1:15]), mutualInformationSig[1:15], 
  names(dicesig[1:15]), dicesig[1:15], 
  names(logsig[1:15]), logsig[1:15],
  row.names = NULL)
colnames(resultOverView) <- c("Freq-terms", "Freq", "MI-terms", "MI", 
                              "Dice-Terms", "Dice", "LL-Terms", "LL")
print(resultOverView)


# ---- 3.2 Automated calculation of significance tests ----

# Define parameters for the central co-occurrence term of interest & number of co-occurrences to include in analysis
coocTerm <- "sea_ice"
numberOfCoocs <- 15

# Calculate statistics for coocTerm 
coocs <- calculateCoocStatistics(coocTerm, binDTM, measure = "LOGLIK")

# Display the main terms (n = numberOfCoocs)
print(coocs[1:numberOfCoocs])


# Create dummy data frame for results 
resultGraph <- data.frame(from = character(), to = character(), 
                          sig = numeric(0))


# Structure of the temporary graph object is equal to that of the resultGraph
tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

# Fill the data.frame to produce the correct number of lines
tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
# Entry of the search word into the first column in all lines
tmpGraph[, 1] <- coocTerm
# Entry of the co-occurrences into the second column of the respective line
tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
# Set the significances
tmpGraph[, 3] <- coocs[1:numberOfCoocs]

# Attach the triples to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)


# Iterate over the most significant numberOfCoocs co-occurrences search term
for (i in 1:numberOfCoocs){
  
  # Calling up the co-occurrence calculation for term i from the search words co-occurrences
  newCoocTerm <- names(coocs)[i]
  coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure = "LOGLIK")
  
  #print the co-occurrences
  coocs2[1:10]
  
  # Structure of the temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
  tmpGraph[, 1] <- newCoocTerm
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
  
  #Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
}


# ---- 3.3 Post-process output resultGraph data frame, readying for visualization ----

resultGraph <-
  resultGraph %>%
  mutate(from = stringr::str_replace_all(from, "_", " "),
         from = stringr::str_replace_all(from, "specie", "species"),
         to = stringr::str_replace_all(to, "_", " "),
         to = stringr::str_replace_all(to, "specie", "species"))



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: VISUALIZATION OF CO-OCCURRENCES ACROSS FULL CORPUS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 4.1 Import library, prepare graphical object for plotting ----

require(igraph)

# set seed for graph plot
set.seed(1)

# Create the graph object as undirected graph
graphNetwork <- graph.data.frame(resultGraph, directed = F)


# Identification of all nodes with less than 2 edges
verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]

# OPTIONAL: Remove edges with less than 2 connections from the graph
# graphNetwork <- delete.vertices(graphNetwork, verticesToRemove) 


# # Define the frame and spacing for the plot
# par(mai=c(0,0,1,0)) 


# ---- 4.2 Final Plot, able to be manipulated in an output window prior to export ----

tkplot(
  graphNetwork,             
  # layout = layout.fruchterman.reingold, # Force Directed Layout
  main = "Sea Ice Graph",
  edge.color = "#C0C0C0",
  edge.frame.color = "#A9A9A9",
  edge.width = scales::rescale(E(graphNetwork)$sig, to = c(1, 10)), # scale edge width by significance
  edge.curved = 0.15,
  vertex.size = scales::rescale(log(degree(graphNetwork)), to = c(5,20)), # scale vertex size by number of connections
  vertex.color = ifelse(V(graphNetwork)$name == "sea ice", "#44AA99", 
                        ifelse(degree(graphNetwork) < 2, "#FFFFFF", "#88CCEE")),
  vertex.label.family = "sans",
  vertex.label.cex = 0.8,
  vertex.shape = "circle",
  # vertex.label.dist = 0.5,          # Labels of the nodes moved slightly
  vertex.frame.color = ifelse(degree(graphNetwork) < 2, "#FFFFFF", "#A9A9A9"),
  vertex.label.color = 'black',     # Color of node names
  vertex.label.font = 2,            # Font of node names
  vertex.label = V(graphNetwork)$name,      # node names
  vertex.label.cex = 0.8, # font size of node names
  ylim=c(-10, 10),
  xlim=c(-10, 10),
  asp = 0,
  rescale = FALSE
)



# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: CO-OCCURRENCES PER TIME PERIOD ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 5.1 Define functions to run analyses for different year periods, with different parameters ----

# -- Subsetting function
subsetDTM <- function(dat = ADN, years = NULL, minDocs = 2) {
  
  # filter data & turn into corpus
  dat <- dat %>% filter(year%in%years)
  corp <- corpus(dat)
  sentences <- corpus_reshape(corp, to = "sentences")
  
  # tokenize
  sentence_tokens <- sentences %>% 
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
    tokens_remove(pattern = stopwords_extended, padding = T)
  
  # identify candidate collocations
  collocations <- textstat_collocations(sentence_tokens, min_count = 5)
  collocations <- collocations[1:250, ]
  
  sentence_tokens <- tokens_compound(sentence_tokens, collocations)
  
  
  # identify minimum number of docs
  minimumFrequency <- minDocs
  
  # create DTM, prune vocabulary and set binary values for presence/absence of types
  binDTM <- sentence_tokens %>% 
    tokens_remove("") %>%
    dfm() %>% 
    dfm_trim(min_docfreq = minimumFrequency, max_docfreq = 100000) %>% 
    dfm_weight("boolean")
  
  return(binDTM)
  
}

# -- Sig test function
sigTests <- function(dat = NULL, numCoocs = 15, coocTerm = "sea_ice") {
  
  # Define parameters for the central co-occurrence term of interest & number of co-occurrences to include in analysis
  coocTerm <- coocTerm
  numberOfCoocs <- numCoocs
  
  # Calculate statistics for coocTerm 
  coocs <- calculateCoocStatistics(coocTerm, dat, measure = "LOGLIK")
  
  # Display the main terms (n = numberOfCoocs)
  print(coocs[1:numberOfCoocs])
  
  
  # Create dummy data frame for results 
  resultGraph <- data.frame(from = character(), to = character(), 
                    sig = numeric(0))
  
  
  # Structure of the temporary graph object is equal to that of the resultGraph
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # Fill the data.frame to produce the correct number of lines
  tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
  # Entry of the search word into the first column in all lines
  tmpGraph[, 1] <- coocTerm
  # Entry of the co-occurrences into the second column of the respective line
  tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
  # Set the significances
  tmpGraph[, 3] <- coocs[1:numberOfCoocs]
  
  # attach the triples to resultGraph
  resultGraph <- rbind(resultGraph, tmpGraph)
  
  
  # iterate over the most significant numberOfCoocs co-occurrences search term
  for (i in 1:numberOfCoocs){
    
    # calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- names(coocs)[i]
    coocs2 <- calculateCoocStatistics(newCoocTerm, dat, measure = "LOGLIK")
    
    # print the co-occurrences
    coocs2[1:10]
    
    # structure of the temporary graph object
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
    tmpGraph[, 1] <- newCoocTerm
    tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
    
    # append the result to the result graph
    resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
  }
  
  
  # post-process output resultGraph data frame, readying for visualization
  resultGraph <- resultGraph %>%
    mutate(from = stringr::str_replace_all(from, "_", " "),
           from = stringr::str_replace_all(from, "specie", "species"),
           to = stringr::str_replace_all(to, "_", " "),
           to = stringr::str_replace_all(to, "specie", "species"))
  
  return(resultGraph)
  
}

# -- Visualization function
visualizeByPeriod <- function(dat = NULL, removeDegreeOne = FALSE, cooc = "sea ice", titleTerm = "Sea Ice") {
  
  # set seed for graph plot
  set.seed(1)
  
  # Create the graph object as undirected graph
  graphNetwork <- graph.data.frame(dat, directed = F)
  
  
  # Identification of all nodes with less than 2 edges
  if(removeDegreeOne==TRUE) { 
    
    verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]
  
    graphNetwork <- delete.vertices(graphNetwork, verticesToRemove)  
    
  }
  
  
  # Define the frame and spacing for the plot
  par(mai=c(0,0,1,0)) 
  
  
  # ---- 4.2 Final Plot, able to be manipulated in an output window prior to export ----
  
  tkplot(
    graphNetwork,             
    layout = layout.fruchterman.reingold, # Force Directed Layout
    edge.color = "#C0C0C0",
    edge.frame.color = "#A9A9A9",
    edge.width = scales::rescale(E(graphNetwork)$sig, to = c(1, 10)), # scale edge width by significance
    edge.curved = 0.15,
    vertex.size = scales::rescale(log(degree(graphNetwork)), to = c(5,20)), # scale vertex size by number of connections
    vertex.color = ifelse(V(graphNetwork)$name == cooc, "#44AA99", 
                          ifelse(degree(graphNetwork) < 2, "#FFFFFF", "#88CCEE")),
    vertex.label.family = "sans",
    vertex.label.cex = 0.8,
    vertex.shape = "circle",
    # vertex.label.dist = 0.5,          # Labels of the nodes moved slightly
    vertex.frame.color = ifelse(degree(graphNetwork) < 2, "#FFFFFF", "#A9A9A9"),
    vertex.label.color = 'black',     # Color of node names
    vertex.label.font = 2,            # Font of node names
    vertex.label = V(graphNetwork)$name,      # node names
    vertex.label.cex = 1 # font size of node names
  )
}


# ---- 5.2 Run functions for different year sets and parameters ----

# Years 1995-2003
binDTM_pd1 <- subsetDTM(dat = ADN, years = c(1995:2003), minDocs = 2)
resultGraph_pd1 <- sigTests(dat = binDTM_pd1, numCoocs = 15, coocTerm = "sea_ice")

# Years 2004 - 2012
binDTM_pd2 <- subsetDTM(dat = ADN, years = c(2004:2012), minDocs = 2)
resultGraph_pd2 <- sigTests(dat = binDTM_pd2, numCoocs = 15, coocTerm = "sea_ice")

# Years 2013 - 2021
binDTM_pd3 <- subsetDTM(dat = ADN, years = c(2013:2021), minDocs = 2)
resultGraph_pd3 <- sigTests(dat = binDTM_pd3, numCoocs = 15, coocTerm = "sea_ice")


# ---- 5.3 Visualizations per year set

# Full sample
visualizeByPeriod(dat = resultGraph, cooc = "sea ice", titleTerm = "Sea Ice")

# Years 1995 - 2003
visualizeByPeriod(dat = resultGraph_pd1, cooc = "sea ice", titleTerm = "Sea Ice")

# Years 2004 - 2012
visualizeByPeriod(dat = resultGraph_pd2, cooc = "sea ice", titleTerm = "Sea Ice")

# Years 2013 - 2021
visualizeByPeriod(dat = resultGraph_pd3, cooc = "sea ice", titleTerm = "Sea Ice")
