# 
# code: Topic model visualizations
# 
# author: Kelly Claborn, clabornkelly@gmail.com; Masha Monakhova, mmonakho@asu.edu
# date: November 2021
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE FLAT DATA FILES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 1.1 Import packages, source scripts, define input directory ----

pacman::p_load(rio, dplyr, wordcloud2, htmlwidgets, webshot, ggplot2)

source("code/plotThemes.R")

input.dir <- "data/outputs/topicmodel/20211118/"


# ---- NOTE: Have decided to only run the topic modelling function a single time across the full corpus ----
# Instead, we simply disaggregate by time period when looking at posterior probabilities.  
# This way, we can compare the same topics through time.


# # ---- 1.2 Import topic model outputs for period 1 (1995-2003) ----
# 
# top5_pd1 <- import(paste(input.dir, 'top5_pd1.csv', sep = ''))
# 
# names_pd1 <- import(paste(input.dir, 'names_pd1.csv', sep = ''), fread = FALSE) %>%
#   rename("topicNames" = "topicModel_pd1.names") %>%
#   mutate(topic = row.names(.))
# 
# termProbabilities_pd1 <- import(paste(input.dir, 'termProbabilities_pd1.csv', sep = ''))
# 
# topicProbabilities_byYear_pd1 <- import(paste(input.dir, 'topicProbs_byYear_pd1.csv', sep = '')) %>%
#   pivot_longer(cols = c(`1`:`8`), names_to = "topic") %>%
#   left_join(names_pd1, by = "topic")
# 
# 
# # ---- 1.3 Import topic model outputs for period 2 (2004-2012) ----
# 
# top5_pd2 <- import(paste(input.dir, 'top5_pd2.csv', sep = ''))
# 
# names_pd2 <- import(paste(input.dir, 'names_pd2.csv', sep = ''), fread = FALSE) %>%
#   rename("topicNames" = "topicModel_pd2.names") %>%
#   mutate(topic = row.names(.))
# 
# termProbabilities_pd2 <- import(paste(input.dir, 'termProbabilities_pd2.csv', sep = ''))
# 
# topicProbabilities_byYear_pd2 <- import(paste(input.dir, 'topicProbs_byYear_pd2.csv', sep = '')) %>%
#   pivot_longer(cols = c(`1`:`8`), names_to = "topic") %>%
#   left_join(names_pd2, by = "topic")
# 
# 
# # ---- 1.4 Import topic model outputs for period 3 (2013-2021) ----
# 
# top5_pd3 <- import(paste(input.dir, 'top5_pd3.csv', sep = ''))
# 
# names_pd3 <- import(paste(input.dir, 'names_pd3.csv', sep = ''), fread = FALSE) %>%
#   rename("topicNames" = "topicModel_pd3.names") %>%
#   mutate(topic = row.names(.))
# 
# termProbabilities_pd3 <- import(paste(input.dir, 'termProbabilities_pd3.csv', sep = ''))
# 
# topicProbabilities_byYear_pd3 <- import(paste(input.dir, 'topicProbs_byYear_pd3.csv', sep = '')) %>%
#   pivot_longer(cols = c(`1`:`8`), names_to = "topic") %>%
#   left_join(names_pd3, by = "topic")

# ---- 1.4 Import topic model outputs for full corpus (1995-2021) ----

top5_full <- import(paste(input.dir, 'top5_full.csv', sep = ''))

names_full <- import(paste(input.dir, 'names_full.csv', sep = ''), fread = FALSE) %>%
  rename("topicNames" = "topicModel_full.names") %>%
  mutate(topic = row.names(.))

termProbabilities_full <- import(paste(input.dir, 'termProbabilities_full.csv', sep = ''))

topicProbabilities_byYear_full <- import(paste(input.dir, 'topicProbs_byYear_full.csv', sep = '')) %>%
  pivot_longer(cols = c(`1`:`10`), names_to = "topic") %>%
  left_join(names_full, by = "topic")


Probs_byYear_toPlot <- 
  topicProbabilities_byYear_full %>%
  mutate(period = ifelse(year%in%c(1995:2003), "1995-2003",
                         ifelse(year%in%c(2004:2012), "2004-2012",
                                "2013-2021"))) %>%
  group_by(period, topic, topicNames) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  mutate(period = factor(period, 
                         levels = c("1995-2003", "2004-2012", "2013-2021"),
                         ordered = T),
         topic = factor(topic, levels = c("10","9","8","7","6","5","4","3","2","1"),
                        ordered = T))

names_full <- names_full %>% mutate(topic = as.numeric(topic)) %>% .[rev(order(.$topic)),]


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: WORD CLOUDS PER TOPIC, PER TIME PERIOD ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 2.1 Topic word clouds ----

ntopics <- 10
timePeriod <- "full" # Choose time period ("pd1", "pd2", "pd3", "full") -- "full" means entire corpus, this is currently the strategy we are using

for(i in 1:ntopics) {
  
  # Identify the necessary columns from termProbabilities, per topic
  colsToPull <- c(paste("terms_", i, sep = ""), paste("prob_", i, sep = ""))
  timePeriodData <- get(paste("termProbabilities_", timePeriod, sep = ""))
  
  # Pull out each column for topicToViz
  words <- timePeriodData[,colsToPull[1]]
  probabilities <- timePeriodData[,colsToPull[2]]
  
  # Visualize the terms as wordcloud
  wordcloud <- wordcloud2(data.frame(words, probabilities), shuffle = FALSE, size = 0.8)
  
  # EXPORT
  saveWidget(wordcloud, "tmp.html", selfcontained = F)
  # install_phantomjs()
  outputfile <- paste("data/outputs/topicmodel/Wordcloud_", i, ".png", sep = "")
  webshot("tmp.html", outputfile, delay = 10)
  
}


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: PROBABILITY OF TOPIC PER TIME PERIOD ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

  
# ---- 3.1 Full Corpus (1995-2021) ----

plot_topics_byperiod <- 
    ggplot(Probs_byYear_toPlot) +
    geom_bar(aes(x = period, y = value, group = topic, fill = topic),
             stat = "identity", position = "dodge", width = 0.75) +
    scale_y_continuous(name = "Probability",
                       expand = c(0,0),
                       limits = c(0, 0.2)) +
    scale_x_discrete(name = "",
                     limits = rev(levels(Probs_byYear_toPlot$period))) +
    scale_fill_ptol(name = "",
                    labels = names_full$topicNames) +
    labs(title = "Topics per time period") +
    seaice.plot.theme + coord_flip() + guides(fill = guide_legend(reverse = T)) +
    theme(panel.grid.major.x = element_line(colour = "#C0C0C0",
                                            linetype = 3,
                                            size = 0.4),
          panel.grid.major.y = element_blank())
  

# EXPORT
png("data/outputs/topicmodel/Topics_byperiod.png",
    units = "in", height = 6, width = 10, res = 400)
grid.newpage()
grid.draw(plot_topics_byperiod)
dev.off()
