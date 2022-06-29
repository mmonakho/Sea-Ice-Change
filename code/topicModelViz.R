# 
# code: Topic model visualizations
# 
# author: Kelly Claborn, clabornkelly@gmail.com; Masha Monakhova, mmonakho@asu.edu
# date: November 2021; modified: June 2022
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE FLAT DATA FILES ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 1.1 Import packages, source scripts, define input directory ----

pacman::p_load(rio, dplyr, tidyr, wordcloud2, htmlwidgets, webshot, ggthemes, grid, gridExtra, ggplot2)

source("code/metadataExtract.R")
source("code/plotThemes.R")

input.dir <- "data/outputs/topicmodel/20220409/"


# ---- NOTE: Have decided to only run the topic modelling function a single time across the full corpus ----
# Instead, we simply disaggregate by time period when looking at posterior probabilities.  
# This way, we can compare the same topics through time.


# ---- 1.1 Import topic model outputs for full corpus (1995-2021) ----

top5_full <- import(paste(input.dir, 'top5_full.csv', sep = ''))

names_full <- import(paste(input.dir, 'names_full.csv', sep = ''), fread = FALSE) %>%
  rename("topicNames" = "topicModel_full.names") %>%
  mutate(topic = row.names(.),
         name = c("Arctic Oil & Drilling",
                  "Arctic Animals & Habitat",
                  "International Coordination",
                  "Dog Sled Racing",
                  "Scientific Perspectives",
                  "Learning Opportunities",
                  "Local Storm Impacts",
                  "Polar Bears",
                  "Storytelling & the Arts",
                  "Political Perspectives"),
         name = factor(name, rev(c("Scientific Perspectives",
                                   "Political Perspectives",
                                   "Arctic Animals & Habitat",
                                   "Polar Bears",
                                   "Storytelling & the Arts",
                                   "Local Storm Impacts",
                                   "International Coordination",
                                   "Arctic Oil & Drilling",
                                   "Dog Sled Racing",
                                   "Learning Opportunities")), ordered = T))


termProbabilities_full <- import(paste(input.dir, 'termProbabilities_full.csv', sep = ''))

topicProbabilities_byYear_full <- import(paste(input.dir, 'topicProbs_byYear_full.csv', sep = '')) %>%
  pivot_longer(cols = c(`1`:`10`), names_to = "topic") %>%
  left_join(names_full, by = "topic")


Probs_byYear_toPlot <- 
  topicProbabilities_byYear_full %>%
  mutate(period = ifelse(year%in%c(1995:2003), "1995-2003",
                         ifelse(year%in%c(2004:2012), "2004-2012",
                                "2013-2021"))) %>%
  group_by(period, topic, topicNames, name) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  arrange(desc(period), value) %>%
  mutate(period = factor(period, 
                         levels = c("1995-2003", "2004-2012", "2013-2021"),
                         ordered = T),
         topic = factor(topic, levels = c("10","9","8","7","6","5","4","3","2","1"),
                        ordered = T),
         name = factor(name, levels = unique(name), ordered = T))



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
  outputfile <- paste(input.dir, "Wordcloud_", i, ".png", sep = "")
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
    geom_bar(aes(x = period, y = value, group = name, fill = name),
             stat = "identity", position = "dodge", width = 0.75) +
    scale_y_continuous(name = "",
                       expand = c(0,0),
                       limits = c(0, 0.2)) +
    scale_x_discrete(name = "",
                     limits = rev(levels(Probs_byYear_toPlot$period))) +
    scale_fill_ptol(name = "") +
    labs(title = "Topic Probabilities", subtitle = "Posterior probabilities per time period") +
    seaice.plot.theme + coord_flip()  + guides(fill = guide_legend(reverse = T)) +
    theme(panel.grid.major.x = element_line(colour = "#C0C0C0",
                                            linetype = 3,
                                            size = 0.5),
          panel.grid.major.y = element_blank())
  

# EXPORT
png("data/outputs/topicmodel/20220409/Topics_byperiod.png",
    units = "in", height = 6, width = 10, res = 400)
grid.newpage()
grid.draw(plot_topics_byperiod)
dev.off()

export(Probs_byYear_toPlot, "data/outputs/topicmodel/20220409/topicProbs_byTimePeriod.csv")


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: CHARACTERIZING COHESIVENSS & SPREAD OF TOPIC ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


topicProbabilities_byDoc <- import(paste(input.dir, "topicProbs_byDoc_full.csv", sep = "/"), header = T)

colnames(topicProbabilities_byDoc) <- c("filename", as.character(names_full$name))


# ---- 4.1 Identifying number of docs per topic with at least 66%, 50%, and 33% probability, respectively ----

Docs_grt66 <- topicProbabilities_byDoc %>%
  summarise(`Arctic Oil & Drilling` = length(filename[`Arctic Oil & Drilling`>=0.66]),
            `Arctic Animals & Habitat` = length(filename[`Arctic Animals & Habitat`>=0.66]),
            `International Coordination` = length(filename[`International Coordination`>=0.66]),
            `Dog Sled Racing` = length(filename[`Dog Sled Racing`>=0.66]),
            `Scientific Perspectives` = length(filename[`Scientific Perspectives`>=0.66]),
            `Learning Opportunities` = length(filename[`Learning Opportunities`>=0.66]),
            `Local Storm Impacts` = length(filename[`Local Storm Impacts`>=0.66]),
            `Polar Bears` = length(filename[`Polar Bears`>=0.66]),
            `Storytelling & the Arts` = length(filename[`Storytelling & the Arts`>=0.66]),
            `Political Perspectives` = length(filename[`Political Perspectives`>=0.66])) %>% 
  pivot_longer(cols = 1:10, values_to = "ndocs_66perc")

Docs_grt50 <- topicProbabilities_byDoc %>%
  summarise(`Arctic Oil & Drilling` = length(filename[`Arctic Oil & Drilling`>=0.5]),
            `Arctic Animals & Habitat` = length(filename[`Arctic Animals & Habitat`>=0.5]),
            `International Coordination` = length(filename[`International Coordination`>=0.5]),
            `Dog Sled Racing` = length(filename[`Dog Sled Racing`>=0.5]),
            `Scientific Perspectives` = length(filename[`Scientific Perspectives`>=0.5]),
            `Learning Opportunities` = length(filename[`Learning Opportunities`>=0.5]),
            `Local Storm Impacts` = length(filename[`Local Storm Impacts`>=0.5]),
            `Polar Bears` = length(filename[`Polar Bears`>=0.5]),
            `Storytelling & the Arts` = length(filename[`Storytelling & the Arts`>=0.5]),
            `Political Perspectives` = length(filename[`Political Perspectives`>=0.5])) %>% 
  pivot_longer(cols = 1:10, values_to = "ndocs_50perc")

Docs_grt33 <- topicProbabilities_byDoc %>%
  summarise(`Arctic Oil & Drilling` = length(filename[`Arctic Oil & Drilling`>=0.33]),
            `Arctic Animals & Habitat` = length(filename[`Arctic Animals & Habitat`>=0.33]),
            `International Coordination` = length(filename[`International Coordination`>=0.33]),
            `Dog Sled Racing` = length(filename[`Dog Sled Racing`>=0.33]),
            `Scientific Perspectives` = length(filename[`Scientific Perspectives`>=0.33]),
            `Learning Opportunities` = length(filename[`Learning Opportunities`>=0.33]),
            `Local Storm Impacts` = length(filename[`Local Storm Impacts`>=0.33]),
            `Polar Bears` = length(filename[`Polar Bears`>=0.33]),
            `Storytelling & the Arts` = length(filename[`Storytelling & the Arts`>=0.33]),
            `Political Perspectives` = length(filename[`Political Perspectives`>=0.33])) %>% 
  pivot_longer(cols = 1:10, values_to = "ndocs_33perc")


# NOTE: topics with a smaller spread between high probability docs (e.g., 66% or even 50%) and lower probability docs (e.g., 33%) 
#       may indicate a more "cohesive" topic, while topics with more docs at a lower probability (e.g., at least 33%)
#       may indicate a more widespread topic

Ndocs_pertopic <- 
  left_join(Docs_grt66, Docs_grt50, by = "name") %>%
  left_join(Docs_grt33, by = "name")


# ---- 4.1 Identifying number of docs per topic, PER TIME PERIOD, with at least 66%, 50%, and 33% probability, respectively ----

Docs_grt66_byperiod <- topicProbabilities_byDoc %>%
  left_join(ADN%>%mutate(filename = paste("text", docid, sep = ""))%>%select(filename, year), by = "filename") %>%
  mutate(period = ifelse(year<2004, "1995-2003", 
                         ifelse(year<2013 & year>=2004, "2004-2012", "2013-2021"))) %>%
  group_by(period) %>%
  summarise(`Arctic Oil & Drilling` = length(filename[`Arctic Oil & Drilling`>=0.66]),
            `Arctic Animals & Habitat` = length(filename[`Arctic Animals & Habitat`>=0.66]),
            `International Coordination` = length(filename[`International Coordination`>=0.66]),
            `Dog Sled Racing` = length(filename[`Dog Sled Racing`>=0.66]),
            `Scientific Perspectives` = length(filename[`Scientific Perspectives`>=0.66]),
            `Learning Opportunities` = length(filename[`Learning Opportunities`>=0.66]),
            `Local Storm Impacts` = length(filename[`Local Storm Impacts`>=0.66]),
            `Polar Bears` = length(filename[`Polar Bears`>=0.66]),
            `Storytelling & the Arts` = length(filename[`Storytelling & the Arts`>=0.66]),
            `Political Perspectives` = length(filename[`Political Perspectives`>=0.66])) %>% 
  pivot_longer(cols = 2:11, values_to = "ndocs_66perc")


Docs_grt50_byperiod <- topicProbabilities_byDoc %>%
  left_join(ADN%>%mutate(filename = paste("text", docid, sep = ""))%>%select(filename, year), by = "filename") %>%
  mutate(period = ifelse(year<2004, "1995-2003", 
                         ifelse(year<2013 & year>=2004, "2004-2012", "2013-2021"))) %>%
  group_by(period) %>%
  summarise(`Arctic Oil & Drilling` = length(filename[`Arctic Oil & Drilling`>=0.5]),
            `Arctic Animals & Habitat` = length(filename[`Arctic Animals & Habitat`>=0.5]),
            `International Coordination` = length(filename[`International Coordination`>=0.5]),
            `Dog Sled Racing` = length(filename[`Dog Sled Racing`>=0.5]),
            `Scientific Perspectives` = length(filename[`Scientific Perspectives`>=0.5]),
            `Learning Opportunities` = length(filename[`Learning Opportunities`>=0.5]),
            `Local Storm Impacts` = length(filename[`Local Storm Impacts`>=0.5]),
            `Polar Bears` = length(filename[`Polar Bears`>=0.5]),
            `Storytelling & the Arts` = length(filename[`Storytelling & the Arts`>=0.5]),
            `Political Perspectives` = length(filename[`Political Perspectives`>=0.5])) %>% 
  pivot_longer(cols = 2:11, values_to = "ndocs_50perc")


Docs_grt33_byperiod <- topicProbabilities_byDoc %>%
  left_join(ADN%>%mutate(filename = paste("text", docid, sep = ""))%>%select(filename, year), by = "filename") %>%
  mutate(period = ifelse(year<2004, "1995-2003", 
                         ifelse(year<2013 & year>=2004, "2004-2012", "2013-2021"))) %>%
  group_by(period) %>%
  summarise(`Arctic Oil & Drilling` = length(filename[`Arctic Oil & Drilling`>=0.33]),
            `Arctic Animals & Habitat` = length(filename[`Arctic Animals & Habitat`>=0.33]),
            `International Coordination` = length(filename[`International Coordination`>=0.33]),
            `Dog Sled Racing` = length(filename[`Dog Sled Racing`>=0.33]),
            `Scientific Perspectives` = length(filename[`Scientific Perspectives`>=0.33]),
            `Learning Opportunities` = length(filename[`Learning Opportunities`>=0.33]),
            `Local Storm Impacts` = length(filename[`Local Storm Impacts`>=0.33]),
            `Polar Bears` = length(filename[`Polar Bears`>=0.33]),
            `Storytelling & the Arts` = length(filename[`Storytelling & the Arts`>=0.33]),
            `Political Perspectives` = length(filename[`Political Perspectives`>=0.33])) %>% 
  pivot_longer(cols = 2:11, values_to = "ndocs_33perc")


# NOTE: topics with a smaller spread between high probability docs (e.g., 66% or even 50%) and lower probability docs (e.g., 33%) 
#       may indicate a more "cohesive" topic, while topics with more docs at a lower probability (e.g., at least 33%)
#       may indicate a more widespread topic

Ndocs_pertopic_byperiod <- 
  left_join(Docs_grt66_byperiod, Docs_grt50_byperiod, by = c("period", "name"))%>%
  left_join(Docs_grt33_byperiod, by =  c("period", "name"))


# ---- 4.3 Visualize spread of number of docs per topic, from number of docs with 66% to 33% probability ----

# Ndocs_pertopic_plot <-
#   ggplot(Ndocs_pertopic %>% arrange(ndocs_33perc) %>%
#            mutate(name = factor(name, levels = unique(name), ordered = T)) %>%
#            pivot_longer(cols = 2:4, names_to = "prob") %>% 
#            mutate(prob = factor(prob, levels = c("ndocs_33perc",
#                                                  "ndocs_50perc",
#                                                  "ndocs_66perc"),
#                                 labels = c("33%", "50%", "66%"), 
#                                 ordered = T))) +
#   geom_bar(aes(x = name, y = value, fill = prob, group = prob), 
#            stat = "identity", position = "dodge") +
#   scale_fill_manual(name = "Probability (>=)",
#                     values = color.fill.3cats) +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "Number documents",
#                      expand = c(0, 0),
#                      limits = c(0, 200)) +
#   coord_flip() +
#   seaice.plot.theme + guides(fill = guide_legend(reverse = T)) +
#   theme(panel.grid.major.x = element_line(colour = "#C0C0C0",
#                                           linetype = 3,
#                                           size = 0.4),
#         panel.grid.major.y = element_blank())


# - Across full corpus
Ndocs_pertopic_plot <-
  ggplot(Ndocs_pertopic %>% arrange(ndocs_33perc) %>%
           mutate(name = factor(name, levels = unique(name), ordered = T))) +
  geom_text(aes(y = 10.4, x = Ndocs_pertopic$ndocs_66perc[Ndocs_pertopic$name=="Scientific Perspectives"],
                 label = "33%")) +
  geom_text(aes(y = 10.4, x = (Ndocs_pertopic$ndocs_33perc[Ndocs_pertopic$name=="Scientific Perspectives"] +
                  Ndocs_pertopic$ndocs_66perc[Ndocs_pertopic$name=="Scientific Perspectives"])/2,
                label = "probability of occurrence")) +
  geom_text(aes(y = 10.4, x = Ndocs_pertopic$ndocs_33perc[Ndocs_pertopic$name=="Scientific Perspectives"],
                label = "66%")) +
  geom_point(aes(y = name, x = ndocs_33perc),
             shape = 16, size = 2, colour = "#332288") +
  geom_point(aes(y = name, x = ndocs_66perc),
             shape = 16, size = 2, colour = "#332288") +
  geom_linerange(aes(y = name, xmin = ndocs_66perc, xmax = ndocs_33perc), 
           stat = "identity", size = 1, colour = "#332288") +
  scale_y_discrete(name = "",
                   expand = c(0.1, 0)) +
  scale_x_continuous(name = "# documents",
                     expand = c(0, 0),
                     limits = c(-5, 200)) +
  seaice.plot.theme + labs(title = "Prevalence and Cohesiveness of Topics", 
                           subtitle = "Number of documents with at least 66% and 33% probability of occurrence per topic") +
  theme(panel.grid.major.x = element_line(colour = "#C0C0C0",
                                          linetype = 3,
                                          size = 0.5),
        panel.grid.major.y = element_blank())


# EXPORT
png("data/outputs/topicmodel/20220409/TopicPrevalence_Cohesiveness.png",
    units = "in", height = 6, width = 10, res = 400)
grid.newpage()
grid.draw(Ndocs_pertopic_plot)
dev.off()


# # - By time period
# Ndocs_pertopic_byperiod_plot <-
#   ggplot(Ndocs_pertopic_byperiod %>% arrange(period, ndocs_33perc) %>%
#            mutate(name = factor(name, levels = unique(name), ordered = T))) +
#   geom_text(aes(y = 10.4, x = Ndocs_pertopic_byperiod$ndocs_66perc[Ndocs_pertopic_byperiod$name=="Scientific Perspectives" & 
#                                                                      Ndocs_pertopic_byperiod$period=="2013-2021"],
#                 label = "33%")) +
#   geom_text(aes(y = 10.4, x = (Ndocs_pertopic_byperiod$ndocs_33perc[Ndocs_pertopic_byperiod$name=="Scientific Perspectives" & 
#                                                                       Ndocs_pertopic_byperiod$period=="2013-2021"] +
#                                  Ndocs_pertopic_byperiod$ndocs_66perc[Ndocs_pertopic_byperiod$name=="Scientific Perspectives" & 
#                                                                         Ndocs_pertopic_byperiod$period=="2013-2021"])/2,
#                 label = "probability of occurrence")) +
#   geom_text(aes(y = 10.4, x = Ndocs_pertopic_byperiod$ndocs_33perc[Ndocs_pertopic_byperiod$name=="Scientific Perspectives" & 
#                                                                      Ndocs_pertopic_byperiod$period=="2013-2021"],
#                 label = "66%")) +
#   geom_point(aes(y = name, x = ndocs_33perc, colour = period, group = period),
#              shape = 16, size = 2, position = position_dodge(width = 0.5)) +
#   geom_point(aes(y = name, x = ndocs_66perc, colour = period, group = period),
#              shape = 16, size = 2, position = position_dodge(width= 0.5)) +
#   geom_linerange(aes(y = name, xmin = ndocs_66perc, xmax = ndocs_33perc, colour = period, group = period), 
#                  stat = "identity", size = 1, position = position_dodge(width = 0.5)) +
#   scale_y_discrete(name = "",
#                    expand = c(0.1, 0)) +
#   scale_x_continuous(name = "# documents",
#                      expand = c(0, 0),
#                      limits = c(-5, 200)) +
#   seaice.plot.theme + labs(title = "Prevalence and Cohesiveness of Topics, per Time Period", 
#                            subtitle = "Number of documents with at least 66% and 33% probability of occurrence per topic") +
#   theme(panel.grid.major.x = element_line(colour = "#C0C0C0",
#                                           linetype = 3,
#                                           size = 0.5),
#         panel.grid.major.y = element_blank())
