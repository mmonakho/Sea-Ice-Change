

# ---- Create filtered data frames for top most probable articles by topic of choice ----

ADN_texts <- ADN %>%
  mutate(filename = paste("text", docid, sep = ""))


topicProbs_byDoc <- import("data/outputs/topicmodel/20220322/topicProbs_byDoc_full.csv") %>%
  # rename("ScientificPerspectives" = "Topic 1",
  #        "InstitutionalPerspectives" = "Topic 2",
  #        "SummerEducation" = "Topic 3",
  #        "PolarBears" = "Topic 4",
  #        "LocalLivelihoods" = "Topic 5",
  #        "DogSledRacing" = "Topic 6",
  #        "ArcticOilDrilling" = "Topic 7",
  #        "LocalImpacts" = "Topic 8",
  #        "CommunityActivities" = "Topic 9",
  #        "ArcticAnimalsHabitat" = "Topic 10") %>%
  left_join(ADN_texts, by = "filename") 

topicProbs_DogSledRacing_grt0.6 <- 
  topicProbs_byDoc %>%
  filter(DogSledRacing>=0.6) %>%
  arrange(year) %>%
  select(filename, DogSledRacing, year, text) %>%
  rename("probability" = "DogSledRacing") %>%
  mutate(iditarod_mentioned = ifelse(grepl("iditarod", text, ignore.case = T)==T, "yes", "no"))


export(topicProbs_DogSledRacing_grt0.6, "data/outputs/DogSledRacing_toptexts.csv")


# ---- Find articles that have intermediate topic probabilities for >1 topic ----

# At least two topics with prob >20%
topicProbs_grt1topic_0.2 <-
  topicProbs_byDoc %>%
  mutate(topic1 = ifelse(ScientificPerspectives>=0.2, 1, 0),
         topic2 = ifelse(InstitutionalPerspectives>=0.2, 1, 0),
         topic3 = ifelse(SummerEducation>=0.2, 1, 0),
         topic4 = ifelse(PolarBears>=0.2, 1, 0),
         topic5 = ifelse(LocalLivelihoods>=0.2, 1, 0),
         topic6 = ifelse(DogSledRacing>=0.2, 1, 0),
         topic7 = ifelse(ArcticOilDrilling>=0.2, 1, 0),
         topic8 = ifelse(LocalImpacts>=0.2, 1, 0),
         topic9 = ifelse(CommunityActivities>=0.2, 1, 0),
         topic10 = ifelse(ArcticAnimalsHabitat>=0.2, 1, 0)) %>%
  mutate(sumprobs = rowSums(.[16:25])) %>%
  filter(sumprobs>=2)

# At least two topics with prob >30%
topicProbs_grt1topic_0.3 <-
  topicProbs_byDoc %>%
  mutate(topic1 = ifelse(ScientificPerspectives>=0.3, 1, 0),
         topic2 = ifelse(InstitutionalPerspectives>=0.3, 1, 0),
         topic3 = ifelse(SummerEducation>=0.3, 1, 0),
         topic4 = ifelse(PolarBears>=0.3, 1, 0),
         topic5 = ifelse(LocalLivelihoods>=0.3, 1, 0),
         topic6 = ifelse(DogSledRacing>=0.3, 1, 0),
         topic7 = ifelse(ArcticOilDrilling>=0.3, 1, 0),
         topic8 = ifelse(LocalImpacts>=0.3, 1, 0),
         topic9 = ifelse(CommunityActivities>=0.3, 1, 0),
         topic10 = ifelse(ArcticAnimalsHabitat>=0.3, 1, 0)) %>%
  mutate(sumprobs = rowSums(.[16:25])) %>%
  filter(sumprobs>=2)

# At least two topics with prob >33%
topicProbs_grt1topic_0.33 <-
  topicProbs_byDoc %>%
  mutate(topic1 = ifelse(ScientificPerspectives>=0.33, 1, 0),
         topic2 = ifelse(InstitutionalPerspectives>=0.33, 1, 0),
         topic3 = ifelse(SummerEducation>=0.33, 1, 0),
         topic4 = ifelse(PolarBears>=0.33, 1, 0),
         topic5 = ifelse(`???`>=0.33, 1, 0),
         topic6 = ifelse(DogSledRacing>=0.33, 1, 0),
         topic7 = ifelse(ArcticOilDrilling>=0.33, 1, 0),
         topic8 = ifelse(LocalImpacts>=0.33, 1, 0),
         topic9 = ifelse(CommunityActivities>=0.33, 1, 0),
         topic10 = ifelse(ArcticAnimalsHabitat>=0.33, 1, 0)) %>%
  mutate(sumprobs = rowSums(.[17:26])) %>%
  filter(sumprobs>=2)
