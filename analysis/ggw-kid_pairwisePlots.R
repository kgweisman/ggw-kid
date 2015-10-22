########################################################### preliminaries #####

# --- PACKAGES & FUNCTIONS ----------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scatterplot3d)
library(lme4)
library(psych)
# library(stats)
library(scales)
library(smacof)
library(eba)

# clear environment
rm(list=ls())

# clear graphics
dev.off()

# --- IMPORTING DATA ----------------------------------------------------------

# --------> adults ----------------------------------------------------------

# read in data: individual scores
dd_adults = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/run-01_2015-08-17_data_anonymized.csv")[-1] # get rid of column of obs numbers

# add in ageGroup
dd_adults <- dd_adults %>%
  mutate(ageGroup = "adults")

glimpse(dd_adults)

# --------> children ----------------------------------------------------------

# read in data: individual scores
# # ... FULL DATASET
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01&02_2015-10-21_data_anonymized.csv")[-1] # get rid of column of obs numbers
# 
# # ... RUN01
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01_2015-06-13_data_anonymized.csv")[-1] # get rid of column of obs numbers

# ... RUN02
dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-02_2015-10-21_data_anonymized.csv")[-1] # get rid of column of obs numbers

# add in ageGroup
dd_children <- dd_children %>%
  mutate(ageGroup = "children")

glimpse(dd_children)

# --------> FILTERING (adult data only) ---------------------------------------

# --------------->-> by country ---------------------------------------------

dd_adults_us = dd_adults %>%
  filter(country == "us")

dd_adults_india = dd_adults %>%
  filter(country == "india")

# set group of interest
# ... to us:
# dd_adults = dd_adults_us

# ... to india:
# dd_adults = dd_adults_india

# --------> FILTERING (child data only) ---------------------------------------

# --------------->-> by ethnicity ---------------------------------------------

# dd_children_white = dd_children %>%
#   filter(ethnicity == "white")
# 
# dd_children_nonwhite = dd_children %>%
#   filter(ethnicity != "white" & 
#            ethnicity != "NA" & 
#            ethnicity != "other_prefNo")

# set group of interest
# ... to white:
# dd_children = dd_children_white

# # ... to nonwhite:
# dd_children = dd_children_nonwhite

# --------------->-> by age ---------------------------------------------------

# set lower limit
lowerLim <- 4.4
upperLim <- 5.6

dd_children_exact = dd_children %>%
  filter(ageCalc >= lowerLim & ageCalc <= upperLim)

# set group of interest
# ... to exact:
dd_children = dd_children_exact

# --------> merge adults & children -------------------------------------------

# merge datasets
dd <- full_join(dd_adults, dd_children) %>%
  mutate(ageGroup = factor(ageGroup))

glimpse(dd)

# --------> FILTERING (child and adult data) ----------------------------------

# --------------->-> by ageGroup ----------------------------------------------

# set group of interest
# ... to adults:
# dd = dd_adults
# ... to children:
# dd = dd_children

# --------------->-> exclude stapler trials -----------------------------------

dd_nostapler <- dd %>%
  filter(leftCharacter != "stapler" & rightCharacter != "stapler") %>%
  mutate(leftCharacter = factor(leftCharacter),
         rightCharacter = factor(rightCharacter))

# set group of interest
# ... to no stapler:
# dd = dd_nostapler

# --------------->-> exclude baby trials --------------------------------------

dd_nobaby <- dd %>%
  filter(leftCharacter != "baby" & rightCharacter != "baby") %>%
  mutate(leftCharacter = factor(leftCharacter),
         rightCharacter = factor(rightCharacter))

# set group of interest
# ... to no baby:
# dd = dd_nobaby

########################################################### summary stats #####

# --- DEMOGRAPHICS ------------------------------------------------------------

demo = dd %>% distinct(subid)

# total n
demo %>% group_by(ageGroup) %>% summarise(n = length(subid))

# condition assignment
dd %>% group_by(ageGroup, sequence) %>% distinct(subid) %>% summarise(n = length(subid))

# gender
demo %>% group_by(ageGroup) %>% count(gender)

# ethnicity
demo %>% group_by(ageGroup) %>% count(ethnicity)

# age
demo %>% group_by(ageGroup) %>% summarise(mean_age = mean(ageCalc, na.rm = T), sd_age = sd(ageCalc, na.rm = T))
qplot(subset(demo, ageGroup == "children")$ageCalc)
qplot(subset(demo, ageGroup == "adults")$ageCalc)

# demo %>% group_by(ageGroup) %>% filter(age < 100) %>% summarise(mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm = T))
# qplot(subset(demo, ageGroup == "children")$age[demo$age < 100])
# qplot(subset(demo, ageGroup == "adults")$age[demo$age < 100])

# age by condition
demo %>% group_by(ageGroup, sequence) %>% summarise(mean_age = round(mean(ageCalc, na.rm = T), 2), sd_age = round(sd(ageCalc, na.rm = T), 2))

######################################################## analysis & plots #####

# --- MAKE CHARACTER PAIRS DATAFRAME ------------------------------------------

d1 = dd %>% 
  filter(phase == "test") %>%
  mutate(
    pair = 
      ifelse(
        leftCharacter == "grownup" | 
          rightCharacter == "grownup",
        ifelse(
          rightCharacter == "kid" | 
            leftCharacter == "kid", 
          "grownup.kid",
          ifelse(
            rightCharacter == "baby" | 
              leftCharacter == "baby", 
            "grownup.baby",
            ifelse(
              rightCharacter == "dog" | 
                leftCharacter == "dog", 
              "grownup.dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "grownup.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "grownup.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "grownup.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "grownup.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "grownup.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "grownup.stapler",
                          NA))))))))),
        ifelse(
          leftCharacter == "kid" | 
            rightCharacter == "kid",
          ifelse(
            rightCharacter == "baby" | 
              leftCharacter == "baby", 
            "kid.baby",
            ifelse(
              rightCharacter == "dog" | 
                leftCharacter == "dog", 
              "kid.dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "kid.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "kid.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "kid.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "kid.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "kid.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "kid.stapler",
                          NA)))))))),
          ifelse(
            leftCharacter == "baby" | 
              rightCharacter == "baby",
            ifelse(
              rightCharacter == "dog" | 
                leftCharacter == "dog", 
              "baby.dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "baby.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "baby.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "baby.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "baby.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "baby.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "baby.stapler",
                          NA))))))),
            ifelse(
              leftCharacter == "dog" | 
                rightCharacter == "dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "dog.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "dog.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "dog.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "dog.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "dog.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "dog.stapler",
                          NA)))))),
              ifelse(
                leftCharacter == "bear" | 
                  rightCharacter == "bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "bear.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "bear.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "bear.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "bear.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "bear.stapler",
                          NA))))),
                ifelse(
                  leftCharacter == "bug" | 
                    rightCharacter == "bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "bug.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "bug.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "bug.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "bug.stapler",
                          NA)))),
                  ifelse(
                    leftCharacter == "robot" | rightCharacter == "robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "robot.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "robot.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "robot.stapler",
                          NA))),
                    ifelse(
                      leftCharacter == "computer" | 
                        rightCharacter == "computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "computer.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "computer.stapler",
                          NA)),
                      ifelse(
                        leftCharacter == "car" | 
                          rightCharacter == "car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "car.stapler",
                          NA),
                        NA)))))))))) %>%
  mutate(pair = factor(pair,
                       levels = c("grownup.kid",
                                  "grownup.baby",
                                  "kid.baby",
                                  "grownup.dog",
                                  "kid.dog",
                                  "baby.dog",
                                  "grownup.bear",
                                  "kid.bear",
                                  "baby.bear",
                                  "grownup.bug",
                                  "kid.bug",
                                  "baby.bug",
                                  "grownup.robot",
                                  "kid.robot",
                                  "baby.robot",
                                  "grownup.computer",
                                  "kid.computer",
                                  "baby.computer",
                                  "grownup.car",
                                  "kid.car",
                                  "baby.car",
                                  "grownup.stapler",
                                  "kid.stapler",
                                  "baby.stapler",
                                  "dog.bear",
                                  "dog.bug",
                                  "bear.bug",
                                  "dog.robot",
                                  "bear.robot",
                                  "bug.robot",
                                  "dog.computer",
                                  "bear.computer",
                                  "bug.computer",
                                  "dog.car",
                                  "bear.car",
                                  "bug.car",
                                  "dog.stapler",
                                  "bear.stapler",
                                  "bug.stapler",
                                  "robot.computer",
                                  "robot.car",
                                  "computer.car",
                                  "robot.stapler",
                                  "computer.stapler",
                                  "car.stapler")),
         predicate = factor(predicate, 
                            levels = c("hunger", "feelings", "thinking")),
         responseNumFlip = 
           ifelse(
             substr(leftCharacter,1,3) == substr(pair,1,3),
             responseNum,
             -1 * responseNum),
         pairCat = 
           # check if there is a human
           ifelse(
             grepl("grownup", pair) == T | 
               grepl("kid", pair) == T |
               grepl("baby", pair) == T,
             # if there is, check if there are two humans
             ifelse(
               grepl("grownup.kid", pair) == T | 
                 grepl("grownup.baby", pair) == T |
                 grepl("kid.baby", pair) == T,
               # if there are, done
               "human.human",
               # if not, check if there is an animal
               ifelse(
                 grepl("dog", pair) == T | 
                   grepl("bear", pair) == T |
                   grepl("bug", pair) == T,
                 # if there is, done
                 "human.animal",
                 # if not, check if there is a tech
                 ifelse(
                   grepl("robot", pair) == T | 
                     grepl("computer", pair) == T |
                     grepl("car", pair) == T,
                   # if there is, done
                   "human.tech",
                   # if not, check if there is a stapler
                   ifelse(
                     grepl("stapler", pair) == T,
                     # if there is, done
                     "control",
                     # if not, error
                     NA)))),
             # if not, check if there is an animal
             ifelse(
               grepl("dog", pair) == T | 
                 grepl("bear", pair) == T |
                 grepl("bug", pair) == T,
               # if there is, check if there are two animals
               ifelse(
                 grepl("dog.bear", pair) == T | 
                   grepl("dog.bug", pair) == T |
                   grepl("bear.bug", pair) == T,
                 # if there are, done
                 "animal.animal",
                 # if not, check if there is a tech
                 ifelse(
                   grepl("robot", pair) == T | 
                     grepl("computer", pair) == T |
                     grepl("car", pair) == T,
                   # if there is, done
                   "animal.tech",
                   # if not, check if there is a stapler
                   ifelse(
                     grepl("stapler", pair) == T,
                     # if there is, done
                     "control",
                     # if not, error
                     NA))),
               # if not, check if there is a tech
               ifelse(
                 grepl("robot", pair) == T | 
                   grepl("computer", pair) == T |
                   grepl("car", pair) == T,
                 # if there is, check if there are two techs
                 ifelse(
                   grepl("robot.computer", pair) == T | 
                     grepl("robot.car", pair) == T |
                     grepl("computer.car", pair) == T,
                   # if there are, done
                   "tech.tech",
                   # if not, check if there is a stapler
                   ifelse(
                     grepl("stapler", pair) == T,
                     # if there is, done
                     "control",
                     # if not, error
                     NA)),
                 # if not, error
                 NA)))) %>%
  mutate(pairCat = factor(pairCat,
                          levels = c("human.human",
                                     "animal.animal",
                                     "tech.tech",
                                     "human.animal",
                                     "human.tech",
                                     "animal.tech",
                                     "control")))

# --- PLOT MEANS BY COMPARISON: character level --------------------------------

# make table for plotting
plotTable1 = d1 %>% 
  group_by(predicate, pairCat, pair) %>% 
  summarise(mean = mean(responseNumFlip, na.rm = T),
            sd = sd(responseNumFlip, na.rm = T),
            n = length(responseNumFlip))

plot_char <- ggplot(aes(x = 
                          reorder(pair,
                                  as.numeric(
                                    factor(pairCat,
                                           levels = c("human.human",
                                                      "animal.animal",
                                                      "tech.tech",
                                                      "human.animal",
                                                      "human.tech",
                                                      "animal.tech",
                                                      "control")))),
                        y = mean,
                        fill = pairCat),
                    data = plotTable1) +
  facet_grid(predicate ~ .,
             labeller = labeller(predicate = c("hunger" = "...get hungry",
                                               "feelings" = "...have feelings",
                                               "thinking" = "...think"))) +
  geom_bar(stat = "identity", position = "identity") +
  geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
                    ymax = mean + 2*sd/sqrt(n),
                    width = 0.1)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme_bw() +
  scale_fill_brewer(type = "qual",
                    palette = 2) +
  theme(text = element_text(size = 20),
        legend.position = "top",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  labs(title = "MEAN COMPARISON SCORES\nby character pair\n",
       x = "\nCHARACTER PAIR",
       y = "MEAN RESPONSE\n-2 (-1): 1st character is much (slightly) more likely to...,\n0: characters are both equally likely to...,\n+2 (+1): 2nd character is much (slightly) more likely to...\n",
       fill = "PAIR CATEGORY: ")

# --- PLOT MEANS BY COMPARISON: category level --------------------------------

# make table for plotting
plotTable2 = d1 %>% 
  group_by(predicate, pairCat) %>% 
  summarise(mean = mean(responseNumFlip, na.rm = T),
            sd = sd(responseNumFlip, na.rm = T),
            n = length(responseNumFlip))

plot_cat <- ggplot(aes(x = 
                         reorder(pairCat,
                                 as.numeric(
                                   factor(pairCat,
                                          levels = c("human.human",
                                                     "animal.animal",
                                                     "tech.tech",
                                                     "human.animal",
                                                     "human.tech",
                                                     "animal.tech",
                                                     "control")))),
                       y = mean,
                       fill = pairCat),
                   data = plotTable2) +
  facet_grid(predicate ~ .,
             labeller = labeller(predicate = c("hunger" = "...get hungry",
                                               "feelings" = "...have feelings",
                                               "thinking" = "...think"))) +
  geom_bar(stat = "identity", position = "identity") +
  geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
                    ymax = mean + 2*sd/sqrt(n),
                    width = 0.1)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme_bw() +
  scale_fill_brewer(type = "qual",
                    palette = 2) +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  labs(title = "MEAN COMPARISON SCORES\nby character pair category\n",
       x = "\nCHARACTER PAIR CATEGORY",
       y = "MEAN RESPONSE\n-2 (-1): 1st character is much (slightly) more likely to...,\n0: characters are both equally likely to...,\n+2 (+1): 2nd character is much (slightly) more likely to...\n")

plot_cat.v2 <- ggplot(aes(x = interaction(predicate, pairCat),
                          y = mean,
                          group = predicate,
                          fill = predicate),
                      data = plotTable2) +
  geom_bar(stat = "identity", position = "identity") + 
  geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
                    ymax = mean + 2*sd/sqrt(n),
                    width = 0.1)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme_bw() +
  scale_fill_brewer(type = "qual",
                    palette = 2) +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1))

# --- PRINT PLOTS -------------------------------------------------------------

plot_char
plot_cat
# plot_cat.v2

# --- COMPARE CHILDREN TO ADULTS ----------------------------------------------

temp = d1 %>% 
  group_by(predicate, pairCat, ageGroup, subid) %>% 
  summarise(mean = mean(responseNumFlip, na.rm = T),
            sd = sd(responseNumFlip, na.rm = T),
            n = length(responseNumFlip)) %>%
  group_by(predicate, pairCat, ageGroup) %>%
  summarise(mean_mean = mean(mean, na.rm = T),
            sd_mean = sd(mean, na.rm = T),
            n_mean = length(mean))

gtemp <- ggplot(aes(x = 
                      reorder(pairCat,
                              as.numeric(
                                factor(pairCat,
                                       levels = c("human.human",
                                                  "animal.animal",
                                                  "tech.tech",
                                                  "human.animal",
                                                  "human.tech",
                                                  "animal.tech",
                                                  "control")))),
                    y = mean_mean,
                    fill = pairCat),
                data = temp) +
  facet_grid(predicate ~ ageGroup,
             labeller = labeller(predicate = c("hunger" = "...get hungry",
                                               "feelings" = "...have feelings",
                                               "thinking" = "...think"))) +
  geom_bar(stat = "identity", position = "identity") +
  geom_errorbar(aes(ymin = mean_mean - 2*sd_mean/sqrt(n_mean),
                    ymax = mean_mean + 2*sd_mean/sqrt(n_mean),
                    width = 0.1)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme_bw() +
  scale_fill_brewer(type = "qual",
                    palette = 2) +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  labs(title = "mean_mean COMPARISON SCORES\nby character pair category\n",
       x = "\nCHARACTER PAIR CATEGORY",
       y = "mean_mean RESPONSE\n-2 (-1): 1st character is much (slightly) more likely to...,\n0: characters are both equally likely to...,\n+2 (+1): 2nd character is much (slightly) more likely to...\n")

temp2 = d1 %>% 
  group_by(predicate, pairCat, pair, ageGroup, subid) %>% 
  summarise(mean = mean(responseNumFlip, na.rm = T),
            sd = sd(responseNumFlip, na.rm = T),
            n = length(responseNumFlip)) %>%
  group_by(predicate, pairCat, pair, ageGroup) %>%
  summarise(mean_mean = mean(mean, na.rm = T),
            sd_mean = sd(mean, na.rm = T),
            n_mean = length(mean))

gtemp2 <- ggplot(aes(x = 
                          reorder(pair,
                                  as.numeric(
                                    factor(pairCat,
                                           levels = c("human.human",
                                                      "animal.animal",
                                                      "tech.tech",
                                                      "human.animal",
                                                      "human.tech",
                                                      "animal.tech",
                                                      "control")))),
                        y = mean_mean,
                        fill = pairCat),
                    data = temp2) +
  facet_grid(predicate ~ ageGroup,
             labeller = labeller(predicate = c("hunger" = "...get hungry",
                                               "feelings" = "...have feelings",
                                               "thinking" = "...think"))) +
  geom_bar(stat = "identity", position = "identity") +
  geom_errorbar(aes(ymin = mean_mean - 2*sd_mean/sqrt(n_mean),
                    ymax = mean_mean + 2*sd_mean/sqrt(n_mean),
                    width = 0.1)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  theme_bw() +
  scale_fill_brewer(type = "qual",
                    palette = 2) +
  theme(text = element_text(size = 20),
        legend.position = "top",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  labs(title = "mean_mean COMPARISON SCORES\nby character pair\n",
       x = "\nCHARACTER PAIR",
       y = "mean_mean RESPONSE\n-2 (-1): 1st character is much (slightly) more likely to...,\n0: characters are both equally likely to...,\n+2 (+1): 2nd character is much (slightly) more likely to...\n",
       fill = "PAIR CATEGORY: ")

# --- CLOSE LOOK PLOTTING ----------------------------------------------------

closeLook <- function(chosenCharacters) {
  
  allPairs <- levels(d1$pair)
  
  chosenPair <- data.frame(allPairs) %>% 
    filter(grepl(chosenCharacters[1], allPairs) & 
             grepl(chosenCharacters[2], allPairs))
  
  chosenPair <- as.character(chosenPair$allPairs)
  
  df <- d1 %>%
    filter(pair == chosenPair) %>%
    mutate(responseSpec = factor(responseNumFlip,
                                 levels = c(-2, -1, 0, 1, 2),
                                 labels = c(paste("much more", chosenCharacters[1]),
                                            paste("slightly more", chosenCharacters[1]),
                                            "both equally",
                                            paste("slightly more", chosenCharacters[2]),
                                            paste("much more", chosenCharacters[2]))))
  
  g <- ggplot(aes(x = responseSpec),
              data = df) +
    facet_grid(predicate ~ ageGroup,
               labeller = labeller(predicate = c("hunger" = "'...get hungry?'",
                                                 "feelings" = "'...have feelings?'",
                                                 "thinking" = "'...think?'"),
                                   ageGroup = c("adults" = "Adults (Study 1)", 
                                                "children" = "Children (Study 2)"))) +
    geom_bar() +
    theme_bw() +
    theme(text = element_text(size = 20),
          axis.text.x = element_text(angle = 30, hjust = 1)) +
    scale_x_discrete() +
    scale_y_discrete() +
    labs(title = paste0(toupper(substring(chosenCharacters[1], 1, 1)), substring(chosenCharacters[1], 2), " vs. ", toupper(substring(chosenCharacters[2], 1, 1)), substring(chosenCharacters[2], 2), " comparisons:\n'Which one is more likely to...'\n"),
         x = "\nResponse",
         y = "Count\n")
  
  return(g)
}

# robot vs. bug only

gtemp3 <- closeLook(c("robot", "bug")); gtemp3

# make all plots for all comparisons
plots = list(NULL)
for(i in 1:length(levels(d1$pair))) {
  chosenCharacters <- unlist(strsplit(levels(d1$pair)[i], "[.]"))
  
  df <- d1 %>%
    filter(pair == levels(d1$pair)[i]) %>%
    mutate(responseSpec = factor(responseNumFlip,
                                 levels = c(-2, -1, 0, 1, 2),
                                 labels = c(paste("much more", chosenCharacters[1]),
                                            paste("slightly more", chosenCharacters[1]),
                                            "both equally",
                                            paste("slightly more", chosenCharacters[2]),
                                            paste("much more", chosenCharacters[2]))))
  
  g <- ggplot(aes(x = responseSpec),
              data = df) +
    facet_grid(predicate ~ ageGroup,
               labeller = labeller(predicate = c("hunger" = "'...get hungry?'",
                                                 "feelings" = "'...have feelings?'",
                                                 "thinking" = "'...think?'"),
                                   ageGroup = c("adults" = "Adults (Study 1)", 
                                                "children" = "Children (Study 2)"))) +
    geom_bar() +
    theme_bw() +
    theme(text = element_text(size = 40),
          axis.text.x = element_text(angle = 30, hjust = 1)) +
    scale_x_discrete() +
    scale_y_discrete() +
    labs(title = paste0(toupper(substring(chosenCharacters[1], 1, 1)), substring(chosenCharacters[1], 2), " vs. ", toupper(substring(chosenCharacters[2], 1, 1)), substring(chosenCharacters[2], 2), " comparisons:\n'Which one is more likely to...'\n"),
         x = "\nResponse",
         y = "Count\n")
  
  plots[[i]] <- g

  rm(chosenCharacters, df, g, i)
}

graphics.off()
setwd("./plots")
png(filename = "comparison%03d.png", width = 1400, height = 1300)
plots
graphics.off()


# --- RAW COUNTS PLOTTING -----------------------------------------------------

temp1 <- dd %>% 
  filter(phase == "test" & is.na(responseNum) == F) %>%
  mutate(responseCat = factor(responseNum,
                              levels = c("-2", "-1", "0", "1", "2")),
         responseFlip = factor(responseNum, # need to flip for leftCharacter
                               labels = c("2", "1", "0", "-1", "-2"))) %>%
  # count(ageGroup, predicate, leftCharacter) %>% 
  count(ageGroup, predicate, leftCharacter, responseFlip) %>% 
  rename(character = leftCharacter,
         countLeft = n)
temp2 <- dd %>% 
  filter(phase == "test" & is.na(responseNum) == F) %>%
  mutate(responseCat = factor(responseNum, # don't need to flip for rightCharacter
                              levels = c("2", "1", "0", "-1", "-2")),
         responseFlip = responseCat) %>%
  # count(ageGroup, predicate, leftCharacter) %>% 
  count(ageGroup, predicate, rightCharacter, responseFlip) %>% 
  rename(character = rightCharacter,
         countRight = n)
temp3 <- full_join(temp1, temp2) %>%
  mutate(countLeft = ifelse(is.na(countLeft) == TRUE, 0, as.numeric(countLeft)),
         countRight = ifelse(is.na(countRight) == TRUE, 0, as.numeric(countRight)),
         countTotal = countLeft + countRight,
         responseFlip = factor(responseFlip,
                              levels = c("2", "1", "0", "-1", "-2"))) %>%
  select(-countLeft, -countRight) %>%
  ungroup() %>%
  mutate(character = factor(character, levels = c("grownup", "kid", "baby",
                                                  "dog", "bear", "bug",
                                                  "robot", "computer", "car",
                                                  "stapler")),
         predicate = factor(predicate, levels = c("hunger", "feelings", "thinking")))

# check <- temp3 %>%
#   group_by(ageGroup, predicate, character) %>%
#   summarise(sum = sum(countTotal, na.rm = T))

temp4_adults <- temp3 %>% filter(ageGroup == "adults")
temp4_children <- temp3 %>% filter(ageGroup == "children")

# plot adults
# ggplot(data = temp4_adults, aes(x = responseFlip, y = countTotal,
#                                 fill = responseFlip)) +
#   geom_bar(position = "identity", stat = "identity") +
#   facet_grid(predicate ~ character) +
#   scale_fill_brewer(type = "div", palette = 4, 
#                     labels = c("much more this one", "slightly more this one",
#                                "both the same", "slightly more the other one",
#                                "much more the other one")) +
#   theme_bw() +
#   scale_y_continuous(breaks = seq(0, 90, 15)) +
#   theme(text = element_text(size = 20)) +
#   labs(x = "\nResponse",
#        y = "Count\n",
#        fill = "Response",
#        title = "Adults: Raw counts of responses by character and capacity\n")

ggplot(data = temp4_adults, aes(x = character, y = countTotal, 
                                fill = responseFlip,
                                order = responseFlip)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(predicate ~ .) +
  scale_fill_brewer(type = "div", palette = 4, 
                    labels = c("much more this one", "slightly more this one",
                               "both the same", "slightly more the other one",
                               "much more the other one")) +
  scale_y_continuous(breaks = seq(0, 90, 15)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "bottom") +
  labs(x = "\nCharacter",
       y = "Count\n",
       fill = "Response",
       title = "Adults: Raw counts of responses by character and capacity\n")

ggplot(data = temp4_adults %>% filter(predicate == "hunger"), 
       aes(x = character, y = countTotal,
           fill = responseFlip, order = responseFlip)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(type = "div", palette = 4, 
                    labels = c(" much more this one ", " slightly more this one ",
                               " both equally ", " slightly more the other one ",
                               " much more the other one ")) +
  scale_y_continuous(breaks = seq(0, 90, 15)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2)) +
  # guides(fill = guide_legend(nrow = 5, reverse = TRUE)) +
  labs(x = "\nCharacter",
       y = "Count\n",
       fill = "Response: ",
       title = "Adults: Hunger (Raw counts)\n")

ggplot(data = temp4_adults %>% filter(predicate == "feelings"), 
       aes(x = character, y = countTotal,
           fill = responseFlip, order = responseFlip)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(type = "div", palette = 4, 
                    labels = c(" much more this one ", " slightly more this one ",
                               " both equally ", " slightly more the other one ",
                               " much more the other one ")) +
  scale_y_continuous(breaks = seq(0, 90, 15)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2)) +
  # guides(fill = guide_legend(nrow = 5, reverse = TRUE)) +
  labs(x = "\nCharacter",
       y = "Count\n",
       fill = "Response: ",
       title = "Adults: Feelings (Raw counts)\n")

ggplot(data = temp4_adults %>% filter(predicate == "thinking"), 
       aes(x = character, y = countTotal,
           fill = responseFlip, order = responseFlip)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(type = "div", palette = 4, 
                    labels = c(" much more this one ", " slightly more this one ",
                               " both equally ", " slightly more the other one ",
                               " much more the other one ")) +
  scale_y_continuous(breaks = seq(0, 90, 15)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2)) +
  # guides(fill = guide_legend(nrow = 5, reverse = TRUE)) +
  labs(x = "\nCharacter",
       y = "Count\n",
       fill = "Response: ",
       title = "Adults: Thinking (Raw counts)\n")

# plot children
# ggplot(data = temp4_children, aes(x = responseFlip, y = countTotal,
#                                   fill = responseFlip)) +
#   geom_bar(position = "identity", stat = "identity") +
#   facet_grid(predicate ~ character) +
#   scale_fill_brewer(type = "div", palette = 4, 
#                     labels = c("much more this one", "slightly more this one",
#                                "both the same", "slightly more the other one",
#                                "much more the other one")) +
#   scale_y_continuous(breaks = seq(0, 90, 15)) +
#   theme_bw() +
#   theme(text = element_text(size = 20)) +
#   labs(x = "\nResponse",
#        y = "Count\n",
#        fill = "Response",
#        title = "Children: Raw counts of responses by character and capacity\n")

ggplot(data = temp4_children, aes(x = character, y = countTotal, 
                                  fill = responseFlip,
                                  order = responseFlip)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(predicate ~ .) +
  scale_fill_brewer(type = "div", palette = 4, 
                    labels = c("much more this one", "slightly more this one",
                               "both the same", "slightly more the other one",
                               "much more the other one")) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 90, 15)) +
  theme(text = element_text(size = 20)) +
  labs(x = "\nCharacter",
       y = "Count\n",
       fill = "Response",
       title = "Children: Raw counts of responses by character and capacity\n")

ggplot(data = temp4_children %>% filter(predicate == "hunger"), 
       aes(x = character, y = countTotal,
           fill = responseFlip, order = responseFlip)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(type = "div", palette = 4, 
                    labels = c(" much more this one ", " slightly more this one ",
                               " both equally ", " slightly more the other one ",
                               " much more the other one ")) +
  scale_y_continuous(breaks = seq(0, 90, 15)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2)) +
  # guides(fill = guide_legend(nrow = 5, reverse = TRUE)) +
  labs(x = "\nCharacter",
       y = "Count\n",
       fill = "Response: ",
       title = "Children: Hunger (Raw counts)\n")

ggplot(data = temp4_children %>% filter(predicate == "feelings"), 
       aes(x = character, y = countTotal,
           fill = responseFlip, order = responseFlip)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(type = "div", palette = 4, 
                    labels = c(" much more this one ", " slightly more this one ",
                               " both equally ", " slightly more the other one ",
                               " much more the other one ")) +
  scale_y_continuous(breaks = seq(0, 90, 15)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2)) +
  # guides(fill = guide_legend(nrow = 5, reverse = TRUE)) +
  labs(x = "\nCharacter",
       y = "Count\n",
       fill = "Response: ",
       title = "Children: Feelings (Raw counts)\n")

ggplot(data = temp4_children %>% filter(predicate == "thinking"), 
       aes(x = character, y = countTotal,
           fill = responseFlip, order = responseFlip)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(type = "div", palette = 4, 
                    labels = c(" much more this one ", " slightly more this one ",
                               " both equally ", " slightly more the other one ",
                               " much more the other one ")) +
  scale_y_continuous(breaks = seq(0, 90, 15)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2)) +
  # guides(fill = guide_legend(nrow = 5, reverse = TRUE)) +
  labs(x = "\nCharacter",
       y = "Count\n",
       fill = "Response: ",
       title = "Children: Thinking (Raw counts)\n")