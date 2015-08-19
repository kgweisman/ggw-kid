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
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01&02_2015-08-19_data_anonymized.csv")[-1] # get rid of column of obs numbers
# 
# # ... RUN01
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01_2015-06-13_data_anonymized.csv")[-1] # get rid of column of obs numbers

# ... RUN02
dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-02_2015-08-19_data_anonymized.csv")[-1] # get rid of column of obs numbers

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

dd_children_exact = dd_children %>%
  filter(ageCalc >= 4.5 & ageCalc <= 5.5)

# set group of interest
# ... to exact:
dd_children = dd_children_exact

# --------------->-> by finished ----------------------------------------------

dd_children_finished = dd_children %>%
  filter(subid != "d107" &
           subid != "k104")

# set group of interest
# ... to exact:
dd_children = dd_children_finished

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
dd = dd_children

# --------------->-> exclude stapler trials -----------------------------------

dd_nostapler <- dd %>%
  filter(leftCharacter != "stapler" & rightCharacter != "stapler") %>%
  mutate(leftCharacter = factor(leftCharacter),
         rightCharacter = factor(rightCharacter))

# set group of interest
# ... to no stapler:
# dd = dd_nostapler

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