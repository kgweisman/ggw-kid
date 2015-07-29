########################################################### preliminaries #####

# --- PACKAGES & FUNCTIONS ----------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scatterplot3d)
library(lme4)
library(psych)
library(stats)
library(scales)
library(smacof)

# clear environment
rm(list=ls())

# clear graphics
dev.off()

# --- IMPORTING DATA ----------------------------------------------------------

# read in data: individual scores
# # ... FULL DATASET
# dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid-run-01&02_2015-07-29_data_anonymized.csv")[-1] # get rid of column of obs numbers
# 
# # ... RUN01
# dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid-run-01_2015-06-13_data_anonymized.csv")[-1] # get rid of column of obs numbers

# ... RUN02
dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid-run-02_2015-07-29_data_anonymized.csv")[-1] # get rid of column of obs numbers

glimpse(dd)

# --- FILTERING BY ETHNICITY --------------------------------------------------

dd_white = dd %>%
  filter(ethnicity == "white")

dd_nonwhite = dd %>%
  filter(ethnicity != "white" & 
           ethnicity != "NA" & 
           ethnicity != "other_prefNo")

# set group of interest
# ... to white:
# dd = dd_white

# # ... to nonwhite:
# dd = dd_nonwhite

# --- FORMATTING DATA ---------------------------------------------------------

# make table of character means by mental capacity
charmeans = dd %>%
  filter(phase == "test") %>%
  select(subid, predicate, leftCharacter, rightCharacter, response, responseNum) %>%
  mutate(grownup = ifelse(leftCharacter == "grownup", responseNum,
                          ifelse(rightCharacter == "grownup", -1 * responseNum,
                                 NA)),
         kid = ifelse(leftCharacter == "kid", responseNum,
                      ifelse(rightCharacter == "kid", -1 * responseNum,
                             NA)),
         baby = ifelse(leftCharacter == "baby", responseNum,
                       ifelse(rightCharacter == "baby", -1 * responseNum,
                              NA)),
         dog = ifelse(leftCharacter == "dog", responseNum,
                      ifelse(rightCharacter == "dog", -1 * responseNum,
                             NA)),
         bear = ifelse(leftCharacter == "bear", responseNum,
                       ifelse(rightCharacter == "bear", -1 * responseNum,
                              NA)),
         bug = ifelse(leftCharacter == "bug", responseNum,
                      ifelse(rightCharacter == "bug", -1 * responseNum,
                             NA)),
         robot = ifelse(leftCharacter == "robot", responseNum,
                        ifelse(rightCharacter == "robot", -1 * responseNum,
                               NA)),
         computer = ifelse(leftCharacter == "computer", responseNum,
                           ifelse(rightCharacter == "computer", -1 * responseNum,
                                  NA)),
         car = ifelse(leftCharacter == "car", responseNum,
                      ifelse(rightCharacter == "car", -1 * responseNum,
                             NA)),
         stapler = ifelse(leftCharacter == "stapler", responseNum,
                          ifelse(rightCharacter == "stapler", -1 * responseNum,
                                 NA))
  ) %>%
  select(predicate, subid, grownup, kid, baby, dog, bear, bug, 
         robot, computer, car, stapler) %>%  
  gather(character, response,
         -predicate, -subid) %>%
  group_by(predicate, character) %>%
  summarise(mean = mean(response, na.rm = T))

glimpse(charmeans)

# format into wideform with characters as rows
charmeans_table = charmeans %>%
  spread(predicate, mean)

charnames = as.character(charmeans_table$character)

d1 = charmeans_table[-1]
rownames(d1) = charnames
print(d1)

# NOTE: not yet implemented for this study as of 2015-05-09
# # make table of mental capacity means by character
# # formatted in wideform with characters as rows
# condmeans = d %>%
#   select(condition, subid, gerald_schiff_pvs, toby_chimp, fetus, god,
#          delores_gleitman_deceased, sharon_harvey_woman, green_frog,
#          todd_billingsley_man, charlie_dog, nicholas_gannon_baby,
#          samantha_hill_girl, kismet_robot, you) %>%  
#   gather(character, response,
#          -condition, -subid) %>%
#   group_by(condition, character) %>%
#   summarise(mean = mean(response, na.rm = T))
# 
# # format into wideform with characters as rows
# condmeans_table = condmeans %>%
#   spread(character, mean)
# 
# subidnames = condmeans_table$subid
# 
# d3 = condmeans_table[-1]
# d3 = d3[-1]
# names(d3) = charnames
# rownames(d3) = subidnames
# print(d3)

##################################################### regression analyses #####

# read in contrasts: adult vs. children (kid and baby)
c0a = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/Stim development/analysis/contrasts 2015-05-15a.csv")

# read in contrasts: baby vs. non-babies (kid and adult)
c0b = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/Stim development/analysis/contrasts 2015-05-15b.csv")

# choose contrast to use
# c0 = c0a
c0 = c0b

c1 = c0 %>% arrange(pair)
rownames(c1) = c1[,1]
c1 = c1[-1]

# add pairs variable to dataframe ('d1') ------
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

# set contrasts ----
contrasts(d1$pair) = as.matrix(c1)
contrasts(d1$predicate) = cbind(bio.v.nonbio = c(-1,2,-1),
                                think.v.feel = c(1,0,-1))

r0 = lm(responseNumFlip ~ pair, d1); summary(r0)
r1 = lmer(responseNumFlip ~ pair + (1 | subid), d1); summary(r1)
# View(as.matrix(summary(r1)$coefficients))
r2 = lmer(responseNumFlip ~ pair + predicate + (1 | subid), d1); summary(r2)
r3 = lmer(responseNumFlip ~ pair * predicate + (1 | subid), d1); summary(r3)
anova(r1, r2, r3)
  
# plot by pair ----
plotTable = d1 %>% 
  group_by(predicate, pairCat, pair) %>% 
  summarise(mean = mean(responseNumFlip, na.rm = T),
            sd = sd(responseNumFlip, na.rm = T),
            n = length(responseNumFlip))

ggplot(aes(x = 
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
       data = plotTable) +
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

# plot by pair category ----
plotTable2 = d1 %>% 
  group_by(predicate, pairCat) %>% 
  summarise(mean = mean(responseNumFlip, na.rm = T),
            sd = sd(responseNumFlip, na.rm = T),
            n = length(responseNumFlip))

ggplot(aes(x = 
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


