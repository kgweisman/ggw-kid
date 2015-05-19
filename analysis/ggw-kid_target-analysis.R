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

# read in data: character means
d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/data/run-01&02&03&04&india01_2015-04-17_charmeans.csv")[-1] # get rid of column of obs numbers

glimpse(d)

# read in data: individual scores
dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/data/run-01&02&03&04&india01_2015-04-17_data_anonymized.csv")[-1] # get rid of column of obs numbers

glimpse(dd)

# --- FILTERING BY RTS --------------------------------------------------------

# # filter out trials where log_rt < 2SDs below mean
# dd = dd %>%
#   filter(under_lower == 0)
# 
# View(dd %>% group_by(subid) %>% summarise(trials_completed = length(log_rt)))

# # filter out participants where >50% trials have log_rt < 2SDs below mean
# dd = dd %>%
#   filter(prop_under > .5)
# 
# View(dd %>% group_by(subid) %>% summarise(trials_completed = length(log_rt)))

# --- FILTERING BY COMPREHENSION CHECK ----------------------------------------

# NOTE: currently only works for india run 03 and on!
d_comp2 = d %>% filter (compCheckCount < 2)
dd_comp2 = dd %>% filter (compCheckCount < 2)

# d = d_comp2
# dd = dd_comp2

# --- FILTERING BY COUNTRY ----------------------------------------------------

d_us = d %>% filter(country == "us")
dd_us = dd %>% filter(country == "us")

d_india = d %>% filter(country == "india")
dd_india = dd %>% filter(country == "india")

# set group of interest
# ... to US:
d = d_us
dd = dd_us

# # ... to India:
# d = d_india
# dd = dd_india

# --- FILTERING BY ETHNICITY --------------------------------------------------

d_white = d %>%
  filter(ethnicity == "white")
dd_white = dd %>%
  filter(ethnicity == "white")

d_nonwhite = d %>%
  filter(ethnicity != "white" & 
           ethnicity != "NA" & 
           ethnicity != "other_prefNo")

dd_nonwhite = dd %>%
  filter(ethnicity != "white" & 
           ethnicity != "NA" & 
           ethnicity != "other_prefNo")

# set group of interest
# ... to white:
# d = d_white
# dd = dd_white

# # ... to nonwhite:
# d = d_nonwhite
# dd = dd_nonwhite

# --- FORMATTING DATA ---------------------------------------------------------

# make table of character means by mental capacity
charmeans = d %>%
  select(condition, subid, gerald_schiff_pvs, toby_chimp, fetus, god,
         delores_gleitman_deceased, sharon_harvey_woman, green_frog,
         todd_billingsley_man, charlie_dog, nicholas_gannon_baby,
         samantha_hill_girl, kismet_robot, you) %>%  
  gather(character, response,
         -condition, -subid) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T))

glimpse(charmeans)

# format into wideform with characters as rows
charmeans_table = charmeans %>%
  spread(condition, mean)

charnames = as.character(charmeans_table$character)
charnames = ifelse(charnames == "charlie_dog", "dog",
                   ifelse(charnames == "delores_gleitman_deceased", "dead woman",
                          ifelse(charnames == "gerald_schiff_pvs", "PVS man", 
                                 ifelse(charnames == "green_frog", "frog",
                                        ifelse(charnames == "samantha_hill_girl", "girl",
                                               ifelse(charnames == "kismet_robot", "robot",
                                                      ifelse(charnames == "nicholas_gannon_baby", "baby",
                                                             ifelse(charnames == "sharon_harvey_woman", "woman",
                                                                    ifelse(charnames == "toby_chimp", "chimp",
                                                                           ifelse(charnames == "todd_billingsley_man", "man",
                                                                                  as.character(charnames)))))))))))

d1 = charmeans_table[-1]
rownames(d1) = charnames
print(d1)

# make table of mental capacity means by character
# formatted in wideform with characters as rows
condmeans = d %>%
  select(condition, subid, gerald_schiff_pvs, toby_chimp, fetus, god,
         delores_gleitman_deceased, sharon_harvey_woman, green_frog,
         todd_billingsley_man, charlie_dog, nicholas_gannon_baby,
         samantha_hill_girl, kismet_robot, you) %>%  
  gather(character, response,
         -condition, -subid) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T))

# format into wideform with characters as rows
condmeans_table = condmeans %>%
  spread(character, mean)

subidnames = condmeans_table$subid

d3 = condmeans_table[-1]
d3 = d3[-1]
names(d3) = charnames
rownames(d3) = subidnames
print(d3)

# --- MULTIDIMENSIONAL SCALING ANALYSIS by condition --------------------------

# --------> data formatting ---------------------------------------------------

# --------------->-> condition: COMMUNICATION

dissim_communication = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_communication <- dd %>%
  filter(condition == "Communication") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_communication$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_communication <- dissim_communication %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_communication <- dissim_communication %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_communication <- dissim_communication %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_communication = dissim_communication[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_communication = dissim_communication[-1]
rownames(dissim_communication) = names
colnames(dissim_communication) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_communication[j,i] = dissim_communication[i,j]
  }
}

dissim_communication = as.dist(dissim_communication)

# --------------->-> condition: CONSCIOUSNESS 

dissim_consciousness = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_consciousness <- dd %>%
  filter(condition == "Consciousness") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_consciousness$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_consciousness <- dissim_consciousness %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_consciousness <- dissim_consciousness %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_consciousness <- dissim_consciousness %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_consciousness = dissim_consciousness[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_consciousness = dissim_consciousness[-1]
rownames(dissim_consciousness) = names
colnames(dissim_consciousness) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_consciousness[j,i] = dissim_consciousness[i,j]
  }
}

dissim_consciousness = as.dist(dissim_consciousness)

# --------------->-> condition: DESIRE

dissim_desire = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_desire <- dd %>%
  filter(condition == "Desire") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_desire$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_desire <- dissim_desire %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_desire <- dissim_desire %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_desire <- dissim_desire %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_desire = dissim_desire[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_desire = dissim_desire[-1]
rownames(dissim_desire) = names
colnames(dissim_desire) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_desire[j,i] = dissim_desire[i,j]
  }
}

dissim_desire = as.dist(dissim_desire)

# --------------->-> condition: EMBARRASSMENT

dissim_embarrassment = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_embarrassment <- dd %>%
  filter(condition == "Embarrassment") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_embarrassment$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_embarrassment <- dissim_embarrassment %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_embarrassment <- dissim_embarrassment %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_embarrassment <- dissim_embarrassment %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_embarrassment = dissim_embarrassment[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_embarrassment = dissim_embarrassment[-1]
rownames(dissim_embarrassment) = names
colnames(dissim_embarrassment) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_embarrassment[j,i] = dissim_embarrassment[i,j]
  }
}

dissim_embarrassment = as.dist(dissim_embarrassment)

# --------------->-> condition: EMOTION RECOGNITION

dissim_emotionrecognition = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_emotionrecognition <- dd %>%
  filter(condition == "EmotionRecognition") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_emotionrecognition$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_emotionrecognition <- dissim_emotionrecognition %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_emotionrecognition <- dissim_emotionrecognition %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_emotionrecognition <- dissim_emotionrecognition %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_emotionrecognition = dissim_emotionrecognition[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_emotionrecognition = dissim_emotionrecognition[-1]
rownames(dissim_emotionrecognition) = names
colnames(dissim_emotionrecognition) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_emotionrecognition[j,i] = dissim_emotionrecognition[i,j]
  }
}

dissim_emotionrecognition = as.dist(dissim_emotionrecognition)

# --------------->-> condition: FEAR

dissim_fear = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_fear <- dd %>%
  filter(condition == "Fear") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_fear$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_fear <- dissim_fear %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_fear <- dissim_fear %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_fear <- dissim_fear %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_fear = dissim_fear[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_fear = dissim_fear[-1]
rownames(dissim_fear) = names
colnames(dissim_fear) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_fear[j,i] = dissim_fear[i,j]
  }
}

dissim_fear = as.dist(dissim_fear)

# --------------->-> condition: HUNGER

dissim_hunger = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_hunger <- dd %>%
  filter(condition == "Hunger") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_hunger$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_hunger <- dissim_hunger %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_hunger <- dissim_hunger %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_hunger <- dissim_hunger %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_hunger = dissim_hunger[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_hunger = dissim_hunger[-1]
rownames(dissim_hunger) = names
colnames(dissim_hunger) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_hunger[j,i] = dissim_hunger[i,j]
  }
}

dissim_hunger = as.dist(dissim_hunger)

# --------------->-> condition: JOY

dissim_joy = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_joy <- dd %>%
  filter(condition == "Joy") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_joy$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_joy <- dissim_joy %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_joy <- dissim_joy %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_joy <- dissim_joy %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_joy = dissim_joy[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_joy = dissim_joy[-1]
rownames(dissim_joy) = names
colnames(dissim_joy) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_joy[j,i] = dissim_joy[i,j]
  }
}

dissim_joy = as.dist(dissim_joy)

# --------------->-> condition: MEMORY

dissim_memory = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_memory <- dd %>%
  filter(condition == "Memory") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_memory$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_memory <- dissim_memory %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_memory <- dissim_memory %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_memory <- dissim_memory %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_memory = dissim_memory[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_memory = dissim_memory[-1]
rownames(dissim_memory) = names
colnames(dissim_memory) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_memory[j,i] = dissim_memory[i,j]
  }
}

dissim_memory = as.dist(dissim_memory)

# --------------->-> condition: MORALITY

dissim_morality = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_morality <- dd %>%
  filter(condition == "Morality") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_morality$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_morality <- dissim_morality %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_morality <- dissim_morality %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_morality <- dissim_morality %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_morality = dissim_morality[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_morality = dissim_morality[-1]
rownames(dissim_morality) = names
colnames(dissim_morality) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_morality[j,i] = dissim_morality[i,j]
  }
}

dissim_morality = as.dist(dissim_morality)

# --------------->-> condition: PAIN

dissim_pain = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_pain <- dd %>%
  filter(condition == "Pain") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_pain$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_pain <- dissim_pain %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_pain <- dissim_pain %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_pain <- dissim_pain %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_pain = dissim_pain[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_pain = dissim_pain[-1]
rownames(dissim_pain) = names
colnames(dissim_pain) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_pain[j,i] = dissim_pain[i,j]
  }
}

dissim_pain = as.dist(dissim_pain)

# --------------->-> condition: PERSONALITY

dissim_personality = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_personality <- dd %>%
  filter(condition == "Personality") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_personality$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_personality <- dissim_personality %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_personality <- dissim_personality %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_personality <- dissim_personality %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_personality = dissim_personality[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_personality = dissim_personality[-1]
rownames(dissim_personality) = names
colnames(dissim_personality) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_personality[j,i] = dissim_personality[i,j]
  }
}

dissim_personality = as.dist(dissim_personality)

# --------------->-> condition: PLANNING

dissim_planning = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_planning <- dd %>%
  filter(condition == "Planning") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_planning$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_planning <- dissim_planning %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_planning <- dissim_planning %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_planning <- dissim_planning %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_planning = dissim_planning[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_planning = dissim_planning[-1]
rownames(dissim_planning) = names
colnames(dissim_planning) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_planning[j,i] = dissim_planning[i,j]
  }
}

dissim_planning = as.dist(dissim_planning)

# --------------->-> condition: PLEASURE

dissim_pleasure = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_pleasure <- dd %>%
  filter(condition == "Pleasure") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_pleasure$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_pleasure <- dissim_pleasure %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_pleasure <- dissim_pleasure %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_pleasure <- dissim_pleasure %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_pleasure = dissim_pleasure[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_pleasure = dissim_pleasure[-1]
rownames(dissim_pleasure) = names
colnames(dissim_pleasure) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_pleasure[j,i] = dissim_pleasure[i,j]
  }
}

dissim_pleasure = as.dist(dissim_pleasure)

# --------------->-> condition: PRIDE

dissim_pride = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_pride <- dd %>%
  filter(condition == "Pride") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_pride$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_pride <- dissim_pride %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_pride <- dissim_pride %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_pride <- dissim_pride %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_pride = dissim_pride[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_pride = dissim_pride[-1]
rownames(dissim_pride) = names
colnames(dissim_pride) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_pride[j,i] = dissim_pride[i,j]
  }
}

dissim_pride = as.dist(dissim_pride)

# --------------->-> condition: RAGE

dissim_rage = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_rage <- dd %>%
  filter(condition == "Rage") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_rage$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_rage <- dissim_rage %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_rage <- dissim_rage %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_rage <- dissim_rage %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_rage = dissim_rage[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_rage = dissim_rage[-1]
rownames(dissim_rage) = names
colnames(dissim_rage) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_rage[j,i] = dissim_rage[i,j]
  }
}

dissim_rage = as.dist(dissim_rage)

# --------------->-> condition: SELF-CONTROL

dissim_selfcontrol = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_selfcontrol <- dd %>%
  filter(condition == "Self-Control") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_selfcontrol$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_selfcontrol <- dissim_selfcontrol %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_selfcontrol <- dissim_selfcontrol %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_selfcontrol <- dissim_selfcontrol %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_selfcontrol = dissim_selfcontrol[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_selfcontrol = dissim_selfcontrol[-1]
rownames(dissim_selfcontrol) = names
colnames(dissim_selfcontrol) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_selfcontrol[j,i] = dissim_selfcontrol[i,j]
  }
}

dissim_selfcontrol = as.dist(dissim_selfcontrol)

# --------------->-> condition: THOUGHT

dissim_thought = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_thought <- dd %>%
  filter(condition == "Thought") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_thought$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_thought <- dissim_thought %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
dissim_thought <- dissim_thought %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_thought <- dissim_thought %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_thought = dissim_thought[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_thought = dissim_thought[-1]
rownames(dissim_thought) = names
colnames(dissim_thought) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_thought[j,i] = dissim_thought[i,j]
  }
}

dissim_thought = as.dist(dissim_thought)

# --------> ordinal MDS -------------------------------------------------------
# NOTE: could also explore fitting with more than 2 dimensions...

# do MDS: COMMUNICATION
mds_communication_Aordinal = mds(dissim_communication, ndim = 2, type = "ordinal")
summary(mds_communication_Aordinal)
mds_communication_Aordinal

# do MDS: CONSCIOUSNESS
mds_consciousness_Aordinal = mds(dissim_consciousness, ndim = 2, type = "ordinal")
summary(mds_consciousness_Aordinal)
mds_consciousness_Aordinal

# do MDS: DESIRE
mds_desire_Aordinal = mds(dissim_desire, ndim = 2, type = "ordinal")
summary(mds_desire_Aordinal)
mds_desire_Aordinal

# do MDS: EMBARRASSMENT
mds_embarrassment_Aordinal = mds(dissim_embarrassment, ndim = 2, type = "ordinal")
summary(mds_embarrassment_Aordinal)
mds_embarrassment_Aordinal

# do MDS: EMOTION RECOGNITION
mds_emotionrecognition_Aordinal = mds(dissim_emotionrecognition, ndim = 2, type = "ordinal")
summary(mds_emotionrecognition_Aordinal)
mds_emotionrecognition_Aordinal

# do MDS: FEAR
mds_fear_Aordinal = mds(dissim_fear, ndim = 2, type = "ordinal")
summary(mds_fear_Aordinal)
mds_fear_Aordinal

# do MDS: HUNGER
mds_hunger_Aordinal = mds(dissim_hunger, ndim = 2, type = "ordinal")
summary(mds_hunger_Aordinal)
mds_hunger_Aordinal

# do MDS: JOY
mds_joy_Aordinal = mds(dissim_joy, ndim = 2, type = "ordinal")
summary(mds_joy_Aordinal)
mds_joy_Aordinal

# do MDS: MEMORY
mds_memory_Aordinal = mds(dissim_memory, ndim = 2, type = "ordinal")
summary(mds_memory_Aordinal)
mds_memory_Aordinal

# do MDS: MORALITY
mds_morality_Aordinal = mds(dissim_morality, ndim = 2, type = "ordinal")
summary(mds_morality_Aordinal)
mds_morality_Aordinal

# do MDS: PAIN
mds_pain_Aordinal = mds(dissim_pain, ndim = 2, type = "ordinal")
summary(mds_pain_Aordinal)
mds_pain_Aordinal

# do MDS: PERSONALITY
mds_personality_Aordinal = mds(dissim_personality, ndim = 2, type = "ordinal")
summary(mds_personality_Aordinal)
mds_personality_Aordinal

# do MDS: PLANNING
mds_planning_Aordinal = mds(dissim_planning, ndim = 2, type = "ordinal")
summary(mds_planning_Aordinal)
mds_planning_Aordinal

# do MDS: PLEASURE
mds_pleasure_Aordinal = mds(dissim_pleasure, ndim = 2, type = "ordinal")
summary(mds_pleasure_Aordinal)
mds_pleasure_Aordinal

# do MDS: PRIDE
mds_pride_Aordinal = mds(dissim_pride, ndim = 2, type = "ordinal")
summary(mds_pride_Aordinal)
mds_pride_Aordinal

# do MDS: RAGE
mds_rage_Aordinal = mds(dissim_rage, ndim = 2, type = "ordinal")
summary(mds_rage_Aordinal)
mds_rage_Aordinal

# do MDS: SELF-CONTROL
mds_selfcontrol_Aordinal = mds(dissim_selfcontrol, ndim = 2, type = "ordinal")
summary(mds_selfcontrol_Aordinal)
mds_selfcontrol_Aordinal

# do MDS: THOUGHT
mds_thought_Aordinal = mds(dissim_thought, ndim = 2, type = "ordinal")
summary(mds_thought_Aordinal)
mds_thought_Aordinal

# # --------> plots -------------------------------------------------------------
# 
# # plot dimension space
# plot(mds_communication_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: COMMUNICATION",
#      xlab = "")
# plot(mds_consciousness_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: CONSCIOUSNESS",
#      xlab = "")
# plot(mds_desire_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: DESIRE",
#      xlab = "")
# plot(mds_embarrassment_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: EMBARRASSMENT",
#      xlab = "")
# plot(mds_emotionrecognition_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: EMOTION RECOGNITION",
#      xlab = "")
# plot(mds_fear_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: FEAR",
#      xlab = "")
# plot(mds_hunger_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: HUNGER",
#      xlab = "")
# plot(mds_joy_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: JOY",
#      xlab = "")
# plot(mds_memory_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: MEMORY",
#      xlab = "")
# plot(mds_morality_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: MORALITY",
#      xlab = "")
# plot(mds_pain_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: PAIN",
#      xlab = "")
# plot(mds_personality_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: PERSONALITY",
#      xlab = "")
# plot(mds_planning_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: PLANNING",
#      xlab = "")
# plot(mds_pleasure_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: PLEASURE",
#      xlab = "")
# plot(mds_pride_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: PRIDE",
#      xlab = "")
# plot(mds_rage_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: RAGE",
#      xlab = "")
# plot(mds_selfcontrol_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: SELF-CONTROL",
#      xlab = "")
# plot(mds_thought_Aordinal, 
#      plot.type = "confplot", 
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),
#      main = "MDS solution: THOUGHT",
#      xlab = "")

# --------> distances ---------------------------------------------------------

## ----------------> target: ROBOT --------------------------------------------

# distance between robot and all other characters
# ... communication
communication_dist_robot = sqrt((mds_communication_Aordinal$conf["robot","D1"] - mds_communication_Aordinal$conf[,"D1"])^2 + (mds_communication_Aordinal$conf["robot","D2"] - mds_communication_Aordinal$conf[,"D2"])^2)
# ... consciousness
consciousness_dist_robot = sqrt((mds_consciousness_Aordinal$conf["robot","D1"] - mds_consciousness_Aordinal$conf[,"D1"])^2 + (mds_consciousness_Aordinal$conf["robot","D2"] - mds_consciousness_Aordinal$conf[,"D2"])^2)
# ... desire
desire_dist_robot = sqrt((mds_desire_Aordinal$conf["robot","D1"] - mds_desire_Aordinal$conf[,"D1"])^2 + (mds_desire_Aordinal$conf["robot","D2"] - mds_desire_Aordinal$conf[,"D2"])^2)
# ... embarrassment
embarrassment_dist_robot = sqrt((mds_embarrassment_Aordinal$conf["robot","D1"] - mds_embarrassment_Aordinal$conf[,"D1"])^2 + (mds_embarrassment_Aordinal$conf["robot","D2"] - mds_embarrassment_Aordinal$conf[,"D2"])^2)
# ... emotionrecognition
emotionrecognition_dist_robot = sqrt((mds_emotionrecognition_Aordinal$conf["robot","D1"] - mds_emotionrecognition_Aordinal$conf[,"D1"])^2 + (mds_emotionrecognition_Aordinal$conf["robot","D2"] - mds_emotionrecognition_Aordinal$conf[,"D2"])^2)
# ... fear
fear_dist_robot = sqrt((mds_fear_Aordinal$conf["robot","D1"] - mds_fear_Aordinal$conf[,"D1"])^2 + (mds_fear_Aordinal$conf["robot","D2"] - mds_fear_Aordinal$conf[,"D2"])^2)
# ... hunger
hunger_dist_robot = sqrt((mds_hunger_Aordinal$conf["robot","D1"] - mds_hunger_Aordinal$conf[,"D1"])^2 + (mds_hunger_Aordinal$conf["robot","D2"] - mds_hunger_Aordinal$conf[,"D2"])^2)
# ... joy
joy_dist_robot = sqrt((mds_joy_Aordinal$conf["robot","D1"] - mds_joy_Aordinal$conf[,"D1"])^2 + (mds_joy_Aordinal$conf["robot","D2"] - mds_joy_Aordinal$conf[,"D2"])^2)
# ... selfcontrol
selfcontrol_dist_robot = sqrt((mds_selfcontrol_Aordinal$conf["robot","D1"] - mds_selfcontrol_Aordinal$conf[,"D1"])^2 + (mds_selfcontrol_Aordinal$conf["robot","D2"] - mds_selfcontrol_Aordinal$conf[,"D2"])^2)
# ... morality
morality_dist_robot = sqrt((mds_morality_Aordinal$conf["robot","D1"] - mds_morality_Aordinal$conf[,"D1"])^2 + (mds_morality_Aordinal$conf["robot","D2"] - mds_morality_Aordinal$conf[,"D2"])^2)
# ... pain
pain_dist_robot = sqrt((mds_pain_Aordinal$conf["robot","D1"] - mds_pain_Aordinal$conf[,"D1"])^2 + (mds_pain_Aordinal$conf["robot","D2"] - mds_pain_Aordinal$conf[,"D2"])^2)
# ... personality
personality_dist_robot = sqrt((mds_personality_Aordinal$conf["robot","D1"] - mds_personality_Aordinal$conf[,"D1"])^2 + (mds_personality_Aordinal$conf["robot","D2"] - mds_personality_Aordinal$conf[,"D2"])^2)
# ... planning
planning_dist_robot = sqrt((mds_planning_Aordinal$conf["robot","D1"] - mds_planning_Aordinal$conf[,"D1"])^2 + (mds_planning_Aordinal$conf["robot","D2"] - mds_planning_Aordinal$conf[,"D2"])^2)
# ... pleasure
pleasure_dist_robot = sqrt((mds_pleasure_Aordinal$conf["robot","D1"] - mds_pleasure_Aordinal$conf[,"D1"])^2 + (mds_pleasure_Aordinal$conf["robot","D2"] - mds_pleasure_Aordinal$conf[,"D2"])^2)
# ... pride
pride_dist_robot = sqrt((mds_pride_Aordinal$conf["robot","D1"] - mds_pride_Aordinal$conf[,"D1"])^2 + (mds_pride_Aordinal$conf["robot","D2"] - mds_pride_Aordinal$conf[,"D2"])^2)
# ... rage
rage_dist_robot = sqrt((mds_rage_Aordinal$conf["robot","D1"] - mds_rage_Aordinal$conf[,"D1"])^2 + (mds_rage_Aordinal$conf["robot","D2"] - mds_rage_Aordinal$conf[,"D2"])^2)
# ... selfcontrol
selfcontrol_dist_robot = sqrt((mds_selfcontrol_Aordinal$conf["robot","D1"] - mds_selfcontrol_Aordinal$conf[,"D1"])^2 + (mds_selfcontrol_Aordinal$conf["robot","D2"] - mds_selfcontrol_Aordinal$conf[,"D2"])^2)
# ... thought
thought_dist_robot = sqrt((mds_thought_Aordinal$conf["robot","D1"] - mds_thought_Aordinal$conf[,"D1"])^2 + (mds_thought_Aordinal$conf["robot","D2"] - mds_thought_Aordinal$conf[,"D2"])^2)

dist_robot = cbind(communication_dist_robot, consciousness_dist_robot, desire_dist_robot, embarrassment_dist_robot, emotionrecognition_dist_robot, fear_dist_robot, hunger_dist_robot, joy_dist_robot, selfcontrol_dist_robot, morality_dist_robot, pain_dist_robot, personality_dist_robot, planning_dist_robot, pleasure_dist_robot, pride_dist_robot, rage_dist_robot, selfcontrol_dist_robot, thought_dist_robot)
colnames(dist_robot) = tolower(levels(d$condition))

## ----------------> target: BABY ---------------------------------------------

# distance between baby and all other characters
# ... communication
communication_dist_baby = sqrt((mds_communication_Aordinal$conf["baby","D1"] - mds_communication_Aordinal$conf[,"D1"])^2 + (mds_communication_Aordinal$conf["baby","D2"] - mds_communication_Aordinal$conf[,"D2"])^2)
# ... consciousness
consciousness_dist_baby = sqrt((mds_consciousness_Aordinal$conf["baby","D1"] - mds_consciousness_Aordinal$conf[,"D1"])^2 + (mds_consciousness_Aordinal$conf["baby","D2"] - mds_consciousness_Aordinal$conf[,"D2"])^2)
# ... desire
desire_dist_baby = sqrt((mds_desire_Aordinal$conf["baby","D1"] - mds_desire_Aordinal$conf[,"D1"])^2 + (mds_desire_Aordinal$conf["baby","D2"] - mds_desire_Aordinal$conf[,"D2"])^2)
# ... embarrassment
embarrassment_dist_baby = sqrt((mds_embarrassment_Aordinal$conf["baby","D1"] - mds_embarrassment_Aordinal$conf[,"D1"])^2 + (mds_embarrassment_Aordinal$conf["baby","D2"] - mds_embarrassment_Aordinal$conf[,"D2"])^2)
# ... emotionrecognition
emotionrecognition_dist_baby = sqrt((mds_emotionrecognition_Aordinal$conf["baby","D1"] - mds_emotionrecognition_Aordinal$conf[,"D1"])^2 + (mds_emotionrecognition_Aordinal$conf["baby","D2"] - mds_emotionrecognition_Aordinal$conf[,"D2"])^2)
# ... fear
fear_dist_baby = sqrt((mds_fear_Aordinal$conf["baby","D1"] - mds_fear_Aordinal$conf[,"D1"])^2 + (mds_fear_Aordinal$conf["baby","D2"] - mds_fear_Aordinal$conf[,"D2"])^2)
# ... hunger
hunger_dist_baby = sqrt((mds_hunger_Aordinal$conf["baby","D1"] - mds_hunger_Aordinal$conf[,"D1"])^2 + (mds_hunger_Aordinal$conf["baby","D2"] - mds_hunger_Aordinal$conf[,"D2"])^2)
# ... joy
joy_dist_baby = sqrt((mds_joy_Aordinal$conf["baby","D1"] - mds_joy_Aordinal$conf[,"D1"])^2 + (mds_joy_Aordinal$conf["baby","D2"] - mds_joy_Aordinal$conf[,"D2"])^2)
# ... selfcontrol
selfcontrol_dist_baby = sqrt((mds_selfcontrol_Aordinal$conf["baby","D1"] - mds_selfcontrol_Aordinal$conf[,"D1"])^2 + (mds_selfcontrol_Aordinal$conf["baby","D2"] - mds_selfcontrol_Aordinal$conf[,"D2"])^2)
# ... morality
morality_dist_baby = sqrt((mds_morality_Aordinal$conf["baby","D1"] - mds_morality_Aordinal$conf[,"D1"])^2 + (mds_morality_Aordinal$conf["baby","D2"] - mds_morality_Aordinal$conf[,"D2"])^2)
# ... pain
pain_dist_baby = sqrt((mds_pain_Aordinal$conf["baby","D1"] - mds_pain_Aordinal$conf[,"D1"])^2 + (mds_pain_Aordinal$conf["baby","D2"] - mds_pain_Aordinal$conf[,"D2"])^2)
# ... personality
personality_dist_baby = sqrt((mds_personality_Aordinal$conf["baby","D1"] - mds_personality_Aordinal$conf[,"D1"])^2 + (mds_personality_Aordinal$conf["baby","D2"] - mds_personality_Aordinal$conf[,"D2"])^2)
# ... planning
planning_dist_baby = sqrt((mds_planning_Aordinal$conf["baby","D1"] - mds_planning_Aordinal$conf[,"D1"])^2 + (mds_planning_Aordinal$conf["baby","D2"] - mds_planning_Aordinal$conf[,"D2"])^2)
# ... pleasure
pleasure_dist_baby = sqrt((mds_pleasure_Aordinal$conf["baby","D1"] - mds_pleasure_Aordinal$conf[,"D1"])^2 + (mds_pleasure_Aordinal$conf["baby","D2"] - mds_pleasure_Aordinal$conf[,"D2"])^2)
# ... pride
pride_dist_baby = sqrt((mds_pride_Aordinal$conf["baby","D1"] - mds_pride_Aordinal$conf[,"D1"])^2 + (mds_pride_Aordinal$conf["baby","D2"] - mds_pride_Aordinal$conf[,"D2"])^2)
# ... rage
rage_dist_baby = sqrt((mds_rage_Aordinal$conf["baby","D1"] - mds_rage_Aordinal$conf[,"D1"])^2 + (mds_rage_Aordinal$conf["baby","D2"] - mds_rage_Aordinal$conf[,"D2"])^2)
# ... selfcontrol
selfcontrol_dist_baby = sqrt((mds_selfcontrol_Aordinal$conf["baby","D1"] - mds_selfcontrol_Aordinal$conf[,"D1"])^2 + (mds_selfcontrol_Aordinal$conf["baby","D2"] - mds_selfcontrol_Aordinal$conf[,"D2"])^2)
# ... thought
thought_dist_baby = sqrt((mds_thought_Aordinal$conf["baby","D1"] - mds_thought_Aordinal$conf[,"D1"])^2 + (mds_thought_Aordinal$conf["baby","D2"] - mds_thought_Aordinal$conf[,"D2"])^2)

dist_baby = cbind(communication_dist_baby, consciousness_dist_baby, desire_dist_baby, embarrassment_dist_baby, emotionrecognition_dist_baby, fear_dist_baby, hunger_dist_baby, joy_dist_baby, selfcontrol_dist_baby, morality_dist_baby, pain_dist_baby, personality_dist_baby, planning_dist_baby, pleasure_dist_baby, pride_dist_baby, rage_dist_baby, selfcontrol_dist_baby, thought_dist_baby)
colnames(dist_baby) = tolower(levels(d$condition))

## ----------------> target: PVS MAN ------------------------------------------

# distance between PVS man and all other characters
# ... communication
communication_dist_PVS = sqrt((mds_communication_Aordinal$conf["PVS man","D1"] - mds_communication_Aordinal$conf[,"D1"])^2 + (mds_communication_Aordinal$conf["PVS man","D2"] - mds_communication_Aordinal$conf[,"D2"])^2)
# ... consciousness
consciousness_dist_PVS = sqrt((mds_consciousness_Aordinal$conf["PVS man","D1"] - mds_consciousness_Aordinal$conf[,"D1"])^2 + (mds_consciousness_Aordinal$conf["PVS man","D2"] - mds_consciousness_Aordinal$conf[,"D2"])^2)
# ... desire
desire_dist_PVS = sqrt((mds_desire_Aordinal$conf["PVS man","D1"] - mds_desire_Aordinal$conf[,"D1"])^2 + (mds_desire_Aordinal$conf["PVS man","D2"] - mds_desire_Aordinal$conf[,"D2"])^2)
# ... embarrassment
embarrassment_dist_PVS = sqrt((mds_embarrassment_Aordinal$conf["PVS man","D1"] - mds_embarrassment_Aordinal$conf[,"D1"])^2 + (mds_embarrassment_Aordinal$conf["PVS man","D2"] - mds_embarrassment_Aordinal$conf[,"D2"])^2)
# ... emotionrecognition
emotionrecognition_dist_PVS = sqrt((mds_emotionrecognition_Aordinal$conf["PVS man","D1"] - mds_emotionrecognition_Aordinal$conf[,"D1"])^2 + (mds_emotionrecognition_Aordinal$conf["PVS man","D2"] - mds_emotionrecognition_Aordinal$conf[,"D2"])^2)
# ... fear
fear_dist_PVS = sqrt((mds_fear_Aordinal$conf["PVS man","D1"] - mds_fear_Aordinal$conf[,"D1"])^2 + (mds_fear_Aordinal$conf["PVS man","D2"] - mds_fear_Aordinal$conf[,"D2"])^2)
# ... hunger
hunger_dist_PVS = sqrt((mds_hunger_Aordinal$conf["PVS man","D1"] - mds_hunger_Aordinal$conf[,"D1"])^2 + (mds_hunger_Aordinal$conf["PVS man","D2"] - mds_hunger_Aordinal$conf[,"D2"])^2)
# ... joy
joy_dist_PVS = sqrt((mds_joy_Aordinal$conf["PVS man","D1"] - mds_joy_Aordinal$conf[,"D1"])^2 + (mds_joy_Aordinal$conf["PVS man","D2"] - mds_joy_Aordinal$conf[,"D2"])^2)
# ... selfcontrol
selfcontrol_dist_PVS = sqrt((mds_selfcontrol_Aordinal$conf["PVS man","D1"] - mds_selfcontrol_Aordinal$conf[,"D1"])^2 + (mds_selfcontrol_Aordinal$conf["PVS man","D2"] - mds_selfcontrol_Aordinal$conf[,"D2"])^2)
# ... morality
morality_dist_PVS = sqrt((mds_morality_Aordinal$conf["PVS man","D1"] - mds_morality_Aordinal$conf[,"D1"])^2 + (mds_morality_Aordinal$conf["PVS man","D2"] - mds_morality_Aordinal$conf[,"D2"])^2)
# ... pain
pain_dist_PVS = sqrt((mds_pain_Aordinal$conf["PVS man","D1"] - mds_pain_Aordinal$conf[,"D1"])^2 + (mds_pain_Aordinal$conf["PVS man","D2"] - mds_pain_Aordinal$conf[,"D2"])^2)
# ... personality
personality_dist_PVS = sqrt((mds_personality_Aordinal$conf["PVS man","D1"] - mds_personality_Aordinal$conf[,"D1"])^2 + (mds_personality_Aordinal$conf["PVS man","D2"] - mds_personality_Aordinal$conf[,"D2"])^2)
# ... planning
planning_dist_PVS = sqrt((mds_planning_Aordinal$conf["PVS man","D1"] - mds_planning_Aordinal$conf[,"D1"])^2 + (mds_planning_Aordinal$conf["PVS man","D2"] - mds_planning_Aordinal$conf[,"D2"])^2)
# ... pleasure
pleasure_dist_PVS = sqrt((mds_pleasure_Aordinal$conf["PVS man","D1"] - mds_pleasure_Aordinal$conf[,"D1"])^2 + (mds_pleasure_Aordinal$conf["PVS man","D2"] - mds_pleasure_Aordinal$conf[,"D2"])^2)
# ... pride
pride_dist_PVS = sqrt((mds_pride_Aordinal$conf["PVS man","D1"] - mds_pride_Aordinal$conf[,"D1"])^2 + (mds_pride_Aordinal$conf["PVS man","D2"] - mds_pride_Aordinal$conf[,"D2"])^2)
# ... rage
rage_dist_PVS = sqrt((mds_rage_Aordinal$conf["PVS man","D1"] - mds_rage_Aordinal$conf[,"D1"])^2 + (mds_rage_Aordinal$conf["PVS man","D2"] - mds_rage_Aordinal$conf[,"D2"])^2)
# ... selfcontrol
selfcontrol_dist_PVS = sqrt((mds_selfcontrol_Aordinal$conf["PVS man","D1"] - mds_selfcontrol_Aordinal$conf[,"D1"])^2 + (mds_selfcontrol_Aordinal$conf["PVS man","D2"] - mds_selfcontrol_Aordinal$conf[,"D2"])^2)
# ... thought
thought_dist_PVS = sqrt((mds_thought_Aordinal$conf["PVS man","D1"] - mds_thought_Aordinal$conf[,"D1"])^2 + (mds_thought_Aordinal$conf["PVS man","D2"] - mds_thought_Aordinal$conf[,"D2"])^2)

dist_PVS = cbind(communication_dist_PVS, consciousness_dist_PVS, desire_dist_PVS, embarrassment_dist_PVS, emotionrecognition_dist_PVS, fear_dist_PVS, hunger_dist_PVS, joy_dist_PVS, selfcontrol_dist_PVS, morality_dist_PVS, pain_dist_PVS, personality_dist_PVS, planning_dist_PVS, pleasure_dist_PVS, pride_dist_PVS, rage_dist_PVS, selfcontrol_dist_PVS, thought_dist_PVS)
colnames(dist_PVS) = tolower(levels(d$condition))

