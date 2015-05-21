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
dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid-run-01_2015-05-21_data_anonymized.csv")[-1] # get rid of column of obs numbers

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

########################################################## pairwise means #####

temp = dd %>%
  filter(phase == "test") %>%
  select(subid, predicate, leftCharacter, rightCharacter, responseNum) %>%
  mutate(
    grownup.kid = as.numeric(
      ifelse(
        leftCharacter == "grownup" & rightCharacter == "kid",
        responseNum,
        ifelse(leftCharacter == "kid" & rightCharacter == "grownup",
               -1*responseNum,
               NA))),
    grownup.baby = as.numeric(
      ifelse(
        leftCharacter == "grownup" & rightCharacter == "baby",
        responseNum,
        ifelse(leftCharacter == "baby" & rightCharacter == "grownup",
               -1*responseNum,
               NA))),
    grownup.dog = as.numeric(
      ifelse(
        leftCharacter == "grownup" & rightCharacter == "dog",
        responseNum,
        ifelse(leftCharacter == "dog" & rightCharacter == "grownup",
               -1*responseNum,
               NA))),
    grownup.bear = as.numeric(
      ifelse(
        leftCharacter == "grownup" & rightCharacter == "bear",
        responseNum,
        ifelse(leftCharacter == "bear" & rightCharacter == "grownup",
               -1*responseNum,
               NA))),
    grownup.bug = as.numeric(
      ifelse(
        leftCharacter == "grownup" & rightCharacter == "bug",
        responseNum,
        ifelse(leftCharacter == "bug" & rightCharacter == "grownup",
               -1*responseNum,
               NA))),
    grownup.robot = as.numeric(
      ifelse(
        leftCharacter == "grownup" & rightCharacter == "robot",
        responseNum,
        ifelse(leftCharacter == "robot" & rightCharacter == "grownup",
               -1*responseNum,
               NA))),
    grownup.computer = as.numeric(
      ifelse(
        leftCharacter == "grownup" & rightCharacter == "computer",
        responseNum,
        ifelse(leftCharacter == "computer" & rightCharacter == "grownup",
               -1*responseNum,
               NA))),
    grownup.car = as.numeric(
      ifelse(
        leftCharacter == "grownup" & rightCharacter == "car",
        responseNum,
        ifelse(leftCharacter == "car" & rightCharacter == "grownup",
               -1*responseNum,
               NA))),
    grownup.stapler = as.numeric(
      ifelse(
        leftCharacter == "grownup" & rightCharacter == "stapler",
        responseNum,
        ifelse(leftCharacter == "stapler" & rightCharacter == "grownup",
               -1*responseNum,
               NA))),
    kid.baby = as.numeric(
      ifelse(
        leftCharacter == "kid" & rightCharacter == "baby",
        responseNum,
        ifelse(leftCharacter == "baby" & rightCharacter == "kid",
               -1*responseNum,
               NA))),
    kid.dog = as.numeric(
      ifelse(
        leftCharacter == "kid" & rightCharacter == "dog",
        responseNum,
        ifelse(leftCharacter == "dog" & rightCharacter == "kid",
               -1*responseNum,
               NA))),
    kid.bear = as.numeric(
      ifelse(
        leftCharacter == "kid" & rightCharacter == "bear",
        responseNum,
        ifelse(leftCharacter == "bear" & rightCharacter == "kid",
               -1*responseNum,
               NA))),
    kid.bug = as.numeric(
      ifelse(
        leftCharacter == "kid" & rightCharacter == "bug",
        responseNum,
        ifelse(leftCharacter == "bug" & rightCharacter == "kid",
               -1*responseNum,
               NA))),
    kid.robot = as.numeric(
      ifelse(
        leftCharacter == "kid" & rightCharacter == "robot",
        responseNum,
        ifelse(leftCharacter == "robot" & rightCharacter == "kid",
               -1*responseNum,
               NA))),
    kid.computer = as.numeric(
      ifelse(
        leftCharacter == "kid" & rightCharacter == "computer",
        responseNum,
        ifelse(leftCharacter == "computer" & rightCharacter == "kid",
               -1*responseNum,
               NA))),
    kid.car = as.numeric(
      ifelse(
        leftCharacter == "kid" & rightCharacter == "car",
        responseNum,
        ifelse(leftCharacter == "car" & rightCharacter == "kid",
               -1*responseNum,
               NA))),
    kid.stapler = as.numeric(
      ifelse(
        leftCharacter == "kid" & rightCharacter == "stapler",
        responseNum,
        ifelse(leftCharacter == "stapler" & rightCharacter == "kid",
               -1*responseNum,
               NA))),
    baby.dog = as.numeric(
      ifelse(
        leftCharacter == "baby" & rightCharacter == "dog",
        responseNum,
        ifelse(leftCharacter == "dog" & rightCharacter == "baby",
               -1*responseNum,
               NA))),
    baby.bear = as.numeric(
      ifelse(
        leftCharacter == "baby" & rightCharacter == "bear",
        responseNum,
        ifelse(leftCharacter == "bear" & rightCharacter == "baby",
               -1*responseNum,
               NA))),
    baby.bug = as.numeric(
      ifelse(
        leftCharacter == "baby" & rightCharacter == "bug",
        responseNum,
        ifelse(leftCharacter == "bug" & rightCharacter == "baby",
               -1*responseNum,
               NA))),
    baby.robot = as.numeric(
      ifelse(
        leftCharacter == "baby" & rightCharacter == "robot",
        responseNum,
        ifelse(leftCharacter == "robot" & rightCharacter == "baby",
               -1*responseNum,
               NA))),
    baby.computer = as.numeric(
      ifelse(
        leftCharacter == "baby" & rightCharacter == "computer",
        responseNum,
        ifelse(leftCharacter == "computer" & rightCharacter == "baby",
               -1*responseNum,
               NA))),
    baby.car = as.numeric(
      ifelse(
        leftCharacter == "baby" & rightCharacter == "car",
        responseNum,
        ifelse(leftCharacter == "car" & rightCharacter == "baby",
               -1*responseNum,
               NA))),
    baby.stapler = as.numeric(
      ifelse(
        leftCharacter == "baby" & rightCharacter == "stapler",
        responseNum,
        ifelse(leftCharacter == "stapler" & rightCharacter == "baby",
               -1*responseNum,
               NA))),  
    dog.bear = as.numeric(
      ifelse(
        leftCharacter == "dog" & rightCharacter == "bear",
        responseNum,
        ifelse(leftCharacter == "bear" & rightCharacter == "dog",
               -1*responseNum,
               NA))),
    dog.bug = as.numeric(
      ifelse(
        leftCharacter == "dog" & rightCharacter == "bug",
        responseNum,
        ifelse(leftCharacter == "bug" & rightCharacter == "dog",
               -1*responseNum,
               NA))),
    dog.robot = as.numeric(
      ifelse(
        leftCharacter == "dog" & rightCharacter == "robot",
        responseNum,
        ifelse(leftCharacter == "robot" & rightCharacter == "dog",
               -1*responseNum,
               NA))),
    dog.computer = as.numeric(
      ifelse(
        leftCharacter == "dog" & rightCharacter == "computer",
        responseNum,
        ifelse(leftCharacter == "computer" & rightCharacter == "dog",
               -1*responseNum,
               NA))),
    dog.car = as.numeric(
      ifelse(
        leftCharacter == "dog" & rightCharacter == "car",
        responseNum,
        ifelse(leftCharacter == "car" & rightCharacter == "dog",
               -1*responseNum,
               NA))),
    dog.stapler = as.numeric(
      ifelse(
        leftCharacter == "dog" & rightCharacter == "stapler",
        responseNum,
        ifelse(leftCharacter == "stapler" & rightCharacter == "dog",
               -1*responseNum,
               NA))),     
    bear.bug = as.numeric(
      ifelse(
        leftCharacter == "bear" & rightCharacter == "bug",
        responseNum,
        ifelse(leftCharacter == "bug" & rightCharacter == "bear",
               -1*responseNum,
               NA))),
    bear.robot = as.numeric(
      ifelse(
        leftCharacter == "bear" & rightCharacter == "robot",
        responseNum,
        ifelse(leftCharacter == "robot" & rightCharacter == "bear",
               -1*responseNum,
               NA))),
    bear.computer = as.numeric(
      ifelse(
        leftCharacter == "bear" & rightCharacter == "computer",
        responseNum,
        ifelse(leftCharacter == "computer" & rightCharacter == "bear",
               -1*responseNum,
               NA))),
    bear.car = as.numeric(
      ifelse(
        leftCharacter == "bear" & rightCharacter == "car",
        responseNum,
        ifelse(leftCharacter == "car" & rightCharacter == "bear",
               -1*responseNum,
               NA))),
    bear.stapler = as.numeric(
      ifelse(
        leftCharacter == "bear" & rightCharacter == "stapler",
        responseNum,
        ifelse(leftCharacter == "stapler" & rightCharacter == "bear",
               -1*responseNum,
               NA))),      
    bug.robot = as.numeric(
      ifelse(
        leftCharacter == "bug" & rightCharacter == "robot",
        responseNum,
        ifelse(leftCharacter == "robot" & rightCharacter == "bug",
               -1*responseNum,
               NA))),
    bug.computer = as.numeric(
      ifelse(
        leftCharacter == "bug" & rightCharacter == "computer",
        responseNum,
        ifelse(leftCharacter == "computer" & rightCharacter == "bug",
               -1*responseNum,
               NA))),
    bug.car = as.numeric(
      ifelse(
        leftCharacter == "bug" & rightCharacter == "car",
        responseNum,
        ifelse(leftCharacter == "car" & rightCharacter == "bug",
               -1*responseNum,
               NA))),
    bug.stapler = as.numeric(
      ifelse(
        leftCharacter == "bug" & rightCharacter == "stapler",
        responseNum,
        ifelse(leftCharacter == "stapler" & rightCharacter == "bug",
               -1*responseNum,
               NA))),   
    robot.computer = as.numeric(
      ifelse(
        leftCharacter == "robot" & rightCharacter == "computer",
        responseNum,
        ifelse(leftCharacter == "computer" & rightCharacter == "robot",
               -1*responseNum,
               NA))),
    robot.car = as.numeric(
      ifelse(
        leftCharacter == "robot" & rightCharacter == "car",
        responseNum,
        ifelse(leftCharacter == "car" & rightCharacter == "robot",
               -1*responseNum,
               NA))),
    robot.stapler = as.numeric(
      ifelse(
        leftCharacter == "robot" & rightCharacter == "stapler",
        responseNum,
        ifelse(leftCharacter == "stapler" & rightCharacter == "robot",
               -1*responseNum,
               NA))),   
    computer.car = as.numeric(
      ifelse(
        leftCharacter == "computer" & rightCharacter == "car",
        responseNum,
        ifelse(leftCharacter == "car" & rightCharacter == "computer",
               -1*responseNum,
               NA))),
    computer.stapler = as.numeric(
      ifelse(
        leftCharacter == "computer" & rightCharacter == "stapler",
        responseNum,
        ifelse(leftCharacter == "stapler" & rightCharacter == "computer",
               -1*responseNum,
               NA))),   
    car.stapler = as.numeric(
      ifelse(
        leftCharacter == "car" & rightCharacter == "stapler",
        responseNum,
        ifelse(leftCharacter == "stapler" & rightCharacter == "car",
               -1*responseNum,
               NA)))) %>%
  group_by(predicate)
#   %>% group_by(subid)
  
fine.summary = temp %>%
  summarise(avg.grownup.kid = mean(grownup.kid, na.rm = T),
            avg.grownup.baby = mean(grownup.baby, na.rm = T),
            avg.grownup.dog = mean(grownup.dog, na.rm = T),
            avg.grownup.bear = mean(grownup.bear, na.rm = T),
            avg.grownup.bug = mean(grownup.bug, na.rm = T),
            avg.grownup.robot = mean(grownup.robot, na.rm = T),
            avg.grownup.computer = mean(grownup.computer, na.rm = T),
            avg.grownup.car = mean(grownup.car, na.rm = T),
            avg.grownup.stapler = mean(grownup.stapler, na.rm = T),
            avg.kid.baby = mean(kid.baby, na.rm = T),
            avg.kid.dog = mean(kid.dog, na.rm = T),
            avg.kid.bear = mean(kid.bear, na.rm = T),
            avg.kid.bug = mean(kid.bug, na.rm = T),
            avg.kid.robot = mean(kid.robot, na.rm = T),
            avg.kid.computer = mean(kid.computer, na.rm = T),
            avg.kid.car = mean(kid.car, na.rm = T),
            avg.kid.stapler = mean(kid.stapler, na.rm = T),
            avg.baby.dog = mean(baby.dog, na.rm = T),
            avg.baby.bear = mean(baby.bear, na.rm = T),
            avg.baby.bug = mean(baby.bug, na.rm = T),
            avg.baby.robot = mean(baby.robot, na.rm = T),
            avg.baby.computer = mean(baby.computer, na.rm = T),
            avg.baby.car = mean(baby.car, na.rm = T),
            avg.baby.stapler = mean(baby.stapler, na.rm = T),
            avg.dog.bear = mean(dog.bear, na.rm = T),
            avg.dog.bug = mean(dog.bug, na.rm = T),
            avg.dog.robot = mean(dog.robot, na.rm = T),
            avg.dog.computer = mean(dog.computer, na.rm = T),
            avg.dog.car = mean(dog.car, na.rm = T),
            avg.dog.stapler = mean(dog.stapler, na.rm = T),
            avg.bear.bug = mean(bear.bug, na.rm = T),
            avg.bear.robot = mean(bear.robot, na.rm = T),
            avg.bear.computer = mean(bear.computer, na.rm = T),
            avg.bear.car = mean(bear.car, na.rm = T),
            avg.bear.stapler = mean(bear.stapler, na.rm = T),
            avg.bug.robot = mean(bug.robot, na.rm = T),
            avg.bug.computer = mean(bug.computer, na.rm = T),
            avg.bug.car = mean(bug.car, na.rm = T),
            avg.bug.stapler = mean(bug.stapler, na.rm = T),
            avg.robot.computer = mean(robot.computer, na.rm = T),
            avg.robot.car = mean(robot.car, na.rm = T),
            avg.robot.stapler = mean(robot.stapler, na.rm = T),
            avg.computer.car = mean(computer.car, na.rm = T),
            avg.computer.stapler = mean(computer.stapler, na.rm = T),
            avg.car.stapler = mean(car.stapler, na.rm = T)) %>%
  gather(comparison, average, -predicate) %>%
  spread(predicate, average)

temp2 = fine.summary %>%
  mutate(
    coarse = factor(
      # is there a human?
      ifelse(grepl("grownup", comparison) == TRUE |
               grepl("kid", comparison) == TRUE |
               grepl("baby", comparison) == TRUE,
             # yes, there is a human.
             # is there another human?
             ifelse(grepl("grownup.kid", comparison) == TRUE |
                      grepl("grownup.baby", comparison) == TRUE |
                      grepl("kid.baby", comparison) == TRUE,
                    # yes, there is another human
                    "human.human",
                    # no, there is not another human
                    # is there an animal?
                    ifelse(grepl("dog", comparison) == TRUE |
                             grepl("bear", comparison) == TRUE |
                             grepl("bug", comparison) == TRUE,
                           # yes, there is an animal
                           "human.animal",
                           # no, there is not an animal
                           # is there tech?
                           ifelse(grepl("robot", comparison) == TRUE |
                                    grepl("computer", comparison) == TRUE |
                                    grepl("car", comparison) == TRUE,
                                  # yes, there is tech
                                  "human.tech",
                                  # no, there is not tech
                                  # is there a stapler?
                                  ifelse(grepl("stapler", comparison) == TRUE,
                                         # yes, there is a stapler
                                         "human.stapler",
                                         # no, there is not a stapler
                                         NA)))),
             # no, there is not a human.
             # is there an animal?
             ifelse(grepl("dog", comparison) == TRUE |
                      grepl("bear", comparison) == TRUE |
                      grepl("bug", comparison) == TRUE,
                    # yes, there is an animal
                    # is there another animal?
                    ifelse(grepl("dog.bear", comparison) == TRUE |
                             grepl("dog.bug", comparison) == TRUE |
                             grepl("bear.bug", comparison) == TRUE,
                           # yes, there is another animal
                           "animal.animal",
                           # no, there is not another animal
                           # is there technology?
                           ifelse(grepl("robot", comparison) == TRUE |
                                    grepl("computer", comparison) == TRUE |
                                    grepl("car", comparison) == TRUE,
                                  # yes, there is technology
                                  "animal.technology",
                                  # no, there is not technology
                                  # is there a stapler?
                                  ifelse(grepl("stapler", comparison) == TRUE,
                                         # yes, there is a stapler
                                         "animal.stapler",
                                         NA))),
                    # no, there is not an animal
                    # is there technology?
                    ifelse(grepl("robot", comparison) == TRUE |
                             grepl("computer", comparison) == TRUE |
                             grepl("car", comparison) == TRUE,
                           # yes, there is technology
                           # is there another technology?
                           ifelse(grepl("robot.computer", comparison) == TRUE |
                                    grepl("robot.car", comparison) == TRUE |
                                    grepl("computer.car", comparison) == TRUE,
                                  # yes, there is another technology
                                  "tech.tech",
                                  # no, there is not another technology
                                  # is there a stapler?
                                  ifelse(grepl("stapler", comparison) == TRUE,
                                         # yes, there is a stapler
                                         "tech.stapler",
                                         # no, there is not a stapler
                                         NA)),
                           # no, there is not technology
                           NA)
                    )
             )
      )
    )

coarse.summary = temp2 %>%
  group_by(coarse) %>%
  summarise(avg.feelings = mean(feelings, na.rm = T),
            avg.hunger = mean(hunger, na.rm = T),
            avg.thinking = mean(thinking, na.rm = T))

