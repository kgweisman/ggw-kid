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
library(abind)
library(langcog)
library(grid)

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
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01&02_2016-02-02_data_anonymized.csv")[-1] # get rid of column of obs numbers
# 
# # ... RUN01
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01_2015-06-13_data_anonymized.csv")[-1] # get rid of column of obs numbers

# ... RUN02
dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-02_2016-02-02_data_anonymized.csv")[-1] # get rid of column of obs numbers

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
dd_adults = dd_adults_us

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

# set age limit
lowerLim <- 4.45
upperLim <- 5.55

dd_children_exact = dd_children %>%
  filter(ageCalc >= lowerLim & ageCalc <= upperLim)

dd_children_older = dd_children %>%
  filter(ageCalc >= upperLim)

# set group of interest
# ... to exact:
dd_children = dd_children_exact

# # to older
# dd_children = dd_children_older

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
qplot(subset(demo, ageGroup == "children")$ageCalc, binwidth = 1/12,
      xlab = "\nAge (years)", ylab = "Count\n") +
  scale_x_continuous(breaks = seq(4,6,.25)) +
  theme(text = element_text(size = 20))
# qplot(subset(demo, ageGroup == "adults")$ageCalc)

# demo %>% group_by(ageGroup) %>% filter(age < 100) %>% summarise(mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm = T))
# qplot(subset(demo, ageGroup == "children")$age[demo$age < 100])
# qplot(subset(demo, ageGroup == "adults")$age[demo$age < 100])

# age by condition
demo %>% group_by(ageGroup, sequence) %>% summarise(mean_age = round(mean(ageCalc, na.rm = T), 2), sd_age = round(sd(ageCalc, na.rm = T), 2))

######################################################## analysis & plots #####

# --- choice frequencies data-formatting function -----------------------------

# make dataframe of absolute choice frequencies
# count number of times each row character is chosen over each column stimulus
makeM <- function(selectPredicate = c("hunger", "feelings", "thinking"),
                  selectAgeGroup = c("adults", "children")) {
  tempM <- NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  tempM <- dd %>%
    filter(predicate %in% selectPredicate &
             ageGroup %in% selectAgeGroup) %>%
    mutate(character1 = array(),
           character2 = array(),
           leftCharacter = factor(leftCharacter),
           rightCharacter = factor(rightCharacter))
  
  charsort = sort(levels(tempM$leftCharacter), decreasing = TRUE)
  
  for(i in 1:length(charsort)) {
    tempM <- tempM %>%
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
  
  # count number of times each character is chosen over every other character
  tempM <- tempM %>%
    select(predicate, subid, leftCharacter, rightCharacter, 
           character1, character2, responseNum) %>%
    mutate(chooseChar = ifelse(responseNum == 0, NA,
                               ifelse((as.character(character1) == as.character(leftCharacter) 
                                       & responseNum < 0) |
                                        (as.character(character1) == as.character(rightCharacter) 
                                         & responseNum > 0), "char1", "char2"))) %>%
    select(-leftCharacter, -rightCharacter, -responseNum)
  
  # name rows and columns with character names
  characterNames <- with(subset(dd, phase == "test"), levels(factor(leftCharacter)))
  
  # create empty dataframe
  tempDf <- data.frame(matrix(0, nrow = length(characterNames), ncol = length(characterNames)))
  names(tempDf) = characterNames
  rownames(tempDf) = characterNames
  
  # fill in frequency data: choose row character over column character
  for (i in 1:length(characterNames)) {
    char1 <- characterNames[i]
    
    for (j in 1:length(characterNames)) {
      char2 <- characterNames[j]
      
      countA <- tempM %>%
        filter(character1 == char1 & character2 == char2 & chooseChar == "char1") %>%
        count()
      
      countB <- tempM %>%
        filter(character1 == char1 & character2 == char2 & chooseChar == "char2") %>%
        count()
      
      tempDf[char1, char2] <- tempDf[char1, char2] + countA
      tempDf[char2, char1] <- tempDf[char2, char1] + countB
    }
  }
  
  # set diagonal to NA
  diag(tempDf) <- NA
  
  return(tempDf)
  
}

# --- ELIMINATION-BY-ASPECTS (EBA) ANALYSIS -----------------------------------

# GENERAL PARAMETERS

# aspects for EBA analyses
# ... option 1: living, human, technology, individuals
aspects13a <- list(baby = c(1, 11, 12),
                  bear = c(2, 12),
                  bug = c(3, 12),
                  car = c(4, 13),
                  computer = c(5, 13),
                  dog = c(6, 12),
                  grownup = c(7, 11, 12),
                  kid = c(8, 11, 12),
                  robot = c(9, 13),
                  stapler = c(10))

# ... option 2: human, animal, technology, individuals
aspects13b <- list(baby = c(1, 11),
                  bear = c(2, 12),
                  bug = c(3, 12),
                  car = c(4, 13),
                  computer = c(5, 13),
                  dog = c(6, 12),
                  grownup = c(7, 11),
                  kid = c(8, 11),
                  robot = c(9, 13),
                  stapler = c(10))

# ... option 3: living, individuals
aspects11 <- list(baby = c(1, 11),
                  bear = c(2, 11),
                  bug = c(3, 11),
                  car = c(4),
                  computer = c(5),
                  dog = c(6, 11),
                  grownup = c(7, 11),
                  kid = c(8, 11),
                  robot = c(9),
                  stapler = c(10))

# ... option 4: individuals
aspects10 <- list(baby = c(1),
                bear = c(2),
                bug = c(3),
                car = c(4),
                computer = c(5),
                dog = c(6),
                grownup = c(7),
                kid = c(8),
                robot = c(9),
                stapler = c(10))

# set option
# aspects <- aspects13a
# aspects <- aspects13b
# aspects <- aspects11
aspects <- aspects10

# set continuity correction
# continuityCorr <- 1
continuityCorr <- 0

# xlim for EBA plots
upperLim <- 0.4

# --------> adults ------------------------------------------------------------

# --------------->-> all predicates -------------------------------------------

M_adults_all <- makeM(selectAgeGroup = "adults") + continuityCorr # continuity correction

eba_adults_all <- OptiPt(M_adults_all,
                         A = aspects); summary(eba_adults_all)

eba_adults_all_df_0 <- data.frame(eba_adults_all$u) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(ifelse(character %in% c("grownup", "kid", "baby"), 
                                  "human",
                                  ifelse(character %in% c("dog", "bear", "bug"), 
                                         "animal",
                                         ifelse(character %in% c("robot", "computer", "car"), 
                                                
                                                "technology",
                                                "control"))),
                           levels = c("human", "animal", "technology", "control")),
         character = factor(character,
                            levels = c("grownup", "kid", "baby",
                                       "dog", "bear", "bug",
                                       "robot", "computer", "car", "stapler")),
         order = ifelse(character == "grownup", 10,
                        ifelse(character == "kid", 9,
                               ifelse(character == "baby", 8,
                                      ifelse(character == "dog", 7,
                                             ifelse(character == "bear", 6,
                                                    ifelse(character == "bug", 5,
                                                           ifelse(character == "robot", 4,
                                                                  ifelse(character == "computer", 3,
                                                                         ifelse(character == "car", 2,
                                                                                ifelse(character == "stapler", 1, NA)))))))))))

# eba_adults_all_df <- multi_boot.data.frame(data = eba_adults_all_df_0,
#                                               summary_function = "mean",
#                                               column = "eba_adults_all.u",
#                                               summary_groups = c("category", 
#                                                                  "character",
#                                                                  "order"),
#                                               statistics_functions = c("ci_lower", 
#                                                                        "mean", "ci_upper")) %>%
#   mutate(ci_upper_new = as.numeric(ifelse(mean + ci_upper > upperLim, upperLim - mean, NA)))
# 
# ggplot(aes(y = reorder(character, order), 
#            x = mean,
#            colour = category), 
#        data = eba_adults_all_df) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         axis.title = element_blank(),
#         legend.position = "none",
#         panel.border = element_rect(size = 2),
#         panel.grid.major.y = element_line(size = 1)) +
#   geom_point(size = 5) +
#   labs(title = "EBA Solution: All predicates\n") +
#   geom_errorbarh(aes(xmin = mean - ci_lower,
#                      xmax = mean + ci_upper,
#                      height = 0.1)) +
#   xlim(c(0, upperLim)) +
# #   geom_segment(aes(y = character, yend = character, 
# #                    x = mean - ci_lower, xend = mean + ci_upper_new), 
# #                arrow = arrow(length = unit(0.1, "inches"))) +
# #   geom_text(aes(y = character, vjust = 2,
# #                 x = 2*upperLim - (mean + ci_upper_new), hjust = 1,
# #                 label = paste("upper limit:", round(mean + ci_upper, 2)))) +
#   scale_colour_brewer(type = "qual", palette = 6)

# --------------->-> HUNGER ---------------------------------------------------

M_adults_hunger <- makeM(selectAgeGroup = "adults", 
                      selectPredicate = "hunger") + 1 # add one for continuity correction

eba_adults_hunger <- OptiPt(M_adults_hunger,
                         A = aspects); summary(eba_adults_hunger)

eba_adults_hunger_df_0 <- data.frame("utility" = uscale(eba_adults_hunger)) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(ifelse(character %in% c("grownup", "kid", "baby"), 
                                  "human",
                                  ifelse(character %in% c("dog", "bear", "bug"), 
                                         "animal",
                                         ifelse(character %in% c("robot", "computer", "car"), 
                                                
                                                "technology",
                                                "control"))),
                           levels = c("human", "animal", "technology", "control")),
         character = factor(character,
                            levels = c("grownup", "kid", "baby",
                                       "dog", "bear", "bug",
                                       "robot", "computer", "car", "stapler")),
         order = ifelse(character == "grownup", 10,
                        ifelse(character == "kid", 9,
                               ifelse(character == "baby", 8,
                                      ifelse(character == "dog", 7,
                                             ifelse(character == "bear", 6,
                                                    ifelse(character == "bug", 5,
                                                           ifelse(character == "robot", 4,
                                                                  ifelse(character == "computer", 3,
                                                                         ifelse(character == "car", 2,
                                                                                ifelse(character == "stapler", 1, NA)))))))))))

ci_adults_hunger <- data.frame("me" = 1.96 * sqrt(diag(cov.u(eba_adults_hunger)))) %>%
  add_rownames(var = "character")

eba_adults_hunger_df <- eba_adults_hunger_df_0 %>%
  full_join(ci_adults_hunger) %>%
  mutate(ci_lower = utility - me,
         ci_upper = utility + me,
         ci_lower_new = as.numeric(ifelse(ci_lower < 0, 0, ci_lower)), # hacky :(
         ci_upper_new = as.numeric(ifelse(ci_upper > upperLim, upperLim, NA)))

ggplot(aes(y = reorder(character, order), 
           x = utility,
           colour = category), 
       data = eba_adults_hunger_df) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2),
        panel.grid.major.y = element_line(size = 1)) +
  geom_point(size = 5) +
  labs(title = "Adults: Hunger (EBA)\n",
       x = "\nUtility scale value") +
  geom_errorbarh(aes(xmin = ci_lower_new,
                     xmax = ci_upper,
                     height = 0.1)) +
  xlim(c(0, upperLim)) +
#   geom_segment(aes(y = character, yend = character, 
#                    x = ci_lower_new, xend = ci_upper_new), 
#                arrow = arrow(length = unit(0.1, "inches"))) +
#   geom_text(aes(y = character, vjust = 2,
#                 x = 2*upperLim - (ci_upper_new), hjust = 1,
#                 label = paste("upper limit:", round(ci_upper, 2)))) +
  geom_vline(aes(xintercept = 0.1), lty = 2) +
  scale_colour_brewer(type = "qual", palette = 6)

# --------------->-> FEELINGS ---------------------------------------------------

M_adults_feelings <- makeM(selectAgeGroup = "adults", 
                         selectPredicate = "feelings") + 1 # add one for continuity correction

eba_adults_feelings <- OptiPt(M_adults_feelings,
                            A = aspects); summary(eba_adults_feelings)

eba_adults_feelings_df_0 <- data.frame("utility" = uscale(eba_adults_feelings)) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(ifelse(character %in% c("grownup", "kid", "baby"), 
                                  "human",
                                  ifelse(character %in% c("dog", "bear", "bug"), 
                                         "animal",
                                         ifelse(character %in% c("robot", "computer", "car"), 
                                                
                                                "technology",
                                                "control"))),
                           levels = c("human", "animal", "technology", "control")),
         character = factor(character,
                            levels = c("grownup", "kid", "baby",
                                       "dog", "bear", "bug",
                                       "robot", "computer", "car", "stapler")),
         order = ifelse(character == "grownup", 10,
                        ifelse(character == "kid", 9,
                               ifelse(character == "baby", 8,
                                      ifelse(character == "dog", 7,
                                             ifelse(character == "bear", 6,
                                                    ifelse(character == "bug", 5,
                                                           ifelse(character == "robot", 4,
                                                                  ifelse(character == "computer", 3,
                                                                         ifelse(character == "car", 2,
                                                                                ifelse(character == "stapler", 1, NA)))))))))))

ci_adults_feelings <- data.frame("me" = 1.96 * sqrt(diag(cov.u(eba_adults_feelings)))) %>%
  add_rownames(var = "character")

eba_adults_feelings_df <- eba_adults_feelings_df_0 %>%
  full_join(ci_adults_feelings) %>%
  mutate(ci_lower = utility - me,
         ci_upper = utility + me,
         ci_lower_new = as.numeric(ifelse(ci_lower < 0, 0, ci_lower)), # hacky :(
         ci_upper_new = as.numeric(ifelse(ci_upper > upperLim, upperLim, NA)))

ggplot(aes(y = reorder(character, order), 
           x = utility,
           colour = category), 
       data = eba_adults_feelings_df) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2),
        panel.grid.major.y = element_line(size = 1)) +
  geom_point(size = 5) +
  labs(title = "Adults: Feelings (EBA)\n",
       x = "\nUtility scale value") +
  geom_errorbarh(aes(xmin = ci_lower_new,
                     xmax = ci_upper,
                     height = 0.1)) +
#   xlim(c(0, upperLim)) +
#   geom_segment(aes(y = character, yend = character, 
#                    x = ci_lower_new, xend = ci_upper_new), 
#                arrow = arrow(length = unit(0.1, "inches"))) +
#   geom_text(aes(y = character, vjust = 2,
#                 x = 2*upperLim - (ci_upper_new), hjust = 1,
#                 label = paste("upper limit:", round(ci_upper, 2)))) +
  geom_vline(aes(xintercept = 0.1), lty = 2) +
  scale_colour_brewer(type = "qual", palette = 6)

# --------------->-> THINKING ---------------------------------------------------

M_adults_thinking <- makeM(selectAgeGroup = "adults", 
                         selectPredicate = "thinking") + 1 # add one for continuity correction

eba_adults_thinking <- OptiPt(M_adults_thinking,
                            A = aspects); summary(eba_adults_thinking)

eba_adults_thinking_df_0 <- data.frame("utility" = uscale(eba_adults_thinking)) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(ifelse(character %in% c("grownup", "kid", "baby"), 
                                  "human",
                                  ifelse(character %in% c("dog", "bear", "bug"), 
                                         "animal",
                                         ifelse(character %in% c("robot", "computer", "car"), 
                                                
                                                "technology",
                                                "control"))),
                           levels = c("human", "animal", "technology", "control")),
         character = factor(character,
                            levels = c("grownup", "kid", "baby",
                                       "dog", "bear", "bug",
                                       "robot", "computer", "car", "stapler")),
         order = ifelse(character == "grownup", 10,
                        ifelse(character == "kid", 9,
                               ifelse(character == "baby", 8,
                                      ifelse(character == "dog", 7,
                                             ifelse(character == "bear", 6,
                                                    ifelse(character == "bug", 5,
                                                           ifelse(character == "robot", 4,
                                                                  ifelse(character == "computer", 3,
                                                                         ifelse(character == "car", 2,
                                                                                ifelse(character == "stapler", 1, NA)))))))))))

ci_adults_thinking <- data.frame("me" = 1.96 * sqrt(diag(cov.u(eba_adults_thinking)))) %>%
  add_rownames(var = "character")

eba_adults_thinking_df <- eba_adults_thinking_df_0 %>%
  full_join(ci_adults_thinking) %>%
  mutate(ci_lower = utility - me,
         ci_upper = utility + me,
         ci_lower_new = as.numeric(ifelse(ci_lower < 0, 0, ci_lower)), # hacky :(
         ci_upper_new = as.numeric(ifelse(ci_upper > upperLim, upperLim, NA)))

ggplot(aes(y = reorder(character, order), 
           x = utility,
           colour = category), 
       data = eba_adults_thinking_df) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2),
        panel.grid.major.y = element_line(size = 1)) +
  geom_point(size = 5) +
  labs(title = "Adults: Thinking (EBA)\n",
       x = "\nUtility scale value") +
  geom_errorbarh(aes(xmin = ci_lower_new,
                     xmax = ci_upper,
                     height = 0.1)) +
#   xlim(c(0, upperLim)) +
#   geom_segment(aes(y = character, yend = character, 
#                    x = ci_lower_new, xend = ci_upper_new), 
#                arrow = arrow(length = unit(0.1, "inches"))) +
#   geom_text(aes(y = character, vjust = 2,
#                 x = 2*upperLim - (ci_upper_new), hjust = 1,
#                 label = paste("upper limit:", round(ci_upper, 2)))) +
  geom_vline(aes(xintercept = 0.1), lty = 2) +
  scale_colour_brewer(type = "qual", palette = 6)

# --------> children ------------------------------------------------------------

# --------------->-> all predicates -------------------------------------------

M_children_all <- makeM(selectAgeGroup = "children") + continuityCorr # continuity correction

eba_children_all <- OptiPt(M_children_all,
                         A = aspects); summary(eba_children_all)

eba_children_all_df_0 <- data.frame(eba_children_all$u) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(ifelse(character %in% c("grownup", "kid", "baby"), 
                                  "human",
                                  ifelse(character %in% c("dog", "bear", "bug"), 
                                         "animal",
                                         ifelse(character %in% c("robot", "computer", "car"), 
                                                
                                                "technology",
                                                "control"))),
                           levels = c("human", "animal", "technology", "control")),
         character = factor(character,
                            levels = c("grownup", "kid", "baby",
                                       "dog", "bear", "bug",
                                       "robot", "computer", "car", "stapler")),
         order = ifelse(character == "grownup", 10,
                        ifelse(character == "kid", 9,
                               ifelse(character == "baby", 8,
                                      ifelse(character == "dog", 7,
                                             ifelse(character == "bear", 6,
                                                    ifelse(character == "bug", 5,
                                                           ifelse(character == "robot", 4,
                                                                  ifelse(character == "computer", 3,
                                                                         ifelse(character == "car", 2,
                                                                                ifelse(character == "stapler", 1, NA)))))))))))

# eba_children_all_df <- multi_boot.data.frame(data = eba_children_all_df_0,
#                                            summary_function = "mean",
#                                            column = "eba_children_all.u",
#                                            summary_groups = c("category", 
#                                                               "character",
#                                                               "order"),
#                                            statistics_functions = c("ci_lower", 
#                                                                     "mean", "ci_upper")) %>%
#   mutate(ci_upper_new = as.numeric(ifelse(mean + ci_upper > upperLim, upperLim - mean, NA)))
# 
# ggplot(aes(y = reorder(character, order), 
#            x = mean,
#            colour = category), 
#        data = eba_children_all_df) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         axis.title = element_blank(),
#         legend.position = "none",
#         panel.border = element_rect(size = 2),
#         panel.grid.major.y = element_line(size = 1)) +
#   geom_point(size = 5) +
#   labs(title = "EBA Solution: All predicates\n") +
#   geom_errorbarh(aes(xmin = mean - ci_lower,
#                      xmax = mean + ci_upper,
#                      height = 0.1)) +
#   xlim(c(0, upperLim)) +
# #   geom_segment(aes(y = character, yend = character, 
# #                    x = mean - ci_lower, xend = mean + ci_upper_new), 
# #                arrow = arrow(length = unit(0.1, "inches"))) +
# #   geom_text(aes(y = character, vjust = 2,
# #                 x = 2*upperLim - (mean + ci_upper_new), hjust = 1,
# #                 label = paste("upper limit:", round(mean + ci_upper, 2)))) +
#   scale_colour_brewer(type = "qual", palette = 6)

# --------------->-> HUNGER ---------------------------------------------------

M_children_hunger <- makeM(selectAgeGroup = "children", 
                         selectPredicate = "hunger") + 1 # add one for continuity correction

eba_children_hunger <- OptiPt(M_children_hunger,
                            A = aspects); summary(eba_children_hunger)

eba_children_hunger_df_0 <- data.frame("utility" = uscale(eba_children_hunger)) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(ifelse(character %in% c("grownup", "kid", "baby"), 
                                  "human",
                                  ifelse(character %in% c("dog", "bear", "bug"), 
                                         "animal",
                                         ifelse(character %in% c("robot", "computer", "car"), 
                                                
                                                "technology",
                                                "control"))),
                           levels = c("human", "animal", "technology", "control")),
         character = factor(character,
                            levels = c("grownup", "kid", "baby",
                                       "dog", "bear", "bug",
                                       "robot", "computer", "car", "stapler")),
         order = ifelse(character == "grownup", 10,
                        ifelse(character == "kid", 9,
                               ifelse(character == "baby", 8,
                                      ifelse(character == "dog", 7,
                                             ifelse(character == "bear", 6,
                                                    ifelse(character == "bug", 5,
                                                           ifelse(character == "robot", 4,
                                                                  ifelse(character == "computer", 3,
                                                                         ifelse(character == "car", 2,
                                                                                ifelse(character == "stapler", 1, NA)))))))))))

ci_children_hunger <- data.frame("me" = 1.96 * sqrt(diag(cov.u(eba_children_hunger)))) %>%
  add_rownames(var = "character")

eba_children_hunger_df <- eba_children_hunger_df_0 %>%
  full_join(ci_children_hunger) %>%
  mutate(ci_lower = utility - me,
         ci_upper = utility + me,
         ci_lower_new = as.numeric(ifelse(ci_lower < 0, 0, ci_lower)), # hacky :(
         ci_upper_new = as.numeric(ifelse(ci_upper > upperLim, upperLim, NA)))

ggplot(aes(y = reorder(character, order), 
           x = utility,
           colour = category), 
       data = eba_children_hunger_df) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2),
        panel.grid.major.y = element_line(size = 1)) +
  geom_point(size = 5) +
  labs(title = "Children: Hunger (EBA)\n",
       x = "\nUtility scale value") +
  geom_errorbarh(aes(xmin = ci_lower_new,
                     xmax = ci_upper,
                     height = 0.1)) +
#   xlim(c(0, upperLim)) +
#   geom_segment(aes(y = character, yend = character, 
#                    x = ci_lower_new, xend = ci_upper_new), 
#                arrow = arrow(length = unit(0.1, "inches"))) +
#   geom_text(aes(y = character, vjust = 2,
#                 x = 2*upperLim - (ci_upper_new), hjust = 1,
#                 label = paste("upper limit:", round(ci_upper, 2)))) +
  geom_vline(aes(xintercept = 0.1), lty = 2) +
  scale_colour_brewer(type = "qual", palette = 6)

# --------------->-> FEELINGS ---------------------------------------------------

M_children_feelings <- makeM(selectAgeGroup = "children", 
                           selectPredicate = "feelings") + 1 # add one for continuity correction

eba_children_feelings <- OptiPt(M_children_feelings,
                              A = aspects); summary(eba_children_feelings)

eba_children_feelings_df_0 <- data.frame("utility" = uscale(eba_children_feelings)) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(ifelse(character %in% c("grownup", "kid", "baby"), 
                                  "human",
                                  ifelse(character %in% c("dog", "bear", "bug"), 
                                         "animal",
                                         ifelse(character %in% c("robot", "computer", "car"), 
                                                
                                                "technology",
                                                "control"))),
                           levels = c("human", "animal", "technology", "control")),
         character = factor(character,
                            levels = c("grownup", "kid", "baby",
                                       "dog", "bear", "bug",
                                       "robot", "computer", "car", "stapler")),
         order = ifelse(character == "grownup", 10,
                        ifelse(character == "kid", 9,
                               ifelse(character == "baby", 8,
                                      ifelse(character == "dog", 7,
                                             ifelse(character == "bear", 6,
                                                    ifelse(character == "bug", 5,
                                                           ifelse(character == "robot", 4,
                                                                  ifelse(character == "computer", 3,
                                                                         ifelse(character == "car", 2,
                                                                                ifelse(character == "stapler", 1, NA)))))))))))

ci_children_feelings <- data.frame("me" = 1.96 * sqrt(diag(cov.u(eba_children_feelings)))) %>%
  add_rownames(var = "character")

eba_children_feelings_df <- eba_children_feelings_df_0 %>%
  full_join(ci_children_feelings) %>%
  mutate(ci_lower = utility - me,
         ci_upper = utility + me,
         ci_lower_new = as.numeric(ifelse(ci_lower < 0, 0, ci_lower)), # hacky :(
         ci_upper_new = as.numeric(ifelse(ci_upper > upperLim, upperLim, NA)))

ggplot(aes(y = reorder(character, order), 
           x = utility,
           colour = category), 
       data = eba_children_feelings_df) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2),
        panel.grid.major.y = element_line(size = 1)) +
  geom_point(size = 5) +
  labs(title = "Children: Feelings (EBA)\n",
       x = "\nUtility scale value") +
  geom_errorbarh(aes(xmin = ci_lower_new,
                     xmax = ci_upper,
                     height = 0.1)) +
#   xlim(c(0, upperLim)) +
#   geom_segment(aes(y = character, yend = character, 
#                    x = ci_lower_new, xend = ci_upper_new), 
#                arrow = arrow(length = unit(0.1, "inches"))) +
#   geom_text(aes(y = character, vjust = 2,
#                 x = 2*upperLim - (ci_upper_new), hjust = 1,
#                 label = paste("upper limit:", round(ci_upper, 2)))) +
  geom_vline(aes(xintercept = 0.1), lty = 2) +
  scale_colour_brewer(type = "qual", palette = 6)

# --------------->-> THINKING ---------------------------------------------------

M_children_thinking <- makeM(selectAgeGroup = "children", 
                           selectPredicate = "thinking") + 1 # add one for continuity correction

eba_children_thinking <- OptiPt(M_children_thinking,
                              A = aspects); summary(eba_children_thinking)

eba_children_thinking_df_0 <- data.frame("utility" = uscale(eba_children_thinking)) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(ifelse(character %in% c("grownup", "kid", "baby"), 
                                  "human",
                                  ifelse(character %in% c("dog", "bear", "bug"), 
                                         "animal",
                                         ifelse(character %in% c("robot", "computer", "car"), 
                                                
                                                "technology",
                                                "control"))),
                           levels = c("human", "animal", "technology", "control")),
         character = factor(character,
                            levels = c("grownup", "kid", "baby",
                                       "dog", "bear", "bug",
                                       "robot", "computer", "car", "stapler")),
         order = ifelse(character == "grownup", 10,
                        ifelse(character == "kid", 9,
                               ifelse(character == "baby", 8,
                                      ifelse(character == "dog", 7,
                                             ifelse(character == "bear", 6,
                                                    ifelse(character == "bug", 5,
                                                           ifelse(character == "robot", 4,
                                                                  ifelse(character == "computer", 3,
                                                                         ifelse(character == "car", 2,
                                                                                ifelse(character == "stapler", 1, NA)))))))))))

ci_children_thinking <- data.frame("me" = 1.96 * sqrt(diag(cov.u(eba_children_thinking)))) %>%
  add_rownames(var = "character")

eba_children_thinking_df <- eba_children_thinking_df_0 %>%
  full_join(ci_children_thinking) %>%
  mutate(ci_lower = utility - me,
         ci_upper = utility + me,
         ci_lower_new = as.numeric(ifelse(ci_lower < 0, 0, ci_lower)), # hacky :(
         ci_upper_new = as.numeric(ifelse(ci_upper > upperLim, upperLim, NA)))

ggplot(aes(y = reorder(character, order), 
           x = utility,
           colour = category), 
       data = eba_children_thinking_df) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 2),
        panel.grid.major.y = element_line(size = 1)) +
  geom_point(size = 5) +
  labs(title = "Children: Thinking (EBA)\n",
       x = "\nUtility scale value") +
  geom_errorbarh(aes(xmin = ci_lower_new,
                     xmax = ci_upper,
                     height = 0.1)) +
#   xlim(c(0, upperLim)) +
#   geom_segment(aes(y = character, yend = character, 
#                    x = ci_lower_new, xend = ci_upper_new), 
#                arrow = arrow(length = unit(0.1, "inches"))) +
#   geom_text(aes(y = character, vjust = 2,
#                 x = 2*upperLim - (ci_upper_new), hjust = 1,
#                 label = paste("upper limit:", round(ci_upper, 2)))) +
  geom_vline(aes(xintercept = 0.1), lty = 2) +
  scale_colour_brewer(type = "qual", palette = 6)

# --------> test differences between groupings --------------------------------

# --------------->-> adults: among predicates ---------------------------------

M_adults_compPredicates <- abind(M_adults_hunger, M_adults_feelings, M_adults_thinking, 
                                 along = 3,
                                 new.names = c("hunger", "feelings", "thinking"))
# double-check same as above
# dotchart(uscale(eba(M_adults_compPredicates[,,"hunger"])))
# dotchart(uscale(eba(M_adults_compPredicates[,,"feelings"])))
# dotchart(uscale(eba(M_adults_compPredicates[,,"thinking"])))

test_adults_compPredicates <- group.test(M_adults_compPredicates,
                                         A = aspects)
test_adults_compPredicates

# --------------->-> children: among predicates ---------------------------------

M_children_compPredicates <- abind(M_children_hunger, M_children_feelings, M_children_thinking, 
                                   along = 3,
                                   new.names = c("hunger", "feelings", "thinking"))
# double-check same as above
# dotchart(uscale(eba(M_children_compPredicates[,,"hunger"])))
# dotchart(uscale(eba(M_children_compPredicates[,,"feelings"])))
# dotchart(uscale(eba(M_children_compPredicates[,,"thinking"])))

test_children_compPredicates <- group.test(M_children_compPredicates,
                                           A = aspects)
test_children_compPredicates

# --------------->-> adults vs. children: across predicates -------------------

M_compAgeGroups <- abind(M_adults_all, M_children_all, 
                         along = 3,
                         new.names = c("adults", "children"))
# double-check same as above
# dotchart(uscale(eba(M_compAgeGroups[,,"adults"])))
# dotchart(uscale(eba(M_compAgeGroups[,,"children"])))

test_compAgeGroups <- group.test(M_compAgeGroups, 
                                 A = aspects)
test_compAgeGroups

# --------------->-> interaction: adults vs. children X predicates -------------------------

# try 6 groups
M_compInteraction <- abind(M_adults_hunger, M_adults_feelings, M_adults_thinking,
                           M_children_hunger, M_children_feelings, M_children_thinking, 
                           along = 3,
                           new.names = c("adults.hunger", 
                                         "adults.feelings", 
                                         "adults.thinking",
                                         "children.hunger",
                                         "children.feelings",
                                         "children.thinking"))

test_compInteraction <- group.test(M_compInteraction,
                                   A = aspects)
test_compInteraction

# hunger only
M_compInteraction_hunger <- abind(M_adults_hunger, M_children_hunger, 
                                  along = 3,
                                  new.names = c("adults.hunger", 
                                                "children.hunger"))
test_compInteraction_hunger <- group.test(M_compInteraction_hunger,
                                          A = aspects)
test_compInteraction_hunger

# feelings only
M_compInteraction_feelings <- abind(M_adults_feelings, M_children_feelings, 
                                    along = 3,
                                    new.names = c("adults.feelings", 
                                                  "children.feelings"))
test_compInteraction_feelings <- group.test(M_compInteraction_feelings,
                                            A = aspects)
test_compInteraction_feelings

# thinking only
M_compInteraction_thinking <- abind(M_adults_thinking, M_children_thinking, 
                                    along = 3,
                                    new.names = c("adults.thinking", 
                                                  "children.thinking"))
test_compInteraction_thinking <- group.test(M_compInteraction_thinking,
                                            A = aspects)
test_compInteraction_thinking
