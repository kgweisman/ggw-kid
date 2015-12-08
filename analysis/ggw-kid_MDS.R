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
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01&02_2015-12-08_data_anonymized.csv")[-1] # get rid of column of obs numbers
# 
# # ... RUN01
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01_2015-06-13_data_anonymized.csv")[-1] # get rid of column of obs numbers

# ... RUN02
dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-02_2015-12-08_data_anonymized.csv")[-1] # get rid of column of obs numbers

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
lowerLim <- 4.5
upperLim <- 5.5

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
  theme(text = element_text(size = 40))
# qplot(subset(demo, ageGroup == "adults")$ageCalc)

# demo %>% group_by(ageGroup) %>% filter(age < 100) %>% summarise(mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm = T))
# qplot(subset(demo, ageGroup == "children")$age[demo$age < 100])
# qplot(subset(demo, ageGroup == "adults")$age[demo$age < 100])

# age by condition
demo %>% group_by(ageGroup, sequence) %>% summarise(mean_age = round(mean(ageCalc, na.rm = T), 2), sd_age = round(sd(ageCalc, na.rm = T), 2))

######################################################## analysis & plots #####

# --- dissimilarities data-formatting function --------------------------------

makeDissimByPredicate <- function(selectPredicate, selectAgeGroup) {
  tempDissim <- NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  tempDissim <- dd %>%
    filter(predicate %in% selectPredicate & 
             ageGroup %in% selectAgeGroup) %>%
    mutate(character1 = array(),
           character2 = array())
  
  charsort = sort(levels(tempDissim$leftCharacter), decreasing = TRUE)
  
  for(i in 1:length(charsort)) {
    tempDissim <- tempDissim %>%
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
  
  # make upper matrix of tempDissimilarity values
  tempDissim <- tempDissim %>%
    select(predicate, subid, character1, character2, responseNum) %>%
    group_by(character1, character2) %>%
    mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
    summarise(mean = mean(dist, na.rm = TRUE)) %>%
    spread(character2, mean)
  
  # add NA columns and rows - depends on whether including stapler or not
  checkNoStapler <- count(dd) == count(dd_nostapler)
  if (FALSE %in% checkNoStapler) {
    # including stapler
    tempDissim <- tempDissim %>%
      mutate(baby = NA,
             character1 = as.character(character1)) %>%
      rbind(c("stapler", rep(NA, 13))) %>%
      mutate(character1 = factor(character1))
  } else {
    # NOT including stapler
    tempDissim <- tempDissim %>%
      mutate(baby = NA,
             character1 = as.character(character1)) %>%
      rbind(c("robot", rep(NA, 12))) %>%
      mutate(character1 = factor(character1))
  }
  
  # reorder columns
  tempDissim = tempDissim[, c(1, length(tempDissim), 2:(length(tempDissim) - 1))]
  
  # get character names
  names = tempDissim[[1]]
  
  # rename rows and columns
  tempDissim = tempDissim[-1]
  rownames(tempDissim) = names
  colnames(tempDissim) = names
  
  # fill in lower triangle matrix - depends on whether including stapler or not
  if (FALSE %in% checkNoStapler) {
    # fill in lower triangle matrix
    for(i in 1:9) {
      for(j in (i+1):10) {
        tempDissim[j,i] = tempDissim[i,j]
      }
    }
  } else {
    # fill in lower triangle matrix
    for(i in 1:8) {
      for(j in (i+1):9) {
        tempDissim[j,i] = tempDissim[i,j]
      }
    }
  }
  
  tempDissim = as.dist(tempDissim)
  return(tempDissim)
}

# --- adults ------------------------------------------------------------------

# --------> MULTIDIMENSIONAL SCALING ANALYSIS A -------------------------------

# -------------> non-metric (ordinal) MDS -------------------------------------
# NOTE: could also explore fitting with more than 2 dimensions...

# make dissimilarity matrix for all predicates
dissim_adults <- makeDissimByPredicate(selectPredicate = c("thinking", "feelings", "hunger"),
                                selectAgeGroup = "adults")

# do MDS
mds_adults_Aordinal = mds(dissim_adults, ndim = 2, type = "ordinal")
summary(mds_adults_Aordinal)
mds_adults_Aordinal

# plot dimension space (built in function)
# plot(mds_adults_Aordinal,
#      plot.type = "confplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS solution: All conditions")

# plot (ggplot)
conf_Aordinal <- data.frame(mds_adults_Aordinal$conf) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(
    ifelse(character %in% c("grownup", "kid", "baby"), 
           "human",
           ifelse(character %in% c("dog", "bear", "bug"),
                  "animal",
                  ifelse(character %in% c("robot", "computer", "car"),
                         "technology",
                         "control"))),
    levels = c("human", "animal", "technology", "control")))
xRange_Aordinal <- range(conf_Aordinal$D1)
yRange_Aordinal <- range(conf_Aordinal$D2)

ggplot(aes(x = D1, y = D2, colour = category, label = character), 
       data = conf_Aordinal) +
  theme_bw() +
  theme(text = element_text(size = 40),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 4)) +
  geom_point(size = 10) +
  geom_text(vjust = -1, size = 14) +
  labs(x = "\nDimension 1", y = "Dimension 2\n",
       title = "Adults: All predicates (MDS)\n") +
  xlim(c(-1, 1)) +
  ylim(c(-1, 1)) +
  scale_color_brewer(type = "qual", palette = 6)
#   xlim(c(xRange_Aordinal[1] - 0.10*(xRange_Aordinal[2]-xRange_Aordinal[1])),
#        xRange_Aordinal[2] + 0.10*(xRange_Aordinal[2]-xRange_Aordinal[1])) +
#   ylim(c(yRange_Aordinal[1] - 0.05*(yRange_Aordinal[2]-yRange_Aordinal[1])),
#        yRange_Aordinal[2] + 0.15*(yRange_Aordinal[2]-yRange_Aordinal[1]))
  
# # plot space and stress (bigger bubble = better fit)
# plot(mds_adults_Aordinal, plot.type = "bubbleplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS bubble plot: All conditions")
# 
# # plot stress (higher = worse fit)
# plot(mds_adults_Aordinal, plot.type = "stressplot",
#      main = "MDS stress: All conditions")
# 
# # Shepard plot
# plot(mds_adults_Aordinal, plot.type = "Shepard",
#      main = "MDS Shepard plot: All conditions")
# 
# # plot residuals
# plot(mds_adults_Aordinal, plot.type = "resplot",
#      main = "MDS residuals: All conditions")

# --------> MULTIDIMENSIONAL SCALING ANALYSIS B -------------------------------

# -------------> predicate: THINKING ------------------------------------------

# make dissimilarity matrix for all predicates
dissim_adults_thinking <- makeDissimByPredicate(selectPredicate = "thinking",
                                         selectAgeGroup = "adults")

# do MDS
mds_adults_thinking_Aordinal = mds(dissim_adults_thinking, ndim = 2, type = "ordinal")
summary(mds_adults_thinking_Aordinal)
mds_adults_thinking_Aordinal

# plot dimension space (built in function)
# plot(mds_adults_thinking_Aordinal,
#      plot.type = "confplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS solution: Thinking")

# plot (ggplot)
conf_adults_thinking_Aordinal <- data.frame(mds_adults_thinking_Aordinal$conf) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(
    ifelse(character %in% c("grownup", "kid", "baby"), 
           "human",
           ifelse(character %in% c("dog", "bear", "bug"),
                  "animal",
                  ifelse(character %in% c("robot", "computer", "car"),
                         "technology",
                         "control"))),
    levels = c("human", "animal", "technology", "control")))
xRange_thinking_Aordinal <- range(conf_adults_thinking_Aordinal$D1)
yRange_thinking_Aordinal <- range(conf_adults_thinking_Aordinal$D2)

p_adultsThinking <- ggplot(aes(x = D1, y = D2, colour = category, label = character), 
       data = conf_adults_thinking_Aordinal) +
  theme_bw() +
  theme(text = element_text(size = 40),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 4)) +
  geom_point(size = 10) +
  geom_text(vjust = -1, size = 14) +
  labs(x = "\nDimension 1", y = "Dimension 2\n",
       title = "Adults: Thinking (MDS)\n") +
  xlim(c(-1, 1)) +
  ylim(c(-1, 1)) +
  scale_color_brewer(type = "qual", palette = 6)

# # Shepard plot
# plot(mds_adults_thinking_Aordinal, plot.type = "Shepard",
#      main = "MDS Shepard plot: THINKING")
# 
# # plot residuals
# plot(mds_adults_thinking_Aordinal, plot.type = "resplot",
#      main = "MDS residuals: THINKING")

# -------------> predicate: FEELINGS ------------------------------------------

# make dissimilarity matrix for all predicates
dissim_adults_feelings <- makeDissimByPredicate(selectPredicate = "feelings",
                                         selectAgeGroup = "adults")

# do MDS
mds_adults_feelings_Aordinal = mds(dissim_adults_feelings, ndim = 2, type = "ordinal")
summary(mds_adults_feelings_Aordinal)
mds_adults_feelings_Aordinal

# plot dimension space (built in function)
# plot(mds_adults_feelings_Aordinal,
#      plot.type = "confplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS solution: Feelings")

# plot (ggplot)
conf_adults_feelings_Aordinal <- data.frame(mds_adults_feelings_Aordinal$conf) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(
    ifelse(character %in% c("grownup", "kid", "baby"), 
           "human",
           ifelse(character %in% c("dog", "bear", "bug"),
                  "animal",
                  ifelse(character %in% c("robot", "computer", "car"),
                         "technology",
                         "control"))),
    levels = c("human", "animal", "technology", "control")))
xRange_feelings_Aordinal <- range(conf_adults_feelings_Aordinal$D1)
yRange_feelings_Aordinal <- range(conf_adults_feelings_Aordinal$D2)

p_adultsFeelings <- ggplot(aes(x = D1, y = D2, colour = category, label = character), 
       data = conf_adults_feelings_Aordinal) +
  theme_bw() +
  theme(text = element_text(size = 40),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 4)) +
  geom_point(size = 10) +
  geom_text(vjust = -1, size = 14) +
  labs(x = "\nDimension 1", y = "Dimension 2\n",
       title = "Adults: Feelings (MDS)\n") +
  xlim(c(-1, 1)) +
  ylim(c(-1, 1)) +
  scale_color_brewer(type = "qual", palette = 6)

# # plot space and stress (bigger bubble = better fit)
# plot(mds_adults_feelings_Aordinal, plot.type = "bubbleplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS bubble plot: FEELINGS")
# 
# # plot stress (higher = worse fit)
# plot(mds_adults_feelings_Aordinal, plot.type = "stressplot",
#      main = "MDS stress: FEELINGS")
# 
# # Shepard plot
# plot(mds_adults_feelings_Aordinal, plot.type = "Shepard",
#      main = "MDS Shepard plot: FEELINGS")
# 
# # plot residuals
# plot(mds_adults_feelings_Aordinal, plot.type = "resplot",
#      main = "MDS residuals: FEELINGS")

# -------------> predicate: HUNGER --------------------------------------------

# make dissimilarity matrix for all predicates
dissim_adults_hunger <- makeDissimByPredicate(selectPredicate = "hunger",
                                       selectAgeGroup = "adults")

# do MDS
mds_adults_hunger_Aordinal = mds(dissim_adults_hunger, ndim = 2, type = "ordinal")
summary(mds_adults_hunger_Aordinal)
mds_adults_hunger_Aordinal

# plot dimension space (built in function)
# plot(mds_adults_hunger_Aordinal,
#      plot.type = "confplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS solution: Hunger")

# plot (ggplot)
conf_adults_hunger_Aordinal <- data.frame(mds_adults_hunger_Aordinal$conf) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(
    ifelse(character %in% c("grownup", "kid", "baby"), 
           "human",
           ifelse(character %in% c("dog", "bear", "bug"),
                  "animal",
                  ifelse(character %in% c("robot", "computer", "car"),
                         "technology",
                         "control"))),
    levels = c("human", "animal", "technology", "control")))
xRange_hunger_Aordinal <- range(conf_adults_hunger_Aordinal$D1)
yRange_hunger_Aordinal <- range(conf_adults_hunger_Aordinal$D2)

p_adultsHunger <- ggplot(aes(x = D1, y = D2, colour = category, label = character), 
       data = conf_adults_hunger_Aordinal) +
  theme_bw() +
  theme(text = element_text(size = 40),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 4)) +
  geom_point(size = 10) +
  geom_text(vjust = -1, size = 14) +
  labs(x = "\nDimension 1", y = "Dimension 2\n",
       title = "Adults: Hunger (MDS)\n") +
  xlim(c(-1, 1)) +
  ylim(c(-1, 1)) +
  scale_color_brewer(type = "qual", palette = 6)
#   xlim(c(xRange_hunger_Aordinal[1] - 0.10*(xRange_hunger_Aordinal[2]-xRange_hunger_Aordinal[1])),
#        xRange_hunger_Aordinal[2] + 0.10*(xRange_hunger_Aordinal[2]-xRange_hunger_Aordinal[1])) +
#   ylim(c(yRange_hunger_Aordinal[1] - 0.05*(yRange_hunger_Aordinal[2]-yRange_hunger_Aordinal[1])),
#        yRange_hunger_Aordinal[2] + 0.15*(yRange_hunger_Aordinal[2]-yRange_hunger_Aordinal[1]))

# # plot space and stress (bigger bubble = better fit)
# plot(mds_adults_hunger_Aordinal, plot.type = "bubbleplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS bubble plot: HUNGER")
# 
# # plot stress (higher = worse fit)
# plot(mds_adults_hunger_Aordinal, plot.type = "stressplot",
#      main = "MDS stress: HUNGER")
# 
# # Shepard plot
# plot(mds_adults_hunger_Aordinal, plot.type = "Shepard",
#      main = "MDS Shepard plot: HUNGER")
# 
# # plot residuals
# plot(mds_adults_hunger_Aordinal, plot.type = "resplot",
#      main = "MDS residuals: HUNGER")

# --- children ----------------------------------------------------------------

# --------> MULTIDIMENSIONAL SCALING ANALYSIS A -------------------------------

# -------------> non-metric (ordinal) MDS -------------------------------------
# NOTE: could also explore fitting with more than 2 dimensions...

# make dissimilarity matrix for all predicates
dissim_children <- makeDissimByPredicate(selectPredicate = c("thinking", "feelings", "hunger"),
                                       selectAgeGroup = "children")

# do MDS
mds_children_Aordinal = mds(dissim_children, ndim = 2, type = "ordinal")
summary(mds_children_Aordinal)
mds_children_Aordinal

# plot dimension space (built in function)
# plot(mds_children_Aordinal,
#      plot.type = "confplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS solution: All conditions")

# plot (ggplot)
conf_Aordinal <- data.frame(mds_children_Aordinal$conf) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(
    ifelse(character %in% c("grownup", "kid", "baby"), 
           "human",
           ifelse(character %in% c("dog", "bear", "bug"),
                  "animal",
                  ifelse(character %in% c("robot", "computer", "car"),
                         "technology",
                         "control"))),
    levels = c("human", "animal", "technology", "control")))
xRange_Aordinal <- range(conf_Aordinal$D1)
yRange_Aordinal <- range(conf_Aordinal$D2)

ggplot(aes(x = D1, y = D2, colour = category, label = character), 
       data = conf_Aordinal) +
  theme_bw() +
  theme(text = element_text(size = 40),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 4)) +
  geom_point(size = 10) +
  geom_text(vjust = -1, size = 14) +
  labs(x = "\nDimension 1", y = "Dimension 2\n",
       title = "Children: All predicates (MDS)\n") +
  xlim(c(-1, 1)) +
  ylim(c(-1, 1)) +
  scale_color_brewer(type = "qual", palette = 6)
#   xlim(c(xRange_Aordinal[1] - 0.10*(xRange_Aordinal[2]-xRange_Aordinal[1])),
#        xRange_Aordinal[2] + 0.10*(xRange_Aordinal[2]-xRange_Aordinal[1])) +
#   ylim(c(yRange_Aordinal[1] - 0.05*(yRange_Aordinal[2]-yRange_Aordinal[1])),
#        yRange_Aordinal[2] + 0.15*(yRange_Aordinal[2]-yRange_Aordinal[1]))

# # plot space and stress (bigger bubble = better fit)
# plot(mds_children_Aordinal, plot.type = "bubbleplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS bubble plot: All conditions")
# 
# # plot stress (higher = worse fit)
# plot(mds_children_Aordinal, plot.type = "stressplot",
#      main = "MDS stress: All conditions")
# 
# # Shepard plot
# plot(mds_children_Aordinal, plot.type = "Shepard",
#      main = "MDS Shepard plot: All conditions")
# 
# # plot residuals
# plot(mds_children_Aordinal, plot.type = "resplot",
#      main = "MDS residuals: All conditions")

# --------> MULTIDIMENSIONAL SCALING ANALYSIS B -------------------------------

# -------------> predicate: THINKING ------------------------------------------

# make dissimilarity matrix for all predicates
dissim_children_thinking <- makeDissimByPredicate(selectPredicate = "thinking",
                                                selectAgeGroup = "children")

# do MDS
mds_children_thinking_Aordinal = mds(dissim_children_thinking, ndim = 2, type = "ordinal")
summary(mds_children_thinking_Aordinal)
mds_children_thinking_Aordinal

# plot dimension space (built in function)
# plot(mds_children_thinking_Aordinal,
#      plot.type = "confplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS solution: Thinking")

# plot (ggplot)
conf_children_thinking_Aordinal <- data.frame(mds_children_thinking_Aordinal$conf) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(
    ifelse(character %in% c("grownup", "kid", "baby"), 
           "human",
           ifelse(character %in% c("dog", "bear", "bug"),
                  "animal",
                  ifelse(character %in% c("robot", "computer", "car"),
                         "technology",
                         "control"))),
    levels = c("human", "animal", "technology", "control")))
xRange_thinking_Aordinal <- range(conf_children_thinking_Aordinal$D1)
yRange_thinking_Aordinal <- range(conf_children_thinking_Aordinal$D2)

p_childrenThinking <- ggplot(aes(x = D1, y = D2, colour = category, label = character), 
       data = conf_children_thinking_Aordinal) +
  theme_bw() +
  theme(text = element_text(size = 40),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 4)) +
  geom_point(size = 10) +
  geom_text(vjust = -1, size = 14) +
  labs(x = "\nDimension 1", y = "Dimension 2\n",
       title = "Children: Thinking (MDS)\n") +
  xlim(c(-1, 1)) +
  ylim(c(-1, 1)) +
  scale_color_brewer(type = "qual", palette = 6)

# # Shepard plot
# plot(mds_children_thinking_Aordinal, plot.type = "Shepard",
#      main = "MDS Shepard plot: THINKING")
# 
# # plot residuals
# plot(mds_children_thinking_Aordinal, plot.type = "resplot",
#      main = "MDS residuals: THINKING")

# -------------> predicate: FEELINGS ------------------------------------------

# make dissimilarity matrix for all predicates
dissim_children_feelings <- makeDissimByPredicate(selectPredicate = "feelings",
                                                selectAgeGroup = "children")

# do MDS
mds_children_feelings_Aordinal = mds(dissim_children_feelings, ndim = 2, type = "ordinal")
summary(mds_children_feelings_Aordinal)
mds_children_feelings_Aordinal

# plot dimension space (built in function)
# plot(mds_children_feelings_Aordinal,
#      plot.type = "confplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS solution: Feelings")

# plot (ggplot)
conf_children_feelings_Aordinal <- data.frame(mds_children_feelings_Aordinal$conf) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(
    ifelse(character %in% c("grownup", "kid", "baby"), 
           "human",
           ifelse(character %in% c("dog", "bear", "bug"),
                  "animal",
                  ifelse(character %in% c("robot", "computer", "car"),
                         "technology",
                         "control"))),
    levels = c("human", "animal", "technology", "control")))
xRange_feelings_Aordinal <- range(conf_children_feelings_Aordinal$D1)
yRange_feelings_Aordinal <- range(conf_children_feelings_Aordinal$D2)

p_childrenFeelings <- ggplot(aes(x = D1, y = D2, colour = category, label = character), 
       data = conf_children_feelings_Aordinal) +
  theme_bw() +
  theme(text = element_text(size = 40),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 4)) +
  geom_point(size = 10) +
  geom_text(vjust = -1, size = 14) +
  labs(x = "\nDimension 1", y = "Dimension 2\n",
       title = "Children: Feelings (MDS)\n") +
  xlim(c(-1, 1)) +
  ylim(c(-1, 1)) +
  scale_color_brewer(type = "qual", palette = 6)

# # plot space and stress (bigger bubble = better fit)
# plot(mds_children_feelings_Aordinal, plot.type = "bubbleplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS bubble plot: FEELINGS")
# 
# # plot stress (higher = worse fit)
# plot(mds_children_feelings_Aordinal, plot.type = "stressplot",
#      main = "MDS stress: FEELINGS")
# 
# # Shepard plot
# plot(mds_children_feelings_Aordinal, plot.type = "Shepard",
#      main = "MDS Shepard plot: FEELINGS")
# 
# # plot residuals
# plot(mds_children_feelings_Aordinal, plot.type = "resplot",
#      main = "MDS residuals: FEELINGS")

# -------------> predicate: HUNGER --------------------------------------------

# make dissimilarity matrix for all predicates
dissim_children_hunger <- makeDissimByPredicate(selectPredicate = "hunger",
                                              selectAgeGroup = "children")

# do MDS
mds_children_hunger_Aordinal = mds(dissim_children_hunger, ndim = 2, type = "ordinal")
summary(mds_children_hunger_Aordinal)
mds_children_hunger_Aordinal

# plot dimension space (built in function)
# plot(mds_children_hunger_Aordinal,
#      plot.type = "confplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS solution: Hunger")

# plot (ggplot)
conf_children_hunger_Aordinal <- data.frame(mds_children_hunger_Aordinal$conf) %>%
  add_rownames(var = "character") %>%
  mutate(category = factor(
    ifelse(character %in% c("grownup", "kid", "baby"), 
           "human",
           ifelse(character %in% c("dog", "bear", "bug"),
                  "animal",
                  ifelse(character %in% c("robot", "computer", "car"),
                         "technology",
                         "control"))),
    levels = c("human", "animal", "technology", "control")))
xRange_hunger_Aordinal <- range(conf_children_hunger_Aordinal$D1)
yRange_hunger_Aordinal <- range(conf_children_hunger_Aordinal$D2)

p_childrenHunger <- ggplot(aes(x = D1, y = D2, colour = category, label = character), 
       data = conf_children_hunger_Aordinal) +
  theme_bw() +
  theme(text = element_text(size = 40),
        # axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(size = 4)) +
  geom_point(size = 10) +
  geom_text(vjust = -1, size = 14) +
  labs(x = "\nDimension 1", y = "Dimension 2\n",
       title = "Children: Hunger (MDS)\n") +
  xlim(c(-1, 1)) +
  ylim(c(-1, 1)) +
  scale_color_brewer(type = "qual", palette = 6)

# # plot space and stress (bigger bubble = better fit)
# plot(mds_children_hunger_Aordinal, plot.type = "bubbleplot",
#      xlim = c(-1, 1),
#      ylim = c(-1, 1),     
#      main = "MDS bubble plot: HUNGER")
# 
# # plot stress (higher = worse fit)
# plot(mds_children_hunger_Aordinal, plot.type = "stressplot",
#      main = "MDS stress: HUNGER")
# 
# # Shepard plot
# plot(mds_children_hunger_Aordinal, plot.type = "Shepard",
#      main = "MDS Shepard plot: HUNGER")
# 
# # plot residuals
# plot(mds_children_hunger_Aordinal, plot.type = "resplot",
#      main = "MDS residuals: HUNGER")

######################################################### exporting plots #####

# make all plots for all predicates & ageGroups
graphics.off()
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/plots/mds_large")
png(filename = "mds%03d.png", width = 1200, height = 1200)
p_adultsHunger
p_adultsFeelings
p_adultsThinking
p_childrenHunger
p_childrenFeelings
p_childrenThinking
graphics.off()

setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/plots/mds_small")
png(filename = "mds%03d.png", width = 700, height = 700)
p_adultsHunger
p_adultsFeelings
p_adultsThinking
p_childrenHunger
p_childrenFeelings
p_childrenThinking
graphics.off()
