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

# --------> HIERARCHICAL CLUSTER ANALYSIS -------------------------------------
# Roughly equivalent to pca_B

# ...for all conditions
dissim_adults <- makeDissimByPredicate(selectPredicate = c("thinking", "feelings", "hunger"),
                                       selectAgeGroup = "adults")

# Conduct hierarchical cluster analysis
hcb_adults = hclust(dissim_adults); hcb_adults

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_adults$merge
# plot(hcb_adults$height)
plot(hcb_adults,
     font.main = 1,
     main = "Adults: All predicates (HCA)",
     sub = "",
     xlab = "",
     ylab = "Height",
     edgePar = list(col = 1:2, lty = 2:3))

# ...for thinking
dissim_adults_thinking <- makeDissimByPredicate(selectPredicate = "thinking",
                                                selectAgeGroup = "adults")

# Conduct hierarchical cluster analysis
hcb_adults_thinking = hclust(dissim_adults_thinking); hcb_adults_thinking

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_adults_thinking$merge
# plot(hcb_adults_thinking$height)
plot(hcb_adults_thinking,
     font.main = 1,
     main = "Adults: Thinking (HCA)",
     sub = "",
     xlab = "",
     ylab = "Height")

# ...for feelings
dissim_adults_feelings <- makeDissimByPredicate(selectPredicate = "feelings",
                                                selectAgeGroup = "adults")

# Conduct hierarchical cluster analysis
hcb_adults_feelings = hclust(dissim_adults_feelings); hcb_adults_feelings

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_adults_feelings$merge
# plot(hcb_adults_feelings$height)
plot(hcb_adults_feelings,
     font.main = 1,
     main = "Adults: Feelings (HCA)",
     sub = "",
     xlab = "",
     ylab = "Height")

# ...for hunger
dissim_adults_hunger <- makeDissimByPredicate(selectPredicate = "hunger",
                                              selectAgeGroup = "adults")

# Conduct hierarchical cluster analysis
hcb_adults_hunger = hclust(dissim_adults_hunger); hcb_adults_hunger

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_adults_hunger$merge
# plot(hcb_adults_hunger$height)
plot(hcb_adults_hunger,
     font.main = 1,
     main = "Adults: Hunger (HCA)",
     sub = "",
     xlab = "",
     ylab = "Height")

# --- children ----------------------------------------------------------------

# --------> HIERARCHICAL CLUSTER ANALYSIS -------------------------------------
# Roughly equivalent to pca_B

# ...for all conditions
dissim_children <- makeDissimByPredicate(selectPredicate = c("thinking", "feelings", "hunger"),
                                       selectAgeGroup = "children")

# Conduct hierarchical cluster analysis
hcb_children = hclust(dissim_children); hcb_children

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_children$merge
# plot(hcb_children$height)
plot(hcb_children,
     font.main = 1,
     main = "Children: All predicates (HCA)",
     sub = "",
     xlab = "",
     ylab = "Height")

# ...for thinking
dissim_children_thinking <- makeDissimByPredicate(selectPredicate = "thinking",
                                                selectAgeGroup = "children")

# Conduct hierarchical cluster analysis
hcb_children_thinking = hclust(dissim_children_thinking); hcb_children_thinking

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_children_thinking$merge
# plot(hcb_children_thinking$height)
plot(hcb_children_thinking,
     font.main = 1,
     main = "Children: Thinking (HCA)",
     sub = "",
     xlab = "",
     ylab = "Height")

# ...for feelings
dissim_children_feelings <- makeDissimByPredicate(selectPredicate = "feelings",
                                                selectAgeGroup = "children")

# Conduct hierarchical cluster analysis
hcb_children_feelings = hclust(dissim_children_feelings); hcb_children_feelings

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_children_feelings$merge
# plot(hcb_children_feelings$height)
plot(hcb_children_feelings,
     font.main = 1,
     main = "Children: Feelings (HCA)",
     sub = "",
     xlab = "",
     ylab = "Height")

# ...for hunger
dissim_children_hunger <- makeDissimByPredicate(selectPredicate = "hunger",
                                              selectAgeGroup = "children")

# Conduct hierarchical cluster analysis
hcb_children_hunger = hclust(dissim_children_hunger); hcb_children_hunger

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_children_hunger$merge
# plot(hcb_children_hunger$height)
plot(hcb_children_hunger,
     font.main = 1,
     main = "Children: Hunger (HCA)",
     sub = "",
     xlab = "",
     ylab = "Height")
