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
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01&02_2015-08-21_data_anonymized.csv")[-1] # get rid of column of obs numbers
# 
# # ... RUN01
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01_2015-06-13_data_anonymized.csv")[-1] # get rid of column of obs numbers

# ... RUN02
dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-02_2015-08-21_data_anonymized.csv")[-1] # get rid of column of obs numbers

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
# # ... to adults:
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
qplot(subset(demo, ageGroup == "children")$ageCalc, binwidth = 1/12,
      xlab = "\nAge (years)", ylab = "Count\n") +
  scale_x_continuous(breaks = seq(4,6,.25)) +
  theme(text = element_text(size = 20))
# qplot(subset(demo, ageGroup == "adults")$ageCalc)

# demo %>% group_by(ageGroup) %>% filter(age < 100) %>% summarise(mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm = T))
# qplot(subset(demo, ageGroup == "children")$age[demo$age < 100])
# qplot(subset(demo, ageGroup == "adults")$age[demo$age < 100])

######################################################## analysis & plots #####

# --- dissimilarities data-formatting function --------------------------------

makeDissimByPredicate <- function(selectPredicate) {
  tempDissim <- NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  tempDissim <- dd %>%
    filter(predicate %in% selectPredicate) %>%
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

# --- HIERARCHICAL CLUSTER ANALYSIS A -----------------------------------------
# IS THIS RIGHT???
# # Roughly equivalent to pca_A
# # Could also do the parallel version of pca_B
# 
# # Construct dissimilarity matrix
# d2 = as.dist((1-cor(d1))/2) # NEED TO CHECK ON WHY WE DIVIDE CORRELATIONS BY 2
# 
# # Conduct hierarchical cluster analysis
# hca = hclust(d2); hca
# 
# # Plot dendogram
# par(mfrow=c(1,2))
# rs1=hclust(d2)
# rs1$merge
# plot(rs1$height)
# plot(rs1)

# --- HIERARCHICAL CLUSTER ANALYSIS B -----------------------------------------
# Roughly equivalent to pca_B

# ...for all conditions
dissim <- makeDissimByPredicate(selectPredicate = c("thinking", "feelings", "hunger"))

# Conduct hierarchical cluster analysis
hcb = hclust(dissim); hcb

# Plot dendogram
# par(mfrow=c(1,2))
# hcb$merge
# plot(hcb$height)
plot(hcb)

# ...for thinking
dissim_thinking <- makeDissimByPredicate(selectPredicate = "thinking")

# Conduct hierarchical cluster analysis
hcb_thinking = hclust(dissim_thinking); hcb_thinking

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_thinking$merge
# plot(hcb_thinking$height)
plot(hcb_thinking)

# ...for feelings
dissim_feelings <- makeDissimByPredicate(selectPredicate = "feelings")

# Conduct hierarchical cluster analysis
hcb_feelings = hclust(dissim_feelings); hcb_feelings

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_feelings$merge
# plot(hcb_feelings$height)
plot(hcb_feelings)

# ...for hunger
dissim_hunger <- makeDissimByPredicate(selectPredicate = "hunger")

# Conduct hierarchical cluster analysis
hcb_hunger = hclust(dissim_hunger); hcb_hunger

# Plot dendogram
# par(mfrow=c(1,2))
# hcb_hunger$merge
# plot(hcb_hunger$height)
plot(hcb_hunger)

