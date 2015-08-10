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
library(eba)

# clear environment
rm(list=ls())

# clear graphics
dev.off()

# --- IMPORTING DATA ----------------------------------------------------------

# read in data: individual scores
# # ... FULL DATASET
# dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid-run-01&02_2015-08-04_data_anonymized.csv")[-1] # get rid of column of obs numbers
# 
# # ... RUN01
# dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid-run-01_2015-06-13_data_anonymized.csv")[-1] # get rid of column of obs numbers

# ... RUN02
dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid-run-02_2015-08-04_data_anonymized.csv")[-1] # get rid of column of obs numbers

glimpse(dd)

# --- FILTERING BY ETHNICITY --------------------------------------------------

# dd_white = dd %>%
#   filter(ethnicity == "white")
# 
# dd_nonwhite = dd %>%
#   filter(ethnicity != "white" & 
#            ethnicity != "NA" & 
#            ethnicity != "other_prefNo")

# set group of interest
# ... to white:
# dd = dd_white

# # ... to nonwhite:
# dd = dd_nonwhite

# --- FILTERING BY AGE --------------------------------------------------------

dd_exact = dd %>%
  filter(ageCalc >= 4.5 & ageCalc <= 5.5)

# set group of interest
# ... to exact:
dd = dd_exact

# --- FILTERING BY FINISHED ---------------------------------------------------

dd_finished = dd %>%
  filter(subid != "d107" &
           subid != "k104")

# set group of interest
# ... to exact:
dd = dd_finished

# --- FILTERING BY NO STAPLER -------------------------------------------------

dd_nostapler <- dd %>%
  filter(leftCharacter != "stapler" & rightCharacter != "stapler") %>%
  mutate(leftCharacter = factor(leftCharacter),
         rightCharacter = factor(rightCharacter))

# set group of interest
# ... to exact:
# dd = dd_nostapler

# --- FORMATTING DATA ---------------------------------------------------------

# need absolute choice frequencies: row stimuli are chosen over column stimuli
makeM <- function(selectPredicate) {
  tempM <- NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  tempM <- dd %>%
    filter(predicate %in% selectPredicate) %>%
    mutate(character1 = array(),
           character2 = array())
  
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
  
  # create empty dataframe
  tempDf <- data.frame(matrix(0, nrow = 10, ncol = 10))
  
  # name rows and columns with character names
  characterNames <- with(subset(dd, phase == "test"), levels(factor(leftCharacter)))
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
  
  return(tempDf)
  
}

# --- EBA ANALYSIS ------------------------------------------------------------

# --------> all predicates ----------------------------------------------------

M_all <- makeM(selectPredicate = c("thinking", "feelings", "hunger"))

eba_all <- eba(M_all)
summary(eba_all)
# plot(eba_all)
scaleVals_all <- uscale(eba_all)
dotchart(scaleVals_all, pch=16,
         main = "All predicates")

# --------> predicate: THINKING -----------------------------------------------

M_thinking <- makeM(selectPredicate = "thinking")
eba_thinking <- eba(M_thinking)
summary(eba_thinking)
# plot(eba_thinking)
scaleVals_thinking <- uscale(eba_thinking)
dotchart(scaleVals_thinking, pch=16, xlim = c(0, .5),
         main = "Thinking")

# --------> predicate: FEELINGS -----------------------------------------------

M_feelings <- makeM(selectPredicate = "feelings")
eba_feelings <- eba(M_feelings)
summary(eba_feelings)
# plot(eba_feelings)
scaleVals_feelings <- uscale(eba_feelings)
dotchart(scaleVals_feelings, pch=16, xlim = c(0, .5),
         main = "Feelings")

# --------> predicate: HUNGER -------------------------------------------------

M_hunger <- makeM(selectPredicate = "hunger")
eba_hunger <- eba(M_hunger)
summary(eba_hunger)
# plot(eba_hunger)
scaleVals_hunger <- uscale(eba_hunger)
dotchart(scaleVals_hunger, pch=16, xlim = c(0, .5),
         main = "Hunger")
