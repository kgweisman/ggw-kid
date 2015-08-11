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
library(abind)

# clear environment
rm(list=ls())

# clear graphics
dev.off()

# --- IMPORTING DATA ----------------------------------------------------------

# --------> adults ----------------------------------------------------------

# read in data: individual scores
dd_adults = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/run-01_2015-05-09_data_anonymized.csv")[-1] # get rid of column of obs numbers

# add in ageGroup
dd_adults <- dd_adults %>%
  mutate(ageGroup = "adults")

glimpse(dd_adults)

# --------> children ----------------------------------------------------------

# read in data: individual scores
# # ... FULL DATASET
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01&02_2015-08-04_data_anonymized.csv")[-1] # get rid of column of obs numbers
# 
# # ... RUN01
# dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01_2015-06-13_data_anonymized.csv")[-1] # get rid of column of obs numbers

# ... RUN02
dd_children = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-02_2015-08-04_data_anonymized.csv")[-1] # get rid of column of obs numbers

# add in ageGroup
dd_children <- dd_children %>%
  mutate(ageGroup = "children")

glimpse(dd_children)

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
# dd = dd_children

# --------------->-> exclude stapler trials -----------------------------------

dd_nostapler <- dd %>%
  filter(leftCharacter != "stapler" & rightCharacter != "stapler") %>%
  mutate(leftCharacter = factor(leftCharacter),
         rightCharacter = factor(rightCharacter))

# set group of interest
# ... to exact:
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
  
  return(tempDf)
  
}

# --- BRADLEY-TERRY-LUCE (BTL) ANALYSIS ---------------------------------------

# --------> adults ------------------------------------------------------------

# --------------->-> all predicates -------------------------------------------

M_adults_all <- makeM(selectAgeGroup = "adults")
btl_adults_all <- eba(M_adults_all); summary(btl_adults_all)
scaleVals_adults_all <- uscale(btl_adults_all, norm = NULL)
dotchart(scaleVals_adults_all, pch=16,
         main = "Adults: All predicates")

# --------------->-> predicate: HUNGER ----------------------------------------

M_adults_hunger <- makeM(selectPredicate = "hunger",
                         selectAgeGroup = "adults")
btl_adults_hunger <- eba(M_adults_hunger); summary(btl_adults_hunger)
scaleVals_adults_hunger <- uscale(btl_adults_hunger, norm = NULL)
dotchart(scaleVals_adults_hunger, pch=16,
         main = "Adults: Hunger")

# --------------->-> predicate: FEELINGS --------------------------------------

M_adults_feelings <- makeM(selectPredicate = "feelings",
                           selectAgeGroup = "adults")
btl_adults_feelings <- eba(M_adults_feelings); summary(btl_adults_feelings)
scaleVals_adults_feelings <- uscale(btl_adults_feelings, norm = NULL)
dotchart(scaleVals_adults_feelings, pch=16,
         main = "Adults: Feelings")

# --------------->-> predicate: THINKING --------------------------------------

M_adults_thinking <- makeM(selectPredicate = "thinking",
                           selectAgeGroup = "adults")
btl_adults_thinking <- eba(M_adults_thinking); summary(btl_adults_thinking)
scaleVals_adults_thinking <- uscale(btl_adults_thinking, norm = NULL)
dotchart(scaleVals_adults_thinking, pch=16,
         main = "Adults: Thinking")

# --------> children ------------------------------------------------------------

# --------------->-> all predicates -------------------------------------------

M_children_all <- makeM(selectAgeGroup = "children")
btl_children_all <- eba(M_children_all); summary(btl_children_all)
scaleVals_children_all <- uscale(btl_children_all, norm = NULL)
dotchart(scaleVals_children_all, pch=16,
         main = "Children: All predicates")

# --------------->-> predicate: HUNGER ----------------------------------------

M_children_hunger <- makeM(selectPredicate = "hunger",
                         selectAgeGroup = "children")
btl_children_hunger <- eba(M_children_hunger); summary(btl_children_hunger)
scaleVals_children_hunger <- uscale(btl_children_hunger, norm = NULL)
dotchart(scaleVals_children_hunger, pch=16,
         main = "Children: Hunger")

# --------------->-> predicate: FEELINGS --------------------------------------

M_children_feelings <- makeM(selectPredicate = "feelings",
                           selectAgeGroup = "children")
btl_children_feelings <- eba(M_children_feelings); summary(btl_children_feelings)
scaleVals_children_feelings <- uscale(btl_children_feelings, norm = NULL)
dotchart(scaleVals_children_feelings, pch=16,
         main = "Children: Feelings")

# --------------->-> predicate: THINKING --------------------------------------

M_children_thinking <- makeM(selectPredicate = "thinking",
                           selectAgeGroup = "children")
btl_children_thinking <- eba(M_children_thinking); summary(btl_children_thinking)
scaleVals_children_thinking <- uscale(btl_children_thinking, norm = NULL)
dotchart(scaleVals_children_thinking, pch=16,
         main = "Children: Thinking")

# --------> test differences between groupings --------------------------------

# --------------->-> adults: among predicates ---------------------------------

M_adults_compPredicates <- abind(M_adults_hunger, M_adults_feelings, M_adults_thinking, 
                                 along = 3,
                                 new.names = c("hunger", "feelings", "thinking"))
# double-check same as above
# dotchart(uscale(eba(M_adults_compPredicates[,,"hunger"])))
# dotchart(uscale(eba(M_adults_compPredicates[,,"feelings"])))
# dotchart(uscale(eba(M_adults_compPredicates[,,"thinking"])))

test_adults_compPredicates <- group.test(M_adults_compPredicates)
test_adults_compPredicates

# --------------->-> children: among predicates ---------------------------------

M_children_compPredicates <- abind(M_children_hunger, M_children_feelings, M_children_thinking, 
                                 along = 3,
                                 new.names = c("hunger", "feelings", "thinking"))
# double-check same as above
# dotchart(uscale(eba(M_children_compPredicates[,,"hunger"])))
# dotchart(uscale(eba(M_children_compPredicates[,,"feelings"])))
# dotchart(uscale(eba(M_children_compPredicates[,,"thinking"])))

test_children_compPredicates <- group.test(M_children_compPredicates)
test_children_compPredicates

# --------------->-> adults vs. children: across predicates -------------------

M_compAgeGroups <- abind(M_adults_all, M_children_all, 
                                   along = 3,
                                   new.names = c("adults", "children"))
# double-check same as above
# dotchart(uscale(eba(M_compAgeGroups[,,"adults"])))
# dotchart(uscale(eba(M_compAgeGroups[,,"children"])))

test_compAgeGroups <- group.test(M_compAgeGroups)
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

test_compInteraction <- group.test(M_compInteraction)
test_compInteraction

# hunger only
M_compInteraction_hunger <- abind(M_adults_hunger, M_children_hunger, 
                           along = 3,
                           new.names = c("adults.hunger", 
                                         "children.hunger"))
test_compInteraction_hunger <- group.test(M_compInteraction_hunger)
test_compInteraction_hunger

# feelings only
M_compInteraction_feelings <- abind(M_adults_feelings, M_children_feelings, 
                                  along = 3,
                                  new.names = c("adults.feelings", 
                                                "children.feelings"))
test_compInteraction_feelings <- group.test(M_compInteraction_feelings)
test_compInteraction_feelings

# thinking only
M_compInteraction_thinking <- abind(M_adults_thinking, M_children_thinking, 
                                    along = 3,
                                    new.names = c("adults.thinking", 
                                                  "children.thinking"))
test_compInteraction_thinking <- group.test(M_compInteraction_thinking)
test_compInteraction_thinking