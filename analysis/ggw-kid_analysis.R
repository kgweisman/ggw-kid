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

########################################################### summary stats #####

# --- DEMOGRAPHICS ------------------------------------------------------------

demo = dd %>% distinct(subid)

# total n
demo %>% summarise(n = length(subid))

# condition assignment
dd %>% group_by(sequence) %>% distinct(subid) %>% summarise(n = length(subid))

# gender
demo %>% count(gender)

# ethnicity
demo %>% count(ethnicity)

# age
demo %>% summarise(mean_age = mean(ageCalc, na.rm = T), sd_age = sd(ageCalc, na.rm = T))
qplot(demo$ageCalc)

# demo %>% filter(age < 100) %>% summarise(mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm = T))
# qplot(demo$age[demo$age < 100])

################################################### analysis & plots pt 1 #####

# --- PRINCIPAL COMPONENTS ANALYSIS A: ORIGINAL GGW2007 ----------------------

# --------> 2-factor PCA (NO rotation, using principal) ----------

# FROM GGW2007: "For each survey, each character appeared in 12 different comparisons, and mean relative ratings were computed for each character across all respondents to that survey. We merged data sets from the 18 mental capacity surveys to compute correlations between mental capacities across the characters, and submitted these to principal components factor analysis with varimax rotation." (SOM p. 3)

# extract factors
# NOTE: UNROTATED
pca_A2 = principal(d1, nfactors = 2, rotate = "none"); pca_A2

# extract eigenvalues
pca_A2$values

# extract PCA loadings
pca_A2_pc1 = pca_A2$loadings[,1]; sort(pca_A2_pc1)
pca_A2_pc2 = pca_A2$loadings[,2]; sort(pca_A2_pc2)

# --------------->-> plots ----------------------------------------------------

# plot PCs against each other
# NOTE: need to adjust "1:18" depending on how many conditions are run
ggplot(data.frame(pca_A2$loadings[1:3,]), aes(x = PC1, y = PC2, label = names(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings\n",
       x = "\nPrincipal Component 2",
       y = "Principal Component 1\n")

# FROM GGW2007: "We used the regression approach to estimate factor scores for each character." (SOM p. 3) 
# ?principal confirms that "component scores are found by regression"

# plot characters by principal components
ggplot(data.frame(pca_A2$scores), aes(x = PC1, y = PC2, label = rownames(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Raw character factor scores\n",
       x = "\nPrincipal Component 2",
       y = "Principal Component 1\n")

# FROM GGW2007: "For ease of interpretation, factor scores in Figure 1 were adjusted to be anchored at 0 and 1" (SOM p. 3)

# re-plot characters with rescaling (as in GGW2007 original), PC1 on y-axis
ggplot(data.frame(pca_A2$scores), 
       aes(x = rescale(PC1, to = c(0,1)), 
           y = rescale(PC2, to = c(0,1)), 
           label = rownames(d1))) +
  geom_point() +
  geom_text(angle = 0,
            vjust = -1,
            size = 6) +
  xlim(-0.01, 1.01) +
  ylim(-0.01, 1.01) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Adjusted character factor scores\n",
       x = "\nPrincipal Component 1, rescaled",
       y = "Principal Component 2, rescaled\n")

# --------> 2-factor PCA (varimax rotation, using principal) ----------

# FROM GGW2007: "For each survey, each character appeared in 12 different comparisons, and mean relative ratings were computed for each character across all respondents to that survey. We merged data sets from the 18 mental capacity surveys to compute correlations between mental capacities across the characters, and submitted these to principal components factor analysis with varimax rotation." (SOM p. 3)

# extract factors
# NOTE: ROTATED
pca_A2_rot = principal(d1, nfactors = 2, rotate = "varimax"); pca_A2_rot

# extract eigenvalues
pca_A2_rot$values

# extract PCA loadings
pca_A2_rot_pc1 = pca_A2_rot$loadings[,1]; sort(pca_A2_rot_pc1)
pca_A2_rot_pc2 = pca_A2_rot$loadings[,2]; sort(pca_A2_rot_pc2)

# --------------->-> plots ----------------------------------------------------

# plot PCs against each other
# NOTE: need to adjust "1:18" depending on how many conditions are run
ggplot(data.frame(pca_A2_rot$loadings[1:3,]), aes(x = PC1, y = PC2, label = names(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings\n",
       x = "\nPrincipal Component 2",
       y = "Principal Component 1\n")

# FROM GGW2007: "We used the regression approach to estimate factor scores for each character." (SOM p. 3) 
# ?principal confirms that "component scores are found by regression"

# plot characters by principal components
ggplot(data.frame(pca_A2_rot$scores), aes(x = PC1, y = PC2, label = rownames(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Raw character factor scores\n",
       x = "\nPrincipal Component 2",
       y = "Principal Component 1\n")

# FROM GGW2007: "For ease of interpretation, factor scores in Figure 1 were adjusted to be anchored at 0 and 1" (SOM p. 3)

# re-plot characters with rescaling (as in GGW2007 original), PC1 on y-axis
ggplot(data.frame(pca_A2_rot$scores), 
       aes(x = rescale(PC1, to = c(0,1)), 
           y = rescale(PC2, to = c(0,1)), 
           label = rownames(d1))) +
  geom_point() +
  geom_text(angle = 0,
            vjust = -1,
            size = 6) +
  xlim(-0.01, 1.01) +
  ylim(-0.01, 1.01) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Adjusted character factor scores\n",
       x = "\nPrincipal Component 1, rescaled",
       y = "Principal Component 2, rescaled\n")

################################################### analysis & plots pt 2 #####

# -- MULTIDIMENSIONAL SCALING ANALYSIS A --------------------------------------

# --------> data formatting ---------------------------------------------------
dissim = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim <- dd %>%
  filter(phase == "test") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim <- dissim %>%
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
dissim <- dissim %>%
  select(predicate, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for baby, NA row for stapler
dissim <- dissim %>%
  mutate(baby = NA,
         character1 = as.character(character1)) %>%
  rbind(c("stapler", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim = dissim[, c(1, 11, 2:10)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = names[names != "strawberries"]
names = names[names != "grapes"]
names = names[names != "icecream"]
names = names[names != "pizza"]

dissim = dissim[-1]
rownames(dissim) = names
colnames(dissim) = names

# fill in lower triangle matrix
for(i in 1:9) {
  for(j in (i+1):10) {
    dissim[j,i] = dissim[i,j]
  }
}

dissim = as.dist(dissim)

# --------> non-metric (ordinal) MDS ------------------------------------------
# NOTE: could also explore fitting with more than 2 dimensions...

# do MDS
mds_Aordinal = mds(dissim, ndim = 2, type = "ordinal")
summary(mds_Aordinal)
mds_Aordinal

# --------------->-> plots ----------------------------------------------------

# plot dimension space
plot(mds_Aordinal,
     plot.type = "confplot",
     xlim = c(-1, 1),
     ylim = c(-1, 1),     
     main = "MDS solution: All conditions")

# plot space and stress (bigger bubble = better fit)
plot(mds_Aordinal, plot.type = "bubbleplot",
     xlim = c(-1, 1),
     ylim = c(-1, 1),     
     main = "MDS bubble plot: All conditions")

# plot stress (higher = worse fit)
plot(mds_Aordinal, plot.type = "stressplot",
     main = "MDS stress: All conditions")

# Shepard plot
plot(mds_Aordinal, plot.type = "Shepard",
     main = "MDS Shepard plot: All conditions")

# plot residuals
plot(mds_Aordinal, plot.type = "resplot",
     main = "MDS residuals: All conditions")

################################################### analysis & plots pt 3 #####

# -- MULTIDIMENSIONAL SCALING ANALYSIS B --------------------------------------

# --------> data formatting ---------------------------------------------------

# --------------->-> predicate: THINKING -------------------------------------

dissim_thinking = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_thinking <- dd %>%
  filter(predicate == "thinking") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_thinking$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_thinking <- dissim_thinking %>%
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

# make upper matrix of dissim_thinkingilarity values
dissim_thinking <- dissim_thinking %>%
  select(predicate, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for baby, NA row for stapler
dissim_thinking <- dissim_thinking %>%
  mutate(baby = NA,
         character1 = as.character(character1)) %>%
  rbind(c("stapler", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_thinking = dissim_thinking[, c(1, 11, 2:10)]

# rename rows and columns
dissim_thinking = dissim_thinking[-1]
rownames(dissim_thinking) = names
colnames(dissim_thinking) = names

# fill in lower triangle matrix
for(i in 1:9) {
  for(j in (i+1):10) {
    dissim_thinking[j,i] = dissim_thinking[i,j]
  }
}

dissim_thinking = as.dist(dissim_thinking)

# --------------->-> predicate: FEELINGS -------------------------------------

dissim_feelings = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_feelings <- dd %>%
  filter(predicate == "feelings") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_feelings$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_feelings <- dissim_feelings %>%
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

# make upper matrix of dissim_feelingsilarity values
dissim_feelings <- dissim_feelings %>%
  select(predicate, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for baby, NA row for stapler
dissim_feelings <- dissim_feelings %>%
  mutate(baby = NA,
         character1 = as.character(character1)) %>%
  rbind(c("stapler", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_feelings = dissim_feelings[, c(1, 11, 2:10)]

# rename rows and columns
dissim_feelings = dissim_feelings[-1]
rownames(dissim_feelings) = names
colnames(dissim_feelings) = names

# fill in lower triangle matrix
for(i in 1:9) {
  for(j in (i+1):10) {
    dissim_feelings[j,i] = dissim_feelings[i,j]
  }
}

dissim_feelings = as.dist(dissim_feelings)

# --------------->-> predicate: HUNGER ----------------------------------------

dissim_hunger = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_hunger <- dd %>%
  filter(predicate == "hunger") %>%
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

# make upper matrix of dissim_hungerilarity values
dissim_hunger <- dissim_hunger %>%
  select(predicate, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for baby, NA row for stapler
dissim_hunger <- dissim_hunger %>%
  mutate(baby = NA,
         character1 = as.character(character1)) %>%
  rbind(c("stapler", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_hunger = dissim_hunger[, c(1, 11, 2:10)]

# rename rows and columns
dissim_hunger = dissim_hunger[-1]
rownames(dissim_hunger) = names
colnames(dissim_hunger) = names

# fill in lower triangle matrix
for(i in 1:9) {
  for(j in (i+1):10) {
    dissim_hunger[j,i] = dissim_hunger[i,j]
  }
}

dissim_hunger = as.dist(dissim_hunger)

# --------> non-metric (ordinal) MDS ------------------------------------------
# NOTE: could also explore fitting with more than 2 dimensions...

# do MDS: THINKING
mds_thinking_Aordinal = mds(dissim_thinking, ndim = 2, type = "ordinal")
summary(mds_thinking_Aordinal)
mds_thinking_Aordinal

# do MDS: FEELINGS
mds_feelings_Aordinal = mds(dissim_feelings, ndim = 2, type = "ordinal")
summary(mds_feelings_Aordinal)
mds_feelings_Aordinal

# do MDS: HUNGER
mds_hunger_Aordinal = mds(dissim_hunger, ndim = 2, type = "ordinal")
summary(mds_hunger_Aordinal)
mds_hunger_Aordinal

# --------------->-> plots ----------------------------------------------------

# plot dimension space
plot(mds_thinking_Aordinal, 
     plot.type = "confplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS solution: THINKING",
     xlab = "")
plot(mds_feelings_Aordinal, 
     plot.type = "confplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS solution: FEELINGS",
     xlab = "")
plot(mds_hunger_Aordinal, 
     plot.type = "confplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS solution: HUNGER",
     xlab = "")

# plot space and stress (bigger bubble = better fit)
plot(mds_thinking_Aordinal, 
     plot.type = "bubbleplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS bubble plot: THINKING",
     xlab = "")
plot(mds_feelings_Aordinal, 
     plot.type = "bubbleplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS bubble plot: FEELINGS",
     xlab = "")
plot(mds_hunger_Aordinal, 
     plot.type = "bubbleplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS bubble plot: HUNGER",
     xlab = "")

# plot stress (higher = worse fit)
plot(mds_thinking_Aordinal, 
     plot.type = "stressplot", 
     main = "MDS stress: THINKING",
     xlab = "")
plot(mds_feelings_Aordinal, 
     plot.type = "stressplot", 
     main = "MDS stress: FEELINGS",
     xlab = "")
plot(mds_hunger_Aordinal, 
     plot.type = "stressplot", 
     main = "MDS stress: HUNGER",
     xlab = "")

# Shepard plot
plot(mds_thinking_Aordinal, 
     plot.type = "Shepard", 
     main = "MDS Shepard plot: THINKING",
     xlab = "")
plot(mds_feelings_Aordinal, 
     plot.type = "Shepard", 
     main = "MDS Shepard plot: FEELINGS",
     xlab = "")
plot(mds_hunger_Aordinal, 
     plot.type = "Shepard", 
     main = "MDS Shepard plot: HUNGER",
     xlab = "")

# plot residuals
plot(mds_thinking_Aordinal, 
     plot.type = "resplot", 
     main = "MDS residuals: THINKING",
     xlab = "")
plot(mds_feelings_Aordinal, 
     plot.type = "resplot", 
     main = "MDS residuals: FEELINGS",
     xlab = "")
plot(mds_hunger_Aordinal, 
     plot.type = "resplot", 
     main = "MDS residuals: HUNGER",
     xlab = "")

################################################### analysis & plots pt 4 #####

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

# Conduct hierarchical cluster analysis
hcb = hclust(dissim); hcb

# Plot dendogram
par(mfrow=c(1,2))
hcb$merge
plot(hcb$height)
plot(hcb)

# ...for thinking

# Conduct hierarchical cluster analysis
hcb_thinking = hclust(dissim_thinking); hcb_thinking

# Plot dendogram
par(mfrow=c(1,2))
hcb_thinking$merge
plot(hcb_thinking$height)
plot(hcb_thinking)

# ...for feelings

# Conduct hierarchical cluster analysis
hcb_feelings = hclust(dissim_feelings); hcb_feelings

# Plot dendogram
par(mfrow=c(1,2))
hcb_feelings$merge
plot(hcb_feelings$height)
plot(hcb_feelings)

# ...for hunger

# Conduct hierarchical cluster analysis
hcb_hunger = hclust(dissim_hunger); hcb_hunger

# Plot dendogram
par(mfrow=c(1,2))
hcb_hunger$merge
plot(hcb_hunger$height)
plot(hcb_hunger)