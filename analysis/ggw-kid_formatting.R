# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(stats)
library(stringr)
library(lubridate)

# clear environment
rm(list=ls())

# --- READING IN DATA OBJECTS -------------------------------------------------

# kid run 01 (2015-05-28)
files_run01 <- dir("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid_run_01/")

d_kid_run_01_raw <- data.frame()

for(i in 1:length(files_run01)) {
  # gather files
  f = files_run01[i]
  d_temp = read.csv(paste0("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid_run_01/", files_run01[i]))
  d_kid_run_01_raw = bind_rows(d_kid_run_01_raw, d_temp)
}

glimpse(d_kid_run_01_raw)

# --- TIDYING -----------------------------------------------------------------

# clean up variables
d_tidy = d_kid_run_01_raw %>%
#   full_join(d_kid_run_02_raw) %>% ...etc.
  mutate(
    subid = factor(subid),
    sequence = factor(sequence),
    dateOfBirth = parse_date_time(dateOfBirth, orders = "mdy"),
    dateOfTest = parse_date_time(dateOfTest, orders = "mdy"),
    ageCalc = as.numeric((dateOfTest - dateOfBirth)/365),
    gender = factor(gender),
    ethnicity = factor(ethnicity),
    predicate = factor(predicate),
    leftCharacter = factor(leftCharacter),
    rightCharacter = factor(rightCharacter),
    response = factor(response),
    responseNum =
      ifelse(response == "much more left", -2,
             ifelse(response == "slightly more left", -1,
                    ifelse(response == "both", 0,
                           ifelse(response == "slightly more right", 1,
                                  ifelse(response == "much more right", 2, NA))))),
    experimenter = factor(experimenter),
    testingSite = factor(testingSite)
    ) %>%
  select(-dateOfBirth, -dateOfTest)

glimpse(d_tidy)

# --- FILTERING BY PROBLEMS AND BY PRACTICE TRIAL #1 (COLDER) -----------------

# exclude people who said they had problems loading pictures
# NOTE: make sure to redo whenever new participants are added!!
# d_tidy = d_tidy %>%
#   filter(subid != "XX",
#          subid != "XX")

subidsToKeep = d_tidy %>%
  select(-ageCalc) %>%
  filter(predicate == "colder") %>%
  mutate(drop = ifelse(leftCharacter == "icecream",
                       ifelse(responseNum >= 0, 
                              "drop", 
                              "keep"),
                       ifelse(rightCharacter == "icecream",
                              ifelse(responseNum <= 0,
                                     "drop",
                                     "keep"),
                              NA))) %>%
  select(sequence, subid, drop) %>%
  filter(drop != "drop")
                       
d_tidy = d_tidy %>%
  filter(is.element(subid, subidsToKeep$subid))

# --- EVENING OUT CONDITION ASSIGNMENT ----------------------------------------

# # randomly choose N participants from each sequence
# n = 10 # set number to choose
# subidList = d_tidy %>%
#   select(sequence, subid) %>%
#   distinct() %>%
#   group_by(sequence) %>%
#   sample_n(n, replace = FALSE)
# 
# d_tidy = d_tidy %>%
#   filter(is.element(subid, subidList$subid))
#   
# # check
# d_tidy %>% group_by(sequence) %>% select(subid) %>% unique() %>% summarise(count = length(subid))

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# # write subidList to csv file
# write.csv(subidList, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/randomized_subidList.csv")

# write data to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid-run-01_2015-05-28_data_anonymized.csv")

d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/kid-run-01_2015-05-28_data_anonymized.csv")[-1] # get rid of column of obs numbers

