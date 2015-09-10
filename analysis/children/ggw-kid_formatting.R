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

# kid run 01 (2015-06-13)
files_run01 <- dir("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid_run_01/")

d_kid_run_01_raw <- data.frame()

for(i in 1:length(files_run01)) {
  # gather files
  f = files_run01[i]
  d_temp = read.csv(paste0("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid_run_01/", files_run01[i]))
  d_kid_run_01_raw = bind_rows(d_kid_run_01_raw, d_temp)
}

d_kid_run_01_raw = d_kid_run_01_raw %>% mutate(run = factor("run01"))

glimpse(d_kid_run_01_raw)

# kid run 02 (2015-09-10)
files_run02 <- dir("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid_run_02/")

d_kid_run_02_raw <- data.frame()

for(i in 1:length(files_run02)) {
  # gather files
  f = files_run02[i]
  d_temp = read.csv(paste0("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid_run_02/", files_run02[i]))
  d_kid_run_02_raw = bind_rows(d_kid_run_02_raw, d_temp)
}

d_kid_run_02_raw = d_kid_run_02_raw %>% mutate(run = factor("run02"))

# exclude kids by hand (for now)
d_kid_run_02_raw = d_kid_run_02_raw %>% filter(leftCharacter != "CHECK VIDEO" &
                                                 rightCharacter != "CHECK VIDEO")

glimpse(d_kid_run_02_raw)

# --- TIDYING -----------------------------------------------------------------

# clean up variables
d_tidy = d_kid_run_01_raw %>%
  full_join(d_kid_run_02_raw) %>% 
#   full_join(d_kid_run_03_raw) %>% #...etc.
  mutate(
    run = factor(run),
    subid = factor(subid),
    sequence = factor(sequence),
    dateOfBirth = parse_date_time(dateOfBirth, orders = "mdy"),
    dateOfTest = update(parse_date_time(dateOfTest, orders = "mdy"), 
                        year = 2015),
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

# filter out dropouts by hand
subidsToKeep = subidsToKeep %>%
  filter(subid != "d107" & # didn't finish
           subid != "k104" & # didn't finish
           subid != "k109" & # duplicate
           subid != "k111" & # duplicate
           subid != "k116" # duplicate
         )
                       
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

# write FULL DATASET to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01&02_2015-09-10_data_anonymized.csv")

# write RUN01 DATASET to de-identified csv file
write.csv(subset(d_tidy, run == "run01"), "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01_2015-06-13_data_anonymized.csv")

# write RUN02 DATASET to de-identified csv file
write.csv(subset(d_tidy, run == "run02"), "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-02_2015-09-10_data_anonymized.csv")