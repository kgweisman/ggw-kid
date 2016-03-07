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

# kid run 02 (2016-03-07)
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

# make list of 2015 subids
# ids2015 <- levels(d_tidy$subid)
# ids2015 <- ids2015[!ids2015 %in% c("k809", "k810", "d801", "d802", "d803", "d804", "d805", "d806", "d807")]

ids2015 <- c("D101", "D102", "D103", "d104", "d105", "d106", "d107", "d108", 
             "d109", "d110", "d111", "d112", "d1120", "d1121", "d1122", "d1123", 
             "d1124", "d1125", "d1126", "d1127", "d1128", "d1129", "d113", 
             "d1130", "d1131", "d1132", "d1133", "d1134", "d1135", "d1136", 
             "d1137", "d1138", "d1139", "d114", "d1140", "d1141", "d115", 
             "d116", "d117", "d118", "d119", "J01", "J02", "J03", "J04", "J05", 
             "J06", "J07", "J08", "J09", "k02", "k03", "k04", "k05", "k06", 
             "k07", "k101", "k102", "k103", "k104", "k105", "k106", "k107", 
             "k108", "k109", "k110", "k111", "k112", "k1120", "k1121", "k1122", 
             "k1123", "k1124", "k113", "k114", "k115", "k116", "k117", "k118", 
             "k119", "k501", "k502", "k503", "k504", "k505", "k506", "k507", 
             "k508", "k509", "k510", "k511", "k512", "k513", "k514", "k515", 
             "k516", "k517", "k518", "k801", "k802", "k803", "k804", "k805", 
             "k806", "k807", "k808", "k901", "k902", "k903", "k904", "k905", 
             "k906" )

# clean up variables
d_tidy = d_kid_run_01_raw %>%
  full_join(d_kid_run_02_raw) %>% 
#   full_join(d_kid_run_03_raw) %>% #...etc.
  mutate(
    run = factor(run),
    subid = factor(subid),
    whichYear = ifelse(as.character(subid) %in% ids2015, 2015, 2016),
    sequence = factor(sequence),
    dateOfBirth = parse_date_time(dateOfBirth, orders = "mdy"),
    dateOfTest = update(parse_date_time(dateOfTest, orders = "mdy"), 
                        year = whichYear),
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
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01&02_2016-03-07_data_anonymized.csv")

# write RUN01 DATASET to de-identified csv file
write.csv(subset(d_tidy, run == "run01"), "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-01_2015-06-13_data_anonymized.csv")

# write RUN02 DATASET to de-identified csv file
write.csv(subset(d_tidy, run == "run02"), "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/children/kid-run-02_2016-03-07_data_anonymized.csv")

