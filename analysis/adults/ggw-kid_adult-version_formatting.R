# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(jsonlite)
# library(stats)
library(stringr)

# clear environment
rm(list=ls())

# set up function for reading in data from JSON objects
jsonFormat = function(wd, runName) {
  # NOTE: requires dplyr and jsonlite packages
  # library(jsonlite)
  # library(dplyr)
  
  # set working directory
  setwd(wd)
  
  # gather files
  files = dir("production-results/")
  
  # make dataframe
  d.raw = data.frame()
  
  for(i in 1:length(files)) {
    
    # gather files
    f = files[i]
    jf <- paste0("production-results/", f)
    
    # parse JSON object
    jd <- fromJSON(paste(readLines(jf), collapse=""))
    
    # store relevant variables in dataframe 
    id <- data.frame(
      # run
      run = runName,
      
      # subject-level data: identity
      subid = paste0(runName, "_", i),
      sequence = jd$answers$data$newData$sequence,
      
      # subject-level data: demographics
      country = ifelse(
        is.null(jd$answers$data$newData$country) == TRUE, NA,
        jd$answers$data$newData$country),    
      age = ifelse(
        is.null(jd$answers$data$newData$age) == TRUE, NA,
        jd$answers$data$newData$age),
      gender = ifelse(
        is.null(jd$answers$data$newData$gender) == TRUE, NA,
        jd$answers$data$newData$gender),
      englishNative = ifelse(
        is.null(jd$answers$data$newData$englishNative) == TRUE, NA,
        jd$answers$data$newData$englishNative),
      ethnicity = ifelse(
        is.null(jd$answers$data$newData$ethnicity) == TRUE, NA,
        paste(jd$answers$data$newData$ethnicity, collapse = ', ')),
      education = ifelse(
        is.null(jd$answers$data$newData$education) == TRUE, NA,
        jd$answers$data$newData$education),
      religionChild = ifelse(
        is.null(jd$answers$data$newData$religionChild) == TRUE, NA,
        paste(jd$answers$data$newData$religionChild, collapse = ', ')),
      religionNow = ifelse(
        is.null(jd$answers$data$newData$religionNow) == TRUE, NA,
        paste(jd$answers$data$newData$religionNow, collapse = ', ')),
      job = ifelse(
        is.null(jd$answers$data$newData$job) == TRUE | 
          jd$answers$data$newData$job == "", NA,
        jd$answers$data$newData$job),
      
      # subject-level data: comprehension checks
      cc1 = ifelse(
        is.null(jd$answers$data$newData$comprehensionCheck1) == TRUE, "NA",
        paste(jd$answers$data$newData$comprehensionCheck1, collapse = ', ')),
      cc2 = ifelse(
        is.null(jd$answers$data$newData$comprehensionCheck2) == TRUE, "NA",
        paste(jd$answers$data$newData$comprehensionCheck2, collapse = ', ')),
      cc3 = ifelse(
        is.null(jd$answers$data$newData$comprehensionCheck3) == TRUE, "NA",
        paste(jd$answers$data$newData$comprehensionCheck3, collapse = ', ')),
      
      # subject-level data: open-ended responses
      comments = ifelse(
        is.null(jd$answers$data$newData$comments) | 
          jd$answers$data$newData$comments == "", NA,
        jd$answers$data$newData$comments),
      
      # trial-level data:                    
      phase = jd$answers$data$newData$trialData$phase,
      predicate = jd$answers$data$newData$trialData$predicate,
      trialNum = jd$answers$data$newData$trialData$trialNum,
      leftCharacter = jd$answers$data$newData$trialData$leftCharacter,
      rightCharacter = jd$answers$data$newData$trialData$rightCharacter,
      response = jd$answers$data$newData$trialData$response,
      rt = jd$answers$data$newData$trialData$rt)
    
    # bind into same dataframe
    d.raw = bind_rows(d.raw, id)
  }
  
  return(d.raw)
}

# --- READING IN DATA OBJECTS -------------------------------------------------

# US run 01 (2015-05-09)
d_us_run_01 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/us_run-01/",
  runName = "us_run_01")

# India run 01 (2015-08-15)
d_india_run_01.0 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01/",
  runName = "india_run_01.0")
d_india_run_01.1 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.1/",
  runName = "india_run_01.1")
d_india_run_01.2 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.2/",
  runName = "india_run_01.2")
# SAME AS RUN 01.2!
# d_india_run_01.3 = jsonFormat(
#   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.3/",
#   runName = "india_run_01.3")
# d_india_run_01.4 = jsonFormat(
#   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.4/",
#   runName = "india_run_01.4")
# d_india_run_01.5 = jsonFormat(
#   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.5/",
#   runName = "india_run_01.5")
d_india_run_01.6 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.6/",
  runName = "india_run_01.6")
d_india_run_01.7 = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.7/",
  runName = "india_run_01.7")

# --- TIDYING -----------------------------------------------------------------

# clean up variables
d_tidy <- d_us_run_01 %>%
#   full_join(d_us_run_02) %>% ...etc.
  full_join(d_india_run_01.0) %>%
  full_join(d_india_run_01.1) %>%
  full_join(d_india_run_01.2) %>%
  # full_join(d_india_run_01.3) %>%
  # full_join(d_india_run_01.4) %>%
  # full_join(d_india_run_01.5) %>%
  full_join(d_india_run_01.6) %>%
  full_join(d_india_run_01.7) %>%
  mutate(
    run = factor(run),
    subid = factor(subid),
    country_selfrep = factor(country),
    country = factor(ifelse(grepl("india", subid) == T, "india", 
                            ifelse(grepl("us", subid) == T, "us",
                                   NA))),
    sequence = factor(sequence),
    age = as.numeric(age),
    gender = factor(gender),
    ethnicity = factor(ethnicity),
    education = factor(education),
    religionChild = factor(religionChild),
    religionNow = factor(religionNow),
    englishNative = factor(englishNative),
    cc1 = factor(cc1),
    cc2 = factor(cc2),
    cc3 = factor(cc3),
    job = factor(job),
    predicate = factor(predicate),
    leftCharacter = factor(leftCharacter),
    rightCharacter = factor(rightCharacter),
    response = factor(response),
    responseNum =
      ifelse(response == "much more left", -2,
             ifelse(response == "slightly more left", -1,
                    ifelse(response == "both equally", 0,
                           ifelse(response == "slightly more right", 1,
                                  ifelse(response == "much more right", 2, NA))))))

glimpse(d_tidy)

# --- FILTERING BY REPORTED PROBLEMS AND BY PRACTICE TRIAL #1 (COLDER) --------

# view comments
comments = d_tidy %>%
  #   filter(country == "india") %>%
  select(sequence, subid, comments) %>%
  distinct() %>%
  filter(comments != "NA")
# View(comments)

# exclude people who said they had problems loading pictures
# NOTE: make sure to redo whenever new participants are added!!
d_tidy <- d_tidy %>%
  filter(subid != "us_run_01_10",
         subid != "us_run_01_34")

# exclude people who failed the practice trials or were marked as "drop"
subidsToKeep = d_tidy %>%
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
                       
d_tidy <- d_tidy %>%
  filter(is.element(subid, subidsToKeep$subid))

# add info on whether they passed the comprehension check in 1 try
d_tidy <- d_tidy %>%
  mutate(cc1_attempts = 1 + str_count(cc1, pattern = ","),
         cc2_attempts = 1 + str_count(cc2, pattern = ","),
         cc3_attempts = 1 + str_count(cc3, pattern = ","))

d_pass1 <- d_tidy %>% 
  filter(cc1_attempts < 2 & cc2_attempts < 2 & cc3_attempts < 2)

d_pass2 <- d_tidy %>% 
  filter(cc1_attempts < 3 & cc2_attempts < 3 & cc3_attempts < 3)

# screen by comprehension check (option)
# d_tidy <- d_pass1
d_tidy <- d_pass2

# --- EVENING OUT CONDITION ASSIGNMENT ----------------------------------------

# NEED TO ADD MORE INDIAN Ps!

# # randomly choose N participants from each sequence
# n = 10 # set number to choose
# subidList = d_tidy %>%
#   select(country, sequence, subid) %>%
#   distinct() %>%
#   group_by(country, sequence) %>%
#   sample_n(n, replace = FALSE)
# 
# d_tidy <- d_tidy %>%
#   filter(is.element(subid, subidList$subid))

# (temp) choose ALL participants from each sequence
subidList = d_tidy %>%
  select(country, sequence, subid) %>%
  distinct() %>%
  group_by(country, sequence)

d_tidy <- d_tidy %>%
  filter(is.element(subid, subidList$subid))
  
# check
d_tidy %>% group_by(country, sequence) %>% select(subid) %>% 
  unique() %>% summarise(count = length(subid))

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# # write subidList to csv file
write.csv(subidList, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/randomized_subidList.csv")

# write data to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/run-01_2015-08-17_data_anonymized.csv")

d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/run-01_2015-08-17_data_anonymized.csv")[-1] # get rid of column of obs numbers

