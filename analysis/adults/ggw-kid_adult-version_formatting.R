# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(jsonlite)
library(stats)
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

# --- TIDYING -----------------------------------------------------------------

# clean up variables
d_tidy = d_us_run_01 %>%
#   full_join(d_us_run_02) %>% ...etc.
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
                                  ifelse(response == "much more right", 2, NA)))))
    )

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
d_tidy = d_tidy %>%
  filter(subid != "us_run_01_10",
         subid != "us_run_01_34")

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
                       
d_tidy = d_tidy %>%
  filter(is.element(subid, subidsToKeep$subid))

# --- EVENING OUT CONDITION ASSIGNMENT ----------------------------------------

# randomly choose N participants from each sequence
n = 10 # set number to choose
subidList = d_tidy %>%
  select(sequence, subid) %>%
  distinct() %>%
  group_by(sequence) %>%
  sample_n(n, replace = FALSE)

d_tidy = d_tidy %>%
  filter(is.element(subid, subidList$subid))
  
# check
d_tidy %>% group_by(sequence) %>% select(subid) %>% unique() %>% summarise(count = length(subid))

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# write subidList to csv file
write.csv(subidList, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/randomized_subidList.csv")

# write data to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/run-01_2015-05-09_data_anonymized.csv")

d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/run-01_2015-05-09_data_anonymized.csv")[-1] # get rid of column of obs numbers

