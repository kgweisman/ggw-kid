# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(jsonlite)
library(stats)

# clear environment
rm(list=ls())

# set up function for reading in data from JSON objects
jsonFormatCharmeans = function(wd, runName) {
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
      
      # character means data:
      grownup = paste(jd$answers$data$newData$charScores$grownup, collapse = ','),
      kid = paste(jd$answers$data$newData$charScores$kid, collapse = ','),
      baby = paste(jd$answers$data$newData$charScores$baby, collapse = ','),
      dog = paste(jd$answers$data$newData$charScores$dog, collapse = ','),
      bear = paste(jd$answers$data$newData$charScores$bear, collapse = ','),
      bug = paste(jd$answers$data$newData$charScores$bug, collapse = ','),
      robot = paste(jd$answers$data$newData$charScores$robot, collapse = ','),
      computer = paste(jd$answers$data$newData$charScores$computer, collapse = ','),
      car = paste(jd$answers$data$newData$charScores$car, collapse = ','),
      stapler = paste(jd$answers$data$newData$charScores$stapler, collapse = ','))
    
    # bind into same dataframe
    d.raw = bind_rows(d.raw, id)
  }
  
  return(d.raw)
}

# --- READING IN DATA OBJECTS -------------------------------------------------

# US run 01 (2015-05-09)
d_us_run_01 = jsonFormatCharmeans(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/us_run-01/",
  runName = "us_run_01")

# # India run 01 (2015-08-15)
# d_india_run_01.0 = jsonFormatCharmeans(
#   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01/",
#   runName = "india_run_01.0")
# d_india_run_01.1 = jsonFormatCharmeans(
#   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.1/",
#   runName = "india_run_01.1")
# d_india_run_01.2 = jsonFormatCharmeans(
#   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.2/",
#   runName = "india_run_01.2")
# # SAME AS RUN 01.2!
# # d_india_run_01.3 = jsonFormatCharmeans(
# #   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.3/",
# #   runName = "india_run_01.3")
# # d_india_run_01.4 = jsonFormatCharmeans(
# #   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.4/",
# #   runName = "india_run_01.4")
# # d_india_run_01.5 = jsonFormatCharmeans(
# #   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.5/",
# #   runName = "india_run_01.5")
# d_india_run_01.6 = jsonFormatCharmeans(
#   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.6/",
#   runName = "india_run_01.6")
# d_india_run_01.7 = jsonFormatCharmeans(
#   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/turk/india_run-01.7/",
#   runName = "india_run_01.7")

# --- TIDYING -----------------------------------------------------------------

# clean up variables
d_tidy <- d_us_run_01 
#%>%
  #   full_join(d_us_run_02) %>% ...etc.
#   full_join(d_india_run_01.0) %>%
#   full_join(d_india_run_01.1) %>%
#   full_join(d_india_run_01.2) %>%
#   # full_join(d_india_run_01.3) %>%
#   # full_join(d_india_run_01.4) %>%
#   # full_join(d_india_run_01.5) %>%
#   full_join(d_india_run_01.6) %>%
#   full_join(d_india_run_01.7)

# clean up variables
d_tidy <- d_us_run_01 %>%
  #   full_join(d_us_run_02) %>% ...etc.
#   full_join(d_india_run_01.0) %>%
#   full_join(d_india_run_01.1) %>%
#   full_join(d_india_run_01.2) %>%
#   # full_join(d_india_run_01.3) %>%
#   # full_join(d_india_run_01.4) %>%
#   # full_join(d_india_run_01.5) %>%
#   full_join(d_india_run_01.6) %>%
#   full_join(d_india_run_01.7) %>%
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
    job = factor(job))

glimpse(d_tidy)

# --- EVENING OUT CONDITION ASSIGNMENT ----------------------------------------

# import subidList from first formatting file
subidList = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/randomized_subidList.csv")[-1]

# filter by subidList
d_tidy = d_tidy %>%
  filter(is.element(subid, subidList$subid))

# check
d_tidy %>% group_by(country, sequence) %>% select(subid) %>% unique() %>% summarise(count = length(subid))

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# write to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/run-01_2015-08-17_charmeans.csv")

d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid/data/adults/run-01_2015-08-17_charmeans.csv")[-1] # get rid of column of obs numbers
