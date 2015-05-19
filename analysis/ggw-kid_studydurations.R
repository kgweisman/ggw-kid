# calculate length of study for pilot-b

# library
library("chron")

# --- READING IN DATA OBJECTS -------------------------------------------------

# ----------> run-01 (2015-03-13) ---------------------------------------------

# set working directory for india
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid_adult-version/turk/us_run-01/")

# mike's json for-loop
files <- dir("production-results/")

d.raw_01 <- data.frame()

for(i in 1:length(files)) {
  # gather files
  f = files[i]
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  
  # store relevant variables in dataframe 
  id <- data.frame(
    matrix(
      data = c("AcceptTime", "SubmitTime"),
      nrow = 1, ncol = 0))
  
  id$AcceptTime = jd$AcceptTime  
  id$SubmitTime = jd$SubmitTime
  
  # bind into same dataframe
  d.raw_01 <- bind_rows(d.raw_01, id)
}

glimpse(d.raw_01)

# --- TIDYING -----------------------------------------------------------------

d_times = d.raw_01 %>%
#   full_join(d.raw_02) %>% ... etc.
  mutate(startTime = chron(times = substr(AcceptTime, 12, 19), format = 'h:m:s'),
         endTime = chron(times = substr(SubmitTime, 12, 19), format = 'h:m:s'),
         duration = endTime - startTime,
         duration_mins = as.numeric(substr(duration, 4,5)) + as.numeric(substr(duration, 7,8))/60)

summary(d_times)

qplot(d_times$duration_mins,
      binwidth = 1)

