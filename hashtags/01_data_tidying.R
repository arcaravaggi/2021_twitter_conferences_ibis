setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set active directory to wd

library(stringr)
library(dplyr)
library(lubridate)
library(purrr)
 
# Import data ####
temp <- str_extract(list.files(path = "../data_raw", pattern = "*.csv"), '.*(?=\\.csv)') # List csvs but remove suffix
temp2 <- str_replace_all(string=(paste("../data_raw/", temp, ".csv")), pattern=" ", repl="") # Build filenames
ct <- lapply(temp2, read.csv)
names(ct) <- temp # Name list elements
ct <- Map(cbind, ct, hashtag = names(ct)) # Add df name as column, for reference

# Collapse to single dataframe ####
ct.all <- do.call(rbind.data.frame, ct)
names(ct.all)
names(ct.all) <- c("id", "url", "date_time", "content", "type", "client", "retweets", "likes", "location", 
                    "language",
                    "uid", "name", "username", "bio", "verified", "p_url", "protected", "u_followers", "u_following", 
                    "u_creation", "impressions", "hashtag")
rownames(ct.all) <- NULL

# Split Twitter & IRL conferences ####
tcs <- c("BOU17TC","BOU18TC","BTcon17","BTcon18","WSTC1","WSTC2","WSTC3","WSTC4","WSTC5")
irl <- c("AOSSCO2017","BES2017","BES2018","BOU2018","BOU2019","IOCongress2018","TWS2017","TWS2018")

# Subset all hashtag data into twitter conferences (tcs) and IRL conferences (irl)
tcs.l <- subset(ct, names(ct) %in% tcs)
irl.l <- subset(ct, names(ct) %in% irl)

# Read twitter conference presenters and limit data to those directly involved, only. ####
presenters <- read.csv("../data_raw/presenters/presenters.csv")
presenters$username <- gsub('@', '', presenters$username) # remove 'non-alphanumerics'@'
tcs.all <- do.call(rbind.data.frame, tcs.l)
tcs.all <- subset(tcs.all, Username %in% presenters$username & hashtag %in% presenters$hashtag)
tcs.all$source <- "twitter"

# Number of tweets & retweets per presenter
freq <- tcs.all %>%
  group_by(hashtag, Username, Tweet.Type) %>%
  summarise(tweets = n(), retweets = sum(Retweets.Received)) %>%
  filter(Tweet.Type == "Tweet" & tweets > 3)

# write.csv(freq, "../data_out/TC_presenter_content_count.csv")

# Collapse IRL data 
irl.all <- do.call(rbind.data.frame, irl.l)
irl.all$source <- "irl"

# Combine dataframes ####
ct.pre <- rbind(tcs.all, irl.all)
names(ct.pre)
names(ct.pre) <- c("id", "url", "date_time", "content", "type", "client", "retweets", "likes", "location", 
                    "language",
                   "uid", "name", "username", "bio", "verified", "p_url", "protected", "u_followers", "u_following", 
                   "u_creation", "impressions", "hashtag", "source")

ct.all$hashtag <- as.character(ct.all$hashtag)
ct.pre$hashtag <- as.character(ct.pre$hashtag)
ct.pre$date_time <- as.POSIXct(as.character(ct.pre$date_time), format = "%d %h %Y %H:%M:%S", tz = "GMT")
ct.all$date_time <- as.POSIXct(as.character(ct.all$date_time), format = "%d %h %Y %H:%M:%S", tz = "GMT")


# Subset to Tweets, only ####
tw.pre <- subset(ct.pre, type == "Tweet")
tw.all <- subset(ct.all, type == "Tweet")
rownames(tw.pre) <- NULL
rownames(tw.all) <- NULL

tw.pre$hashtag <- as.character(tw.pre$hashtag)
tw.all$hashtag <- as.character(tw.all$hashtag)

# Threshold by conference dates ####
dates <- read.csv("../data_raw/dates/dates.csv", stringsAsFactors = F)
dates$start <- as.POSIXct(dates$start, format= "%d/%m/%Y")
dates$end <- as.POSIXct(dates$end, format= "%d/%m/%Y")

ct.pre.d <- ct.pre %>% left_join(dates, by = "hashtag") %>% filter(date(date_time)>=start, date(date_time)<=end)
ct.all.d <- ct.all %>% left_join(dates, by = "hashtag") %>% filter(date(date_time)>=start, date(date_time)<=end)
tw.pre.d <- tw.pre %>% left_join(dates, by = "hashtag") %>% filter(date(date_time)>=start, date(date_time)<=end)
tw.all.d <- tw.all %>% left_join(dates, by = "hashtag") %>% filter(date(date_time)>=start, date(date_time)<=end)

# Collect and clear environment ####
#dat.l <- list(ct.all, ct.pre, ct.all.d, ct.pre.d, tw.all, tw.pre, tw.all.d, tw.pre.d)
#names(dat.l) <- c("ct.all", "ct.pre", "ct.all.d", "ct.pre.d", "tw.all", "tw.pre", "tw.all.d", "tw.pre.d")
#rm(list=setdiff(ls(), "dat.l"))
# Export files
#sapply(names(dat.l), 
#       function (x) write.csv(dat.l[[x]], file=paste("../data_out/",x, ".csv", sep="") )   )

dat.l <- list(ct.pre.d, tw.pre.d)
names(dat.l) <- c("all", "tweets")
rm(list=setdiff(ls(), "dat.l"))
