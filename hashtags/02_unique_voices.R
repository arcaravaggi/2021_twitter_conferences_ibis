setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set active directory to wd

library(stringr)
library(dplyr)
library(lubridate)
library(purrr)
library(reshape2)
library(ggplot2)

source("theme_ac1.R")

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

ct.all$hashtag <- as.character(ct.all$hashtag)
ct.all$date_time <- as.POSIXct(as.character(ct.all$date_time), format = "%d %h %Y %H:%M:%S", tz = "GMT")

# Split Twitter & IRL conferences ####
tcs <- c("BOU17TC","BOU18TC","BTcon17","BTcon18","WSTC1","WSTC2","WSTC3","WSTC4","WSTC5")
irl <- c("AOSSCO2017","BES2017","BES2018","BOU2018","BOU2019","IOCongress2018","TWS2017","TWS2018")

# Subset all hashtag data into twitter conferences (tcs) and IRL conferences (irl)
tcs.l <- subset(ct, names(ct) %in% tcs)
irl.l <- subset(ct, names(ct) %in% irl)

# Read twitter conference presenters and limit data to those directly involved to look at output ####
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

# Threshold by conference dates ####
dates <- read.csv("../data_raw/dates/dates.csv", stringsAsFactors = F)
dates$start <- as.POSIXct(dates$start, format= "%d/%m/%Y")
dates$end <- as.POSIXct(dates$end, format= "%d/%m/%Y")

ct.all.d <- ct.all %>% left_join(dates, by = "hashtag") %>% filter(date(date_time)>=start, date(date_time)<=end)

ct.all.d$source <- "all"

ct.pre <- subset(ct.all.d, type == "Tweet")
rownames(ct.pre) <- NULL
ct.pre$hashtag <- as.character(ct.pre$hashtag)
ct.pre$source <- "tweets"

ct.all.d <- rbind(ct.all.d, ct.pre)


tcs.all <- subset(ct.all.d, hashtag %in% irl)
dat.l <- list(tcs.all, tw.pre.d)
names(dat.l) <- c("all", "tweets")



#Plot ####

ct.all.d$source[ct.all.d$hashtag %in% irl] <- "Face-to-face"
ct.all.d$source[ct.all.d$hashtag %in% tcs] <- "Twitter"


uq.plot <- ct.all.d %>%
  group_by(hashtag, source) %>%
  summarise(count = n_distinct(username)) %>%
  melt(id.vars = c("hashtag", "source")) %>%
  ggplot(aes(reorder(hashtag, desc(hashtag)), value)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~source, 
             scales = "free_y",
             ncol = 1,
             strip.position = "right") +
  theme(plot.background = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "black", size=0),
        panel.background = element_rect(fill = "white", colour = "black"), 
        strip.background = element_rect(fill = NA, colour = "black", size=0),  # Make facet label background white.
        strip.text = element_text(size = 14, face = "bold", family = "serif"),
        axis.title = element_text(size = 14, face = "bold", family = "serif"),
        axis.text = element_text(size = 14, family = "serif")) +
  coord_flip() +
#  theme_ac1() +
  xlab("Conference hashtag") +
  ylab("Number of tweets") +
  theme(legend.justification=c(1.1,1.1), legend.position=c(1, 1))


ggsave("../figures/20200512_unique_voices.png", uq.plot, width = 6, height = 4, dpi = 200)

