setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set active directory to wd

library(dplyr)
library(lubridate)
library(ggplot2)

# Calculate engagement metrics per tweet ####
# df = data frame
#
tweetEng <- function(df){
# Calculate engagement metrics (see https://blog.hootsuite.com/calculate-engagement-rate/)
# Engagement rate by impressions
df$eri <- (df$likes + df$retweets) / df$impressions * 100
# Engagement by post
df$ep <- (df$likes + df$retweets) / df$u_followers * 100
return(df)
}

t.eng.l <- lapply(dat.l, function(x) tweetEng(x))
t.eng.l <- rapply(dat.l, f = function(x) ifelse(is.nan(x),0,x), how="replace" )


# Calculate daily metrics ####
# df = data frame
#
dayEng <- function(df){
  d <- df %>%
    group_by(hashtag, as.Date(date_time)) %>%
    summarize(n = n(),
            total.eri = sum((likes + retweets)/impressions, na.rm =  TRUE)*100,
            mean.eri = mean((likes + retweets)/impressions, na.rm =  TRUE)*100,
            sd.eri = sd((likes + retweets)/impressions, na.rm =  TRUE)*100,
            total.ep = sum((likes + retweets)/u_followers, na.rm =  TRUE)*100,
            mean.ep = mean((likes + retweets)/u_followers, na.rm =  TRUE)*100,
            sd.ep = sd((likes + retweets)/u_followers, na.rm =  TRUE)*100)
  d[is.na(d)] <- 0
return(d)
}

d.eng.l <- lapply(dat.l, function(x) dayEng(x))


# Calculate metrics per conference ####
# df = data frame
#
confEng <- function(df){
  d <- df %>%
    group_by(hashtag) %>%
    summarize(n = n(),
              total.eri = sum((likes + retweets)/impressions, na.rm =  TRUE)*100,
              mean.eri = mean((likes + retweets)/impressions, na.rm =  TRUE)*100,
              sd.eri = sd((likes + retweets)/impressions, na.rm =  TRUE)*100,
              total.ep = sum((likes + retweets)/u_followers, na.rm =  TRUE)*100,
              mean.ep = mean((likes + retweets)/u_followers, na.rm =  TRUE)*100,
              sd.ep = sd((likes + retweets)/u_followers, na.rm =  TRUE)*100)
  d[is.na(d)] <- 0
  return(d)
}

c.eng.l <- lapply(dat.l, function(x) confEng(x))

# Calculate percentage of 'high engagement' (>75th quantile) tweet' per group ####
dat.l$all$er <- (dat.l$all$likes + dat.l$all$retweets)/dat.l$all$impressions*100
dat.l$all$er[is.nan(dat.l$all$er)] <- 0
quantile(dat.l$all$er, 0.75)
length(dat.l$all$er[dat.l$all$er > 0.1287001])/length(dat.l$all$er)*100

dat.l$all$ep <- (dat.l$all$likes + dat.l$all$retweets)/dat.l$all$u_followers*100
dat.l$all$ep[is.nan(dat.l$all$ep)] <- 0
quantile(dat.l$all$ep, 0.75)
length(dat.l$all$ep[dat.l$all$ep > 0.257566])/length(dat.l$all$ep)*100

dat.l$tweets$er <- (dat.l$tweets$likes + dat.l$tweets$retweets)/dat.l$tweets$impressions*100
dat.l$tweets$er[is.nan(dat.l$tweets$er)] <- 0
quantile(dat.l$tweets$er, 0.75)
length(dat.l$tweets$er[dat.l$tweets$er > 0.922368])/length(dat.l$tweets$er)*100

dat.l$tweets$ep <- (dat.l$tweets$likes + dat.l$tweets$retweets)/dat.l$tweets$u_followers*100
dat.l$tweets$ep[is.nan(dat.l$tweets$ep)] <- 0
quantile(dat.l$tweets$ep, 0.75)
length(dat.l$tweets$ep[dat.l$tweets$ep > 1.848149])/length(dat.l$tweets$ep)*100
     
# Plots ####
ggplot(c.eng.l$tweets, aes(hashtag, mean.ep)) + 
  geom_point(aes(color=hashtag), position=position_dodge(1))+
  geom_errorbar(aes(ymin=mean.ep-sd.ep, ymax=mean.ep+sd.ep), width=.2,
                position=position_dodge(1)) +
  coord_flip() +
  ggtitle("Mean & SD engagement by post across tweets")
