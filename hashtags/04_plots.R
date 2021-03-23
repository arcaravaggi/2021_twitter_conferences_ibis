setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set active directory to wd

source("ipak.R")

ipak(c("ggplot2", "reshape2"))

source("theme_ac1.R")

# Unique voices per conference per year
dat.l$tweets %>%
  group_by(hashtag, source) %>%
  summarise(count = n_distinct(username)) %>%
  melt(id.vars = c("hashtag", "source")) %>%
  ggplot(aes(reorder(hashtag, desc(hashtag)), value)) +
  geom_col() +
  ggtitle("Number of unique contributors across original tweets") +
  facet_wrap(~source, 
             scales = "free_y",
             ncol = 1,
             strip.position = "left") +
  theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white"),  # Make facet label background white.
        axis.title = element_blank()) +
  coord_flip()

# Total tweets, retweets & likes per conference per year
dat.l$all$source <- factor(dat.l$all$source, levels = c("irl", "twitter"), labels = c("Face-to-face", "Twitter"))
tot.plot <- dat.l$all %>% 
  group_by(hashtag, source) %>%
  summarize(tweets = sum(type == "Tweet"),
            likes = sum(likes),
            rts = sum(retweets)) %>%
  melt(id.vars = c("hashtag", "source")) %>%
  ggplot(aes(reorder(hashtag, desc(hashtag)), value, fill = reorder(variable, desc(variable)))) + 
  geom_col(position = position_dodge()) +
  coord_flip() +
  theme_ac1(base_size_a = 12, base_size_t = 12) +
  facet_wrap(~source, 
             scales = "free_y",
             ncol = 1,
             strip.position = "right") +
  theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white", color = "black",linetype = "solid"),  # Make facet label background white
        strip.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.15),
        legend.direction = "vertical",
        legend.text = element_text(size = 12)) +
  scale_fill_manual(breaks = c("rts", "likes", "tweets"),
                    labels = c("Retweets", "Likes", "Tweets"),
                    values=c("darkgrey", "lightgrey", "Black")) +
  
  xlab("Conference hashtag") +
  ylab("Number of tweets")


ggsave("../figures/20200908_total_tweets_conferences.png",tot.plot, width = 6, height = 4, dpi = 300)
