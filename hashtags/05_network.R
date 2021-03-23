setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set active directory to wd

source('ipak.R')
ipak(c("beepr", "plyr", "dplyr", "tidygraph", "ggraph"))

detach("package:reshape2", unload=TRUE)
detach("package:plyr", unload=TRUE)

# Setup data frames ####
# Hashtags and usernames, only, here for all tweets (see 01b analysis)
df <- ct.all.d

# Split F2F and Twitter conferences to look at differences in connectivity
tcs <- c("BOU17TC","BOU18TC","BTcon17","BTcon18","WSTC1","WSTC2","WSTC3","WSTC4","WSTC5")
irl <- c("AOSSCO2017","BES2017","BES2018","BOU2018","BOU2019","IOCongress2018","TWS2017","TWS2018")

# Subset all hashtag data into twitter conferences (tcs) and IRL conferences (irl)
tcs.l <- subset(df, hashtag %in% tcs)
irl.l <- subset(df, hashtag %in% irl)


# Function to create network data objects ####
# - a data frame of usernames that interact with >1 hashtag ('dataframe')
# - edges and nodes used in creating networks ('edges', 'nodes')
# - the network object ('routes')
# from a list of data frames
# 
#
# See stepwise code, below, for a walk-through
# 
# l = list of data frames
#
confNet <- function(l){
  veCombine <- function(x,m){
    n <- ifelse(length(x) == 1,ifelse(is.numeric(x),x,1),length(x))
    if(n >= m) return(combn(x,m))
    a <- do.call(expand.grid, rep(list(x),m))
    b <- t(unique(t(apply(a,1,sort))))
    `dimnames<-`(b,NULL)
  }
  
  reCombine <-function(x, m){
    a <- veCombine(x, m)
    b <- data.frame(from = a[1,], to = a[2,])
    return(b)
  }
  
  list <- split(l, l$username)
  list <- list[sapply(list, function(x) dim(x)[1]) > 1]
  d <- lapply(list, function(x) reCombine(x$hashtag, 2))
  d2 <- plyr::ldply(d, data.frame)

  edges <- d2[!(d2$from == d2$to),]
  edges[,1] <- NULL
  edges<- na.omit(edges)
  names(edges) <- c("source", "destination")
  
  edges <- edges %>% 
    group_by(source, destination) %>% 
    summarise(n = n()) %>%
    mutate(n = scales::rescale(n, to = c(0:1))) %>%
    rename(weight = n) %>%
    ungroup()
  
  nodes <- data.frame(label = unique(d2$from), id = 1:length(unique(d2$from)))
  
  edges <- edges %>% 
    left_join(nodes, by = c("source" = "label"))  %>%
    rename(from = id)
  
  edges <- edges %>% 
    left_join(nodes, by = c("destination" = "label")) %>%
    rename(to = id)
  
  edges <- select(edges, from, to, weight)
  
  routes <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
  
  routes %>% 
    activate(edges) %>% 
    arrange(desc(weight))
  
  o <- list(d2, edges, nodes, routes)
  names(o) <- c("dataframe", "edges", "nodes", "routes") 
  return(o)
}

# Analyses and plots ####
all <- confNet(df); beep(2)
twc <- confNet(tcs.l); beep(2)
irl <- confNet(irl.l); beep(2)

# Compare weights between Twitter and F2F conferences
t.test(twc$edges$weight, irl$edges$weight)
#write.csv(twc$edges$weight, "../data_out/20200417_tc_weights.csv")
#write.csv(irl$edges$weight, "../data_out/20200417_f2f_weights.csv")

# # Plots
# t.plot.lab <- ggraph(twc$routes, layout = "circle") + 
#   geom_node_point() +
#   geom_edge_link(aes(width = weight), alpha = 0.8) + 
#   scale_edge_width(range = c(0.2, 3)) +
#   geom_node_text(aes(label = label), repel = T) +
#   labs(edge_width = "Letters") +
#   theme_graph()+
#   theme(legend.position="none")
# 
# t.plot.nolab <- ggraph(twc$routes, layout = "circle") + 
#   geom_node_point() +
#   geom_edge_link(aes(width = weight), alpha = 0.8) + 
#   scale_edge_width(range = c(0.2, 3)) +
#   labs(edge_width = "Letters") +
#   theme_graph() +
#   theme(legend.position="none")
# 
# i.plot.lab <- ggraph(irl$routes, layout = "circle") + 
#   geom_node_point() +
#   geom_edge_link(aes(width = weight), alpha = 0.8) + 
#   scale_edge_width(range = c(0.2, 3)) +
#   geom_node_text(aes(label = label), repel = T) +
#   labs(edge_width = "Letters") +
#   theme_graph()+
#   theme(legend.position="none")
# 
# i.plot.nolab <- ggraph(irl$routes, layout = "circle") + 
#   geom_node_point() +
#   geom_edge_link(aes(width = weight), alpha = 0.8) + 
#   scale_edge_width(range = c(0.2, 3)) +
#   labs(edge_width = "Letters") +
#   theme_graph()+
#   theme(legend.position="none")
# 
# ggsave("../figures/20200417_tnetwork.png", t.plot.lab, width = 6, height = 6, dpi = 300)
# ggsave("../figures/20200417_tnetwork2.png", t.plot.nolab, width = 6, height = 6, dpi = 300)
# ggsave("../figures/20200417_inetwork.png", i.plot.lab, width = 6, height = 6, dpi = 300)
# ggsave("../figures/20200417_inetwork2.png", i.plot.nolab, width = 6, height = 6, dpi = 300)
# # Labels added in Illustrator due to ggraph limitations

# Stepwise run through the confNet function ####
# Create all combinations of vector
# x = vector
# m = number of items in combination
# 
# E.g. veCombine(df$categories, 2)
#
# From https://stackoverflow.com/a/49564412/9962100
veCombine <- function(x,m){
  n <- ifelse(length(x) == 1,ifelse(is.numeric(x),x,1),length(x))
  if(n >= m) return(combn(x,m))
  a <- do.call(expand.grid, rep(list(x),m))
  b <- t(unique(t(apply(a,1,sort))))
  `dimnames<-`(b,NULL)
}

reCombine <-function(x, m){
  a <- veCombine(x, m)
  b <- data.frame(from = a[1,], to = a[2,])
  return(b)
}

# Split data frame by grouping column
t.c <- split(tcs.l, tcs.l$username)
t.c <- t.c[sapply(t.c, function(x) dim(x)[1]) > 1]
r3 <- lapply(t.c, function(x) reCombine(x$hashtag, 2))

library(plyr)
# Collapse list to data frame
df2 <- ldply(r3, data.frame)
detach("package:plyr", unload=TRUE)
rm(df.c)

# Remove self-pairs, unwanted columns and NAs
edges <- df2[!(df2$from == df2$to),]
edges[,1] <- NULL
edges<- na.omit(edges)
names(edges) <- c("source", "destination")
edges$count <- 1


# create cumulative edges and nodes
edges <- edges %>% 
  group_by(source, destination) %>% 
  summarise(n = n()) %>%
  mutate(n = scales::rescale(n, to = c(0:1))) %>%
  rename(weight = n) %>%
  ungroup()

nodes <- data.frame(label = unique(df2$from), id = 1:9)

edges <- edges %>% 
  left_join(nodes, by = c("source" = "label"))  %>%
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>%
  rename(to = id)

edges <- select(edges, from, to, weight)


routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

# Example plots ####
ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(all$routes, layout = "circle") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

ggraph(twc$routes, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()


