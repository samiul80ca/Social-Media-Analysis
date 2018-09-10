library(twitteR)
if (!"SocialMediaLab" %in% installed.packages()) install.packages("SocialMediaLab")
require(SocialMediaLab)
if (!"magrittr" %in% installed.packages()) install.packages("magrittr")
require(magrittr)
if (!"igraph" %in% installed.packages()) install.packages("igraph")
require(igraph)

# REPLACE WITH YOUR API KEY
myapikey <- "pY9vu9ZlKAHIVjO79cNAMH8zy"
# REPLACE WITH YOUR API SECRET
myapisecret <- "0TZZl6UW7UYDrt3sh27wB5qKZArrvhSHmw1aDsIz7xOa9vXCKv"
# REPLACE WITH YOUR ACCESS TOKEN
myaccesstoken <- "824392227749298177-UfnBjHAkOaJG6wRIvJye4gN6MOAWp8c"
# REPLACE WITH YOUR ACCESS TOKEN SECRET
myaccesstokensecret <- "Byom7w6p6IUMcrwwKY0jhUCvHGBxMcC5fUZuOash0EHtX"

myTwitterData <- Authenticate("twitter", apiKey=myapikey,
                              apiSecret=myapisecret,
                              accessToken=myaccesstoken,
                              accessTokenSecret=myaccesstokensecret) %>%
  Collect(searchTerm="#cdnhealth", numTweets=1000)


myTwitterData$text <- iconv(myTwitterData$text, to = 'utf-8')

g_twitter_actor <- myTwitterData %>% Create("Actor")

pageRank_cdnhealth_actor <- sort(page.rank(g_twitter_actor)$vector,decreasing=TRUE)
head(pageRank_cdnhealth_actor,n=5)


plot(g_twitter_actor,vertex.shape="none", main="Actor Network",
     vertex.label=ifelse(degree(g_twitter_actor)>40,V(g_twitter_actor)$name,NA),
     edge.width=1.5,edge.curved = .5,edge.arrow.size=0.5,asp=9/16,margin=-0.15)


#Algorithm-1
layout1 <- layout.drl(g_twitter_actor)
layout2 <- layout.fruchterman.reingold(g_twitter_actor)
layout3 <- layout.lgl(g_twitter_actor)
V(g_twitter_actor)$color <- comm$membership+1

wt <- walktrap.community(g_twitter_actor)
plot(wt, g_twitter_actor, vertex.shape="none", main = "Visualization of Walktrap Community Algorithm",
     vertex.label=ifelse(degree(g_twitter_actor)>50,V(g_twitter_actor)$name,NA) ,layout = layout2,
     edge.width=.2, edge.curved = .2, edge.arrow.size=.2, asp=9/16,margin=-0.15,
     vertex.frame.color=V(g_twitter_actor)$color)


a<-groups(wt)
head(a,n=3)

length(wt)
sizes(wt)
membership(wt)
cc <- clusters(wt) #information on connected components
summary(cc)
head(cc$membership) #which component each node is assigned to     
g3 <- induced_subgraph(wt, which(cc$membership == which.max(cc$csize)))
g3


#Algorithm-2
imc <- infomap.community(as.undirected(g_twitter_actor,mode = "collapse"))
plot(imc, g_twitter_actor, vertex.shape="none", main = "Visualization of Infomap Community Algorithm",
     vertex.label=ifelse(degree(g_twitter_actor)>40,V(g_twitter_actor)$name,NA), layout = layout1, 
     edge.width=.2, edge.curved = .2, edge.arrow.size=0.2, asp=9/16,margin=-0.15)

length(imc)
sizes(wt)

#Algorithm-3
lpc <- label.propagation.community(as.undirected(g_twitter_actor,mode="collapse"))
plot(lpc, g_twitter_actor, vertex.shape="none",main = "Visualization of Label Propagation Community Algorithm",
     vertex.label=ifelse(degree(g_twitter_actor)>40,V(g_twitter_actor)$name,NA), 
     edge.width=1.5, edge.curved = .5, edge.arrow.size=0.5, asp=9/16,margin=-0.15)
sizes(lpc)
length(lpc)
membership(lpc)

