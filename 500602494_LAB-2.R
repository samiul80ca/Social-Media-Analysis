# setting up required libraries
library(ggplot2)
library(lubridate)
library(scales)
library(twitteR)
library(dplyr)

# To identify user and check account validity
consumer_key <- "005xyO9oWY9QndMoPAwFADzRb"
consumer_secret <- "uY2u756WScAEtlen4pTGtRSsLzOnGuoalLJo4N35qLpn2eojkE"
# To authorize data operations
access_token <- "957768651960209408-VF7wXJnMhDn5Bj2td8XN4mnbTOngMk8"
access_secret <- "boXFblT9OXDL2PSoj3CBn0kxrXV0zgZNrJnJApO0PYSLf"

# Twitter oauth setup
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Get 1000 tweets from '@RyersonU' as a list using searchTwitter function, and convert them to the dataframe using 'twListToDF' function 
RUtweets  <-twListToDF( searchTwitter("@RyersonU", n=1000) )
# Add boolean feature isReply
RUtweets$isReply <- ifelse((!is.na(RUtweets$replyToSID) |
                               !is.na(RUtweets$replyToSN) |
                               !is.na(RUtweets$replyToUID)), TRUE, FALSE)

## Task 1: Number of Reply posts vs. other posts
ggplot(RUtweets, aes(RUtweets$isReply)) +
  geom_bar(fill = "blue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets") + 
  ggtitle("Number of Reply posts vs. other posts") +
  scale_x_discrete(labels=c("Other posts", "Reply posts"))

## Task 1: Number of Reply posts vs. other posts in percentage
ggplot(RUtweets, aes(x = RUtweets$isReply)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "brown") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Number of Reply posts vs. other posts (in %)", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other posts", "Reply posts"))

## Task 2: Number of Retweets vs other posts

ggplot(RUtweets, aes(RUtweets$isRetweet)) +
  geom_bar(fill = "blue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets") + 
  ggtitle("Number of Retweets vs other posts") +
  scale_x_discrete(labels=c("Other posts", "Retweets"))


#Task 2: Number of Retweets vs other posts in percentage.

ggplot(RUtweets, aes(x = RUtweets$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)),fill = "brown") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(y="Tweets", title="Number of Retweets vs other posts (in %)") +
  scale_x_discrete(labels=c("Other posts","Retweets"))

## Task 3 :Plot comparing the distribution of the number of times a post was retweeted, for the original tweets vs retweets.


ggplot(data = RUtweets, aes(x = retweetCount)) +
  geom_histogram(aes(fill = ..count..), bins=30, binwidth = 1) +
  theme(legend.position = "none") +
  xlab("Retweet count") + ylab("Number of tweets") + ggtitle("Posts retwitted for original tweets vs. Retweets") +
  xlim(0,30) +
  scale_fill_gradient(low = "blue", high = "brown") +
  facet_grid(isReply ~ .)


## Task 4: Propose a new char to visualize any other metadata field(s) available in your dataset.


# Obtain top 10 users by number of tweets
toptenusers <- RUtweets %>% count(screenName) %>% top_n(n=10)

ggplot(data= toptenusers, aes(reorder(screenName, n),n)) +
  geom_bar(stat="identity", fill="midnightblue") +
  coord_flip() +
  labs(y="Number of Tweets", x="User",title="Top ten Users by tweet count")
