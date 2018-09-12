#-------------------------------------------Initial Setup------------------------------------------#
#==================================================================================================#

install.packages("twitteR")   # To use the libraries like 'twitteR','httr'& 'rjson',which provides access to the Twitter API.
install.packages("ggplot2")   # To use the library 'ggplot2' used for plotting purpose.
install.packages("gridExtra") # To use the library 'gridExtra' used for side by side plotting.
install.packages("proto")     # To use the library 'gsubfn' used for regular expression.
install.packages("SnowballC") # To use the SnowballC library.
install.packages("tm")        # To use the 'tm' library used for text mining. 
install.packages("ggpubr")    # To use the library 'ggpubr' used for plotting purpose.
install.packages("stringr")   # To use the library which contains functions to manipulate regular expressions. 
install.packages("wordcloud") # To use the 'wordcloud' library for text visualization purpose
install.packages("RColorBrewer") # To use different color palettes in wordcloud.


library(twitteR)    # To use the functions which provides access to the Twitter API. 
library(dplyr)      # for working with data frame like objects, both in memory and out of memory.
library(httr)       # for working with HTTP organised by HTTP verbs. 
library(rjson)      # Converts R object into JSON objects and vice-versa.
library(gsubfn)     # For Pattern Matching and Replacement in Regular Expression.
library(caTools)    # To use the function sample.split which is used to split the dataset
library(ggplot2)    # To create elegant data visualizations using the grammar of graphics.
library(RTextTools) # Machine learning package for automatic text classification. 
library(e1071)      # Functions for latent class analysis and Naive Bayes' Classifier.
library(scales)     # Automatically determines breaks and labels for axes and legends.
library(gridExtra)  # To use the function 'grid.arrange' used for side by side plotting.
library(tm)         # contains text mining functions 
library(SnowballC)  # Contains text stemming functions
library(stringr)   # To use the function 'regex'
library(wordcloud)  # To use the function for visualization of wordcloud
library(RColorBrewer) # To use the function used for different text colors in wordcloud
library(ggpubr)     # Supporting library the function 'grid.arrange' used for side by side plotting.

#----------------------------------------Twitter Oauth Setup---------------------------------------#
#==================================================================================================#
# To identify user and check account validity
consumer_key <- "005xyO9oWY9QndMoPAwFADzRb"
consumer_secret <- "uY2u756WScAEtlen4pTGtRSsLzOnGuoalLJo4N35qLpn2eojkE"
# To authorize data operations
access_token <- "957768651960209408-VF7wXJnMhDn5Bj2td8XN4mnbTOngMk8"
access_secret <- "boXFblT9OXDL2PSoj3CBn0kxrXV0zgZNrJnJApO0PYSLf"

# Twitter oauth setup
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#------------------------------Searching, loading and prepare sample datasets--------------------------------#
#==================================================================================================#

# Get 1000 tweets from '#DataScience' as a list using searchTwitter function, and convert them to the dataframe using 'twListToDF' function 
dataset1  <-twListToDF( searchTwitter("#DataScience", n=1000) )
# Get 1000 tweets from '"data science"' as a list using searchTwitter function, and convert them to the dataframe using 'twListToDF' function 
dataset2  <-twListToDF( searchTwitter("data science", n=1000) )


# Add boolean feature isReply
dataset1$isReply <- ifelse((!is.na(dataset1$replyToSID) |
                              !is.na(dataset1$replyToSN) |
                              !is.na(dataset1$replyToUID)), TRUE, FALSE)

dataset2$isReply <- ifelse((!is.na(dataset2$replyToSID) |
                              !is.na(dataset2$replyToSN) |
                              !is.na(dataset2$replyToUID)), TRUE, FALSE)


#------------------------------Meta Data Analysis of Sample Datasets--------------------------------#
#==================================================================================================#
# In this section, we have perform metadata analysis of the fetched datasets and produce visualizations.
# The visualizations have been categorized from three different perspectives, namely:
#   1. Number of Reply posts vs. other posts
#   2. Number of Retweets vs other posts
#   3. Plot comparing the distribution of the number of times a post was retweeted,
#      for the original tweets vs retweets.

#--------------------------------------------------------------------------------------------------#
## Chart-1 Dataset-1: Number of Reply posts vs. other posts in percentage
ggplot(dataset1, aes(x = dataset1$isReply)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "brown") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Percentage of Reply vs. other posts(dataset-1)", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other posts", "Reply posts"))

## Chart-1 Dataset-2: Number of Reply posts vs. other posts in percentage
ggplot(dataset2, aes(x = dataset2$isReply)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "blue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Percentage of Reply vs. other posts(dataset-2)", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other posts", "Reply posts"))

#---------------------------------------------------------------------------------------------------#

#Chart-2 Dataset-1: Number of Retweets vs other posts in percentage.

ggplot(dataset1, aes(x = dataset1$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)),fill = "brown") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(y="Tweets", title="Percentage of Retweets vs other posts(dataset-1)") +
  scale_x_discrete(labels=c("Other posts","Retweets"))

#Chart-2 Dataset-2: Number of Retweets vs other posts in percentage.
ggplot(dataset2, aes(x = dataset2$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)),fill = "blue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(y="Tweets", title="Percentage of Retweets vs other posts(dataset-2)") +
  scale_x_discrete(labels=c("Other posts","Retweets"))

#--------------------------------------------------------------------------------------------------#

## Chart-3 Dataset-1: :Plot comparing the distribution of the number of times a post was retweeted, for the original tweets vs retweets.

ggplot(data = dataset1, aes(x = retweetCount)) +
  geom_histogram(aes(fill = ..count..), bins=30, binwidth = 1) +
  theme(legend.position = "none") +
  xlab("Retweet count") + ylab("Number of tweets") + ggtitle("Original tweets vs. Retweets (Dataset-1)") +
  xlim(0,30) +
  scale_fill_gradient(low = "red", high = "green") +
  facet_grid(isReply ~ .)

## Chart-3 Dataset-2: :Plot comparing the distribution of the number of times a post was retweeted, for the original tweets vs retweets.

ggplot(data = dataset2, aes(x = retweetCount)) +
  geom_histogram(aes(fill = ..count..), bins=30, binwidth = 1) +
  theme(legend.position = "none") +
  xlab("Retweet count") + ylab("Number of tweets") + ggtitle("Original tweets vs. Retweets (Dataset-2)") +
  xlim(0,30) +
  scale_fill_gradient(low = "red", high = "green") +
  facet_grid(isReply ~ .)
#---------------------------------------------------------------------------------------------------#

#------------------------------Text Analysis of Sample Datasets------------------------------------#
#==================================================================================================#

#Remove punctuation, numbers, html-links and unecessary spaces:
textScrubber <- function(dataframe) {
  
  dataframe$text <-  gsub("-", " ", dataframe$text)
  dataframe$text <-  gsub("&", " ", dataframe$text)
  dataframe$text <-  gsub("[[:punct:]]", " ", dataframe$text)
  dataframe$text <-  gsub("[[:digit:]]", " ", dataframe$text)
  dataframe$text <-  gsub("http\\w+", " ", dataframe$text)
  dataframe$text <-  gsub("\n", " ", dataframe$text)
  dataframe$text <-  gsub("[ \t]{2,}", " ", dataframe$text)
  dataframe$text <-  gsub("^\\s+|\\s+$", " ", dataframe$text)
  dataframe$text <-  gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", dataframe$text)  # To remove words shorter than 3 chars
  dataframe$text <-  tolower(dataframe$text)
  return(dataframe)
}

#cleaning tweet files
clean_ds1 <- textScrubber(dataset1)
clean_ds2 <- textScrubber(dataset2)

# Performing some extra cleanup to remove noises
clean_ds1$text <-  gsub("http | sdk ", " ", clean_ds1$text)
clean_ds1$text <- gsub("\\b[[:alpha:]]{11,}\\b", " ", clean_ds1$text, perl=T)  # To remove words grater than 11 characters.
clean_ds1$text <- gsub("\\b.{1,3}\\b", " ", clean_ds1$text) # To remove words shorter than 4 characters.

clean_ds2$text <-  gsub("http | sdk ", " ", clean_ds2$text)
clean_ds2$text <- gsub("\\b[[:alpha:]]{11,}\\b", " ", clean_ds2$text, perl=T)  # To remove words grater than 11 characters.
clean_ds2$text <- gsub("\\b.{1,3}\\b", " ", clean_ds2$text) # To remove words shorter than 4 characters.

#removing all "stopwords"(words that do not add meaning to the topic), and convert the text into a 
#Term Document Matrix. The TDM is then summed up so we get a data.frame of words arranged by how often 
#they are used


tdmCreator <- function(dataframe, stemDoc = F, rmStopwords = T){
  
  Stopwords <- c(stopwords('english'))
  
  tdm <- Corpus(VectorSource(dataframe$text))
  if (isTRUE(rmStopwords)) {
    tdm <- tm_map(tdm, removeWords, c("can", "will", "just", "want", "get"))
  }
  if (isTRUE(stemDoc)) {
    tdm <- tm_map(tdm, stemDocument)
  }
  tdm <- TermDocumentMatrix(tdm,
                            control = list(verbose = TRUE,
                                           asPlain = TRUE,
                                           stopwords = TRUE,
                                           tolower = TRUE,
                                           removeNumbers = TRUE,
                                           stemWords = FALSE,
                                           removePunctuation = TRUE,
                                           removeSeparators = TRUE,
                                           removeTwitter = TRUE,
                                           stem = TRUE,
                                           stripWhitespace = TRUE, 
                                           removeWords = TRUE))
  tdm <- rowSums(as.matrix(tdm))
  tdm <- sort(tdm, decreasing = T)
  df <- data.frame(term = names(tdm), freq = tdm)
  return(df)
}

#cleaning and making TDM


clean_ds1 <- tdmCreator(clean_ds1)

clean_ds2 <- tdmCreator(clean_ds2)



#Selects the 20 most used words.
clean_ds1 <- clean_ds1[1:20,]
clean_ds2 <- clean_ds2[1:20,]
print(clean_ds1)
print(clean_ds2)

#Create bar graph with appropriate colours
#and use coord_flip() to help the labels look nicer.
ds1_plot <- ggplot(clean_ds1, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))

ds2_plot <- ggplot(clean_ds2, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))

#side-by-side plotting for better visualization purpose.
grid.arrange(ds1_plot, ds2_plot, ncol=2)



# visualizing the wordclouds
set.seed(500602494)  # to have same data in case of multiple itterations.   
wordcloud(words = clean_ds1$term, freq = clean_ds1$freq, min.freq = 1, max.term = 200,
          random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
wordcloud(words = clean_ds2$term, freq = clean_ds2$freq, min.freq = 1,
          max.term=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#----------------------------------End of Part-1 and 2----------------------------------------------------#





