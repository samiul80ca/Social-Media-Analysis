# installing required libraries
install.packages("twitteR")  # To use the libraries like 'twitteR','httr'& 'rjson',which provides access to the Twitter API.
install.packages("SnowballC") # To use the SnowballC library.
install.packages("tm")        # To use the 'tm' library used for text mining. 
install.packages("ggplot2")   # To use the library 'ggplot2' used for plotting purpose.
install.packages("ggpubr")    # To use the library 'ggpubr' used for plotting purpose.
install.packages("gridExtra") # To use the library 'gridExtra' used for side by side plotting.
install.packages("stringr")   # To use the library which contains functions to manipulate regular expressions. 
install.packages("wordcloud") # To use the 'wordcloud' library for text visualization purpose
install.packages("RColorBrewer") # To use different color palettes in wordcloud.


# Settingup required libraries.

library(twitteR)    # To use the functions which provides access to the Twitter API. 
library(lubridate)  # For algebraic manipulation on date-time and time-span objects.
library(dplyr)      # for working with data frame like objects, both in memory and out of memory.
library(httr)       # for working with HTTP organised by HTTP verbs 
library(rjson)      # Converts R object into JSON objects and vice-versa
library(tm)         # contains text mining functions 
library(SnowballC)  # Contains text stemming functions
library(stringr)   # To use the function 'regex'
library(wordcloud)  # To use the function for visualization of wordcloud
library(RColorBrewer) # To use the function used for different text colors in wordcloud
library(ggplot2)    # To create elegant data visualizations using the grammar of graphics
library(ggpubr)     # Supporting library the function 'grid.arrange' used for side by side plotting.
library(gridExtra)  # To use the function 'grid.arrange' used for side by side plotting.

#Data fetching process

# To identify user and check account validity
consumer_key <- "005xyO9oWY9QndMoPAwFADzRb"
consumer_secret <- "uY2u756WScAEtlen4pTGtRSsLzOnGuoalLJo4N35qLpn2eojkE"

# To authorize data operations
access_token <- "957768651960209408-VF7wXJnMhDn5Bj2td8XN4mnbTOngMk8"
access_secret <- "boXFblT9OXDL2PSoj3CBn0kxrXV0zgZNrJnJApO0PYSLf"

# Twitter oauth setup
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Extract 200 tweets from Hillary's account   

clinton_tweets <- userTimeline(user = "@HillaryClinton",
                               n = 200, includeRts = FALSE, retryOnRateLimit = 2000)

#Extract 200 tweets from Trump's account
trump_tweets <- userTimeline(user = "@realDonaldTrump",
                             n = 200, includeRts = FALSE, retryOnRateLimit = 2000)  

#Put the tweets lists into a data frame
clinton_tweets <- twListToDF(clinton_tweets)
trump_tweets <- twListToDF(trump_tweets)

#Remove punctuation, numbers, html-links and unecessary spaces:
textScrubber <- function(dataframe) {
  
  dataframe$text <-  gsub("-", " ", dataframe$text)
  dataframe$text <-  gsub("&", " ", dataframe$text)
  dataframe$text <-  gsub("[[:punct:]]", " ", dataframe$text)
  dataframe$text <-  gsub("[[:digit:]]", "", dataframe$text)
  dataframe$text <-  gsub("http\\w+", "", dataframe$text)
  dataframe$text <-  gsub("\n", " ", dataframe$text)
  dataframe$text <-  gsub("[ \t]{2,}", "", dataframe$text)
  dataframe$text <-  gsub("^\\s+|\\s+$", "", dataframe$text)
  dataframe$text <-  gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", dataframe$text)  # To remove words shorter than 3 chars
  dataframe$text <-  tolower(dataframe$text)
  return(dataframe)
}

#cleaning tweet files
clinton_tweets <- textScrubber(clinton_tweets)
trump_tweets <- textScrubber(trump_tweets)


# To Keep hashtags

hashtag.regex <- regex("(?<=^|\\s)#\\S+")
clinton_tweets_hashtags <- str_extract_all(clinton_tweets$text, hashtag.regex)  # remove everything that is not a hashtag
clinton_tweets_hashtags <- clinton_tweets_hashtags[lapply(clinton_tweets_hashtags,length)>0]  # remove empty rows that don't have any hashtag
clinton_tweets_hashtags <- unlist(clinton_tweets_hashtags, recursive = TRUE)  #reverse items those contain more than one hashtag per tweet into unified list

trump_tweets_hashtags<- str_extract_all(trump_tweets$text, hashtag.regex)
trump_tweets_hashtags <- trump_tweets_hashtags[lapply(trump_tweets_hashtags,length)>0]
trump_tweets_hashtags <- unlist(trump_tweets_hashtags, recursive = TRUE)


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


clinton_tweets <- tdmCreator(clinton_tweets)

trump_tweets <- tdmCreator(trump_tweets)

#Selects the 20 most used words.
trump_tweets <- trump_tweets[1:20,]
clinton_tweets <- clinton_tweets[1:20,]
head(trump_tweets)
head(clinton_tweets)

#Create bar graph with appropriate colours
#and use coord_flip() to help the labels look nicer.
trump_plot <- ggplot(trump_tweets, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))

clinton_plot <- ggplot(clinton_tweets, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold"))

#side-by-side plotting for better visualization purpose.
grid.arrange(trump_plot, clinton_plot, ncol=2)



# visualizing the wordclouds
set.seed(500602494)  # to have same data in case of multiple itterations.   
wordcloud(words = trump_tweets$term, freq = trump_tweets$freq, min.freq = 1, max.term = 200,
          random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
wordcloud(words = clinton_tweets$term, freq = clinton_tweets$freq, min.freq = 1,
          max.term=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


