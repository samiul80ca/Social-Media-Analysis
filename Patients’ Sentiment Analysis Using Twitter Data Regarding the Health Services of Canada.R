#-----------------------------------------Initial Setup----------------------------------------------
#====================================================================================================
#create a function to check for installed packages and install them if they are not installed
install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

# usage
required.packages <- c("twitteR", "httr","rjson","RColorBrewer","ggpubr", "caTools","syuzhet", "ggplot2","RTextTools","e1071","gridExtra","tm","stringr","wordcloud", "dplyr", "stringr","lubridate","Scale","hms","scales")
install(required.packages)


#----------------------------------------Twitter Oauth Setup---------------------------------------#
#==================================================================================================#
# To identify user and check account validity
consumer_key <- "I4y3uES4BxEsL7p6WZDuS7jX2"
consumer_secret <- "BzAaybDDUuDEf3Sw8TjZpMUQuom60FP55qojjUMc5k6HN2h29n"

# To authorize data operations
access_token <- "979467931724730369-fXwd1hC7mPSwyJkQV4FPAA2eDjr9GwK"
access_secret <- "oILfRFgahKbXzx3t6fsX7DmVbSShxGoNd0UljUwpMSN9Y"

# Twitter oauth setup
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#------------------------------Searching, loading and prepare sample datasets--------------------------------#
#==================================================================================================#

#dataset1  <-twListToDF( searchTwitter("#ptsafety", n=3000) )
#dataset2  <-twListToDF( searchTwitter("@GovCanHealth", n=3000) )
#dataset3  <-twListToDF( searchTwitter("@PatientsCanada", n=1000) )
#dataset4  <-twListToDF( searchTwitter("#cdnhealth", n=4000) )
#dataset5  <-twListToDF( searchTwitter("@HQOntario", n=1000) )
#dataset6  <-twListToDF( searchTwitter("@Patient_Safety", n=1000) )
#dataset7  <-twListToDF( searchTwitter("@OntariosDoctors", n=5000) )

#health_tweet <- rbind.data.frame(dataset1,dataset2,dataset3,dataset4,dataset5,dataset6,dataset7, deparse.level = 1, make.row.names = T,stringsAsFactors = default.stringsAsFactors())
#write.csv(health_tweet, file = "CDN_health_tweet_data.csv")

#--------------------------------------Exploratory Analysis-----------------------------------------#
#===================================================================================================#
#How many unique users are in the data set? We have 12249 unique users.
  
health_tweet <- read.csv("CDN_health_tweet_data.csv") 
 length(unique(health_tweet$id))

  # Tweet frequency over the data collection period
  ggplot(data = health_tweet,
         
         aes(wday(created,label=TRUE), main="Number of tweets by date",
             
             group=factor(date(created)),color=factor(date(created))))+
    
    geom_point(stat="count")+
    
    geom_point(stat="count")+
    
    labs(x="day",colour="date")+
    
    xlab("day")+ylab("Number of tweets") + ggtitle("Tweets count by the day and date")+ 
    
    theme_bw() +  theme(plot.title = element_text(hjust=0.5))
  
  
  #On which day users tend to tweet most?
  ggplot(data = health_tweet, aes(x = wday(created, label = TRUE))) +
    
    geom_bar(aes(fill = ..count..)) + ggtitle("Tweet density through days")+ 

     
    xlab("Day of the week") + ylab("Number of tweets") + 
    
    theme_bw() +  theme(plot.title = element_text(hjust=0.5))
    
    scale_fill_gradient(low = "turquoise3", high = "darkgreen")
  
  
  #At what time of the day users tweeted most?
  # Extract only time from the timestamp, i.e., hour, minute and second 
  
  health_tweet$time <- hms::hms(second(health_tweet$created), 
                             
                             minute(health_tweet$created), 
                             
                             hour(health_tweet$created))
  
  # Time of tweets
  
  health_tweet$time <- as.POSIXct(health_tweet$time)
  
  ggplot(data = health_tweet)+
    
    geom_density(aes(x = time, y = ..scaled..),
                 
                 fill="darkolivegreen4", alpha=0.3) + 
    
    xlab("Time") + ylab("Density") + ggtitle("Tweet density over the time of the day") +
    
    scale_x_datetime(breaks = date_breaks("2 hours"), 
                     
                     labels = date_format("%H:%M")) + 
    
    theme_bw() +  theme(plot.title = element_text(hjust=0.5))

  # Add boolean feature isReply
  health_tweet$isReply <- ifelse((!is.na(health_tweet$replyToSID) |
                                !is.na(health_tweet$replyToSN) |
                                !is.na(health_tweet$replyToUID)), TRUE, FALSE)

  # Number of reply vs other posts and number of Retweets vs other posts in percentage.
  plot1 <- ggplot(health_tweet, aes(x = health_tweet$isReply)) +
    geom_bar(aes(y=(..count..) / sum(..count..)), fill = "brown") + 
    theme(legend.position="none", axis.title.x = element_blank()) +
    scale_y_continuous(labels=percent) +
    labs(title = "Percentage of Reply vs. other posts", y="Percentage of tweets") + 
    scale_x_discrete(labels=c("Other posts", "Reply posts"))  
  
  plot2 <- ggplot(health_tweet, aes(x = health_tweet$isRetweet)) +
    geom_bar(aes(y=(..count..) / sum(..count..)),fill = "brown") + 
    theme(legend.position="none", axis.title.x = element_blank()) +
    scale_y_continuous(labels=percent) +
    labs(y="Tweets", title="Percentage of Retweets vs other posts") +
    scale_x_discrete(labels=c("Other posts","Retweets"))
  
  grid.arrange(plot1, plot2, ncol=2)
  
  #Plot comparing the distribution of the number of times a post was retweeted, for the original tweets vs retweets.
  ggplot(data = health_tweet, aes(x = retweetCount)) +
    geom_histogram(aes(fill = ..count..), bins=30, binwidth = 1) +
    theme(legend.position = "none") +
    xlab("Retweet count") + ylab("Number of tweets") + ggtitle("Original tweets vs. Retweets") +
    xlim(0,30) +
    scale_fill_gradient(low = "red", high = "green") +
    facet_grid(isReply ~ .)
  
  #------------------------------------Text Analysis-----------------------------------------------
  #=================================================================================================
  
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
  clean_ds <- textScrubber(health_tweet)
  # Performing some extra cleanup to remove noises
  clean_ds$text <-  gsub("http | sdk ", " ", clean_ds$text)
  clean_ds$text <- gsub("\\b[[:alpha:]]{11,}\\b", " ", clean_ds$text, perl=T)  # To remove words grater than 11 characters.
  clean_ds$text <- gsub("\\b.{1,3}\\b", " ", clean_ds$text) # To remove words shorter than 4 characters.

  #Removing all "stopwords", and convert the text into a Term Document Matrix. 
  
  
  tdmCreator <- function(dataframe, stemDoc = F, rmStopwords = T){
    
    Stopwords <- c(stopwords('english'))
    
    tdm <- Corpus(VectorSource(dataframe$text))
    if (isTRUE(rmStopwords)) {
      tdm <- tm_map(tdm, removeWords, c("work", "take", "second", "nurses", "patients", "cannabis","docs", "many", "canadians","years","today", "make", "wynne", "ontario", "canada", "kathleen", "cdnhealth", "onpoli", "join", "people", "onhealth","canadian", "will", "hqontario", "fordnation", "health", "ptsafety"))
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
  
  #Cleaning using TDM 
  
  clean_ds1 <- tdmCreator(clean_ds)  
  
  #Observing 20 most used words.
  clean_ds_20 <- clean_ds1[1:20,]  
print(clean_ds_20)  
ggplot(clean_ds_20, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Used") + ylab("How Often") + 
  coord_flip() + theme(text=element_text(size=25,face="bold"))

# visualizing the wordclouds
set.seed(6)  # to have same data in case of multiple itterations.   
wordcloud(words = clean_ds1$term, freq = clean_ds1$freq, min.freq = 1, max.term = 200,
          random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


# Sentiment Analysis


# Converting tweets to ASCII to tackle strange characters

tweet_text <- health_tweet$text
tweet_text <- iconv(tweet_text, from="UTF-8", to="ASCII", sub="")

# removing retweets

tweet_text<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweet_text)

# removing mentions

tweet_text<-gsub("@\\w+","",tweet_text)

# Finding sentiment
sentiment<-get_nrc_sentiment((tweet_text))

sentimentscores<-data.frame(colSums(sentiment[,]))

names(sentimentscores) <- "Score"

sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)

rownames(sentimentscores) <- NULL

#ploting the sentiments
barplot(
  sort(colSums(prop.table(sentiment[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in tweet text", xlab="Percentage"
)


ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  
  geom_bar(aes(fill=sentiment),stat = "identity")+
  
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  
  ggtitle("Patients' sentiment based on scores")+
  
  theme_minimal() 

#_________________________________________END______________________________________________________  
