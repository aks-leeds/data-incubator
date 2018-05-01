#require packages
library(httr)
library(XML)
library(plyr)
library(dplyr)
library(magrittr)
library(twitteR)
library(ggplot2)
library(gridExtra)
library(scales)
library(reshape2)
library(wesanderson)
library(stringr)
library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(purrr)
library(tidyr)
library(lubridate)
library(scales)
library(tidytext)

##Function to find words in a string of words.
find_word = function(words, text){
  a=0
  for (i in 1:length(words))
  {
    if (grepl(words[i],text, ignore.case = T))
    {
      a=a+1
    }
  }
  if (a > 0) print("History") else
    print("Other Topics") 
}
#initialising the input text to NAs, creating a placeholder for the tweets.
input_text <- NA
#creating list of words to identify content as 'history'
list_of_words <- c("16th GRAMMYs", "40 years of legendary performances", "look back",
                   "14th GRAMMYs in 1972", "honor", "lost in the music community this year",
                   "1963", "10 years ago", "20th GRAMMYs", "1978", "Lifetime Achievement",
                   "Legends", "itstonybennett", "Throwback", "legendary", 
                   "In Loving Memory Remembering those we lost in the music community", 
                   "1978", "tribute to legendary", "lost", "tribute to", "GRAMMYs during the 1970s",
                   "GRAMMY Awards during the 1970s", "GRAMMY Hall of Fame", "your idols made music history",
                   "24th GRAMMYs", "GRAMMYs Lifetime Achievement Award", "37th GRAMMYs",
                   "look back at the most memorable", "legends", "first time in over 14 years", 
                   "38th GRAMMYs", "4th GRAMMYs in 1962", "11th GRAMMYs", "27th GRAMMYs in 1985",
                   "10th GRAMMYs in 1967", "first to win", 
                   '1950',
                   '1967',
                   '10th',
                   '11th',
                   '12th',
                   '13th',
                   '15th',
                   '16th',
                   '17th',
                   '18th',
                   '1940s',
                   '1950s',
                   '1960s',
                   '1970s',
                   '1980s',
                   '1990s',
                   '19th',
                   '1st',
                   '20th',
                   '21st',
                   '22nd',
                   '23rd',
                   '24th',
                   '25th',
                   '27th',
                   '28th',
                   '29th',
                   '30th',
                   '31st',
                   '32nd',
                   '33rd',
                   '34th',
                   '35th',
                   '36th',
                   '37th',
                   '38th',
                   '39th',
                   '40th',
                   '41st',
                   '42nd',
                   '46th',
                   'achievement',
                   'ago',
                   'barbrastreisand',
                   'beatles',
                   'beatles50',
                   'beegees',
                   'bobdylan',
                   'carole',
                   'celebrating',
                   'celinedion',
                   'classic',
                   'ddlovato',
                   'documentary',
                   'flashback',
                   'franksinatra',
                   'george',
                   'grammyken',
                   'grammysalute',
                   'herbiehancock',
                   'history',
                   'honor',
                   'honored',
                   'honoree',
                   'honoring',
                   'honors',
                   'itstonybennett',
                   'legacy',
                   'legendary',
                   'legends',
                   'lifetime',
                   'lionelrichie',
                   'madonna',
                   'mapfund',
                   'mariahcarey',
                   'maryjblige',
                   'memorable',
                   'memories',
                   'memory',
                   'metallica',
                   'michael',
                   'michaeljackson',
                   'musicares',
                   'nirvana',
                   'outkast',
                   'pandewing',
                   'past',
                   'paulmccartney',
                   'relive',
                   'rewind',
                   'rickrubin',
                   'salute',
                   'sinatra',
                   'sinatra100',
                   'stevie',
                   'steviewonder',
                   'tbt',
                   'thebeatles',
                   'thebonnieraitt',
                   'tribute',
                   'williams')

###################### Twitter ######################
#reading in the cleaned data file
cleaned_twitter <- read.csv("1-clean-GRAMMY-twitter-data.csv")
#create a copy of the twitter data
exp_cleaned_twitter <- cleaned_twitter
#convert all text to lower case
exp_cleaned_twitter$Text <- tolower(exp_cleaned_twitter$Text)
# Replace @UserName
exp_cleaned_twitter$Text <- gsub("@\\w+", "", exp_cleaned_twitter$Text)
# Remove punctuation
exp_cleaned_twitter$Text <- gsub("[[:punct:]]", "", exp_cleaned_twitter$Text)
# Remove links
exp_cleaned_twitter$Text <- gsub("http\\w+", "", exp_cleaned_twitter$Text)
#creating a new column "History" to check whether a tweet is talking about the history of music
exp_cleaned_twitter$History <- rep(NA, length.out=nrow(exp_cleaned_twitter))
#setting the History column as counter. Populates 'History' for tweets related to history or music
##  and 'Other Topics' for tweets related to topics other than history of music
for(i in 1:nrow(exp_cleaned_twitter)){
  input_text <- exp_cleaned_twitter$Text[i]
  exp_cleaned_twitter$History[i] <- find_word(list_of_words,input_text)
}
#creating subsets of the dataframes for content related to history of music
## and other topics
twitter_history <- subset(exp_cleaned_twitter, History == "History")
twitter_other_topics <- subset(exp_cleaned_twitter, History == "Other Topics")
tweets_by_topic <-
  exp_cleaned_twitter %>%
  ddply(~ History, function(x) {
    data.frame(`Number of Tweets` = nrow(x),
               `Average Favorites` = mean(x$Favorite.count) %>% round(digits = 0),
               `Average Retweets` = mean(x$Retweet.count) %>% round(digits = 0)
    )})
#renaming the column
colnames(tweets_by_topic)[1] <- "Type of Content"
colnames(tweets_by_topic)[2] <- "Number of Tweets"
colnames(tweets_by_topic)[3] <- "Average Favorites"
colnames(tweets_by_topic)[4] <- "Average Retweets"
#Calculating the Favorite Rate rounded off to 2 digits
tweets_by_topic$Favorite_Rate <- round((tweets_by_topic$`Average Favorites`/tweets_by_topic$`Number of Tweets`)*100, 2)
#Calculating the Retweets Rate rounded off to 2 digits
tweets_by_topic$Retweet_Rate <- round((tweets_by_topic$`Average Retweets`/tweets_by_topic$`Number of Tweets`)*100, 2)
#melt the data frame for plotting
tweets.history.m <- melt(tweets_by_topic, id.vars="Type of Content")

###################### Facebook ######################
#reading in the cleaned dataset. Used our consolidated datafile for Q1.
cleaned_FB <- read.csv("1-clean-GRAMMY-fb-data.csv")
#checking for NAs or missing values
colSums(is.na(cleaned_FB))
#replacing missing values in 'like' and 'share' columns with 0
cleaned_FB$Number.of.likes[is.na(cleaned_FB$Number.of.likes)] <- 0
cleaned_FB$Number.of.shares[is.na(cleaned_FB$Number.of.shares)] <- 0
cleaned_FB$Number.of.comments[is.na(cleaned_FB$Number.of.comments)] <- 0
#create a copy of the facebook data
exp_cleaned_FB <- cleaned_FB
#convert all text to lower case
exp_cleaned_FB$Post.Message <- tolower(exp_cleaned_FB$Post.Message)
# Replace @UserName
exp_cleaned_FB$Post.Message <- gsub("@\\w+", "", exp_cleaned_FB$Post.Message)
# Remove punctuation
exp_cleaned_FB$Post.Message <- gsub("[[:punct:]]", "", exp_cleaned_FB$Post.Message)
# Remove links
exp_cleaned_FB$Post.Message <- gsub("http\\w+", "", exp_cleaned_FB$Post.Message)
#creating a new column "History" to check whether a post is talking about the history of music
exp_cleaned_FB$`Type of Content` <- rep(NA, length.out=nrow(exp_cleaned_FB))
#setting the History column as counter. Populates 'History' for tweets related to history or music
##  and 'Other Topics' for tweets related to topics other than history of music
for(i in 1:nrow(exp_cleaned_FB)){
  input_text <- exp_cleaned_FB$Post.Message[i]
  exp_cleaned_FB$`Type of Content`[i] <- find_word(list_of_words,input_text)
}
#creating subsets of the dataframes for content related to history of music
## and other topics
fb_history <- subset(exp_cleaned_FB, `Type of Content` == "History")
fb_other_topics <- subset(exp_cleaned_FB, `Type of Content` == "Other Topics")
posts_by_topic <-
  exp_cleaned_FB %>%
  ddply(~ `Type of Content`, function(x) {
    data.frame(`Number of Posts` = nrow(x),
               `Average Likes` = mean(x$Number.of.likes) %>% round(digits = 0),
               `Average Shares` = mean(x$Number.of.shares) %>% round(digits = 0),
               `Average Comments` = mean(x$Number.of.comments) %>% round(digits = 0)
    )})
#renaming the column
colnames(posts_by_topic)[2] <- "Number of Posts"
colnames(posts_by_topic)[3] <- "Average Likes"
colnames(posts_by_topic)[4] <- "Average Shares"
colnames(posts_by_topic)[5] <- "Average Comments"
#Calculating the Like Rate rounded off to 2 digits
posts_by_topic$Like_Rate <- round((posts_by_topic$`Average Likes`/posts_by_topic$`Number of Posts`)*100, 2)
#Calculating the Share Rate rounded off to 2 digits
posts_by_topic$Share_Rate <- round((posts_by_topic$`Average Shares`/posts_by_topic$`Number of Posts`)*100, 2)
#Calculating the Comment Rate rounded off to 2 digits
posts_by_topic$Comment_Rate <- round((posts_by_topic$`Average Comments`/posts_by_topic$`Number of Posts`)*100, 2)
#melt the data frame for plotting
tweets.history.m <- melt(tweets_by_topic, id.vars="Type of Content")