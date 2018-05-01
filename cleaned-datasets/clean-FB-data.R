#require the above packages
library(readxl)
library(dplyr)
library(XLConnect)
library(xlsx)
require(data.table)
require(lubridate)
require(ggplot2)
#begin reading data from the 8 Facebook excel files into respective dataframes. 
#read only sheets 1 and 3.
#specify starting row, ending row, starting column, ending column for the 2 sheets.
FB2014Feb <- as.data.frame(readWorksheetFromFile("Facebook Post Level Data/Facebook Insights Data Export (Post Level) - The GRAMMYs - 2014-02-03.xlsx",
                                                 sheet = c("Key metrics", "Lifetime Post Stories by act..."),
                                                 startRow = c(3, 2), endRow = c(444, 443), header = F,
                                                 startCol = c(1,9), endCol = c(18,12)))
FB2014May <- as.data.frame(readWorksheetFromFile("Facebook Post Level Data/Facebook Insights Data Export (Post Level) - The GRAMMYs - 2014-05-14.xlsx",
                                                 sheet = c("Key metrics", "Lifetime Post Stories by act..."),
                                                 startRow = c(3, 2), endRow = c(474, 473), header = F,
                                                 startCol = c(1,9), endCol = c(18,12)))
FB2014Oct <- as.data.frame(readWorksheetFromFile("Facebook Post Level Data/Facebook Insights Data Export (Post Level) - The GRAMMYs - 2014-10-31.xlsx",
                                                 sheet = c("Key metrics", "Lifetime Post Stories by act..."),
                                                 startRow = c(3, 2), endRow = c(511, 510), header = F,
                                                 startCol = c(1,9), endCol = c(18,12)))
FB2015 <- as.data.frame(readWorksheetFromFile("Facebook Post Level Data/Facebook Insights Data Export (Post Level) - The GRAMMYs - 2015-02-16.xlsx",
                                              sheet = c("Key metrics", "Lifetime Post Stories by act..."),
                                              startRow = c(3, 2), endRow = c(426, 425), header = F,
                                              startCol = c(1,9), endCol = c(18,11)))
FB2016Apr <- as.data.frame(readWorksheetFromFile("Facebook Post Level Data/Facebook Insights Data Export (Post Level) - The GRAMMYs - 2016-04-10.xlsx",
                                                 sheet = c("Key metrics", "Lifetime Post Stories by act..."),
                                                 startRow = c(3, 2), endRow = c(502, 501), header = F,
                                                 startCol = c(1,10), endCol = c(19,13)))
FB2016Aug <- as.data.frame(readWorksheetFromFile("Facebook Post Level Data/Facebook Insights Data Export (Post Level) - The GRAMMYs - 2016-08-28.xlsx",
                                                 sheet = c("Key metrics", "Lifetime Post Stories by act..."),
                                                 startRow = c(3, 2), endRow = c(218, 217), header = F,
                                                 startCol = c(1,10), endCol = c(19,13)))
FB2017Jan <- as.data.frame(readWorksheetFromFile("Facebook Post Level Data/Facebook Insights Data Export (Post Level) - Recording Academy - GRAMMYs - 2017-01-26.xlsx",
                                                 sheet = c("Key metrics", "Lifetime Post Stories by act..."),
                                                 startRow = c(3, 2), endRow = c(279, 278), header = F,
                                                 startCol = c(1,10), endCol = c(19,12)))
FB2017Apr <- as.data.frame(readWorksheetFromFile("Facebook Post Level Data/Facebook Insights Data Export (Post Level) - Recording Academy - GRAMMYs - 2017-04-12.xlsx",
                                                 sheet = c("Key metrics", "Lifetime Post Stories by act..."),
                                                 startRow = c(3, 2), endRow = c(440, 439), header = F,
                                                 startCol = c(1,10), endCol = c(19,12)))
#select only the requested columns (Post ID, Permalink, Post Message, Type, Posted date and time
#Lifetime Post organic reach, Lifetime Post Total Impressions, Number of likes, Number of shares,
#Lifetime Post Total Reach, Lifetime Engaged Users, comment)
FB2014Feb_subset <- FB2014Feb[ , c(1,2,3,4,7,9,11,20,22,19,8,14,18)]
FB2014May_subset <- FB2014May[ , c(1,2,3,4,7,9,11,20,22,19,8,14,18)]
FB2014Oct_subset <- FB2014Oct[ , c(1,2,3,4,7,9,11,20,22,19,8,14,18)]
FB2015_subset <- FB2015[ , c(1,2,3,4,7,9,11,20,21,19,8,14,18)]
FB2016Apr_subset <- FB2016Apr[ , c(1,2,3,4,7,10,12,21,23,20,9,15,19)]
FB2016Aug_subset <- FB2016Aug[ , c(1,2,3,4,7,10,12,21,23,20,9,15,19)]
FB2017Jan_subset <- FB2017Jan[ , c(1,2,3,4,7,10,12,21,22,20,9,15,19)]
FB2017Apr_subset <- FB2017Apr[ , c(1,2,3,4,7,10,12,21,22,20,9,15,19)]
#merging the above individual dataframes into one single dataframe using rbindlist()
#rbindlist() merges the dataframes irrespective of different column names
FB_df <- rbindlist(list(FB2017Apr_subset, FB2017Jan_subset, FB2016Aug_subset, FB2016Apr_subset,
                        FB2015_subset, FB2014Oct_subset, FB2014May_subset, FB2014Feb_subset))
#renaming column names
colnames(FB_df)[1] <- "Post ID"
colnames(FB_df)[2] <- "Permalink"
colnames(FB_df)[3] <- "Post Message"
colnames(FB_df)[4] <- "Type"
colnames(FB_df)[5] <- "Posted date and time"
colnames(FB_df)[6] <- "Lifetime Post organic reach"
colnames(FB_df)[7] <- "Lifetime Post Total Impressions"
colnames(FB_df)[8] <- "Number of likes"
colnames(FB_df)[9] <- "Number of shares"
colnames(FB_df)[10] <- "Number of comments"
colnames(FB_df)[11] <- "Lifetime Post Total Reach"
colnames(FB_df)[12] <- "Lifetime Engaged Users"
colnames(FB_df)[13] <- "Lifetime Negative Feedback from Users"
#de-dupe based on Post ID. Inlcude only latest instance of unique rows as identified by the Post ID
FB_unique <- FB_df[!rev(duplicated(rev(FB_df$`Post ID`))), ]
#export the dataset to my working directory in csv format.
write.csv(FB_unique, file = "1-clean-GRAMMY-fb-data.csv", row.names = F)