# code to check and install required R packages
missinglibs = c('readxl', 'dplyr', 'ggplot2', 'lubridate', 'plyr')

missinglibs = missinglibs[!missinglibs %in% installed.packages()]

if (!identical(missinglibs, character(0))) {
  for (i in 1:length(missinglibs)) {
    install.packages(missinglibs[i])
  }
} else {
  print("All required packages are already installed.")
}

#require package "readxl"
library(readxl)

#reading the data from the 4 twitter files into respective dataframes
twitter2014 <- read_excel("Twitter Data/twitonomy-2014-02-04.xlsx", skip = 3, range = "A4:I3200")
twitter2015 <- read_excel("Twitter Data/twitonomy-2015-02-16.xlsx", skip = 3, range = "A4:I3199")
twitter2016 <- read_excel("Twitter Data/twitonomy-2016-04-10.xlsx", skip = 3, range = "A4:I3204")
twitter2017 <- read_excel("Twitter Data/twitonomy-2017-04-12.xlsx", skip = 3, range = "A4:I3203")

#combining the above 4 dataframes into one single dataframe
twitter_DF <- rbind(twitter2014, twitter2015, twitter2016, twitter2017)

#Include only rows that have Type = New
twitter_subset <- subset(twitter_DF, Type == "New", select = -c(Handle, Name, Platform))

#converting date from GMT to PST
twitterAdjData <- as.POSIXct(strptime(twitter_subset$`Date (GMT)`, format = "%d/%m/%Y%H:%M:%OS",
                                      tz ="GMT"))
twitter_subset$`Date (GMT)` <- format(twitterAdjData, tz ="America/Vancouver", usetz = TRUE)

#renaming the 'Date (GMT)' column to 'Timestamp (PST)'
colnames(twitter_subset)[1] <- "Timestamp (PST)"

#de-dupe based on latest instance of unique URL using "dplyr" package
require(dplyr)
twitter_unique <- twitter_subset %>% group_by(URL) %>% slice(which.max(as.Date(`Timestamp (PST)`,
                                                                               '%Y-%m-%d')))
#export the dataset to my working directory in csv format.
write.csv(twitter_unique, "1-clean-GRAMMY-twitter-data.csv", row.names = F)