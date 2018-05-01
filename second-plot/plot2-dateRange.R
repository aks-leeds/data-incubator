# Date Range Visual
# specify interval/bin labels
labels2 <- c("Q3 2013", "Q4 2013", "Q1 2014", "Q2 2014","Q3 2014", "Q4 2014",
             "Q1 2015", "Q2 2015", "Q3 2015", "Q4 2015", "Q1 2016", "Q6 2016", "Q3 2016", "Q4 2016",
             "Q1 2017", "Q2 2017")
# reformat dates for binning
bins2 <- as.POSIXct(fb.df$Date, format = "%Y-%m-%d")
bins2 <- cut(bins2, breaks = "3 months", label = labels2)
summary(bins2)
# plug into dataframe
xaxis2 <- cbind(fb.df, bins2)
plot(fb.df$`Post ID`)
# Create plot
bingraph <- ggplot(fb.df, aes(x = xaxis2$bins2))+
  geom_bar(color = "lightskyblue3", fill = "lightskyblue3")+
  scale_x_discrete(drop = F) +
  ylab("Count of FB Posts") +
  ggtitle("Grammy Post Density Over Time")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", label = "Q2 & Q3 2015", x = 8.5, y = 250, color = "red", size = 3, angle = 45) +
  annotate("text", label = "missing?", x = 8.5, y = 200, color = "red", size = 3, angle = 45)

bingraph
