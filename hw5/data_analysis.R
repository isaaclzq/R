# name: Zhiqiang Liao
# ID: 24229382

# Research Questions
###########################################################################
# 1. Does playing at home really have an advantage for the home team?
# 2. Has the total number of scored points per game have changed over time?
###########################################################################


###########################################################################
# Playing at home has an advantage for the home-team?
###########################################################################
library(readr)
data = read_csv("cleandata/nflweather.csv")
win_summary = aggregate(data[,  "home_win"], list(data$year), function(x) sum(x == TRUE)-sum(x == FALSE))
colnames(win_summary) = c("year", "win_diff")
graph = barplot(win_summary$win_diff, win_summary$year, main = "Difference between home 
        wins and home loses by year", names.arg = win_summary$year, las = 2)

library(ggplot2)
win_line = aggregate(data[, c("home_score", "away_score")], list(data$year), mean)
colnames(win_line) = c("year", "home_score", "away_score")

ggplot(data = win_line) + 
  geom_line(aes(x = year, y = home_score, col = "red")) + 
  geom_point(aes(x = year, y = home_score, col = "red")) +  
  geom_line(aes(x = year, y = away_score, col = "blue")) + 
  geom_point(aes(x = year, y = away_score, col = "blue")) + 
  ylab("score") + scale_color_manual(values = c("red", "blue"), 
                                     name= "type",
                                     labels = c("ave_homescore", "ave_awayscore")) + 
  ggtitle("Average score points per year") +
  theme(legend.position = "top")

###########################################################################
# My own plot
###########################################################################
# In stead of examining year by year, I use pie chart to show the big picture
# of home court advantages. 
win_ratio = aggregate(data[,  "home_win"], list(data$year), function(x) sum(x == TRUE)/length(x))
colnames(win_ratio) = c("year", "ratio")
tag = c("win more", "fair", "lost more")
percentage = round(c(sum(win_ratio$ratio > 0.5)/nrow(win_ratio), 
                     sum(win_ratio$ratio == 0.5)/nrow(win_ratio), 
                     sum(win_ratio$ratio < 0.5)/nrow(win_ratio)), 4) * 100
newTag = paste(percentage, "%", tag, c(" than 50%", ""," than 50%"))
pie(c(sum(win_ratio$ratio > 0.5), sum(win_ratio$ratio == 0.5), sum(win_ratio$ratio < 0.5)),
    labels = newTag, radius = 8, col = c("red", "yellow", "blue"),
    main = "Percentage of Home Team Winning Above 50% Between 1960s And 2010s")


###########################################################################
# Has the total number of scored points per game changed over time?
###########################################################################
avePoint = aggregate(data[,c("home_score", "away_score")], list(data$year), 
                       mean)
colnames(avePoint) = c("year", "home_score", "away_score")
avePoint$totalAve = avePoint$home_score + avePoint$away_score
ggplot(data = avePoint) + 
  geom_line(aes(x = year, y = totalAve)) +
  geom_point(aes(x = year, y = totalAve)) + 
  ggtitle("Average total scored points per year")

###########################################################################
# My own plot
###########################################################################
# I use the same technique as the instruction mentions, but I think that 
# the average may not be able to tell the real trend because there could be
# some outliers (some exceptionally high score games) that bring up the 
# average. In addition to the graph with total average, I also graph with 
# median to verify. My expectation is that both graphs with median and average 
# should show the same trend. 
avePoint = aggregate(data[,c("home_score", "away_score")], list(data$year), mean)
colnames(avePoint) = c("year", "home_score", "away_score")
avePoint$totalAve = avePoint$home_score + avePoint$away_score
avePoint1 = aggregate(data[, "total_score"], list(data$year), median)
avePoint$median_score = avePoint1$x
ggplot(data = avePoint) + 
  geom_line(aes(x = year, y = totalAve, col = "blue")) +
  geom_point(aes(x = year, y = totalAve, col = "blue")) + 
  geom_line(aes(x = year, y = median_score, col = "red")) + 
  geom_point(aes(x = year, y = median_score, col = "red")) +
  ggtitle("Average and Median Total Score Points Per Year") + 
  ylab("Points") + 
  scale_color_manual(values = c("red", "blue"), 
                     name= "type",
                     labels = c("median_score", "average_score")) 

