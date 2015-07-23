# name: Zhiqiang Liao
# ID: 24229382

############################################################
# Importing file weather_20131231.csv
############################################################
data = read_csv(file = "~/Desktop/R/solution/raw_nflweather.csv")
library("stringr")

###########################################################
# remove the percent symbol % from the values in 
# column humidity and convert such values to numeric format
###########################################################
colHumidity = data$humidity
colHumidity = gsub(pattern = "%", replacement = "", x = colHumidity)
colHumidity = as.numeric(colHumidity)
data$humidity = colHumidity

###########################################################
# extract the temperature values from column weather; and
# create a column temperature2 with these values (as numeric).
###########################################################
colWeather = data$weather
colWeather = unlist(str_extract_all(string = colWeather, pattern = "-?[[:digit:]]?[[:digit:]]?[[:digit:]] d"))
colWeather = unlist(gsub(pattern = " d", replacement = "", x = colWeather))
colWeather = as.numeric(colWeather)
data$temperature2 = colWeather


###########################################################
# extract the humidity values from column weather; and create 
# a column humidity2 with these values (as numeric)
###########################################################
extWeather = data$weather
extHumidity = unlist(str_extract_all(string = extWeather, pattern = " [[:digit:]]?[[:digit:]]?[[:digit:]]%"))
extHumidity = gsub(pattern = "%", replacement = "", x = extHumidity)
extHumidity = str_trim(string = extHumidity, side = "both")
extHumidity = as.numeric(extHumidity)
data[which(!is.na(data$humidity)), "humidity2"] = extHumidity
data[which(is.na(data$humidity)), "humidity2"] = NA

###########################################################
# extract the wind speed values from column weather; and create
# a column wind2 with these values (as numeric)
###########################################################
extWeather = data$weather
extWind = unlist(str_extract_all(string = extWeather, pattern = "wind.+mph"))
extWind = gsub(pattern = "wind | mph", replacement = "", x = extWind)
extWind = as.numeric(extWind)
data[which(!is.na(data$wind_mph)), "wind2"] = extWind
data[which(is.na(data$wind_mph)), "wind2"] = NA


###########################################################
# Create a column year that contains the number of the year (as numeric).
###########################################################
colTime = data$date
year = substring(colTime, first = nchar(colTime)-3, last = nchar(colTime))
data$year = as.numeric(year)

###########################################################
# Create a column monthnum that contains the number of the month (as numeric)
###########################################################
month = unlist(str_extract_all(string = colTime, pattern = "^[[:digit:]][[:digit:]]?"))
data$monthnum = as.numeric(month)

###########################################################
# Create a column month that contains the name of the corresponding 
# month (as factor); e.g. if the month number is 9 then month will be september.
###########################################################
monthEng = vector(length = length(data$date))
monthEng[month == "1"] = "January"
monthEng[month == "2"] = "Feburary"
monthEng[month == "3"] = "March"
monthEng[month == "4"] = "April"
monthEng[month == "5"] = "May"
monthEng[month == "6"] = "June"
monthEng[month == "7"] = "July"
monthEng[month == "8"] = "August"
monthEng[month == "9"] = "September"
monthEng[month == "10"] = "October"
monthEng[month == "11"] = "November"
monthEng[month == "12"] = "December"
data$month = as.factor(monthEng)


###########################################################
# Create a column decade that indicates the corresponding decade 
# (as factor) of each played game. Use labels: 1960s, 1970s, 1980s,
# 1990s, 2000s, 2010s. For instance, all games between 1970 and 1979 
# will have the associated decade 1970s.
###########################################################
decadeEng = vector(length = length(data$date))
decadeEng[which(year >= 1960 & year < 1970)] = "1960s"
decadeEng[which(year >= 1970 & year < 1980)] = "1970s"
decadeEng[which(year >= 1980 & year < 1990)] = "1980s"
decadeEng[which(year >= 1990 & year < 2000)] = "1990s"
decadeEng[which(year >= 2000 & year < 2010)] = "2000s"
decadeEng[which(year >= 2010)] = "2010s"
data$decade = as.factor(decadeEng)


# Scores Information
###########################################################
# Create a column total_score that contains the total number
# of scored points in each game. In other words, the sum of 
# home_score and away_score
###########################################################
total_score = data$home_score + data$away_score
data$total_score = total_score

###########################################################
# Create a column diff_score that indicates the difference 
# of home_score and away_score. In other words, the subtraction 
# of home_score and away_score
###########################################################
diff_score = data$home_score - data$away_score
data$diff_score = diff_score

###########################################################
# Create a column home_win that shows whether home_score is 
# greater than away_score. This column will have logical values (TRUE or FALSE)
###########################################################
home_win = (data$home_score > data$away_score)
data$home_win = home_win

# Basic Exploration
###########################################################
# Inspect variables home_score, away_score, temperature, and 
# wind_mph by getting summary statistics (this is just a first 
# inspection making sure there are no “abnormal” values)
###########################################################
summary(data$home_score)
summary(data$away_score)
summary(data$temperature)
summary(data$wind_mph)

###########################################################
# Visually inspect variables home_score, away_score, temperature,
# humidity, and wind_mph (these are “quick and basic” plots)
###########################################################
pairs(data[, c("home_score", "away_score", "temperature", "humidity", "wind_mph")])


###########################################################
# What team has the maximum home score?
###########################################################
data$home_team[max(data$home_score)]

###########################################################
# What team has the maximum away score?
###########################################################
data$home_team[max(data$away_score)]

###########################################################
# What is the most common home score?
###########################################################
rownames(sort(table(data$home_score), decreasing = TRUE))[1]

###########################################################
# What is the most common away score?
###########################################################
rownames(sort(table(data$away_score), decreasing = TRUE))[1]

###########################################################
# What has been the maximum temperature in a game?
###########################################################
max(data$temperature2)

###########################################################
# What was the date of the maximum temperature?
###########################################################
tmp = max(data$temperature2)
data$date[data$temperature2 == tmp]

###########################################################
# What has been the minimum temperature in a game?
###########################################################
min(data$temperature2)

###########################################################
# What was the date of the minimum temperature?
###########################################################
tmp = min(data$temperature2)
data$date[data$temperature2 == tmp]

###########################################################
# How many games have been played with a temperature of 90 degrees or more?
###########################################################
sum(data$temperature2 >= 90)

###########################################################
# How many games have been played with a temperature below 0 degrees (do not include 0)?
###########################################################
sum(data$temperature2 < 0)

###########################################################
# What is the most common temperature?
###########################################################
rownames(sort(table(data$temperature2), decreasing = TRUE))[1]


###########################################################
# Make a bar chart with the frequency table of temperatures? 
# Is there anything that catches your attention?
###########################################################
barplot(table(data$temperature2))
#Yes. The frequency of 72 degree is exceptionally high.

# Data files
tmp = subset(x = data, select = c(id, home_team, home_score, away_team, away_score, total_score, diff_score, home_win, date, year, month, decade, temperature, humidity, wind_mph))
dir.create("cleandata")
for (param in c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s")){
  output = tmp[which(tmp$decade == param),]
  file_name = sprintf("cleandata/nflweather%s.csv", param)
  write_csv(x = output, path = file_name)
}
write_csv(x = tmp, path = "cleandata/nflweather.csv")












