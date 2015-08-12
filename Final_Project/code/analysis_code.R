
# =============================================================================
# load csv
# =============================================================================
library(stringr)
storms<-read.csv('storms.csv', stringsAsFactors = FALSE)
tracks<-read.csv('tracks.csv', stringsAsFactors = FALSE)


# =============================================================================
# number of storms per year
# =============================================================================

# first extract years from date
year<-str_extract(storms$date, '^[[:digit:]]{4}')
year = year[which(year >= 1980 & year <= 2010)]
# find the frequency of storms in each year
stormPerYear<-table(year)
# plot a bar graph
barplot(as.numeric(stormPerYear),names.arg = unique(year),
        main = 'number of storms per year' )

# =============================================================================
# number of storms per year with wind >=35
# =============================================================================

# Extract years of which the storm's speed exceed 35
## tracks$id[tracks$wind>=35]: find all ids of storms that have wind speed exceed 35 mph.
## unique the id so we know how many storms there are
## find the date of thoses storms that matches the unique(id)
## extract the year from date we can get years which storm wind speed exceed 35
year = substring(text = tracks$date, first = 7, last = 10)
tmpTracks = tracks[which(year >= 1980 & year <= 2010),]
year35<-str_extract(storms$date[unique(tmpTracks$id[tmpTracks$wind>=35])],'^[[:digit:]]{4}')
#year35 = year35[which(year35 >= 1980 & year35 <= 2010)]
# using table to find frequency of storms in each year
stormPerYear35<-table(year35)
# plot a bar graph
barplot(as.numeric(stormPerYear35), names.arg=unique(year35), 
        main='number of storms per year with wind >=35')

# =============================================================================
# number of storms per year with wind >=64
# =============================================================================

# years of which the storm's speed exceed 64
year64<-str_extract(storms$date[unique(tmpTracks$id[tmpTracks$wind>=64])],'[[:digit:]]{4}')
# using table to find frequency of storms in each year
stormPerYear64<-table(year64)
# plot a bar graph
barplot(as.numeric(stormPerYear64), names.arg=unique(year64),
        main='number of storms per year with wind >= 64')

# =============================================================================
# number of storms per year with wind >=96
# =============================================================================

# first extract years of which the storm's speed exceed 96
year96<-str_extract(storms$date[unique(tmpTracks$id[tmpTracks$wind>=96])],'[[:digit:]]{4}')
# using table to find frequency of storms in each year
stormPerYear96<-table(year96)
#plot a bar graph
barplot(as.numeric(stormPerYear96), names.arg=unique(year96),
        main='number of storms per year with wind >= 96')

# =============================================================================
# =============================================================================
# Extract according to Month
# =============================================================================
# =============================================================================



# =============================================================================
# number of storms per month
# =============================================================================
# extract month from the date column stored in storms
monthWithYear<-str_extract(storms$date,'[[:digit:]]{4}-[[:digit:]]{2}')
monthWithYear = monthWithYear[which(substring(text = monthWithYear, first = 1, last = 4) >= "1980" & substring(text = monthWithYear, first = 1, last = 4) <= "2010")]
# find the frequency of storms that happened in each month
month = substring(text = monthWithYear, first = 6, last = 7)
stormPerMonth<-table(month)
# plot
monthName = month.name[sort(as.numeric(unique(month)))]
barplot(as.numeric(stormPerMonth), names.arg = monthName)
# =============================================================================
# number of storms per month with wind >=35

## tracks$id[tracks$wind>=35]: find all ids of storms that have wind speed exceed 35 mph.
## unique the id so we know how many storms there are that wind speed exceed 35
## find the date of thoses storms that matches the unique(id)
## extract the month from date we can get years which storm wind speed exceed 35 
year = substring(text = tracks$date, first = 7, last = 10)
tmpTracks = tracks[which(year >= 1980 & year <= 2010),]

month35<-str_replace_all(str_extract(storms$date[unique(tmpTracks$id[tmpTracks$wind>=35])], '-[[:digit:]]{2}-'),'-','')
stormPerMonth35<-table(month35)
monthName = month.name[sort(as.numeric(unique(month35)))]
barplot(as.numeric(stormPerMonth35), names.arg = monthName)

# =============================================================================
# number of storms per month with wind >=64
month64<-str_replace_all(str_extract(storms$date[unique(tmpTracks$id[tmpTracks$wind>=64])], '-[[:digit:]]{2}-'),'-','')
stormPerMonth64<-table(month64)
monthName = month.name[sort(as.numeric(unique(month64)))]
barplot(as.numeric(stormPerMonth64), names.arg = monthName)

# =============================================================================
# number of storms per month with wind >=96
month96<-str_replace_all(str_extract(storms$date[unique(tmpTracks$id[tmpTracks$wind>=96])], '-[[:digit:]]{2}-'),'-','')
stormPerMonth96<-table(month96)
monthName = month.name[sort(as.numeric(unique(month96)))]
barplot(as.numeric(stormPerMonth96), names.arg = monthName)



# =============================================================================
# Annual numbers of storms >= 35/64/96
## Average 
mean35<-mean(stormPerYear35)
mean64<-mean(stormPerYear64)
mean96<-mean(stormPerYear96)
## std
std35<-sd(stormPerYear35)
std64<-sd(stormPerYear64)
std96<-sd(stormPerYear96)
## 25 percentile
Q35<-summary(as.numeric(stormPerYear35))[2]
Q64<-summary(as.numeric(stormPerYear64))[2]
Q96<-summary(as.numeric(stormPerYear96))[2]
## median
median35<-summary(as.numeric(stormPerYear35))[3]
median64<-summary(as.numeric(stormPerYear64))[3]
median96<-summary(as.numeric(stormPerYear96))[3]
## 75 percentile
q35<-summary(as.numeric(stormPerYear35))[5]
q64<-summary(as.numeric(stormPerYear64))[5]
q96<-summary(as.numeric(stormPerYear96))[5]

## table
annual<-matrix(c(mean35,std35,Q35, median35,q35,
                 mean64,std64,Q64, median64,q64,
                 mean96,std96,Q96, median96,q96), ncol=5, byrow=TRUE)
colnames(annual)<-c('Avg','Std.Dev','25th','50th','75th')
rownames(annual)<-c('35 knots','64 knots', '96 knots')
annual<-as.table(annual)

# =============================================================================
# Regression Line
# mean pressure and mean wind speed for each storm
vec<-c(1:1777)
pressMean1<-sapply(vec,function(x) mean(tmpTracks$press[tmpTracks$id==x]))
# remove zeros
pressMean<-pressMean1[pressMean1!=0]
# mean wind speed
windMean1<-sapply(vec,function(x) mean(tmpTracks$wind[tmpTracks$id==x]))
#remove zeros
windMean<- windMean1[pressMean1!=0]

reg = lm(pressMean ~ windMean)
plot(windMean, pressMean,xlab = 'mean wind speed', ylab = 'mean pressure')
title('mean pressure VS mean wind speed for each storm ')
abline(reg)

# =============================================================================
# Regression analysis 2
# median pressure and median wind speed for each storm
vec<-c(1:1777)
pressMedian1<-sapply(vec,function(x) median(tmpTracks$press[tmpTracks$id==x]))
# mean wind speed
windMedian1<-sapply(vec,function(x) median(tmpTracks$wind[tmpTracks$id==x]))
# remove zeros
pressMedian<-pressMedian1[pressMedian1!=0]
windMedian<-windMedian1[pressMedian1!=0]

reg1 = lm(pressMean ~ windMean)
plot(windMean, pressMean,xlab = 'mean wind speed', ylab = 'mean pressure')
title('mean pressure VS mean wind speed for each storm ')
abline(reg1)









