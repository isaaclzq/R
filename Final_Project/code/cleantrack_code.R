# =============================================================================
# Stat 133: Final Project
# Part1 Data Cleaning
# Description: work with XML data and introduce the R package “XML”
# Data: International Best Track Archive for Climate Stewardship (IBTrACS)
# ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/cxml/year.
# =============================================================================

# Name:Tong Zhang 24540972, Zhiqiang Liao 24229382

# =============================================================================
# Load packages
library(XML)
library(stringr)
library(maps)

# =============================================================================
# Parse the data
# =============================================================================
data<-readLines("rawdata")
storms<-read.csv('storms.csv')
# =============================================================================
# extract ID 
# =============================================================================
id<-str_replace(str_extract(data,'SNBR= *[[:digit:]]*'), 'SNBR=| ','')
id<-str_trim(id[!is.na(id)], side='left')
id<-rep(id, times=storms$days*4) 
# index<-1
# repName<-vector()
# position<-vector() # position of id 
# for (i in 1:length(data)) {   
#   if (is.na(id[i])==FALSE) {
#     index<-0
#     repName[i]<-index
#     position[i]<-i
#   }
#   else {
#     index<-index+1
#     repName[i]<-index
#     position[i]<-0
#   }
# }
# 
# #the number of times that an id is repeated
# a<-repName[position[position!=0]-1]-1
# a<-4 * c(a, repName[length(repName)]-1)
# id<-str_trim(id[!is.na(id)], side='left')
# id<-rep(id, times=a)  

# =============================================================================
# Extract Year from header and construct date
# =============================================================================
year<-str_extract(str_extract(data, '../../....'), '/[[:digit:]]{4}')
# repeat Year 
year<-rep(year[!is.na(year)], times=(storms$days)*4)
# extract Days from Daily
days<-str_replace(str_extract(data, '../..[*ESWL]'), '[*ESWL]','')
# repeat daily
days<-rep(days[!is.na(days)], each=4)
# paste year and days together
date<-paste(days, year, sep='')

# =============================================================================
# Extract period
# =============================================================================
period<-c('00h','06h','12h','18h')
period<- rep(period, length(id)/4 )


# =============================================================================
# extracting stage
# =============================================================================
index = 1
stage = c()
for (line in data){
  if (!grepl(pattern = "S/N=|SNBR=", x = line)){
    stage[index] = substring(text = line, first = 12, last = 12)
    stage[index+1] = substring(text = line, first = 29, last = 29)
    stage[index+2] = substring(text = line, first = 46, last = 46)
    stage[index+3] = substring(text = line, first = 63, last = 63)
    index = index + 4 
  }
}

stage = unname(sapply(stage, function(x){
  switch(x,
         '*' = "cyclone",
         'S'= "subtropical",
         'E'= "extratropical", 
         'W' = "wave",
         'L' = "remanent low")
}))


# =============================================================================
# extracting pressure
# =============================================================================
press = c()
index = 1
for (line in data){
  if (!grepl(pattern = "S/N=|SNBR=", x = line)){
    press[index] = substring(text = line, first = 25, last = 28)
    press[index+1] = substring(text = line, first = 42, last = 45)
    press[index+2] = substring(text = line, first = 59, last = 62)
    press[index+3] = substring(text = line, first = 75, last = 79)
    index = index + 4 
  }
}
press = as.character(as.numeric(press))

# =============================================================================
# extracting latitude
# =============================================================================
lat = c()
index = 1
for (line in data){
  if (!grepl(pattern = "S/N=|SNBR=", x = line)){
    lat[index] = substring(text = line, first = 13, last = 15)
    lat[index+1] = substring(text = line, first = 30, last = 32)
    lat[index+2] = substring(text = line, first = 47, last = 49)
    lat[index+3] = substring(text = line, first = 64, last = 66)
    index = index + 4 
  }
}
lat = as.numeric(lat) / 10
lat = as.character(lat)

# =============================================================================
# extracting longitude
# =============================================================================
long = c()
index = 1
for (line in data){
  if (!grepl(pattern = "S/N=|SNBR=", x = line)){
    long[index] = substring(text = line, first = 16, last = 19)
    long[index+1] = substring(text = line, first = 33, last = 36)
    long[index+2] = substring(text = line, first = 50, last = 53)
    long[index+3] = substring(text = line, first = 67, last = 70)
    index = index + 4 
  }
}
long = str_trim(string = long, side = "left")


# =============================================================================
# extracting wind
# =============================================================================
wind = c()
index = 1
for (line in data){
  if (!grepl(pattern = "S/N=|SNBR=", x = line)){
    wind[index] = substring(text = line, first = 20, last = 23)
    wind[index+1] = substring(text = line, first = 37, last = 40)
    wind[index+2] = substring(text = line, first = 54, last = 57)
    wind[index+3] = substring(text = line, first = 71, last = 74)
    index = index + 4 
  }
}
wind = as.character(as.numeric(wind))


# =============================================================================
# producing data.frame
# =============================================================================
frame = data.frame(id, date, period, stage, lat, long, wind, press, stringsAsFactors = FALSE)
index = intersect(x = which(frame$lat == "0"), y = which(frame$long == "0" | frame$long == "0000"))
index1 = intersect(x = which(frame$wind == "0"), y = which(frame$press == "0"))  
index = intersect(x = index, y = index1)  
frame = frame[-index,]  
frame$long = as.numeric(frame$long) / 10 - 360
               
# =============================================================================
# write to disk
# =============================================================================
write.csv(x = frame, file = "tracks.csv", row.names = FALSE)             
               
               
  