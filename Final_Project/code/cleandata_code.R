# =============================================================================
# Stat 133: Final Project
# Part1 Data Cleaning
# Description: work with XML data and introduce the R package “XML”
# Data: International Best Track Archive for Climate Stewardship (IBTrACS)
# ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/cxml/year.
# =============================================================================

# Name:Tong Zhang 24540972, Zhiqiang Liao 2422

# =============================================================================
# Load packages
library(XML)
library(stringr)
library(maps)

# Parse the data
data<-readLines("rawdata/rawdata")
# =============================================================================
# extract ID 
# =============================================================================
id<-str_replace(str_extract(data,'SNBR= *[[:digit:]]*'), 'SNBR=| ','')
id<- as.numeric(id[!is.na(id)])

# =============================================================================
# Extract date
# =============================================================================
date<-str_extract(data, '../../....')
date<-date[!is.na(date)]
date<- as.Date(date,"%m/%d/%Y")

# =============================================================================
# Extract days
# =============================================================================
days<- str_extract(data, 'M= *[[:digit:]]*')
days <- days[!is.na(days)]
days = as.numeric(gsub(pattern = 'M= |M=', replacement = "", x = days))

# =============================================================================
# Extract Names
# =============================================================================
name = grep(pattern = "SNBR", x = data, value = TRUE)
name = str_trim(string = substring(text = name, first = 36, last = 47), side = "right")

# =============================================================================
# generating data.frame
# =============================================================================
storms = data.frame(id, date, days, name, stringsAsFactors = FALSE)


# =============================================================================
# writing to disk
# =============================================================================
write.csv(x = storms, file = "storms.csv", row.names = FALSE)


