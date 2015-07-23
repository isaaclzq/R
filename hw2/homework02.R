# =====================================================
# Stat133: HW 2
# Description: Graphics
# Data: Camping Tents
# =====================================================

# Please submit your own R script file to bcourses

# Write your name
# Name: Zhiqiang Liao

# load the package "ggplot2"
library(ggplot2)


# =====================================================
# We'll be working with the dataset 'tents1.csv'
# (available in the folder 'datasets' in the 
# github repository of the course)
# =====================================================
# read the dataset in R; use whatever method you like 
tents1_file = read.table(file = "~/Desktop/R/datasets/tents1.csv", header = TRUE, sep = ",")

# inspect the data structure with str()
str(tents1_file)

# how many rows in the dataset?
length(row.names(tents1_file))

# how many columns in the dataset?
length(colnames(tents1_file))

# names of columns
colnames(tents1_file)

# =====================================================
# Quantitative Variables:
# Let's practice some basic manipulations
# of data structures
# =====================================================
# 'tents1.csv' contains three quantitative variables:
# 1) price
# 2) weight
# 3) height

# get numeric summaries of each variable
summary(tents1_file$price)
summary(tents1_file$weight)
summary(tents1_file$height)

# weight is given in grams
# add a new variable to tents for weight expressed in pounds
tents1_file = cbind(tents1_file, weight_pound = tents1_file$weight * 0.00220462)

# height is given in centimeters
# add a new variable to tents for height expressed in inches
tents1_file = cbind(tents1_file, height_inch = tents1_file$height * 0.393701)

# how many tents have price less than $300
length(which(tents1_file$price <= 300))

# how many tents have price less than $300
length(which(tents1_file$price <= 300))

# what's the name of the tent with maximum price
tents1_file$name[which.max(tents1_file$price)]

# what's the name of the tent with minimum price
tents1_file$name[which.min(tents1_file$price)]

# what's the name of the tent with maximum weight
tents1_file$name[which.max(tents1_file$weight)]

# what's the name of the tent with minimum weight
tents1_file$name[which.min(tents1_file$weight)]

# select the data of tents with price > $400 and 
# weight < 1500 grams
subset(tents1_file, price > 400 & weight < 1500)

# subset those tents with brand 'big-agnes'
subset(tents1_file, brand == "big-agnes")


# Create a factor of prices using the cut() function;
# for the argument 'breaks' give a vector of cutting points
# such that the obtained levels are as follows:
#    (0, 100]
#  (100, 200]
#  (200, 300]
#  (300, 400]
#  (400, 500]
#  (500, 600]
#  (600, 700]





# =====================================================
# Quantitative Variables:
# Statistical graphics
# =====================================================
# for each quantitative variable, obtain the following plots:
# - histogram
# - boxplot
# - density curve





# Obtain a scatter plot of height and weight such that:
# Axis are labeled with the corresponding variable name
# Points are colored with alpha transparency
# (choose a color of your preference)
# x-axis ranges from 80 to 220
# y-axis ranges from 0 to 10000
# include a title




# Obtain a scatter plot matrix of price, height, and weight




# =====================================================
# Qualitative Variables
# =====================================================
# 'tents1.csv' contains four qualitative variables:
# 1) brand
# 2) bestuse
# 3) seasons
# 4) capacity

# for 'bestuse', 'seasons', and 'capacity' obtain 
# the following plots:
# - bar plot
# - dot chart
# - pie chart
# (feel free to change colors, add titles, and rank values)




# --------------------------------------
# Bar chart of brands
# --------------------------------------
# Plot a barplot for 'brand" such that:
# - bars are horizontally oriented
# - bars arranged in decreasing order
# - labels perpendicular to the axis
#   (i.e. labels horizontally oriented)
# - modify margins in order to have enough room for labels 
# - x-axis ranges from 0 to 25
# - include a title







# =====================================================
# More basic manipulations
# =====================================================

# summary of 'price' of big agnes tents


# summary of 'price' of rei tents


# which brands have tents 'bestuse' for Mountaineering


# cross-table of seasons and bestuse




# =====================================================
# Bivariate Plots
# =====================================================

# use boxplot() to plot the distribution of 'weight' 
# conditional to:
# - brand
# - bestuse
# - capacity
# - seasons
# (get a boxplot for each categorical variable)





# =====================================================
# Graphics with 'ggplot2'
# Boxplots of price
# =====================================================

# boxplot of price by brand



# boxplot of price by bestuse



# boxplot of price by capacity



# boxplot of price by seasons




# =====================================================
# Graphics with 'ggplot2'
# Boxplots of weight
# =====================================================

# boxplot of weight by brand (with fill color by brand)



# boxplot of weight by capacity (with fill color by capacity)



# boxplot of weight by bestuse (with fill color by bestuse)




# =====================================================
# Graphics with 'ggplot2'
# Scatter plots
# =====================================================

# get a scatter plot of height and price



# get a scatter plot of height and price
# coloring points by seasons



# get a scatter plot of weight and price



# get a scatter plot of weight and price
# coloring points by bestuse





# faceting
# scatter plot of weight -vs- height 
# coloring by bestuse and
# faceting by capacity









