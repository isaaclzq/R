# =====================================================
# Stat133: HW 1
# Description: Basics of character vectors
# Data: Teams that played the superbowl
# =====================================================

# Write your name
# Name: Zhiqiang(Isaac) Liao 


# Start a new R session and load the data
load(url("http://gastonsanchez.com/teaching/stat133/superbowl_teams.RData"))

# "superbowl_teams.RData" contains 3 vectors:
# year: year of superbowl from 1967 to 2015
# winner: champions
# loser: losing teams

# find the class of each vector (year, winner, loser)
class(year)
class(winner)
class(loser)


# use length() to determine whether the three vectors
# have the same number of elements
length(year) == length(winner) & length(winner) == length(loser)



# =====================================================
# Winning teams
# Write the commands to answer the following questions
# =====================================================

# get the first 5 champions
head(winner, n = 5)


# get the last 5 champions
tail(winner, n = 5)

# how many unique champions?
length(unique(winner))

# use the function table() to get a 
# table of frequencies for the winning teams
# (assign the table to the object 'win_freqs')
win_freqs = table(winner)

# what team has won the superbowl most times?
# and how many times?
head(sort(win_freqs, decreasing = TRUE),1)


# apply table() on 'win_freqs', this will give you
# how many teams have won how many superbowls
table(win_freqs)


# create a vector 'champions' by sorting the frequencies
# 'win_freqs' in decreasing order
champions = c(sort(win_freqs, decreasing = TRUE))

# make a barplot of 'champions' with barplot()
barplot(champions)

# try this command, las := lables are parallel (0) or perpendicular (2)
barplot(champions, las = 2)

# try this other command
op <- par(mar = c(2, 11, 1, 2))
barplot(champions, horiz = TRUE, las = 2)
par(op)


# What are the championships of "San Francisco 49ers"
year[c(which(winner == "San Francisco 49ers"))]

# What are the championships of "Oakland Raiders" 
year[c(which(winner == "Oakland Raiders"))]

# when was the last time Denver Broncos won a superbowl?
tail(sort(year[c(which(winner == "Denver Broncos"))], decreasing = FALSE),1)


# create 'winner2', a copy of the vector 'winner'
winner2 = winner

# replace some team names in 'winner2' as follows:
# "New York Giants" to "NYG"
# "New York Jets" to "NYJ"
# "Kansas City Chiefs" to "KCC"
winner2 = replace(winner2, which(winner2 == "New York Giants"), "NYG")
winner2 = replace(winner2, which(winner2 == "New York Jets"), "NYJ")
replace(winner2, which(winner2 == "Kansas City Chiefs"), "KCC")


# =====================================================
# Losing team
# Write the commands to answer the following questions
# =====================================================

# get the losing teams of the first 5 superbowls
head(loser, 5)



# get the losing teams of the last 5 superbowls
tail(loser, 5)

# create the frequency table 'los_freqs'
# of losing teams
los_freqs = table(loser)

# What is the team that have lost the superbowl
# the most times?, and how many times?
head(sort(los_freqs, decreasing = TRUE),1)


# =====================================================
# Winners and Losers
# Write the commands to answer the following questions
# =====================================================

# how many different teams have played the superbowl?
length(unique(paste(winner, loser, sep = "-")))

# teams that have played the superbowl and have never lost
setdiff(winner, loser)

# teams that have played the superbowl and have never won
setdiff(loser, winner)

# teams that hawinneryed the superbowl (both won and lost)?
intersect(winner, loser)

# how many teams have both won and lost the superbowl?
length(intersect(winner, loser))


# what team won the superbowl in 2000
winner[which(year == 2000)]

# what team lost the superbowl in 2000
loser[which(year == 2000)]

# what teams won the superbowl in the 1970s (1970-1979)
unique(winner[which(year >= 1970 & year <= 1979)])

# what teams lost the superbowl in the 1990s (1990-1999)
unique(loser[which(year >= 1990 & year <= 1999)])

# create a data frame 'superbowl' with the three vectors:
# year, winner, loser
data.frame(year, winner, loser)

