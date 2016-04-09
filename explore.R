library(plyr)
library(ggplot2)
library(gridExtra)
library(maps)
library(RColorBrewer)

storms.file = "storms-tidy-dataset.csv"

#setwd("Reproducible-Research/Assignment2")
#STORMS = read.csv(storms.file)

# First of all, determine the basic results
BASIC.SUMMARY = ddply(STORMS, .( category), summarize, 
  sum.people = sum(dead) + sum(hurt),
  sum.money = sum(prop) + sum(crop)
)
print( BASIC.SUMMARY )

# Render 2 plots in one image.  Show total damages and injuries, by category

basic.categories = levels(BASIC.SUMMARY$category)
color.bins = length(basic.categories)
colors = brewer.pal(color.bins, "Paired")

p1 = ggplot( data=BASIC.SUMMARY, aes(category, sum.people/1000) )
p1 = p1 + geom_bar(stat="identity", fill=colors)
p1 = p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p1 = p1 + xlab("Weather Event Category") + ylab("People Killed or Injured in Thousands")
p1 = p1 + ggtitle("People Hurt by Category")

p2 = ggplot( data=BASIC.SUMMARY, aes(category, sum.money/1000000000) )
p2 = p2 + geom_bar(stat="identity", fill=colors)
p2 = p2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p2 = p2 + xlab("Weather Event Category") + ylab("Damage in Billions of Dollars")
p2 = p2 + ggtitle("Damage by Category")

grid.arrange(p1, p2, ncol=2)

# Now lets break it down by state
SUMMARY = ddply(STORMS, .(state, category), summarize,
  sum.people = sum(dead) + sum(hurt),
  sum.money = sum(prop) + sum(crop)
)

# Determine the category with the greatest cause of money damage by state
MONEY = ddply(SUMMARY, .(state), function(x){x[which.max(x$sum.money),]})

# Load state reference data; we'll use this for matching to states on our map
STATES = read.csv("states.csv")

# Join the state reference data with our damage report by state
MONEY$abbrev = MONEY$state
STATES.MONEY = join( STATES, MONEY, by="abbrev", type="left", match="all" )

# Reduce the category factor to only those in use
STATES.MONEY$category = factor(STATES.MONEY$category)

# Prepare our color palatte and category names
category.names = levels(STATES.MONEY$category)
color.bins = length(category.names)
colors = brewer.pal(color.bins, "Set1")

# Draw Map
map("state", col = "gray80", fill = TRUE, lty = 0) #Base
map("state", col = colors[STATES.MONEY$category], fill = TRUE, lty = 0, add = TRUE) #States
map("state", col = "gray", lwd = 1.4, lty = 1, add = TRUE) #Borders
title("Greatest Cause of Damage in Dollars by State")
legend("bottomright", cex= 0.56, bty="n",
   title="Source of Damage",
   legend = category.names,
   fill = colors
)

# Determine the category with the greatest cause of death and injury by state
PEOPLE = ddply(SUMMARY, .(state), function(x){x[which.max(x$sum.people),]})

# Load state reference data; we'll use this for matching to states on our map
STATES = read.csv("states.csv")

# Join the state reference data with our injury report by state
PEOPLE$abbrev = PEOPLE$state
STATES.PEOPLE = join( STATES, PEOPLE, by="abbrev", type="left", match="all" )

# Reduce the category factor to only those in use
STATES.PEOPLE$category = factor(STATES.PEOPLE$category)

# Prepare our color palatte and category names
category.names = levels(STATES.PEOPLE$category)
color.bins = length(category.names)
colors = brewer.pal(color.bins, "Set1")

# Draw Map
map("state", col = "gray80", fill = TRUE, lty = 0) #Base
map("state", col = colors[STATES.PEOPLE$category], fill = TRUE, lty = 0, add = TRUE) #States
map("state", col = "gray", lwd = 1.4, lty = 1, add = TRUE) #Borders
title("Greatest Cause of Death and Injury by State")
legend("bottomright", cex= 0.56, bty="n",
   title="Source of Damage",
   legend = category.names,
   fill = colors
)







