setwd("C://Users//Bangda//Desktop//project-where to open store")
saledata = read.csv("sale data.csv", header = TRUE, as.is = TRUE)
head(saledata)
summary(saledata)

######################################
#### Data cleaning and processing ####
######################################
## We checked that there only 3 kinds of products, however
## table() function return 4 elements. We can verify that 
## one of the "Product3" has one more space ("Product3 ")
## It should be considered as "Product3"
table(saledata$Product)
saledata$Product[which(saledata$Product == "Product3 ")] = "Product3"
saledata$Product = factor(saledata$Product)

## Convert price from character into numeric
typeof(saledata$Price)
table(saledata$Price)
saledata$Price = as.numeric(saledata$Price)

## set recored of price equal to 13000 (become NA)
sum(is.na(saledata$Price))
narow = is.na(saledata$Price)
saledata$Price[narow] = 13000

# Get the frequency of product
library(plyr)
freqOfProduct = function(data) {
  freqTable = table(data$Product)
  return(freqTable)
}

# Get the frequency of product for each country use freqOfProduct
freqTable = ddply(saledata, .(Country), freqOfProduct)

# Split the date and time of transaction
colnames(saledata)[1] = "Transaction_date"
dateinfo = unlist(strsplit(saledata$Transaction_date, " "))
dateinfo = matrix(dateinfo, byrow = TRUE, ncol = 2)
saledata$Tdate = dateinfo[, 1]
saledata$Ttime = dateinfo[, 2]

#######################
#### Visualization ####
#######################
# Visualize the location
library(ggplot2)

## Concentration of sales
ggplot(data = saledata) +
  geom_point(mapping = aes(x = Longitude, y = Latitude), alpha = .3, size = 3)

## Different products
ggplot(data = saledata) +
  geom_point(mapping = aes(x = Longitude, y = Latitude, color = Product), alpha = .3, size = 3)

ggplot(data = saledata) + 
  geom_point(mapping = aes(x = Longitude, y = Latitude), alpha = .3, size = 3) + 
  facet_wrap(~ Product, nrow = 1)

# Distribution of price
ggplot(data = saledata, aes(Price)) + 
  geom_histogram(aes(y = ..count.. ), binwidth = 1000)

# Count the number of transaction of each day
tranDateCount = function(data) {
  sumTran = count(data$Tdate)
  return(sumTran)
}

dailyCount = ddply(saledata, .(Tdate), tranDateCount)
dailyCount$Tdate = as.Date(dailyCount$Tdate, format = '%m/%d/%Y')

ggplot(dailyCount, aes(Tdate, freq)) +
  geom_point() + 
  geom_line() + 
  labs(x = "Date", y = "Number of transaction")

# Count the number of transaction of each day by country
table(saledata$Country)
top9Country = sort(table(saledata$Country), decreasing = TRUE)[1:9]
top9Country

dailyCountByCountry = ddply(saledata, .(Tdate, Country), tranDateCount)
dailyCountByCountry$Tdate = as.Date(dailyCountByCountry$Tdate, format = '%m/%d/%Y')

ggplot(dailyCountByCountry[dailyCountByCountry$Country%in%names(top9Country), ], aes(Tdate, freq)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ Country, nrow = 3, ncol = 3)
  labs(x = "Date", y = "Number of transaction")

# Count the number of transaction of each day by different states in US
USdata = saledata[saledata$Country == "United States", ]
dailyCountByStates = ddply(USdata, .(Tdate, State), tranDateCount)
dailyCountByStates$Tdate = as.Date(dailyCountByStates$Tdate, format = '%m/%d/%Y')
top9States = sort(table(dailyCountByStates$State), decreasing = TRUE)[1:9]

ggplot(dailyCountByStates[dailyCountByStates$State%in%names(top9States), ], aes(Tdate, freq)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~ State, nrow = 3, ncol = 3) + 
  labs(x = "Date", y = "Number of transaction")

# Visualize location in US
ggplot(USdata) + 
  geom_point(mapping = aes(x = Longitude, y = Latitude), alpha = .1, size = 3)
# Only consider mainland
USdata = USdata[!USdata$State%in%c("AK", "HI", "VI"), ]

library(maps)
all_states = map_data("state")
p = ggplot() + 
  geom_polygon(data = all_states, aes(x = long, y = lat, group = group), 
               color = "white", fill = "lightblue")
p1 = p + geom_point(data = USdata, mapping = aes(x = Longitude, y = Latitude, color = State), size = 3)
p1
p2 = p + geom_point(data = USdata, mapping = aes(x = Longitude, y = Latitude), 
               alpha = .1, size = 4, color = "black")
p2

# Consider the amount of money

# Consider transaction time in one day (24h)
library(chron)
USdata$Ttime = times(paste(USdata$Ttime, "00", sep = ":"))
USdata$Th = hours(USdata$Ttime) + minutes(USdata$Ttime) / 60

ggplot(USdata[USdata$State%in%names(top9States), ], aes(Th)) + 
  geom_histogram(binwidth = .5) + 
  facet_wrap(~ State, nrow = 3, ncol = 3) +
  labs(x = "Time in 24 hours", y = "Frequency")
