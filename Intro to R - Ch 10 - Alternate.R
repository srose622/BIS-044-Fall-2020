############################################################
# Alternate R script to accompany Intro to R for Business, #
# Chapter 10, written by Troy Adair                        #
############################################################

# Clear out Console and Enviroment
rm(list=ls(all=TRUE))
cat("\014")

## Get subsample from YT_Sample_Validated ready for graphing
# Load the previously referenced data frame in 
# "YT_Sample_Validated.RData", copy it to "DF",
# and remove "YT_Sample_Validated" from memory
load("YT_Sample_Validated.RData")
DF <- YT_Sample_Validated
rm(YT_Sample_Validated)

# Reform DF with only the "good" observations
DF<-DF[which(DF$total_amount >0 &
               DF$fare_amount <100000 &
               DF$fare_amount >= 0 &
               DF$trip_distance < 100),]

# Reformat passenger_count as factor and store data frame as DF
DF$passenger_count <- as.factor(DF$passenger_count)

# Select a random subsample of 10,000 rows
set.seed(20)
index <- sample(1:nrow(DF), 10000, replace=FALSE)
# Copy the row numbers for the sample only into DF
DF <- DF[index,]

# To remind us what columns we have in DF
str(DF)

##################################################
cat("\014")

# Using R core plotting capabilities - Strip Charts
attach(DF)
stripchart(total_amount)
stripchart(total_amount,method="stack")
stripchart(total_amount,method="jitter")
?stripchart()

# Using R core plotting capabilities - Scatter Plots
plot(total_amount,trip_distance)
plot(total_amount,trip_distance, 
     main="NYC Taxi Rides - Relationship Between Trip Fare and Distance",
     xlab="Total Fare",
     ylab="Trip Distance")

# Using R core plotting capabilities - Histograms
hist(total_amount)
hist(total_amount,main="Distribution of Total Amount Paid",xlab="Amt Paid")
?hist()

# Using R core plotting capabilities - Boxplots
boxplot(total_amount)
boxplot(tolls_amount)
boxplot(mta_tax)

boxplot(total_amount,
        main='Total Fares for NYC Yellow Taxi Rides',
        ylab='Fare')


# Install and load ggplot2 package
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Using ggplot2
ggplot(data = DF)
ggplot(data = DF)+geom_histogram(aes(x=total_amount))
ggplot(data = DF)+geom_density(aes(x=total_amount),fill='grey50')


ggplot(data=DF) + 
  geom_histogram(binwidth=5, aes(x=total_amount)) + 
  ggtitle("Total Fare Distribution") + 
  xlab("Fare") + 
  ylab("Frequency") 

ggplot(data=DF) + 
  geom_histogram(binwidth=5, aes(x=total_amount)) + 
  ggtitle("Total Fare Distribution") + 
  xlab("Fare") + 
  ylab("Frequency") +
  theme_minimal()
  
ggplot(data=DF) + 
  geom_histogram(binwidth=5, aes(x=total_amount)) + 
  ggtitle("Total Fare Distribution") + 
  xlab("Fare") + 
  ylab("Frequency") +
  theme_minimal() +
  xlim(0,100)

ggplot(data=DF) + 
  geom_histogram(binwidth=5, aes(x=total_amount)) + 
  ggtitle("Total Fare Distribution") + 
  xlab("Fare") + 
  ylab("Frequency") +
  theme_minimal() +
  xlim(0,100) +
  facet_wrap(~passenger_count)

ls(pattern = '^geom_', env = as.environment('package:ggplot2'))
help(aes)
help(aes_colour_fill_alpha)
help(aes_group_order)
help(aes_linetype_size_shape)
help(aes_position)

# Scatter plots with ggplot2:
ggplot(data=DF,
       mapping=aes(x=total_amount,
                   y=trip_distance)) + 
  geom_point() +
  ggtitle("Fare Amount vs. Trip Distance") + 
  xlab("Fare") + 
  ylab("Distance") +
  theme_minimal() +
  scale_x_continuous(labels=scales::dollar_format())

# Same as:
p <- ggplot(data=DF,
            mapping=aes(x=total_amount,
                        y=trip_distance)) 
p + geom_point() +
  ggtitle("Fare Amount vs. Trip Distance") + 
  xlab("Fare") + 
  ylab("Distance") +
  theme_minimal() +
  scale_x_continuous(labels=scales::dollar_format())

# Adding smoothed conditional line

p + geom_point() +
  ggtitle("Fare Amount vs. Trip Distance") + 
  xlab("Fare") + 
  ylab("Distance") +
  theme_minimal() +
  scale_x_continuous(labels=scales::dollar_format()) +
  geom_smooth() 

# Can also produce something similar to the "fitted line" you see in regression

p + geom_point() +
  ggtitle("Fare Amount vs. Trip Distance") + 
  xlab("Fare") + 
  ylab("Distance") +
  theme_minimal() +
  scale_x_continuous(labels=scales::dollar_format()) +
  geom_smooth(method="lm") 


# Histograms with ggplot2:
ggplot(data=DF) + 
  geom_histogram(binwidth=5, aes(x=total_amount)) + 
  ggtitle("Total Fare Distribution") + 
  xlab("Fare") + 
  ylab("Frequency") +
  theme_minimal()

# Boxplots with ggplot2

ggplot(data=DF) + 
  geom_boxplot(aes(x=total_amount)) + 
  ggtitle("Total Fare Distribution") + 
  xlab("Fare") + 
  ylab("Frequency") +
  theme_minimal()

# Graphs you can't do in core R

ggplot(data=DF) + 
  geom_density(aes(x=total_amount)) + 
  ggtitle("Total Fare Distribution") + 
  xlab("Fare") + 
  ylab("Frequency") +
  theme_minimal()

ggplot(data=DF) + 
  geom_violin(aes(x=total_amount,y=trip_distance)) + 
  ggtitle("Total Fare Distribution") + 
  xlab("Fare") + 
  ylab("Frequency") +
  theme_minimal()

p <- ggplot(data = DF,
            mapping = aes(x = trip_distance,
                          y = total_amount))
p + geom_point(mapping=aes(group = passenger_count,
                           color=passenger_count)) +
  geom_smooth(aes(group=passenger_count, color=passenger_count),
              method = "nls", formula = y ~ a * x + b, se = F,
              method.args = list(start = list(a = 0.1, b = 0.1))) +
  labs(x = "Trip Distance",
       y = "Total Amount",
       title = "Total Fare vs. Trip Distance by Passenger Count") +
  xlim(0,25) +
  ylim(0,150)
  
ggplot(data=DF) + 
  geom_bar(aes(x=payment_type,fill=passenger_count)) + 
  ggtitle("Passenger Counts by Payment Type") + 
  xlab("Payment Type") + 
  ylab("Frequency") +
  theme_minimal() +
  xlim(0,3)


ggplot(data=DF) + 
  geom_bar(aes(x=total_amount,fill=passenger_count)) + 
  ggtitle("Fare Distribution Between $0 and $25") + 
  xlab("Fare") + 
  ylab("Frequency") +
  theme_minimal() +
  xlim(0,25)

ggplot(data=DF) + 
  geom_bar(aes(x=total_amount,fill=passenger_count)) + 
  ggtitle("Fare Distribution Between $0 and $10 by Passenger Count") + 
  xlab("Fare") + 
  ylab("Frequency") +
  theme_minimal() +
  facet_wrap(~passenger_count) +
  xlim(0,10)

