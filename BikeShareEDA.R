## 
## Bike Share EDA Code
##

install.packages("tidymodels")
install.packages("installr")

##Libraries
library(tidyverse)
library(vroom)
library(ggplot2)
library(gridExtra)
library(tidymodels)
library(installr)
updateR()

##Read in the Data
bike <- vroom("train.csv")

##Identify variables
head(bike)
ls(bike)

##All data is complete and there are no missing observations. Most of the variables are continuous. 
DataExplorer::plot_intro(bike)

##Creating 4 panel plot 
plot1 <- ggplot(bike, aes(x = atemp, y = count)) +
  geom_point() +
  labs(title = "Scatter Plot: atemp vs. count") +
  theme_minimal()

plot2 <- ggplot(bike, aes(x = workingday, fill = workingday, y = ..count..)) +
  geom_bar() +
  labs(title = "Bar Plot: Season vs. Count") +
  theme_minimal()

plot3 <- ggplot(bike, aes(x = datetime, y = count)) +
  geom_line() +
  labs(title = "Count over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 <- ggplot(bike, aes(x = weather, y = count, fill = weather)) +
  geom_boxplot() +
  labs(title = "Box Plot: Weather vs. Count") +
  facet_wrap(~weather) +
  theme_minimal()

# Combine the plots into a 2x2 grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)




