# Using R as a calculator
5+4

# Save the result of a calculation as an 'object' using <- or =
# Name it whatever you want
whatever <- 5+3
whatever * 5

# Open up the tidyverse so we can make a graph
library(tidyverse)

# Pre-loaded data in R
mtcars
?mtcars

# Simple functions to learn about your data
# The $ lets you pick a column of a data table: [datatable]$[column]
mean(mtcars$mpg)
summary(mtcars$mpg)
View(mtcars)

# Make a graph with ggplot2!
# The aes() function tells us how columns of the data table turn into 
# elements of your graph.
ggplot(data=mtcars, aes(x = wt, y = mpg, size = hp, color = wt)) +
  geom_point() +
  theme_classic() +
  xlab("Weight (1000's of pounds)") +
  ylab("Miles per gallon") +
  ggtitle("Heavy cars get worse mileage") +
  guides(color = "none")


