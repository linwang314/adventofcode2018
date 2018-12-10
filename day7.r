## Part 1 
setwd("C:/Users/linwa_000/adventofcode2018")
input <- read.table("day7input.txt", header = FALSE)

# read.table() break the input by space and turns out to be easy to extract the steps represented by letters. 
input <- input[, c(2,8)]

# sort the steps
library(dplyr)
sorted_input <- arrange(input, V2, V8)
