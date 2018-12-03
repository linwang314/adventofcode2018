## Part 1
setwd("C:/Users/linwa_000/adventofcode2018")
input <- read.delim("day3input.txt", stringsAsFactors = FALSE, header = FALSE)

## replace #, @, :, * with comma
input <- sub("#", "", input[[1]])  ## input changed from a list to a character vector
input <- sub(" @ ", ",", input)
input <- sub(": ", ",", input)
input <- sub("x", ",", input)
## split strings
input_split <- sapply(input, strsplit, split=",")
## coerce into integers
input_split_int <- lapply(input_split, as.integer)

## make a function to find the area represented by each claim
area <- function(claim) {
  id <- claim[1]
  left <- claim[2]
  top <- claim[3]
  width <- claim[4]
  height <- claim[5]
  result <- data.frame(id=integer(), cor1=integer(), cor2=integer())
  count <- 1
  
  while (count <= width * height) {
    for (i in 1:height) {
      for (j in 1:width) { 
        result[count, 1] <- id
        result[count, 2] <- i+top
        result[count, 3] <- j+left
        count <- count + 1
      }
    }
  }
  return(result)
}

results <- lapply(input_split_int, area)  ## results - a large list of 1375 dataframes, each with coordinates

## merge all claim results into one dataframe
final_results <- data.frame(id=integer(), cor1=integer(), cor2=integer())
for (i in 1:length(results)) {
  final_results <- rbind(final_results, results[[i]])
}

## add a column to show cooridate combination
final_results[,"coordinates"] <- paste(final_results[,"cor1"], final_results[,"cor2"], sep = ",") 
sum(table(final_results[,"coordinates"])>1)  #119551

## Part 2
## add a column to indicate counts
for (i in 1:nrow(final_results)) {
  final_results[i,"count"] <- sum(final_results[,"coordinates"] == final_results[i,"coordinates"])
}
## find which claim has mean(count) 1
library(dplyr)
sum <- group_by(final_results, id) %>%
  summarise(mean(count))
sum[sum[,2]==1,]  ## 1124
