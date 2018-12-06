## Part 1
setwd("C:/Users/linwa_000/adventofcode2018")
input <- read.table("day6input.txt", header = FALSE, sep = ",")
dim(input) # 50 observations (50 points)

min(input$V1)  # 44
max(input$V1)  # 349
min(input$V2)  # 50
max(input$V2)  # 355
## All the definite area will be within the rectangle (44, 50), (349, 50), (44, 350), (349, 350)

## find the 4 points that we need to exclude from considering:
input[match(44, input$V1),]  # [29] 44 194
input[match(349, input$V1),] # [36] 349 75
input[match(50, input$V2),]  # [16] 79 50
input[match(355, input$V2),] # [40] 172 355 

## Define the function to calculate Manhanttan distance
ManhanttanDistance <- function(x1, y1, x2, y2) {
  return(abs(x1 - x2) + abs(y1 - y2))
}

## find all the distances
DistanceGrid <- data.frame(cor1 = integer(), cor2 = integer())
count <- 1
for (i in 44:349) {
  for (j in 50:350) {
    DistanceGrid[count,1] <- i
    DistanceGrid[count,2] <- j
    for (k in 1:50) {
      DistanceGrid[count, k+2] <- ManhanttanDistance(i, j, input[k,1], input[k,2])
    }
    count <- count + 1
  }
}

## find which point is closest to which given point
## note that locations that are equally far from two or more coordinates don't count as being closest to any.
for (i in 1:nrow(DistanceGrid)) {
  if (sum(DistanceGrid[i, 3:52] == min(DistanceGrid[i, 3:52])) == 1) {
    DistanceGrid$point[i] <- match(min(DistanceGrid[i, 3:52]), DistanceGrid[i, 3:52])
  }
  else {DistanceGrid$point[i] <- NA}
}

library(dplyr)
sum <- group_by(DistanceGrid, point) %>% summarise(count = n())
sum[match(max(sum$count), sum$count),]  ## 35 4186

## Part 2
for (i in 1:nrow(DistanceGrid)) {
  DistanceGrid$total[i] <- sum(DistanceGrid[i, 3:52])
}
sum(DistanceGrid$total<10000)  ## 45509
