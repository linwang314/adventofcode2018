## Part 1
library(stringr)
library(lubridate)
library(dplyr)

## read raw input
setwd("C:/Users/linwa_000/adventofcode2018")
input <- read.delim("day4input.txt", stringsAsFactors = FALSE, header = FALSE)

## extract timestamp
input[,"timestamp"] <- substr(input[,1], 2, 17)
input[,1] <- sub("\\[.*\\] ","" ,input[,1])

## change timestamp to date-time and sort
input[,"timestamp"] <- ymd_hm(input[,"timestamp"])
input <- arrange(input, timestamp)

## separate date
input[,"date"] <- date(input[,"timestamp"])

## extract guard information
input[,"guard"] <- as.integer(gsub("( )?[a-z]*","",gsub(".*\\#", "", input[,1])))
input[,1] <- sub("Guard \\#[0-9]* ", "", input[,1])

## find the minutes where a guard was asleep
## step 1: extract the minute when a guard falls asleep or wakes up
for (i in 1:nrow(input)) {
  if (is.na(input[i, "guard"])) {input[i, "minute"] <- minute(input[i, "timestamp"])}
}
## step 2: find the minutes between the above
input[,6:65] <- 0
for (i in 1:nrow(input)) {
  if (!is.na(input[i, "guard"])) {
    j <- i+1
    while (input[j,1] != "begins shift") {
      fall_asleep <- input[j, "minute"]
      wakes_up <- input[j+1, "minute"]-1
      for (k in (5+fall_asleep):(5+wakes_up)) {input[i,k] <- 1}
      j <- j + 2
    }
  }
}
## step 3: build the visualization
visual <- input %>%
  select(date, guard, V6:V65) %>%
  filter(!is.na(guard))
  
## find who spent the most minutes sleeping
for (i in 1:nrow(visual)) {visual[i,"minutes_asleep"] <- sum(visual[i, 3:62])}
total <- group_by(visual, guard) %>%
  summarise(total_minutes_asleep = sum(minutes_asleep))

total[total[,2]==max(total[,2]),] ## guard ID 499
guard499 <- filter(visual, guard==499)
count <- integer()
for (i in 3:62) {
  count <- c(count, sum(guard499[,i]))
}
match(max(count), count) ## 44

## Part 2
guards <- unique(visual[,"guard"])
length(guards) ## 23
counts <- matrix(nrow = 23, ncol = 60)

for (i in 1:23) {
  guard <- filter(visual, guard == guards[i])
  for (j in 1:60) {
    counts[i, j] <- sum(guard[,j+2])
  }
}
3449 * 39 ## guard id 3449 fell asleep at minute 39 for 16 times
