## Part 1
setwd("C:/Users/linwa_000/adventofcode2018")
frequency_change <- read.table("day1input.txt")
sum(frequency_change) ## 547

## Part 2
frequencies <- c(0)
frequency_change <- unlist(frequency_change)
size <- length(frequency_change)
i = 2
# found = 0
while (anyDuplicated(frequencies) == 0) {
  if (i <= 952) {frequencies[i] <- frequencies[i-1] + frequency_change[i-1]}
  else {frequencies[i] <- frequencies[i-1] + frequency_change[(i-2)%%size + 1]}
  
  # Original thought was to use another loop to find duplicates and found (0/1) as an indicator of whether there has been a duplicate.
  # for (j in 1:(i-1)) {
  #   if (frequencies[j] == frequencies[i]) {
  #     found = 1
  #     break
  #   }
  # }
  i <- i+1
}

frequencies[anyDuplicated(frequencies)]
