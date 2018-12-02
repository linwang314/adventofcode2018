## Part 1
setwd("C:/Users/linwa_000/adventofcode2018")
input <- read.table("day2input.txt", stringsAsFactors = FALSE)

## for each element in the list, split them into single characters and sapply will simplify the results to character vectors. 
input <- sapply(input, strsplit, split = "")
count2 <- 0
count3 <- 0 
for (i in 1:250) {
  if (sum(table(input[[i]]) == 2)) {
    count2 <- count2 + 1
  }
  if (sum(table(input[[i]]) == 3)) {
    count3 <- count3 + 1
  }
}
count2 * count3 ## 7936

## Part 2
found <- 0
while (found == 0) {
  for (i in 1:249) {
    for (j in i:250) {
      if (sum(input[[i]] == input[[j]]) == 25) {
        found <- 1
        first <- i
        second <- j
      }
    }
  }
}
paste(input[[first]][input[[first]] == input[[second]]], collapse = '')
paste(input[[second]][input[[first]] == input[[second]]], collapse = '')
