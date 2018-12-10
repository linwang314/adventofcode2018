## Input - 416 players; last marble is worth 71617 points (Part 1) and 7161700 (Part 2)

players <- 416
points <- 7161700
player_score <- integer(players)

## set up initial condition
marbles <- c(0, 1)
current_index <-2

for (i in 2:points) {
  current_player <- i %% players
  
  if (i %% 23 == 0) {
    current_index <- ifelse(current_index - 7 > 0, current_index - 7, current_index - 7 + length(marbles))
    player_score[current_player] <- player_score[current_player] + i + marbles[current_index]
    marbles <- marbles[-current_index]
    current_index <- ifelse(current_index <= length(marbles), current_index, 1)
  } 
  
  else {
    if (current_index <= length(marbles) - 1) {
      marbles <- append(marbles, i, after = current_index+1)
    } else {
      marbles <- append(marbles, i, after = 1)
    }
    current_index <- ifelse(current_index <= length(marbles) - 2, current_index + 2, 2)
  }
}

max(player_score)  ## 436,720 for Part 1 and for Part 2