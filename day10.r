## read data and preprocessing
setwd("C:/Users/linwa_000/adventofcode2018")
input <- read.table("day10input.txt", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

## extract position and acceleration
input$X0 <- as.integer(substring(input$V1, 11, 16))
input$Y0 <- as.integer(substring(input$V1, 19, 24))
input$Vx <- as.integer(substring(input$V1, 37, 38))
input$Vy <- as.integer(substring(input$V1, 41, 42))

## write a function to plot
navigation <- function(X0, Y0, Vx, Vy, t) {
  Xt <- X0 + Vx * t
  Yt <- Y0 + Vy * t
  plot(x = Xt, y = Yt, main = paste("t = ", t, "s", sep = ""))
}

## after some explorative trials, narrowing down to between 10350 and 10400 seconds
for (t in 10350:10400) {
  navigation(input$X0, input$Y0, input$Vx, input$Vy, t)
}
## find out that the message appears at 10375 seconds 