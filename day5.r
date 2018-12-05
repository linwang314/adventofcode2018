## Part 1
## read raw input
setwd("C:/Users/linwa_000/adventofcode2018")
input <- read.table("day5input.txt", stringsAsFactors = FALSE, header = FALSE)[1,1]
while (grepl("Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Vv|Ww|Xx|Yy|Zz|aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|vV|wW|xX|yY|zZ", input)) {
  input <- gsub("Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Vv|Ww|Xx|Yy|Zz|aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|vV|wW|xX|yY|zZ", "", input)
}
nchar(input) # 9154

## Part 2
## initialize input
input <- read.table("day5input.txt", stringsAsFactors = FALSE, header = FALSE)[1,1]

## make a function that we can use in a loop function
polymer_collapse <- function(unit, polymer) {
  polymer_removed <- gsub(unit, "", polymer)
  while (grepl("Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Vv|Ww|Xx|Yy|Zz|aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|vV|wW|xX|yY|zZ",polymer_removed)) {
  polymer_removed <- gsub("Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Vv|Ww|Xx|Yy|Zz|aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|vV|wW|xX|yY|zZ", "", polymer_removed)
  }
  return(polymer_removed)
}

units <- c("[Aa]", "[Bb]", "[Cc]", "[Dd]", "[Ee]", "[Ff]", "[Gg]", "[Hh]", "[Ii]", "[Jj]", "[Kk]", "[Ll]", "[Mm]", "[Nn]",
           "[Oo]", "[Pp]", "[Qq]", "[Rr]", "[Ss]", "[Tt]", "[Uu]", "[Vv]", "[Ww]", "[Xx]", "[Yy]", "[Zz]")
results <- sapply(units, polymer_collapse, polymer = input)
min(nchar(results)) ## 4556
match(min(nchar(results)), nchar(results))  ## 5 meaning removing E/e