# --- Day 4: Ceres Search ---
# "Looks like the Chief's not here. Next!" One of The Historians pulls out a
# device and pushes the only button on it. After a brief flash, you recognize
# the interior of the Ceres monitoring station!
#
# As the search for the Chief continues, a small Elf who lives on the station
# tugs on your shirt; she'd like to know if you could help her with her word
# search (your puzzle input). She only has to find one word: XMAS.
#
# This word search allows words to be horizontal, vertical, diagonal, written
# backwards, or even overlapping other words. It's a little unusual, though, as
# you don't merely need to find one instance of XMAS - you need to find all of
# them. Here are a few ways XMAS might appear, where irrelevant characters have
# been replaced with .:
# 
# ..X...
# .SAMX.
# .A..A.
# XMAS.S
# .X....
# 
# The actual word search will be full of letters instead. For example:
# 
# MMMSXXMASM
# MSAMXMSMSA
# AMXSXMAAMM
# MSAMASMSMX
# XMASAMXAMM
# XXAMMXXAMA
# SMSMSASXSS
# SAXAMASAAA
# MAMMMXMMMM
# MXMXAXMASX
#
# In this word search, XMAS occurs a total of 18 times; here's the same word
# search again, but where letters not involved in any XMAS have been replaced
# with .:
#   
# ....XXMAS.
# .SAMXMS...
# ...S..A...
# ..A.A.MS.X
# XMASAMX.MM
# X.....XA.A
# S.S.S.S.SS
# .A.A.A.A.A
# ..M.M.M.MM
# .X.X.XMASX
#
# Take a look at the little Elf's word search. How many times does XMAS appear?

test <- c("MMMSXXMASM", 
          "MSAMXMSMSA", 
          "AMXSXMAAMM", 
          "MSAMASMSMX", 
          "XMASAMXAMM", 
          "XXAMMXXAMA", 
          "SMSMSASXSS", 
          "SAXAMASAAA", 
          "MAMMMXMMMM", 
          "MXMXAXMASX")

xmasSearch <- function(x) {
  length(which(gregexpr("XMAS", x)[[1]] != -1)) + length(which(gregexpr("SAMX", x)[[1]] != -1))
}

reshape <- function(x){
  tX <- as.matrix(as.data.frame(matrix(unlist(str_split(x, "")), nrow = length(x))) %>% 
                       unite(x, paste0("V", c(1:length(x))), sep = ""))
  
  mX <- matrix(unlist(str_split(x, "")), nrow = length(x))
  d1X <- matrix(NA_character_, nrow = nrow(mX) * 2 - 1, ncol = nrow(mX))
  for (i in c(1:nrow(mX))) {
    d1X[,i] <- c(rep("z", i - 1), mX[,i], rep("z", nrow(mX) - i))
  }
  d1X <- as.matrix(as.data.frame(d1X) %>% 
                        unite(x, paste0("V", c(1:ncol(d1X))), sep = ""))
  
  d2X <- matrix(NA_character_, nrow = nrow(mX) * 2 - 1, ncol = nrow(mX))
  for (i in c(1:nrow(mX))) {
    d2X[,i] <- c(rep("z", nrow(mX) - i), mX[,i], rep("z", i - 1))
  }
  d2X <- as.matrix(as.data.frame(d2X) %>% 
                        unite(x, paste0("V", c(1:ncol(d2X))), sep = ""))
  
  list(x,
       tX, 
       d1X,
       d2X)
}
shapes <- reshape(test)
sum(unlist(lapply(shapes, function(x) sapply(x, xmasSearch))))

# data
user <- "polks" #sarahpolk for macbook air
day <- "day04"
data <- readLines(file.path("/Users", user, "Library", "Mobile Documents", "com~apple~CloudDocs",
                            "misc", "aoc2024", day, "data.txt"))

shapes <- reshape(data)
sum(unlist(lapply(shapes, function(x) sapply(x, xmasSearch))))

# --- Part Two ---
# The Elf looks quizzically at you. Did you misunderstand the assignment?
#   
# Looking for the instructions, you flip over the word search to find that this
# isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed
# to find two MAS in the shape of an X. One way to achieve that is like this:
# 
# M.S
# .A.
# M.S
# Irrelevant characters have again been replaced with . in the above diagram.
# Within the X, each MAS can be written forwards or backwards.
# 
# Here's the same example from before, but this time all of the X-MASes have
# been kept instead:
#   
# .M.S......
# ..A..MSMS.
# .M.S.MAA..
# ..A.ASMSM.
# .M.S.M....
# ..........
# S.S.S.S.S.
# .A.A.A.A..
# M.M.M.M.M.
# ..........
#
# In this example, an X-MAS appears 9 times.
# 
# Flip the word search from the instructions back over to the word search side
# and try again. How many times does an X-MAS appear?

crossmasSearch <- function(x) {
  crossMasses <- 0
  for (i in 1:(nrow(x) - 2)){
    for (j in 1:(ncol(x) - 2)){
      cross <- x[c(i:(i + 2)), c(j:(j + 2))]
      if (cross[2,2] == "A") {
        if(sum(paste0(cross[1,1], cross[3,3]) %in% c("SM", "MS"), 
               paste0(cross[1,3], cross[3,1]) %in% c("SM", "MS")) == 2){
          crossMasses <- crossMasses + 1
        }
      }
    }
  }
  return(crossMasses)
}

mTest <- t(matrix(unlist(str_split(test, "")), nrow = length(test)))
crossmasSearch(mTest)

mData <- t(matrix(unlist(str_split(data, "")), nrow = length(data)))
crossmasSearch(mData)
