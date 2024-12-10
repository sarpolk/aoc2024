# --- Day 8: Resonant Collinearity ---
# You find yourselves on the roof of a top-secret Easter Bunny installation.
#
# While The Historians do their thing, you take a look at the familiar huge
# antenna. Much to your surprise, it seems to have been reconfigured to emit a
# signal that makes people 0.1% more likely to buy Easter Bunny brand Imitation
# Mediocre Chocolate as a Christmas gift! Unthinkable!
#
# Scanning across the city, you find that there are actually many such antennas.
# Each antenna is tuned to a specific frequency indicated by a single lowercase
# letter, uppercase letter, or digit. You create a map (your puzzle input) of
# these antennas. For example:
# 
# ............
# ........0...
# .....0......
# .......0....
# ....0.......
# ......A.....
# ............
# ............
# ........A...
# .........A..
# ............
# ............
#
# The signal only applies its nefarious effect at specific antinodes based on
# the resonant frequencies of the antennas. In particular, an antinode occurs at
# any point that is perfectly in line with two antennas of the same frequency -
# but only when one of the antennas is twice as far away as the other. This
# means that for any pair of antennas with the same frequency, there are two
# antinodes, one on either side of them.
#
# So, for these two antennas with frequency a, they create the two antinodes
# marked with #:
# 
# ..........
# ...#......
# ..........
# ....a.....
# ..........
# .....a....
# ..........
# ......#...
# ..........
# ..........
#
# Adding a third antenna with the same frequency creates several more antinodes.
# It would ideally add four antinodes, but two are off the right side of the
# map, so instead it adds only two:
#   
# ..........
# ...#......
# #.........
# ....a.....
# ........a.
# .....a....
# ..#.......
# ......#...
# ..........
# ..........
#
# Antennas with different frequencies don't create antinodes; A and a count as
# different frequencies. However, antinodes can occur at locations that contain
# antennas. In this diagram, the lone antenna with frequency capital A creates
# no antinodes but has a lowercase-a-frequency antinode at its location:
# 
# ..........
# ...#......
# #.........
# ....a.....
# ........a.
# .....a....
# ..#.......
# ......A...
# ..........
# ..........
#
# The first example has antennas with two different frequencies, so the
# antinodes they create look like this, plus an antinode overlapping the topmost
# A-frequency antenna:
# 
# . . . . . . # . . . . #
# . . . # . . . . 0 . . .
# . . . . # 0 . . . . # .
# . . # . . . . 0 . . . .
# . . . . 0 . . . . # . .
# . # . . . . A . . . . .
# . . . # . . . . . . . .
# # . . . . . . # . . . .
# . . . . . . . . A . . .
# . . . . . . . . . A . .
# . . . . . . . . . . # .
# . . . . . . . . . . # .
#
# Because the topmost A-frequency antenna overlaps with a 0-frequency antinode,
# there are 14 total unique locations that contain an antinode within the bounds
# of the map.
#
# Calculate the impact of the signal. How many unique locations within the
# bounds of the map contain an antinode?
library(tidyverse)
library(rstudioapi)    

test <- c("............",
          "........0...",
          ".....0......",
          ".......0....",
          "....0.......",
          "......A.....",
          "............",
          "............",
          "........A...",
          ".........A..",
          "............",
          "............")

reshape <- function(x) {t(matrix(unlist(str_split(x, "")), ncol = length(x)))}

getAntinodes <- function(x, mat) {
  ant <- which(mat == x, arr.ind = TRUE)
  ant.pairs <- do.call(rbind, combn(c(1:nrow(ant)), 2, simplify = FALSE))
  
  positions <- vector(mode = "list")
  for (i in 1:nrow(ant.pairs)){
    pair <- ant[ant.pairs[i,], ]
    
    # y = mx + b
    m <- (pair[2, "col"] - pair[1, "col"]) / (pair[2, "row"] - pair[1, "row"])
    b <- pair[1, "col"] - m * pair[1, "row"]
    
    c <- abs(diff(pair[, "row"]))
    x <- seq(min(pair[, "row"]) - c, max(pair[, "row"]) + c, by = c)
    x <- round(x[which(x <= nrow(mat) & x > 0)])
    
    y <- round(m * x + b)

    positions[[i]] <- cbind(x, y)
  }
  positions <- do.call(rbind, positions)
  positions <- positions[which(positions[,2] <= ncol(mat) & positions[,2] > 0), ]
  
  positions <- as.matrix(anti_join(as.data.frame(positions), 
                                   data.frame(ant) %>% rename(x = row, y = col),
                                   join_by(x, y)))
  
  return(positions)
}

part1 <- function(x) {
  mat <- reshape(x)
  antennae <- unique(matrix(mat))[which(unique(matrix(mat)) != ".")]
  positions <- sapply(antennae, getAntinodes, mat = mat)
  nrow(distinct(data.frame(do.call(rbind, positions))))
}

part1(test)

# data
path.script <- getActiveDocumentContext()$path
setwd(str_remove(path.script, "/code.R"))
data <- readLines("data.txt")

part1(data)

plot1 <- function(x) {
  mat <- reshape(x)
  antennae <- unique(matrix(mat))[which(unique(matrix(mat)) != ".")]
  positions <- sapply(antennae, getAntinodes, mat = mat)
  
  antinodes <- as.data.frame(do.call(rbind, lapply(names(positions), function(name) {
    cbind(antinode = name, positions[[name]])
  })))
  
  antennae <- as.data.frame(which(mat != ".", arr.ind = TRUE)) %>%
    rename(x = row,
           y = col)
  
  max <- nrow(mat)
  p <- ggplot(as.data.frame(antinodes), aes(x = as.numeric(y), y = as.numeric(x))) +
    geom_point(data = antennae, shape = 24, color = "#00cc00") +
    geom_text(label = "*", size = 6, color = "#ffff65") +
    scale_y_reverse(breaks = c(1:max),
                    limits = c(max, 1)) +
    scale_x_continuous(breaks = c(1:max),
                       limits = c(1, max)) +
    theme_void(base_size = 6) +
    theme(plot.background = element_rect(fill = "#0e0e23"),
          legend.background = element_rect(fill = "#0e0e23"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.text = element_text(color = "#019800", family = "mono"),
          # axis.text = element_blank(),
          text = element_text(family = "mono"),
          legend.position = "none") +
    coord_fixed() +
    xlab("") +
    ylab("")
  return(p)
}
p <- plot1(data)
ggsave("part1.png", p, height = 6, width = 6)

# --- Part Two ---
# Watching over your shoulder as you work, one of The Historians asks if you
# took the effects of resonant harmonics into your calculations.
#
# Whoops!
#
# After updating your model, it turns out that an antinode occurs at any grid
# position exactly in line with at least two antennas of the same frequency,
# regardless of distance. This means that some of the new antinodes will occur
# at the position of each antenna (unless that antenna is the only one of its
# frequency).
#
# So, these three T-frequency antennas now create many antinodes:
#   
# T....#....
# ...T......
# .T....#...
# .........#
# ..#.......
# ..........
# ...#......
# ..........
# ....#.....
# ..........
#
# In fact, the three T-frequency antennas are all exactly in line with two
# antennas, so they are all also antinodes! This brings the total number of
# antinodes in the above example to 9.
#
# The original example now has 34 antinodes, including the antinodes that appear
# on every antenna:
#   
# ##....#....#
# .#.#....0...
# ..#.#0....#.
# ..##...0....
# ....0....#..
# .#...#A....#
# ...#..#.....
# #....#.#....
# ..#.....A...
# ....#....A..
# .#........#.
# ...#......##
#
# Calculate the impact of the signal using this updated model. How many unique
# locations within the bounds of the map contain an antinode?

getAllAntinodes <- function(x, mat) {
  ant <- which(mat == x, arr.ind = TRUE)
  ant.pairs <- do.call(rbind, combn(c(1:nrow(ant)), 2, simplify = FALSE))
  
  positions <- vector(mode = "list")
  for (i in 1:nrow(ant.pairs)){
    pair <- ant[ant.pairs[i,], ]
    
    # y = mx + b
    m <- (pair[2, "col"] - pair[1, "col"]) / (pair[2, "row"] - pair[1, "row"])
    b <- pair[1, "col"] - m * pair[1, "row"]
    
    c <- abs(diff(pair[, "row"]))
    x <- seq(min(pair[, "row"]) - c * nrow(mat), 
             max(pair[, "row"]) + c * nrow(mat), 
             by = c)
    x <- round(x[which(x <= nrow(mat) & x > 0)])
    
    y <- round(m * x + b)
    
    positions[[i]] <- cbind(x, y)
  }
  positions <- do.call(rbind, positions)
  positions <- positions[which(positions[,2] <= ncol(mat) & positions[,2] > 0), ]
  
  return(positions)
}

part2 <- function(x) {
  mat <- reshape(x)
  antennae <- unique(matrix(mat))[which(unique(matrix(mat)) != ".")]
  positions <- sapply(antennae, getAllAntinodes, mat = mat)
  nrow(distinct(data.frame(do.call(rbind, positions))))
}

part2(test)
part2(data)

plot2 <- function(x) {
  mat <- reshape(x)
  antennae <- unique(matrix(mat))[which(unique(matrix(mat)) != ".")]
  positions <- sapply(antennae, getAllAntinodes, mat = mat)
  
  antinodes <- as.data.frame(do.call(rbind, lapply(names(positions), function(name) {
    cbind(antinode = name, positions[[name]])
  })))
  
  antennae <- as.data.frame(which(mat != ".", arr.ind = TRUE)) %>%
    rename(x = row,
           y = col)
  
  max <- nrow(mat)
  p <- ggplot(as.data.frame(antinodes), aes(x = as.numeric(y), y = as.numeric(x))) +
    geom_point(data = antennae, shape = 24, color = "#00cc00") +
    geom_text(label = "*", size = 6, color = "#ffff65") +
    scale_y_reverse(breaks = c(1:max),
                    limits = c(max, 1)) +
    scale_x_continuous(breaks = c(1:max),
                       limits = c(1, max)) +
    theme_void(base_size = 6) +
    theme(plot.background = element_rect(fill = "#0e0e23"),
          legend.background = element_rect(fill = "#0e0e23"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.text = element_text(color = "#019800", family = "mono"),
          # axis.text = element_blank(),
          text = element_text(family = "mono"),
          legend.position = "none") +
    coord_fixed() +
    xlab("") +
    ylab("")
  return(p)
}
p <- plot2(data)
ggsave("part2.png", p, height = 6, width = 6)



