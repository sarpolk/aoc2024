# --- Day 6: Guard Gallivant ---
# The Historians use their fancy device again, this time to whisk you all away
# to the North Pole prototype suit manufacturing lab... in the year 1518! It
# turns out that having direct access to history is very convenient for a group
# of historians.
#
# You still have to be careful of time paradoxes, and so it will be important to
# avoid anyone from 1518 while The Historians search for the Chief.
# Unfortunately, a single guard is patrolling this part of the lab.
#
# Maybe you can work out where the guard will go ahead of time so that The
# Historians can search safely?
#
# You start by making a map (your puzzle input) of the situation. For example:
#   
# ....#.....
# .........#
# ..........
# ..#.......
# .......#..
# ..........
# .#..^.....
# ........#.
# #.........
# ......#...
# The map shows the current position of the guard with ^ (to indicate the guard
# is currently facing up from the perspective of the map). Any obstructions -
# crates, desks, alchemical reactors, etc. - are shown as #.
#
# Lab guards in 1518 follow a very strict patrol protocol which involves
# repeatedly following these steps:
#
# If there is something directly in front of you, turn right 90 degrees.
# Otherwise, take a step forward. Following the above protocol, the guard moves
# up several times until she reaches an obstacle (in this case, a pile of failed
# suit prototypes):
#   
# ....#.....
# ....^....#
# ..........
# ..#.......
# .......#..
# ..........
# .#........
# ........#.
# #.........
# ......#...
# Because there is now an obstacle in front of the guard, she turns right before
# continuing straight in her new facing direction:
#   
# ....#.....
# ........>#
# ..........
# ..#.......
# .......#..
# ..........
# .#........
# ........#.
# #.........
# ......#...
# Reaching another obstacle (a spool of several very long polymers), she turns
# right again and continues downward:
#   
# ....#.....
# .........#
# ..........
# ..#.......
# .......#..
# ..........
# .#......v.
# ........#.
# #.........
# ......#...
# This process continues for a while, but the guard eventually leaves the mapped
# area (after walking past a tank of universal solvent):
#   
# ....#.....
# .........#
# ..........
# ..#.......
# .......#..
# ..........
# .#........
# ........#.
# #.........
# ......#v..
# By predicting the guard's route, you can determine which specific positions in the lab will be in the patrol path. Including the guard's starting position, the positions visited by the guard before leaving the area are marked with an X:
#   
# ....#.....
# ....XXXXX#
# ....X...X.
# ..#.X...X.
# ..XXXXX#X.
# ..X.X.X.X.
# .#XXXXXXX.
# .XXXXXXX#.
# #XXXXXXX..
# ......#X..
# In this example, the guard will visit 41 distinct positions on your map.
#
# Predict the path of the guard. How many distinct positions will the guard
# visit before leaving the mapped area?
library(tidyverse)
library(rstudioapi)    

test <- c("....#.....",
          ".........#",
          "..........",
          "..#.......",
          ".......#..",
          "..........",
          ".#..^.....",
          "........#.",
          "#.........",
          "......#...")

reshape <- function(x) {t(matrix(unlist(str_split(x, "")), ncol = length(x)))}

move <- function(x, start, i){
  dir <- which(!is.na(start))
  
  if (dir == 1) {
    path <- start[dir] - 1
  } else if (dir == 2) {
    path <- start[dir] + nrow(x)
  } else if (dir == 3) {
    path <- start[dir] + 1
  } else if (dir == 4) {
    path <- start[dir] - nrow(x)
  }
  
  ifelse(x[path] == "#", continue <- F, continue <- T)
  if (continue) {
    start[dir] <- path
    i <- append(i, path)
  } else if (!continue) {
    start[dir] <- NA_real_
    ifelse(dir == 4, dir <- 1, dir <- dir + 1)
    start[dir] <- i[length(i)]
  }
  return(list(start = start, i = i))
}

part1 <- function(x) {
  m <- reshape(x)
  
  illegal <- cbind(seq(nchar(x[1]) - 1, length(m), nchar(x[1])),
                   seq(nchar(x[1]), length(m), nchar(x[1])))
  illegal <- split(illegal, row(illegal))
  
  start <- match(c("^", ">", "v", "<"), m)
  i <- start[which(!is.na(start))]
 
  repeat {
    out <- move(m, start, i)
    start <- out$start
    i <- out$i
    
    if (i[length(i)] > length(m) | i[length(i)] < 0) {
      i <- i[c(1:(length(i) - 1))]
      return(length(unique(i)))
      break
    } else if (sum(unlist(lapply(illegal, function(x) sum(i[c(length(i) - 1, length(i))] == x)))) == 2) {
      return(length(unique(i)))
      break
    }
  }
  return(length(unique(i)))
}

part1(test)

# data
path.script <- getActiveDocumentContext()$path
setwd(str_remove(path.script, "/code.R"))
data <- readLines("data.txt")

part1(data)


# --- Part Two ---
# While The Historians begin working around the guard's patrol route, you borrow
# their fancy device and step outside the lab. From the safety of a supply
# closet, you time travel through the last few months and record the nightly
# status of the lab's guard post on the walls of the closet.
#
# Returning after what seems like only a few seconds to The Historians, they
# explain that the guard's patrol area is simply too large for them to safely
# search the lab without getting caught.
#
# Fortunately, they are pretty sure that adding a single new obstruction won't
# cause a time paradox. They'd like to place the new obstruction in such a way
# that the guard will get stuck in a loop, making the rest of the lab safe to
# search.
#
# To have the lowest chance of creating a time paradox, The Historians would
# like to know all of the possible positions for such an obstruction. The new
# obstruction can't be placed at the guard's starting position - the guard is
# there right now and would notice.
#
# In the above example, there are only 6 different positions where a new
# obstruction would cause the guard to get stuck in a loop. The diagrams of
# these six situations use O to mark the new obstruction, | to show a position
# where the guard moves up/down, - to show a position where the guard moves
# left/right, and + to show a position where the guard moves both up/down and
# left/right.
#
# Option one, put a printing press next to the guard's starting position:
# 
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ....|..#|.
# ....|...|.
# .#.O^---+.
# ........#.
# #.........
# ......#...
# Option two, put a stack of failed suit prototypes in the bottom right quadrant
# of the mapped area:
#   
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ..+-+-+#|.
# ..|.|.|.|.
# .#+-^-+-+.
# ......O.#.
# #.........
# ......#...
# Option three, put a crate of chimney-squeeze prototype fabric next to the
# standing desk in the bottom right quadrant:
#   
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ..+-+-+#|.
# ..|.|.|.|.
# .#+-^-+-+.
# .+----+O#.
# #+----+...
# ......#...
# Option four, put an alchemical retroencabulator near the bottom left corner:
#   
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ..+-+-+#|.
# ..|.|.|.|.
# .#+-^-+-+.
# ..|...|.#.
# #O+---+...
# ......#...
# Option five, put the alchemical retroencabulator a bit to the right instead:
#   
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ..+-+-+#|.
# ..|.|.|.|.
# .#+-^-+-+.
# ....|.|.#.
# #..O+-+...
# ......#...
# Option six, put a tank of sovereign glue right next to the tank of universal solvent:
#   
# ....#.....
# ....+---+#
# ....|...|.
# ..#.|...|.
# ..+-+-+#|.
# ..|.|.|.|.
# .#+-^-+-+.
# .+----++#.
# #+----++..
# ......#O..
# It doesn't really matter what you choose to use as an obstacle so long as you
# and The Historians can put it into position without the guard noticing. The
# important thing is having enough options that you can find one that minimizes
# time paradoxes, and in this example, there are 6 different positions you could
# choose.
#
# You need to get the guard stuck in a loop by adding a single new obstruction.
# How many different positions could you choose for this obstruction?

test

getIllegal <- function(x) {
  illegal <- rbind(cbind(start = seq(nchar(x[1]) + 1, sum(nchar(x)), nchar(x[1])),
                         dir = 1),
                   cbind(start = seq(nchar(x[1]), sum(nchar(x)), nchar(x[1])),
                         dir = 3))
  illegal <- split(illegal, row(illegal))
  return(illegal)
}

getObstacles <- function(x) {
  obstacles <- which(x == "#")
  return(obstacles)
}

move <- function(x, input) {
  position <- input[[1]]
  illegal <- input[[2]]
  obstacles <- input[[3]]
  
  start <- position[[length(position)]][1]
  dir <- position[[length(position)]][2]
  
  if (dir == 1) {
    path <- start - 1
  } else if (dir == 2) {
    path <- start + nrow(x)
  } else if (dir == 3) {
    path <- start + 1
  } else if (dir == 4) {
    path <- start - nrow(x)
  }
  
  ifelse(path %in% obstacles, continue <- F, continue <- T)
  if (continue) {
    position[[length(position) + 1]] <- c(path, dir)
  } else if (!continue) {
    ifelse(dir == 4, dir <- 1, dir <- dir + 1)
    position[[length(position) + 1]] <- c(position[[length(position)]][1], dir)
  }
  # print(position[[length(position)]])
  
  input[[1]] <- position
  return(input)
}

part2 <- function(x) {
  illegal <- getIllegal(x)

  m <- reshape(x)
  obstacles <- getObstacles(m)
  start <- match(c("^", ">", "v", "<"), m)

  chimneys <- c(1:length(m))
  chimneys <- as.list(chimneys[which(chimneys != start[which(!is.na(start))])])
  
  obstacles <- lapply(chimneys, function(x) c(x, obstacles))
  
  position <- list(c(start[which(!is.na(start))], which(!is.na(start))))
  
  out <- lapply(obstacles, function(x) {
    input <- list(position,
                  illegal,
                  x)
    repeat {
      input <- move(m, input)
      position <- input[[1]]
      if (position[[length(position)]][1] > length(m) | 
          position[[length(position)]][1] < 0) {
        return("exit")
        break
      } else if (sum(unlist(lapply(illegal, function(x) sum(position[[length(position)]] == unlist(x)) == 2))) != 0) {
        return("exit")
        break
      } else if (length(position) > nrow(data.frame(t(data.frame(position))) %>% distinct())) {
        return("loop!")
        break
      }
    }
  })
  print(out)
  return(out)
}

save <- (part2(test))
sum(save == "loop!")

save <- (part2(data)) #takes way too long
sum(save == "loop!")
