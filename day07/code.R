# --- Day 7: Bridge Repair ---
# The Historians take you to a familiar rope bridge over a river in the middle
# of a jungle. The Chief isn't on this side of the bridge, though; maybe he's on
# the other side?
#
# When you go to cross the bridge, you notice a group of engineers trying to
# repair it. (Apparently, it breaks pretty frequently.) You won't be able to
# cross until it's fixed.
#
# You ask how long it'll take; the engineers tell you that it only needs final
# calibrations, but some young elephants were playing nearby and stole all the
# operators from their calibration equations! They could finish the calibrations
# if only someone could determine which test values could possibly be produced
# by placing any combination of operators into their calibration equations (your
# puzzle input).
#
# For example:
# 
# 190: 10 19
# 3267: 81 40 27
# 83: 17 5
# 156: 15 6
# 7290: 6 8 6 15
# 161011: 16 10 13
# 192: 17 8 14
# 21037: 9 7 18 13
# 292: 11 6 16 20
#
# Each line represents a single equation. The test value appears before the
# colon on each line; it is your job to determine whether the remaining numbers
# can be combined with operators to produce the test value.
#
# Operators are always evaluated left-to-right, not according to precedence
# rules. Furthermore, numbers in the equations cannot be rearranged. Glancing
# into the jungle, you can see elephants holding two different types of
# operators: add (+) and multiply (*).
#
# Only three of the above equations can be made true by inserting operators:
# 
# 190: 10 19 has only one position that accepts an operator: between 10 and 19.
# Choosing + would give 29, but choosing * would give the test value (10 * 19 =
# 190). 
# 3267: 81 40 27 has two positions for operators. Of the four possible
# configurations of the operators, two cause the right side to match the test
# value: 81 + 40 * 27 and 81 * 40 + 27 both equal 3267 (when evaluated
# left-to-right)!
# 292: 11 6 16 20 can be solved in exactly one way: 11 + 6 * 16
# + 20. The engineers just need the total calibration result, which is the sum
# of the test values from just the equations that could possibly be true. In the
# above example, the sum of the test values for the three equations listed above
# is 3749.
#
# Determine which equations could possibly be true. What is their total
# calibration result?

test <- c("190: 10 19",
          "3267: 81 40 27",
          "83: 17 5",
          "156: 15 6",
          "7290: 6 8 6 15",
          "161011: 16 10 13",
          "192: 17 8 14",
          "21037: 9 7 18 13",
          "292: 11 6 16 20")

reshape <- function(x) {
  ans <- as.numeric(str_split(x, ":")[[1]][1])
  num <- as.numeric(unlist(str_split(str_split(x, ":")[[1]][2], " ")))
  num <- num[which(!is.na(num))]
  
  return(list(ans, num))
}

nohilo <- function(x) {
  ans <- format(x[[1]], scientific = F)
  num <- x[[2]]
  
  ops <- expand.grid(rep(list(c("+", "*")), (length(num) - 1))) %>% 
    mutate_all(as.character)
  eqs <- apply(ops, 1, function(x) {
    eq <- ""
    for (i in 1:(length(num) - 1)) {eq <- paste0("(", eq, num[i], ")", as.character(x[i]))}
    eq <- paste0("(", eq, num[length(num)], ")")
  })
  
  for (i in 1:length(eqs)) {
    if (ans == format(eval(parse(text = eqs[i])), scientific = F)) {
      return(ans)
      break
    }
    if (i == length(eqs)) {
      return("impossiburu")
    }
  }
}

part1 <- function(x) {
  l <- lapply(as.list(x), reshape)
  out <- lapply(l, nohilo)
  sum(as.numeric(unlist(out[which(out != "impossiburu")])))
}

part1(test)

# data
path.script <- getActiveDocumentContext()$path
setwd(str_remove(path.script, "/code.R"))
data <- readLines("data.txt")

ans <- part1(data)
format(ans, scientific = F)

# 21560297619345 is too low!?
# 21560297620116 is also too low
# 21572148763543

# --- Part Two ---
# The engineers seem concerned; the total calibration result you gave them is
# nowhere close to being within safety tolerances. Just then, you spot your
# mistake: some well-hidden elephants are holding a third type of operator.
#
# The concatenation operator (||) combines the digits from its left and right
# inputs into a single number. For example, 12 || 345 would become 12345. All
# operators are still evaluated left-to-right.
#
# Now, apart from the three equations that could be made true using only
# addition and multiplication, the above example has three more equations that
# can be made true by inserting operators:
#
# 156: 15 6 can be made true through a single concatenation: 15 || 6 = 156.
# 7290: 6 8 6 15 can be made true using 6 * 8 || 6 * 15. 
# 192: 17 8 14 can be made true using 17 || 8 + 14. 
#
# Adding up all six test values (the three that could be made before using only
# + and * plus the new three that can now be made by also using ||) produces the
# new total calibration result of 11387.
#
# Using your new knowledge of elephant hiding spots, determine which equations
# could possibly be true. What is their total calibration result?

# alsocat <- function(x) {
#   solved <- F
#   ans <- x[[1]]
#   num <- x[[2]]
#   
#   eq <- num[1]
#   i <- 2
#   tmp <- 0
#   repeat {
#     eq <- paste0(eq, "*", num[i])
#     tmp <- eval(parse(text = eq))
#     
#     if (is.na(tmp)) {break}
#     if (tmp == ans) {print("yey")}
#     if (tmp > ans) {break}
#   
#     i <- i + 1
#   }
# }
# alsocat(l[[721]])

alsocat <- function(x) {
  solved <- F
  ans <- format(x[[1]], scientific = F)
  num <- x[[2]]

  ops <- expand.grid(rep(list(c("+", "*", "||")), (length(num) - 1))) %>%
    mutate_all(as.character) %>% 
    arrange(across(everything()))

  i <- 1
  repeat {
    eq <- ""
    for (j in 1:length(num)) {
      eq <- paste0(eq, num[j])
      # print(eq)
      eq <- eval(parse(text = eq))
      if (j == length(num) & format(eq, scientific = F) == ans) {
        solved <- T
        print("yey")
        return(ans)
        break
      }
      if (j <= ncol(ops)){
        if(as.character(ops[i, j]) != fixed("||")) {eq <- paste0(eq, ops[i, j])}
      }
    }
    if (solved) {break}
  }
  
  # for (op in 1:nrow(ops)) {
  #   eq <- ""
  #   for (j in 1:length(num)) {
  #     eq <- paste0(eq, num[j])
  #     # print(eq)
  #     eq <- eval(parse(text = eq))
  #     if (j == length(num) & format(eq, scientific = F) == ans) {
  #       solved <- T
  #       print("yey")
  #       return(ans)
  #       break
  #       }
  #     if (j <= ncol(ops)){
  #       if(as.character(ops[i, j]) != fixed("||")) {eq <- paste0(eq, ops[i, j])}
  #     }
  #   }
  #   if (solved) {break}
  # }
  print("impossiburu")
}

part2 <- function(x) {
  l <- lapply(as.list(x), reshape)
  out <- lapply(l, alsocat)
  sum(as.numeric(unlist(out[which(out != "impossiburu")])))
}

part2(test)

ans <- part2(data) # takes literally forever
format(ans, scientific = F)

