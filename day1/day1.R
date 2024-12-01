##########
# DAY 1
##########

########
# Puzzle
# the input https://adventofcode.com/2024/day/1/input
# What is the total distance between your lists?

########
# Libs
library(tidyverse)
library(data.table)

########
# input 
myinput <- data.table::fread(file = "day1/input.tsv", sep = " ", header = F, col.names = c("RL","LL"))

########
# Code part one

# Reorder list and compute total diff
myinput %>%
  mutate(RL = RL[order(RL, decreasing = FALSE)]) %>%
  mutate(LL = LL[order(LL, decreasing = FALSE)]) %>%
  mutate(DIF = abs(RL - LL)) %>%
  select(DIF) %>%
  sum

########
# Code part two

checknumberoftime <- function(number, list){
  sum(list == number, na.rm = TRUE)
}

# change the number in the RL by the x time it is in the LL
myinput %>%
  rowwise() %>%
  mutate(SM = RL * checknumberoftime(RL, myinput$LL)) %>%
  select(SM) %>%
  sum


