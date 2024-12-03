##########
# DAY 2
##########

########
# Puzzle

########
# Libs
library(tidyverse)
library(data.table)

########
# input 
myinput <- data.table::fread(file = "data/inputday2.tsv", sep = ",", header = F)
myinput <- apply(myinput,1, function(x) as.numeric(strsplit(x," ")[[1]]))

########
# Code part one

# Functions

computediff <- function(mylist){
  mylist[-length(mylist)] - mylist[-1]
}

checkupordown <- function(mylist){
  list_length <- length(mylist)
  sum(mylist > 0) == list_length || sum(mylist < 0) == list_length
}

checkdiffer <- function(mylist){
  list_length <- length(mylist)
  sum(abs(mylist) >= 1) == list_length && sum(abs(mylist) <= 3) == list_length
}

checksafe <- function(mylist){
  diffoflist <- computediff(mylist)
  sum(checkupordown(diffoflist),checkdiffer(diffoflist)) == 2
}

print(paste0("Part 1 : ",sum(lapply(myinput, function(x) checksafe(x)) == TRUE)))
  

########
# Code part two

checkeachitem <- function(mylist){
  list_length <- length(mylist)
  for(index in 1:list_length){
    mylist_tmp <- mylist[-index]
    if(checksafe(mylist_tmp)){return(TRUE)}
  }
  return(FALSE)
}

print(paste0("Part 2 : ",sum(lapply(myinput, function(x) checkeachitem(x)) == TRUE)))
