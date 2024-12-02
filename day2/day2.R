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
myinput <- data.table::fread(file = "day2/input.txt", sep = ",", header = F)
myinput <- apply(myinput,1, function(x) as.numeric(strsplit(x," ")[[1]]))

########
# Code part one

# Functions

computediff <- function(mylist){
  mylist[-length(mylist)] - mylist[-1]
}

checkupordown <- function(mylist){
  list_length <- length(mylist)
  if(sum(mylist > 0) == list_length || sum(mylist < 0) == list_length){
    return(TRUE)
  }
  return(FALSE)
}

checkdiffer <- function(mylist){
  list_length <- length(mylist)
  if(sum(abs(mylist) >= 1) == list_length && sum(abs(mylist) <= 3) == list_length){
    return(TRUE)
  }
  return(FALSE)
}

checksafe <- function(mylist){
  diffoflist <- computediff(mylist)
  if(sum(checkupordown(diffoflist),checkdiffer(diffoflist)) == 2){
    return(TRUE)
  }
  return(FALSE)
}

sum(lapply(myinput, function(x) checksafe(x)) == TRUE)
  

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

sum(lapply(myinput, function(x) checkeachitem(x)) == TRUE)
