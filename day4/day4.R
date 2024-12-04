##########
# DAY 4
##########

########
# Puzzle


########
# Libs
library(tidyverse)
library(data.table)
library(stringr)

########
# input 
myinput <- data.table::fread(file = "data/inputday4.txt", sep = NULL, header = F)
myinput <- str_split(myinput$V1, "", simplify = T)

########
# Code part one

# make list of list for combinasons
list2combine <- list(
  list(c(0,0),c(0,1),c(0,2),c(0,3)),
  list(c(0,0),c(0,-1),c(0,-2),c(0,-3)),
  list(c(0,0),c(1,0),c(2,0),c(3,0)),
  list(c(0,0),c(-1,0),c(-2,0),c(-3,0)),
  list(c(0,0),c(1,1),c(2,2),c(3,3)),
  list(c(0,0),c(-1,-1),c(-2,-2),c(-3,-3)),
  list(c(0,0),c(1,-1),c(2,-2),c(3,-3)),
  list(c(0,0),c(-1,1),c(-2,2),c(-3,3))
)

# functions

checktruepositions <- function(mylistofpositions){
  # return TRUE if positions are ok
  # not negative, not over the lenght
  sum(unlist(mylistofpositions) < 0) == 0 && 
  sum(unlist(mylistofpositions) > nrow(myinput)) == 0 && 
  sum(unlist(mylistofpositions) > ncol(myinput)) == 0
}

findworkindata <- function(mylistofpositions){
  # extract word in data from positions
  paste0(unlist(lapply(mylistofpositions, function(x) myinput[x[1],x[2]])),collapse = "")
}

checklikexmas <- function(mylistofpositions){
  # from a list of positions, find the word and compare with XMAS
  if(checktruepositions(mylistofpositions)){
    return(findworkindata(mylistofpositions) %like% "XMAS")
  }
  return(FALSE)
}

sum2listsofpositions <- function(list1,list2){
  lapply(seq_along(list1),function(i)
  unlist(list1[i])+unlist(list2[i]))
}

mixelistofpositions <- function(mylist,listoflist){
  lapply(listoflist,function(x) sum2listsofpositions(mylist,x))
}

look4XMASinposition <- function(position){
  # from a position into matrix, find every list of positions
  positions_tmp <- list(position,position,position,position)
  positions2check <- mixelistofpositions(positions_tmp,list2combine)
  sum(unlist(lapply(positions2check, function(x) checklikexmas(x))))
}

look4XMASindata <- function(mydata){
  nbXMAS <- 0
  for(i in 1:nrow(mydata)){
    for(j in 1:ncol(mydata)){
      nbXMAS <- nbXMAS + look4XMASinposition(c(i,j))
    }
  }
  return(nbXMAS)
}

print(paste0("Part 1 : ",look4XMASindata(myinput)))

########
# Code part two

list2combine <- list(
  list(c(-1,-1),c(0,0),c(1,1),c(-1,1),c(0,0),c(1,-1)),
  list(c(1,-1),c(0,0),c(-1,1),c(1,1),c(0,0),c(-1,-1)),
  list(c(-1,-1),c(0,0),c(1,1),c(1,-1),c(0,0),c(-1,1)),
  list(c(1,1),c(0,0),c(-1,-1),c(-1,1),c(0,0),c(1,-1))
)

checklikecrossmas <- function(mylistofpositions){
  # from a list of positions, find the word and compare with MASMAS
  if(checktruepositions(mylistofpositions)){
    return(findworkindata(mylistofpositions) %like% "MASMAS")
  }
  return(FALSE)
}

look4crossMASinposition <- function(position){
  # from a position into matrix, find every list of positions
  positions_tmp <- list(position,position,position,position,position,position)
  positions2check <- mixelistofpositions(positions_tmp,list2combine)
  sum(unlist(lapply(positions2check, function(x) checklikecrossmas(x))))
}

look4crossMASindata <- function(mydata){
  nbXMAS <- 0
  for(i in 1:nrow(mydata)){
    for(j in 1:ncol(mydata)){
      nbXMAS <- nbXMAS + look4crossMASinposition(c(i,j))
    }
  }
  return(nbXMAS)
}


print(paste0("Part 2 : ",look4crossMASindata(myinput)))
