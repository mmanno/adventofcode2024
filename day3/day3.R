##########
# DAY 3
##########

########
# Puzzle

########
# Libs
library(stringr)
library(data.table)
library(tidyverse)

########
# input 
myinput <- fread("data/inputday3.tsv", sep=NULL,header=F)

########
# Code part one

# functions
computesumofmul <- function(mystring){
  as.data.frame(str_match_all(mystring,"mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")[[1]]) %>%
    mutate(mul = as.numeric(V2) * as.numeric(V3)) %>%
    select(mul) %>%
    sum
}

print(paste0("Part 1 : ",sum(apply(myinput,1,function(x) computesumofmul(x)))))

########
# Code part two

checkcorruption <- function(mystring){
  myregex <- str_extract_all(mystring,"mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don't\\(\\)")[[1]]
  mysum=0
  for(mymatch in myregex){
    if(str_detect(mymatch,"don't\\(\\)")){assign('doit',FALSE, envir = .GlobalEnv)}
    if(str_detect(mymatch,"do\\(\\)")){assign('doit',TRUE, envir = .GlobalEnv)}
    if(doit==TRUE && !(str_detect(mymatch,"do\\(\\)"))){
      mysum=mysum+computesumofmul(mymatch)
    }
  }
  return(mysum)
}

doit <- TRUE
print(paste0("Part 2 : ",sum(apply(myinput,1,function(x) checkcorruption(x)))))

