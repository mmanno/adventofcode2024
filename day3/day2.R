library(stringr)
library(data.table)
library(tidyverse)

myinput <- fread("github/inputd3.tsv", sep="\t",header=F)

computesumofmul <- function(mystring){
  as.data.frame(str_match_all(mystring,"mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")[[1]]) %>%
    mutate(mul = as.numeric(V2) * as.numeric(V3)) %>%
    select(mul) %>%
    sum
}

print(paste0("Part 1 : ",sum(apply(myinput,1,function(x) computesumofmul(x)))))

checkcorruption <- function(mystring){
  myregex <- str_extract_all(mystring,"mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don't\\(\\)")[[1]]
  mysum=0
  for(mymatch in myregex){
    if(str_detect(mymatch,"don't\\(\\)")){doit=FALSE}
    if(str_detect(mymatch,"do\\(\\)")){doit=TRUE}
    #print(paste0("doit : ",doit))
    #print(paste0("mymatch : ",mymatch))
    if(doit==TRUE && !(str_detect(mymatch,"do\\(\\)"))){
      mysum=mysum+computesumofmul(mymatch)
      #print(paste0("computesumofmul(mymatch) : ",computesumofmul(mymatch)))
    }
  }
  return(mysum)
}

doit=TRUE
print(paste0("Part 2 : ",sum(apply(myinput,1,function(x) checkcorruption(x)))))

