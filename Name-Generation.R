######################################################################
######################################################################
library('rvest')
library("e1071")
library("rgl")
library("misc3d")
library("data.table")
library("RImagePalette")
library("png")
library("magick")
library("jpeg")
library("dplyr")
library("scales")

######################################################################
######################################################################
setwd("/Users/nancyorgan/Documents/Nail-Polish/")
total = read.csv("total.csv")

show3dnoClusters(as.character(total$colors[!is.na(total$colors)]))

head(total)

findClosest = function(color){
  # color = "#242491"
  split = splitColor(color)
  x = split$r
  y = split$g
  z = split$b
  
  distance = 30
  result = data.frame()
  while(length(result$r) < 2){
    result = total %>%
      filter(r >= (x - distance ) & r <= (x + distance)) %>%
      filter(g >= (y - distance ) & g <= (y + distance)) %>%
      filter(b >= (z - distance ) & b <= (z + distance))
    
      print(distance)
      distance = distance + 10
  }
  
  print(length(result$r))
  
  names = paste(tolower(as.character(result$names)), collapse = " ", sep = " ")
  names = unlist(strsplit(names, " "))
  nNames = length(names)
  
  final = sample(x = c(1:nNames), size = 2, replace = FALSE)
  final = c(names[final[1]], names[final[2]])
  final = paste(final, collapse = " ", sep = " ")
  show_col(color)
  return(final)
}

findClosest("#242491")
findClosest("#F15D5A")
findClosest("#F75590")
findClosest("#00780E")
