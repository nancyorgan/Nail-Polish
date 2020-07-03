setwd("/Users/nancyorgan/Documents/Nail-Polish/")
url = "https://www.valspar.com/en/colors/browse-colors?pg="
paintColors = function(url){
  
  allData = list()
  for(i in 1:59){
    print(i)
    webpage = read_html(paste0(url,i) )
    webpage = read_html(url)

    colors = webpage %>% html_nodes(css="div.color-swatch")
    colors = gsub("<div class=\"color-swatch\" style=\"background-color: rgb\\(", "", colors)
    colors = gsub("\\);\"></div>", "", colors)
    
    r = as.numeric(sapply(strsplit(colors, ","), function(x) {x[1]}))
    g = as.numeric(sapply(strsplit(colors, ","), function(x) {x[2]}))
    b = as.numeric(sapply(strsplit(colors, ","), function(x) {x[3]}))
    
    colors = sapply(strsplit(colors, ","), function(x){
      rgb(x[1], x[2], x[3], maxColorValue=255)
        }
      )
    
    names = webpage %>% html_nodes(css="div.color-name")
    names = gsub("<div class=\"color-name\">", "", names)
    names = gsub("</div>", "", names)

    allData[[i]] = data.frame(colors, names, r, g, b)
  }
  allData = rbindlist(allData)
  return(allData)
}

paintcolors = paintColors(url)
write.csv(paintcolors, "paintcolors.csv", row.names = FALSE)
