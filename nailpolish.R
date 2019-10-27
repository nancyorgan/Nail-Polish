library('rvest')
library("e1071")
library("rgl")
library("misc3d")
library("RSelenium")
library("V8")
install.packages("rimage")

#Rimagepal
test

url <- list(
  Revlon = 'https://www.revlon.com/nails/nail-color/revlon-nail-enamel?shade=adventurous',
  Maybelline = 'https://www.maybelline.com.au/nail-makeup/nail-color/color-show-60-seconds-nail-lacquer/',
  Maybelline2 = 'https://www.maybelline.com.au/nail-makeup/nail-color/super-stay-7-days',
  Orosa = 'https://orosabeauty.com/products/pre-fall-set',
  NARS = 'https://www.narscosmetics.com/USA/milos-nail-polish/0607845036456.html?gclid=Cj0KCQjwl8XtBRDAARIsAKfwtxBiqFJw74nAgsr_bolu-Uojx7H7jBq-E6rET3DwUlBgaMudndeL96EaAkBtEALw_wcB&gclsrc=aw.ds',
  Essie = 'https://www.essie.com/nail-polish/by-color/reds?selectedProduct=0',
  Dior = 'https://www.dior.com/en_us/products/beauty-Y0002959_F000355257-dior-vernis-couture-color-gel-shine-long-wear-nail-lacquer?gclid=Cj0KCQjwl8XtBRDAARIsAKfwtxAvCXrpqEGt2MfLk3Ta3xJMNDGAExmKGCkqlJ_RYPl-16DUg58BdpwaAj1pEALw_wcB&gclsrc=aw.ds',
  Chanel = 'https://www.chanel.com/us/makeup/p/159705/le-vernis-longwear-nail-colour/',
  YSL = 'https://www.yslbeautyus.com/makeup/nails/la-laque-couture/1023YSL.html',
  Urban = 'https://www.urbanoutfitters.com/shop/uo-nail-polish',
  Jhannah = 'https://jhannahjewelry.com/collections/nailpolish',
  Louboutin = 'http://us.christianlouboutin.com/us_en/pluminette.html'
  )

######################################################################
######################################################################
revlon = function(url){
  webpage <- read_html(url)
  colors = webpage %>% html_nodes(xpath = "//button") %>% html_attr("style")
  names = webpage %>% html_nodes(xpath = "//button") %>% html_attr("aria-label")
  
  colors = colors[1:length(colors) -1  ]
  names = names[1:length(names) -1  ]
  
  for(i in 1:length(colors)){
    colors[i] = regmatches(colors[i], regexpr("#.{1,6}", colors[i]))
    names[i] = substr(names[i], 1, nchar(names[i]) - 6)
  }
  return(data.frame(colors, names))
}

revlonData = revlon(url$Revlon)

######################################################################
######################################################################
maybelline = function(url){
  webpage <- read_html(url)
  xpath = '/html/body/div[1]/div[1]/section[2]/div/div[2]/div[2]/div[2]/div[1]/ul/li'
  
  colors =  webpage %>% html_nodes(xpath=xpath) %>% html_attr("style")
  names = webpage %>% html_nodes(xpath = xpath) %>% html_attr("data-variantname")
  
  for(i in 1:length(colors)){
    colors[i] = regmatches(colors[i], regexpr("#.{1,6}", colors[i]))
  }
  return(data.frame(colors, names))
}

maybellineData = rbind(
  maybelline(url$Maybelline),
  maybelline(url$Maybelline2)
)

######################################################################
######################################################################
nars = function(url){
  webpage <- read_html(url)
  xpathColor = '//*[@id="pdp-swatches"]/li/a/div'
  xpathName = '//*[@id="pdp-swatches"]/li/a/img'

  colors =  webpage %>% html_nodes(xpath = xpathColor) %>% html_attr("style")
  names = webpage %>% html_nodes(xpath = xpathName) %>% html_attr("alt")
  
  for(i in 1:length(colors)){
    colors[i] = regmatches(colors[i], regexpr("#.{1,6}", colors[i]))
  }
  return(data.frame(colors, names))
}

narsData = nars(url$NARS)

######################################################################
######################################################################
essie = function(url){
  
  webpage <- read_html(url$Essie)
  
  xpathName = '//*[@id="main"]/div[3]/div[3]/div/div/div[1]/div[1]/img'
  
  library(rvest)
  library(V8)
  
  webpage <- read_html(url$Essie)
  

  script_data <- html_nodes(webpage, "script")[[3]]
  dat <- gsub("\\$\\(function.*$", "", html_text(script_data))
  
  ctx <- v8()
  ctx$eval(dat)
  head(ctx$get("accounts"))
  
  
  names = webpage %>% html_nodes(xpath = xpathName) %>% html_attr("href")
  
  for(i in 1:length(colors)){
    colors[i] = regmatches(colors[i], regexpr("#.{1,6}", colors[i]))
  }
  return(data.frame(colors, names))
}

narsData = nars(url$NARS)

######################################################################
######################################################################

show3dPolish = function(colors, clusters){
  splitColor = function(color){
    parsedColor = data.frame(
      r = strtoi(paste0("0x", substr(color, 2,3))),
      g = strtoi(paste0("0x", substr(color, 4,5))),
      b = strtoi(paste0("0x", substr(color, 6,7))),
      color = color
    )
    return(parsedColor)
  }
  
  mydata = splitColor(colors)
  mydata$cluster = kmeans(as.matrix(mydata[,1:3]), clusters)$cluster
  open3d()
  with(mydata, plot3d(r, g, b, type="s", col = color))
  
  for(i in 1:length(unique(mydata$cluster))){
    dat = mydata[mydata$cluster == i,]
    plot3d(
      ellipse3d(cov(as.matrix(dat[,1:3])),
                centre = c(mean(dat$r), mean(dat$g), mean(dat$b))),
      col = rgb(mean(dat$r)/255, mean(dat$g)/255, mean(dat$b)/255), 
      alpha = 0.5,
      add = TRUE
    )
  }
}

######################################################################
######################################################################

show3dPolish(revlonData$colors, 7)
show3dPolish(maybellineData$colors, 2)
show3dPolish(narsData$colors, 3)
