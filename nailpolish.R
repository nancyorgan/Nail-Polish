library('rvest')
library("e1071")
library("rgl")
library("misc3d")
library("data.table")
library("RImagePalette")
library("png")
# install_github("andreacirilloac/paletter")


url <- list(
  Revlon = 'https://www.revlon.com/nails/nail-color/revlon-nail-enamel?shade=adventurous',
  Maybelline = 'https://www.maybelline.com.au/nail-makeup/nail-color/color-show-60-seconds-nail-lacquer/',
  Maybelline2 = 'https://www.maybelline.com.au/nail-makeup/nail-color/super-stay-7-days',
  NARS = 'https://www.narscosmetics.com/USA/milos-nail-polish/0607845036456.html?gclid=Cj0KCQjwl8XtBRDAARIsAKfwtxBiqFJw74nAgsr_bolu-Uojx7H7jBq-E6rET3DwUlBgaMudndeL96EaAkBtEALw_wcB&gclsrc=aw.ds',
  Essie = 'https://www.essie.com/nail-polish/by-color/',
  Dior = 'https://www.dior.com/en_us/products/beauty-Y0002959_F000355257-dior-vernis-couture-color-gel-shine-long-wear-nail-lacquer?gclid=Cj0KCQjwl8XtBRDAARIsAKfwtxAvCXrpqEGt2MfLk3Ta3xJMNDGAExmKGCkqlJ_RYPl-16DUg58BdpwaAj1pEALw_wcB&gclsrc=aw.ds',
  Chanel = 'https://www.chanel.com/us/makeup/p/159705/le-vernis-longwear-nail-colour/',
  YSL = 'https://www.yslbeautyus.com/makeup/nails/la-laque-couture/1023YSL.html',
  Urban = 'https://www.urbanoutfitters.com/shop/uo-nail-polish',
  Jhannah = 'https://jhannahjewelry.com/collections/nailpolish',
  Louboutin = 'http://us.christianlouboutin.com/us_en/pluminette.html',
  Orosa = 'https://orosabeauty.com/products/pre-fall-set',
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

  groups = c("sheers", "whites", "nudes", "pinks", "corals", "reds", "purples", 
             "blues", "greens", "yellows", "grays", "metallics-and-glitters")
  allData = list()
  for(i in 1:length(groups)){
    webpage = read_html(paste0(url, groups[i], "?selectedProduct=0") )
  
    names = webpage %>% html_nodes('h3') %>% html_nodes('span')
    names = gsub("<span>", "", names)
    names = gsub("</span>", "", names)
  

    colors = webpage %>% 
      html_nodes(css = "div.product-list-item__image") %>% 
      html_nodes("img") %>% 
      html_attr("src")
    
    palette = NULL
    for(j in 1:length(colors)){
      tryCatch(
        expr = {
          temp.file = tempfile(fileext = ".jpg")
          download.file(paste0("https://www.essie.com/", colors[j]), destfile = temp.file)
          img <- jpeg::readJPEG(temp.file)
          # display_image(img)
          # scales::show_col(image_palette(img, n=10))
          palette[j] = image_palette(img, n=1)
          unlink(temp.file)
        },
        error = function(e){
          message("* Caught an error on itertion ", j)
          print(e)
        }
      )
    }
    
    allData[[i]] = data.frame(group = groups[i], name = names, palette = palette)
  
  }
  allData = rbindlist(allData)
  return(allData)
}

# essieData = essie(url$Essie)

# write.csv(essieData, "/Users/nancyorgan/Desktop/essiedata.csv", row.names = FALSE)

######################################################################
######################################################################

dior = function(url){
  
    webpage = read_html(url$Dior)
    
    names = webpage %>% html_nodes(css = 'label.sr-only') 
    names = gsub("</label>", "", names)
    names = gsub("^<label.*>", "", names)
    names = gsub("^[0-9]{3}.", "", names)
    
    colors = webpage %>% 
      html_nodes(css = "span.swatch") %>% 
      html_nodes("img") %>% 
      html_attr("src")
    
    palette = NULL
    for(i in 1:length(colors)){
      tryCatch(
        expr = {
          extension = substr(colors[i], nchar(colors[i]) - 3, nchar(colors[i]))
          temp.file = ifelse(extension == ".png", 
                             tempfile(fileext = ".png"),
                             tempfile(fileext = ".jpg"))
        
          download.file(paste0("https://www.dior.com/", colors[i]), destfile = temp.file)
          img = ifelse(extension == ".png", 
                       readPNG(temp.file),
                       readJPEG(temp.file))
          
          palette[i] = image_palette(img, n=1)
          unlink(temp.file)
        },
        error = function(e){
          message("* Caught an error on itertion ", i)
          print(e)
        }
      )
    }
    
    allData = data.frame(names = names, colors = color)
  }
  return(allData)
}

dior = essie(url$Dior)

# write.csv(essieData, "/Users/nancyorgan/Desktop/essiedata.csv", row.names = FALSE)

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
  with(mydata, plot3d(r, g, b, type="p", col = color, radius = 10))
  
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
show3dPolish(essieData$palette[!is.na(essieData$palette)], 20)





