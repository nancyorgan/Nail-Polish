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
  YSL2 = "https://www.yslbeautyus.com/makeup/nails/la-laque-couture-fall-look-2019/113YSL.html?cgid=makeup-nails&dwvar_113YSL_color=118%20-%20Marron%20Sulfureux#start=3&cgid=makeup-nails",
  Urban = 'https://www.urbanoutfitters.com/shop/uo-nail-polish',
  Jhannah = 'https://jhannahjewelry.com/collections/nailpolish',
  Louboutin = 'http://us.christianlouboutin.com/us_en/pluminette.html',
  Orosa = 'https://orosabeauty.com/products/pre-fall-set',
  ZOYA = 'https://www.zoya.com/content/category/Zoya_Nail_Polish.html'
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
# write.csv(revlonData, "/Users/nancyorgan/Documents/Nail-Polish/revlonData.csv", row.names = FALSE)
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
#write.csv(maybellineData, "/Users/nancyorgan/Documents/Nail-Polish/maybellineData.csv", row.names = FALSE)
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
#write.csv(narsData, "/Users/nancyorgan/Documents/Nail-Polish/narsData.csv", row.names = FALSE)
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
names(essieData) = c("group", "names", "colors")
# write.csv(essieData, "/Users/nancyorgan/Desktop/essiedata.csv", row.names = FALSE)
######################################################################
######################################################################
dior = function(url){
  
  webpage = read_html(url)
  
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
        temp.file = ifelse(extension == ".png", tempfile(fileext = ".png"),
                           ifelse(extension == ".jpg", tempfile(fileext = ".jpg"),
                                  temptile(fileext = extension)))
        
        download.file(paste0("https://www.dior.com/", colors[i]), destfile = temp.file)
        img = image_write(image_read(temp.file), temp.file, format = "jpeg")
        img = readJPEG(temp.file)
        display_image(img)
        # img = ifelse(extension == ".png", readPNG(temp.file), readPNG(temp.file))
        palette[i] = image_palette(img, n=1)
        unlink(temp.file)
      },
      error = function(e){
        message("* Caught an error on itertion ", i)
        print(e)
      }
    )
  }
  
  allData = data.frame(names = names, colors = palette)
  return(allData)
}

diorData = dior(url$Dior)
#write.csv(diorData, "/Users/nancyorgan/Documents/Nail-Polish/diorData.csv", row.names = FALSE)
######################################################################
######################################################################
urbanOutfitters = function(){
  
  urls = c("https://www.urbanoutfitters.com/shop/uo-mood-nail-polish",
           "https://www.urbanoutfitters.com/shop/uo-neutrals-collection-nail-polish0611299-006",
           "https://www.urbanoutfitters.com/shop/uo-shimmer-nail-polish",
           "https://www.urbanoutfitters.com/shop/uo-nail-polish",
           "https://www.urbanoutfitters.com/shop/uo-classics-collection-nail-polish-001")
  
  allData = list()
  for(j in 1:length(urls)){
    webpage = read_html(urls[j])
    
    names = webpage %>% 
      html_nodes(css = '.o-list-swatches__swatch') %>% 
      html_attr("title")
    
    colors = webpage %>% 
      html_nodes(css = '.o-list-swatches__swatch') %>% 
      html_attr("src")
    
    palette = NULL
    for(i in 1:length(colors)){
      tryCatch(
        expr = {
          temp.file = tempfile(fileext = ".jpg")
          download.file(colors[i], destfile = temp.file)
          img = readJPEG(temp.file)
          display_image(img)
          palette[i] = image_palette(img, n=1)
          unlink(temp.file)
        },
        error = function(e){
          message("* Caught an error on itertion ", i)
          print(e)
        }
      )
    }
    allData[[j]] = data.frame(names = names, colors = palette)
    
  }
  allData = rbindlist(allData)
  return(allData)
}
uoData = urbanOutfitters()
# write.csv(uoData, "/Users/nancyorgan/Documents/Nail-Polish/uoData.csv", row.names = FALSE)

######################################################################
######################################################################
YSL = function(url){
  
  webpage = read_html(url$YSL2)
  
  names =  webpage %>% 
    html_nodes(css = '.swatch_color')
  names = gsub(".*title=", "", names)
  names = gsub("></span>", "", names)
  names = gsub(".*\\s-\\s", "", names)
  names = gsub('.{1}$', '', names)

  colors = webpage %>% 
    html_nodes(css = '.swatch_color') %>%
    html_attr("style")
  colors = gsub(pattern = "background-color: ", replacement = "", colors )
  
  allData = data.frame(colors = colors, names = names)
  
  colors_img = webpage %>%
    html_nodes(css = '.swatch_image_color') %>%
    html_attr("data-retina-src") 
  
  names_img = webpage %>%
    html_nodes(css = '.swatch_image_color') 
  names = gsub(".*title=", "", names)
  names = gsub("></span>", "", names)
  names = gsub(".*\\s-\\s", "", names)
  names = gsub('.{1}$', '', names)
  
  palette = NULL
  if(length(colors_img) > 0){
    for(i in 1:length(colors_img)){
    tryCatch(
      expr = {
        extension = substr(colors_img[i], nchar(colors_img[i]) - 3, nchar(colors_img[i]))
        temp.file = ifelse(extension == ".png", tempfile(fileext = ".png"),
                           ifelse(extension == ".jpg", tempfile(fileext = ".jpg"),
                                  tempfile(fileext = extension)))
        
        download.file(colors_img[i], destfile = temp.file)
        img = image_write(image_read(temp.file), temp.file, format = "jpeg")
        img = readJPEG(temp.file)
        display_image(img)
        # img = ifelse(extension == ".png", readPNG(temp.file), readPNG(temp.file))
        palette[i] = image_palette(img, n=1)
        unlink(temp.file)
      },
      error = function(e){
        message("* Caught an error on itertion ", i)
        print(e)
      }
    )
  }
  }
  
  pictureData = data.frame(colors = palette, names = names)
  allData = rbind(allData, pictureData)
  return(allData)
}

YSLdata = rbind(YSL(url$YSL), YSL(url$YSL2))
#write.csv(diorData, "/Users/nancyorgan/Documents/Nail-Polish/diorData.csv", row.names = FALSE)

######################################################################
######################################################################
ZOYA = function(url){
  
  webpage = read_html(url$ZOYA)
  
  names =  webpage %>% 
    html_nodes(css = '.color_swatch') %>%
    html_attr("title")
  
  names = gsub("^ZP", "", names)
  names = gsub("swatch", "", names)
  names = gsub("[0-9]", "", names)
  names = trimws(names)
  
  colors_img = webpage %>% 
    html_nodes(css = '.image_thumb') %>%
    html_attr("src")
  
  colors_img = gsub("//", "", colors_img)
  
  colors = NULL
  if(length(colors_img) > 0){
    for(i in 1:length(colors_img)){
      tryCatch(
        expr = {
          temp.file = tempfile(fileext = ".jpg")
          download.file(colors_img[i], destfile = temp.file)
          img = image_write(image_read(temp.file), temp.file, format = "jpeg")
          img = readJPEG(temp.file)
          display_image(img)
          colors[i] = image_palette(img, n=1)
          unlink(temp.file)
        },
        error = function(e){
          message("* Caught an error on itertion ", i)
          print(e)
        }
      )
    }
  }
  
  pictureData = data.frame(colors = colors, names = names)
  allData = rbind(allData, pictureData)
  return(allData)
}

# ZOYAdata = ZOYA(url$YSL)'
# ZOYAdata_coords = splitColor(ZOYAdata$colors)
# ZOYAdata = ZOYAdata %>% left_join(ZOYAdata_coords, by = "colors")
# write.csv(ZOYAdata, "/Users/nancyorgan/Documents/Nail-Polish/ZOYAdata.csv", row.names = FALSE)

######################################################################
######################################################################
splitColor = function(color){
  parsedColor = data.frame(
    r = strtoi(paste0("0x", substr(color, 2,3))),
    g = strtoi(paste0("0x", substr(color, 4,5))),
    b = strtoi(paste0("0x", substr(color, 6,7))),
    colors = color
  )
  return(parsedColor)
}

show3dPolish = function(colors, clusters){
  
  mydata = splitColor(colors)
  mydata$cluster = kmeans(as.matrix(mydata[,1:3]), clusters)$cluster
  open3d()
  with(mydata, plot3d(r, g, b, type="p", col = colors, radius = 10))
  
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

show3dnoClusters = function(colors){
  mydata = splitColor(colors)
  open3d()
  with(mydata, plot3d(r, g, b, type="p", col = colors, radius = 40))
}

######################################################################
######################################################################
setwd("/Users/nancyorgan/Documents/Nail-Polish/")
revlonData = read.csv("revlonData.csv")
maybellineData = read.csv("maybellineData.csv")
narsData = read.csv("narsData.csv")
essieData = read.csv("essiedata.csv")
essieData = subset(essieData, select = c("palette", "name"))
names(essieData) = c("colors", "names")
diorData = read.csv("diorData.csv")
diorData = diorData %>% select(colors, names)
uoData = read.csv("uoData.csv")
uoData = uoData %>% select(colors, names)
ZOYAdata = read.csv("ZOYAdata.csv")

total = list(revlonData, maybellineData, narsData, essieData, diorData, uoData, ZOYAdata)
total = rbindlist(total)
total_coords = splitColor(total$colors)
total = total %>% left_join(total_coords, by = "colors")
write.csv(total, "total.csv", row.names = FALSE)
# lapply(total, function(x){names(x)})

####################################################################
####################################################################
setwd("/Users/nancyorgan/Documents/Nail-Polish/")
total = read.csv("total.csv")

show3dPolish(as.character(total$colors[!is.na(total$colors)]), 10)
show3dnoClusters(as.character(total$colors[!is.na(total$colors)]))
show3dPolish(revlonData$colors, 7)
show3dPolish(maybellineData$colors, 2)
show3dPolish(narsData$colors, 3)
show3dPolish(essieData$colors[!is.na(essieData$colors)], 20)
show3dPolish(diorData$colors, 3)
show3dPolish(uoData$colors, 3)
show3dPolish(ZOYAdata$colors[!is.na(ZOYAdata$colors)], 1)


