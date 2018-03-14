# Earthquake quake data crawler

# install.packages("xml2")
# install.packages("rvest")
library(xml2)
library(rvest)
# Crawling earthquake data from cwb date set : 201802
txt=read_html("https://scweb.cwb.gov.tw/GraphicContent.aspx?ItemId=20&Date=201802&t=23d140a6-96f1-420f-b57f-6eee751cd48b")
txt=html_nodes(txt,"#ContentPlaceHolder1_ctl00_gvEarthquake a , #ContentPlaceHolder1_ctl00_gvEarthquake th")  
txt=html_text(txt)   
txt=iconv(txt,"UTF-8")

# data processing : clean the data and store it into dataframe type
name <- txt[1:7]
name[3] <- 'Lon'
name[4] <- 'Lat'
earthquake_data <- matrix(txt[8:length(txt)],(nrow=length(txt)-7)/7,ncol=7,byrow= TRUE)
earthquake_data <- data.frame(earthquake_data)
names(earthquake_data) <- name
View(earthquake_data)

# data image : Use google map to image earthquake data
library(ggmap)
lon <- sapply((strsplit(as.character(earthquake_data$Lon), ",")), as.numeric)
lat <- sapply((strsplit(as.character(earthquake_data$Lat), ",")), as.numeric)
map <- get_map(location = 'Taiwan', zoom = 7, maptype = "terrain")
ggmap(map, darken = c(0.5, "white")) + geom_point(aes(x = lon, y = lat),size=2,color = 'dark blue',data = earthquake_data)


