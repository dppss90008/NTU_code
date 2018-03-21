dat <- read.csv("Earthquake_data.csv")

# February earthquake data imaging

# Using ggmap to visualize earthquake occurence
library(ggmap)
lon <- sapply((strsplit(as.character(dat$Longitude), ",")), as.numeric)
lat <- sapply((strsplit(as.character(dat$Latitude), ",")), as.numeric)
map <- get_map(location = 'Taiwan', zoom = 7, maptype = "terrain")
P1 <- ggmap(map, darken = c(0.5, "white")) + geom_point(aes(x = lon, y = lat),size=2,
                                                  color = 'dark blue',data = dat)
plot(P1)

# Histogram of earthquake magnitude

P2 <- ggplot(data = dat, aes(x = dat$magnitude)) +
  geom_bar(fill = "lightblue", colour = "black")
P2 + xlab("magnitude")
plot(P2)

# Histogram of earthquake depth

P3 <- ggplot(data = dat, aes(x = dat$depth)) +
  geom_bar(fill = "lightblue", colour = "black")
P3 + xlab("depth")
plot(P3)

library(ggplot2)
P4 <- qplot(data= dat, x=dat$magnitude, y=dat$depth, xlab='magnitude',ylab='depth')
plot(P4)

