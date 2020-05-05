
######
###### Load packages
library(raster)
library(rasterVis)
library(viridis)
library(ggplot2)


######
###### Functions

lc_pal_function <- function(map){
  
  pal <- c()
  vals <- getValues(map)

  if(is.element(1,vals)) pal <- c(pal, 'wheat1')
  if(is.element(2,vals)) pal <- c(pal, 'orange2')
  if(is.element(3,vals)) pal <- c(pal, 'darkolivegreen')
  if(is.element(4,vals)) pal <- c(pal, 'gray')
  if(is.element(5,vals)) pal <- c(pal, 'wheat2')
  if(is.element(6,vals)) pal <- c(pal, 'forestgreen')
  if(any(is.na(vals))) pal <- c(pal, 'white')

  return(pal)
}

lc_label_function <- function(map){
  
  pal <- c()
  vals <- getValues(map)

  if(is.element(1,vals)) pal <- c(pal, "Aspen")
  if(is.element(2,vals)) pal <- c(pal, "Lowland Conifer")
  if(is.element(3,vals)) pal <- c(pal, "Other Decid")
  if(is.element(4,vals)) pal <- c(pal, "Open")
  if(is.element(5,vals)) pal <- c(pal, "Other EG")
  if(is.element(6,vals)) pal <- c(pal, "Hardwood")
  if(any(is.na(vals))) pal <- c(pal, "NA")

  return(pal)
}


extent_TR <- function(ras){
  
  e <- extent(
    xmin(ras)+((xmax(ras)-xmin(ras))/2), 
    xmax(ras), 
    ymin(ras)+((ymax(ras)-ymin(ras))/2), 
    ymax(ras)
  )
  
  return(e)
}

raster_plot <- function(ras){
 
  ras_plot <- rasterVis::gplot(ras) + 
    geom_tile(aes(fill = as.factor(value))) +
    scale_fill_manual(values = lc_pal_function(ras), labels=lc_label_function(ras)) +
    labs(fill="Land Cover", x="", y="") 
  
  return(ras_plot)
}


raster_summary <- function(ras){
  
  rxres <- xres(ras)
  rncell <- ncell(ras)
  
  print(paste0("Grain (m): ",rxres))  
  print(paste0("Extent (sq km): ",(rxres*rxres*rncell)/1000000))
  print(paste0("Cells (count): ",rncell))
  print(paste0("Land Covers (count): ",length(unique(LandCov)))) #number of land covers
  
}

#create plotting function
#
#create scaling function (to produce two maps from original)
#
#create similar extent function

######
###### Load data

#extract from 7zip if needed
#library(devtools)
#devtools::install_github("jimhester/archive")
#library(archive)
#archive_extract(archive("covtyp_rc.7z"),dir=".","covtyp_rc.asc")

#first load a raster map in which the value of each pixel is the distance to the nearest lowland conifer pixel
LandCov <- raster("covtyp_rc.asc")             #simply provide file name
crs(LandCov) = "+init=epsg:3160" 


raster_summary(LandCov)

LC_plot <- raster_plot(LandCov)
print(LC_plot)

#ggsave("LC.png", LC_plot) 
#ggsave("LC.pdf", LC_plot) 


##### 20-40
LandCov20 <- aggregate(LandCov, fact=2, fun=modal, expand=TRUE)
LandCov20 <- focal(LandCov20,  w = matrix(1, nrow = 3, ncol = 3), fun = modal)

area(LandCov20)


LandCov40 <- aggregate(LandCov20, fact=2, fun=modal, expand=TRUE)
LandCov40 <- focal(LandCov40,  w = matrix(1, nrow = 3, ncol = 3), fun = modal)
LandCov40 <- raster::trim(LandCov40)

LC40_plot <- gplot(LandCov40) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LandCov40), labels=lc_label_function(LandCov40)) +
  labs(fill="Land Cover", x="", y="")

print(LC40_plot)
ggsave("LC40.png", LC40_plot) 
ggsave("LC40.pdf", LC40_plot)  
ncell(LandCov40)

##### 50-250
LandCov50 <- aggregate(LandCov, fact=5, fun=modal, expand=TRUE)
LandCov50 <- focal(LandCov50,  w = matrix(1, nrow = 3, ncol = 3), fun = modal)
LandCov50 <- raster::trim(LandCov50)

area(LandCov50)
LC_plot50 <- gplot(LandCov50) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LandCov), labels=lc_label_function(LandCov)) +
  labs(fill="Land Cover", x="", y="")

print(LC_plot50)

LandCov250 <- aggregate(LandCov50, fact=5, fun=modal, expand=TRUE)
LandCov250 <- focal(LandCov250,  w = matrix(1, nrow = 3, ncol = 3), fun = modal)

area(LandCov250)
LC_plot250 <- gplot(LandCov250) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LandCov), labels=lc_label_function(LandCov)) +
  labs(fill="Land Cover", x="", y="")

print(LC_plot250)


##### 40-160
LandCov40a <- aggregate(LandCov, fact=4, fun=modal, expand=TRUE)
LandCov40a <- focal(LandCov40a,  w = matrix(1, nrow = 3, ncol = 3), fun = modal)
LandCov40a <- raster::trim(LandCov40a)

area(LandCov40a)
LC_plot40a <- gplot(LandCov40a) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LandCov), labels=lc_label_function(LandCov)) +
  labs(fill="Land Cover", x="", y="")

print(LC_plot40a)

LandCov160 <- aggregate(LandCov40a, fact=4, fun=modal, expand=TRUE)
LandCov160 <- focal(LandCov160,  w = matrix(1, nrow = 3, ncol = 3), fun = modal)
LandCov160 <- raster::trim(LandCov160)

area(LandCov160)
LC_plot160 <- gplot(LandCov160) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LandCov), labels=lc_label_function(LandCov)) +
  labs(fill="Land Cover", x="", y="")

print(LC_plot160)



##### 30-90
LandCov30 <- aggregate(LandCov, fact=3, fun=modal, expand=TRUE)
LandCov30 <- focal(LandCov30,  w = matrix(1, nrow = 3, ncol = 3), fun = modal)
LandCov30 <- raster::trim(LandCov30)

area(LandCov30)
LC_plot30 <- gplot(LandCov30) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LandCov), labels=lc_label_function(LandCov)) +
  labs(fill="Land Cover", x="", y="")

print(LC_plot30)

LandCov90 <- aggregate(LandCov30, fact=3, fun=modal, expand=TRUE)
LandCov90 <- focal(LandCov90,  w = matrix(1, nrow = 3, ncol = 3), fun = modal)
LandCov90 <- raster::trim(LandCov90)

area(LandCov90)
LC_plot90 <- gplot(LandCov90) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LandCov), labels=lc_label_function(LandCov)) +
  labs(fill="Land Cover", x="", y="")

print(LC_plot90)


# LandCov40 <- myRatify(LandCov40)
# png("LC40.png", width=3600, height=3600, res=300)
# levelplot(LandCov40, att = 'landcover', col.regions=lc_pal_function(LandCov40))  #something weired with order of the colours vs levels
# dev.off()

LC20_ext <- extent_TR(LandCov20) 
LandCov20_sub <- raster::crop(LandCov20, LC20_ext)
LandCov20_sub <- raster::trim(LandCov20_sub)

LC20_plot <- gplot(LandCov20_sub) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LandCov20_sub), labels=lc_label_function(LandCov20_sub)) +
  labs(fill="Land Cover", x="", y="")

print(LC20_plot)
ggsave("LC20.png", LC20_plot)  
ggsave("LC20.pdf", LC20_plot) 
ncell(LandCov20_sub)

# LandCov20_sub <- myRatify(LandCov20_sub)
# png("LC20.png", width=3600, height=3600, res=300)
# levelplot(LandCov20_sub, att = 'landcover', col.regions=lc_pal_function(LandCov20_sub))  #something weired with order of the colours vs levels
# dev.off()

LC10_ext <- extent_TR(LandCov20_sub) 
LandCov10_sub <- raster::crop(LandCov, LC10_ext)
LandCov10_sub <- raster::trim(LandCov10_sub)
# LandCov_sub <- myRatify(LandCov_sub)
# png("LC10.png", width=3600, height=3600, res=300)
# levelplot(LandCov_sub, att = 'landcover', col.regions=lc_pal_function(LandCov_sub))  #something weired with order of the colours vs levels
# dev.off()


LC10_plot <- gplot(LandCov10_sub) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LandCov10_sub), labels=lc_label_function(LandCov10_sub)) +
  labs(fill="Land Cover", x="", y="")

print(LC10_plot)
ncell(LandCov10_sub)

ggsave("LC10.png", LC10_plot)  
ggsave("LC10.pdf", LC10_plot)


LC10_extend <- extend(LandCov10_sub, LandCov)
LC20_extend <- extend(LandCov20_sub, LandCov)


LC10_extend_plot <- gplot(LC10_extend) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LC10_extend), labels=lc_label_function(LC10_extend)) +
  labs(fill="Land Cover", x="", y="")

plot(LC10_extend_plot)
ggsave("LC10_extend.png", LC10_extend_plot)  
ggsave("LC10_extend.pdf", LC10_extend_plot)

LC20_extend_plot <- gplot(LC20_extend) + 
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = lc_pal_function(LC20_extend), labels=lc_label_function(LC20_extend)) +
  labs(fill="Land Cover", x="", y="")

plot(LC20_extend_plot)
ggsave("LC20_extend.png", LC20_extend_plot)  
ggsave("LC20_extend.pdf", LC20_extend_plot)

plot(LandCov40)
