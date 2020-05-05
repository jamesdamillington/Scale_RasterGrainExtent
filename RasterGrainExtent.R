
######
###### Load packages
library(raster)
library(rasterVis)
library(ggplot2)


######
###### Functions

##create palette for plots
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

##create labels for plots
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

##plot raster using palette and labels 
raster_plot <- function(ras){
  
  ras_plot <- rasterVis::gplot(ras) + 
    geom_tile(aes(fill = as.factor(value))) +
    scale_fill_manual(values = lc_pal_function(ras), labels=lc_label_function(ras)) +
    labs(fill="Land Cover", x="", y="") 
  
  return(ras_plot)
}


##crop raster to specified extent (top right)
raster_deextent <- function(ras, f){
  
  #extent object for top right of input ras, scaled by f
  e <- extent(
    xmin(ras)+((xmax(ras)-xmin(ras))/f), 
    xmax(ras), 
    ymin(ras)+((ymax(ras)-ymin(ras))/f), 
    ymax(ras)
  )
  
  #crop, trim output
  output <- raster::crop(ras, e)
  output <- raster::trim(output)
  
  return(output)
}

##summarise raster (for chapter figures)
raster_summary <- function(ras){
  
  rxres <- xres(ras)
  rncell <- ncell(ras)
  
  print(paste0("Grain (m): ",rxres))  
  print(paste0("Extent (sq km): ",(rxres*rxres*rncell)/1000000))
  print(paste0("Cells (count): ",rncell))
  print(paste0("Land Covers (count): ",length(unique(ras)))) #number of land covers
  
}

##degrade resolution of a raster by given factor (f)
raster_degrade <- function(ras, f){
  
  output <- aggregate(ras, fact=f, fun=modal, expand=TRUE)
  output <- focal(output,  w = matrix(1, nrow = 3, ncol = 3), fun = modal)
  output <-raster::trim(output)
  return(output)
  
}


######
###### Load data

#extract from 7zip if needed
#library(devtools)
#devtools::install_github("jimhester/archive")
#library(archive)
#archive_extract(archive("covtyp_rc.7z"),dir=".","covtyp_rc.asc")

#original data (10m grain, 20 sq km extent)
LandCov <- raster("covtyp_rc.asc")
raster_summary(LandCov)

LC_plot <- raster_plot(LandCov)
print(LC_plot)

#ggsave("LC.png", LC_plot) 
#ggsave("LC.pdf", LC_plot) 


######
###### Varying Grain

##### 20-40
LandCov20 <- raster_degrade(LandCov, f=2)
LandCov40 <- raster_degrade(LandCov, f=4)

raster_summary(LandCov20)
raster_summary(LandCov40)

LC_plot20 <- raster_plot(LandCov20)
LC_plot40 <- raster_plot(LandCov40)

print(LC_plot20)
print(LC_plot40)

#ggsave("LC40.png", LC_plot40) 
#ggsave("LC40.pdf", LC_plot40)  


##### 30-90 (use this in final version?)
LandCov30 <- raster_degrade(LandCov, f=3)
LandCov90 <- raster_degrade(LandCov, f=9)

raster_summary(LandCov30)
raster_summary(LandCov90)

LC_plot30 <- raster_plot(LandCov30)
LC_plot90 <- raster_plot(LandCov90)

print(LC_plot30)
print(LC_plot90)

#ggsave("LC90.png", LC_plot90) 
#ggsave("LC90.pdf", LC_plot90)  


######
###### Varying Extent

##40-20-10
raster_summary(LandCov40)

LC20_quart <- raster_deextent(LandCov20, 2)
raster_summary(LC20_quart)

#for plotting extend to full extent of original data
LC20_quart_ext <- extend(LC20_quart, LandCov)
LC20_quart_plot <- raster_plot(LC20_quart_ext)
print(LC20_quart_plot)

#ggsave("LC20_quart_ext.png", LC20_quart_plot)  
#ggsave("LC20_quart_ext.pdf", LC20_quart_plot) 

LC10_quart <- raster_deextent(LandCov, 1.333)
raster_summary(LC10_quart)

#for plotting extend to full extent of original data
LC10_quart_ext <- extend(LC10_quart, LandCov)
LC10_quart_plot <- raster_plot(LC10_quart_ext)
print(LC10_quart_plot)

#ggsave("LC10_quart_ext.png", LC10_quart_plot)  
#ggsave("LC10_quart_ext.pdf", LC10_quart_plot) 


##90-30-10 (use this in final version?)
raster_summary(LandCov90)

LC30_third <- raster_deextent(LandCov30, 1.47)
raster_summary(LC30_third)

#for plotting extend to full extent of original data
LC30_third_ext <- extend(LC30_third, LandCov)
LC30_third_plot <- raster_plot(LC30_third_ext)
print(LC30_third_plot)

#ggsave("LC20_quart_ext.png", LC20_quart_plot)  
#ggsave("LC20_quart_ext.pdf", LC20_quart_plot) 

LC10_ninth <- raster_deextent(LandCov, 1.12)
raster_summary(LC10_ninth)

#for plotting extend to full extent of original data
LC10_ninth_ext <- extend(LC10_ninth, LandCov)
LC10_ninth_plot <- raster_plot(LC10_ninth_ext)
print(LC10_ninth_plot)

#ggsave("LC10_quart_ext.png", LC10_quart_plot)  
#ggsave("LC10_quart_ext.pdf", LC10_quart_plot) 
