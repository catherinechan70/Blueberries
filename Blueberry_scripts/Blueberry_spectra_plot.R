### SCRIPTS TO PLOT SAMPLE SPECTRA 
### to easily plot portions of spectrum and spectra of interest 
library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(readxl)
library(reshape2)

setwd("E:/")

## Read in headwall wavelength bandpasses
Headwall_wv<-scan("Blueberries/Headwall_wv",numeric())


# 06072019

setwd("E:/Blueberries/ImageExtractions/06072019/") # Set path for 06072019 samples
mypath_bb1 = "/Blueberries/ImageExtractions/06072019/"
# Import names of blueberry 1 sample tif files into character list
bb1_samples = list.files(mypath_bb1, pattern="\\.tif$")

bb1_raster_list<-lapply(bb1_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",bb1_samples))  # Substitutes file name and removes .tif from the name


#Add column Tree_numbe to each element in dataframe (for now, removed B11 samples)
bb1_raster_list <- Map(cbind, bb1_raster_list, SN = names(bb1_raster_list))


##Renames columns to headwall bandpasses
bb1_raster_list <- lapply(bb1_raster_list, function(bb1_df) {
  names(bb1_df)[3:328] <- Headwall_wv
  bb1_df
})

# 07032019

setwd("E:/Blueberries/ImageExtractions/07032019/") # Set path for 06072019 samples
mypath_bb2 = "/Blueberries/ImageExtractions/07032019/"
# Import names of blueberry 1 sample tif files into character list
bb2_samples = list.files(mypath_bb2, pattern="\\.tif$")

bb2_raster_list<-lapply(bb2_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",bb2_samples))  # Substitutes file name and removes .tif from the name


#Add column Tree_numbe to each element in dataframe
bb2_raster_list <- Map(cbind, bb2_raster_list, SN = names(bb2_raster_list))


#Renames columns to headwall bandpasses
bb2_raster_list <- lapply(bb2_raster_list, function(bb2_df) {
  names(bb2_df)[3:328] <- Headwall_wv
  bb2_df
})

# 07252019

setwd("E:/Blueberries/ImageExtractions/07252019/") # Set path for 06072019 samples
mypath_bb3 = "/Blueberries/ImageExtractions/07252019/"
# Import names of blueberry 1 sample tif files into character list
bb3_samples = list.files(mypath_bb3, pattern="\\.tif$")

bb3_raster_list<-lapply(bb3_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",bb3_samples))  # Substitutes file name and removes .tif from the name


#Add column Tree_numbe to each element in dataframe (for now, removed B11 samples)
bb3_raster_list <- Map(cbind, bb3_raster_list, SN = names(bb3_raster_list))


#Renames columns to headwall bandpasses
bb3_raster_list <- lapply(bb3_raster_list, function(bb3_df) {
  names(bb3_df)[3:328] <- Headwall_wv
  bb3_df
})







#Samples A4, A12, A13, A19 & B6, B9, B12, B14

#BB1 A4

bb1_A4 <- bb1_raster_list$A4[-c(1,2,329)]

bb1_A4 <- rbind(colMeans = colMeans(bb1_A4, na.rm = TRUE),
                bb1_A4) 

bb1_A4 <- rbind(colnames(bb1_A4),
                bb1_A4)

bb1_A4 <- bb1_A4[-c(3:nrow(bb1_A4)), ]

bb1_A4_long <- as.data.frame(t(bb1_A4))

names(bb1_A4_long)[names(bb1_A4_long) == "1"] <- "Wavelength"
names(bb1_A4_long)[names(bb1_A4_long) == "2"] <- "Reflectance"

bb1_A4_long[ ,2] <- as.numeric(as.character( bb1_A4_long[ ,2]))

bb1_A4_long$Wavelength = as.numeric(as.character(bb1_A4_long$Wavelength))

#BB1 A12

bb1_A12 <- bb1_raster_list$A12[-c(1,2,329)]

bb1_A12 <- rbind(colMeans = colMeans(bb1_A12, na.rm = TRUE),
                bb1_A12) 

bb1_A12 <- rbind(colnames(bb1_A12),
                bb1_A12)

bb1_A12 <- bb1_A12[-c(3:nrow(bb1_A12)), ]

bb1_A12_long <- as.data.frame(t(bb1_A12))

names(bb1_A12_long)[names(bb1_A12_long) == "1"] <- "Wavelength"
names(bb1_A12_long)[names(bb1_A12_long) == "2"] <- "Reflectance"

bb1_A12_long[ ,2] <- as.numeric(as.character( bb1_A12_long[ ,2]))

bb1_A12_long$Wavelength = as.numeric(as.character(bb1_A12_long$Wavelength))




#BB1 A13

bb1_A13 <- bb1_raster_list$A13[-c(1,2,329)]

bb1_A13 <- rbind(colMeans = colMeans(bb1_A13, na.rm = TRUE),
                 bb1_A13) 

bb1_A13 <- rbind(colnames(bb1_A13),
                 bb1_A13)

bb1_A13 <- bb1_A13[-c(3:nrow(bb1_A13)), ]

bb1_A13_long <- as.data.frame(t(bb1_A13))

names(bb1_A13_long)[names(bb1_A13_long) == "1"] <- "Wavelength"
names(bb1_A13_long)[names(bb1_A13_long) == "2"] <- "Reflectance"

bb1_A13_long[ ,2] <- as.numeric(as.character( bb1_A13_long[ ,2]))

bb1_A13_long$Wavelength = as.numeric(as.character(bb1_A13_long$Wavelength))


#BB1 A19

bb1_A19 <- bb1_raster_list$A19[-c(1,2,329)]

bb1_A19 <- rbind(colMeans = colMeans(bb1_A19, na.rm = TRUE),
                 bb1_A19) 

bb1_A19 <- rbind(colnames(bb1_A19),
                 bb1_A19)

bb1_A19 <- bb1_A19[-c(3:nrow(bb1_A19)), ]

bb1_A19_long <- as.data.frame(t(bb1_A19))

names(bb1_A19_long)[names(bb1_A19_long) == "1"] <- "Wavelength"
names(bb1_A19_long)[names(bb1_A19_long) == "2"] <- "Reflectance"

bb1_A19_long[ ,2] <- as.numeric(as.character( bb1_A19_long[ ,2]))

bb1_A19_long$Wavelength = as.numeric(as.character(bb1_A19_long$Wavelength))


#BB1 B6

bb1_B6 <- bb1_raster_list$B6[-c(1,2,329)]

bb1_B6 <- rbind(colMeans = colMeans(bb1_B6, na.rm = TRUE),
                 bb1_B6) 

bb1_B6 <- rbind(colnames(bb1_B6),
                 bb1_B6)

bb1_B6 <- bb1_B6[-c(3:nrow(bb1_B6)), ]

bb1_B6_long <- as.data.frame(t(bb1_B6))

names(bb1_B6_long)[names(bb1_B6_long) == "1"] <- "Wavelength"
names(bb1_B6_long)[names(bb1_B6_long) == "2"] <- "Reflectance"

bb1_B6_long[ ,2] <- as.numeric(as.character( bb1_B6_long[ ,2]))

bb1_B6_long$Wavelength = as.numeric(as.character(bb1_B6_long$Wavelength))


#BB1 B9

bb1_B9 <- bb1_raster_list$B9[-c(1,2,329)]

bb1_B9 <- rbind(colMeans = colMeans(bb1_B9, na.rm = TRUE),
                bb1_B9) 

bb1_B9 <- rbind(colnames(bb1_B9),
                bb1_B9)

bb1_B9 <- bb1_B9[-c(3:nrow(bb1_B9)), ]

bb1_B9_long <- as.data.frame(t(bb1_B9))

names(bb1_B9_long)[names(bb1_B9_long) == "1"] <- "Wavelength"
names(bb1_B9_long)[names(bb1_B9_long) == "2"] <- "Reflectance"

bb1_B9_long[ ,2] <- as.numeric(as.character( bb1_B9_long[ ,2]))

bb1_B9_long$Wavelength = as.numeric(as.character(bb1_B9_long$Wavelength))


#BB1 B12

bb1_B12 <- bb1_raster_list$B12[-c(1,2,329)]

bb1_B12 <- rbind(colMeans = colMeans(bb1_B12, na.rm = TRUE),
                bb1_B12) 

bb1_B12 <- rbind(colnames(bb1_B12),
                bb1_B12)

bb1_B12 <- bb1_B12[-c(3:nrow(bb1_B12)), ]

bb1_B12_long <- as.data.frame(t(bb1_B12))

names(bb1_B12_long)[names(bb1_B12_long) == "1"] <- "Wavelength"
names(bb1_B12_long)[names(bb1_B12_long) == "2"] <- "Reflectance"

bb1_B12_long[ ,2] <- as.numeric(as.character( bb1_B12_long[ ,2]))

bb1_B12_long$Wavelength = as.numeric(as.character(bb1_B12_long$Wavelength))


#BB1 B14

bb1_B14 <- bb1_raster_list$B14[-c(1,2,329)]

bb1_B14 <- rbind(colMeans = colMeans(bb1_B14, na.rm = TRUE),
                 bb1_B14) 

bb1_B14 <- rbind(colnames(bb1_B14),
                 bb1_B14)

bb1_B14 <- bb1_B14[-c(3:nrow(bb1_B14)), ]

bb1_B14_long <- as.data.frame(t(bb1_B14))

names(bb1_B14_long)[names(bb1_B14_long) == "1"] <- "Wavelength"
names(bb1_B14_long)[names(bb1_B14_long) == "2"] <- "Reflectance"

bb1_B14_long[ ,2] <- as.numeric(as.character( bb1_B14_long[ ,2]))

bb1_B14_long$Wavelength = as.numeric(as.character(bb1_B14_long$Wavelength))



#BB2 A4
bb2_A4 <- bb2_raster_list$A4[-c(1,2,329)]

bb2_A4 <- rbind(colMeans = colMeans(bb2_A4, na.rm = TRUE),
                bb2_A4) 

bb2_A4 <- rbind(colnames(bb2_A4),
                bb2_A4)

bb2_A4 <- bb2_A4[-c(3:nrow(bb2_A4)), ]

bb2_A4_long <- as.data.frame(t(bb2_A4))

names(bb2_A4_long)[names(bb2_A4_long) == "1"] <- "Wavelength"
names(bb2_A4_long)[names(bb2_A4_long) == "2"] <- "Reflectance"

bb2_A4_long[ ,2] <- as.numeric(as.character( bb2_A4_long[ ,2]))

bb2_A4_long$Wavelength = as.numeric(as.character(bb2_A4_long$Wavelength))


#BB2 A12
bb2_A12 <- bb2_raster_list$A12[-c(1,2,329)]

bb2_A12 <- rbind(colMeans = colMeans(bb2_A12, na.rm = TRUE),
                bb2_A12) 

bb2_A12 <- rbind(colnames(bb2_A12),
                bb2_A12)

bb2_A12 <- bb2_A12[-c(3:nrow(bb2_A12)), ]

bb2_A12_long <- as.data.frame(t(bb2_A12))

names(bb2_A12_long)[names(bb2_A12_long) == "1"] <- "Wavelength"
names(bb2_A12_long)[names(bb2_A12_long) == "2"] <- "Reflectance"

bb2_A12_long[ ,2] <- as.numeric(as.character( bb2_A12_long[ ,2]))

bb2_A12_long$Wavelength = as.numeric(as.character(bb2_A12_long$Wavelength))


#BB2 A13
bb2_A13 <- bb2_raster_list$A13[-c(1,2,329)]

bb2_A13 <- rbind(colMeans = colMeans(bb2_A13, na.rm = TRUE),
                 bb2_A13) 

bb2_A13 <- rbind(colnames(bb2_A13),
                 bb2_A13)

bb2_A13 <- bb2_A13[-c(3:nrow(bb2_A13)), ]

bb2_A13_long <- as.data.frame(t(bb2_A13))

names(bb2_A13_long)[names(bb2_A13_long) == "1"] <- "Wavelength"
names(bb2_A13_long)[names(bb2_A13_long) == "2"] <- "Reflectance"

bb2_A13_long[ ,2] <- as.numeric(as.character( bb2_A13_long[ ,2]))

bb2_A13_long$Wavelength = as.numeric(as.character(bb2_A13_long$Wavelength))


#BB2 A19
bb2_A19 <- bb2_raster_list$A19[-c(1,2,329)]

bb2_A19 <- rbind(colMeans = colMeans(bb2_A19, na.rm = TRUE),
                 bb2_A19) 

bb2_A19 <- rbind(colnames(bb2_A19),
                 bb2_A19)

bb2_A19 <- bb2_A19[-c(3:nrow(bb2_A19)), ]

bb2_A19_long <- as.data.frame(t(bb2_A19))

names(bb2_A19_long)[names(bb2_A19_long) == "1"] <- "Wavelength"
names(bb2_A19_long)[names(bb2_A19_long) == "2"] <- "Reflectance"

bb2_A19_long[ ,2] <- as.numeric(as.character( bb2_A19_long[ ,2]))

bb2_A19_long$Wavelength = as.numeric(as.character(bb2_A19_long$Wavelength))

#BB2 B6
bb2_B6 <- bb2_raster_list$B6[-c(1,2,329)]

bb2_B6 <- rbind(colMeans = colMeans(bb2_B6, na.rm = TRUE),
                 bb2_B6) 

bb2_B6 <- rbind(colnames(bb2_B6),
                 bb2_B6)

bb2_B6 <- bb2_B6[-c(3:nrow(bb2_B6)), ]

bb2_B6_long <- as.data.frame(t(bb2_B6))

names(bb2_B6_long)[names(bb2_B6_long) == "1"] <- "Wavelength"
names(bb2_B6_long)[names(bb2_B6_long) == "2"] <- "Reflectance"

bb2_B6_long[ ,2] <- as.numeric(as.character( bb2_B6_long[ ,2]))

bb2_B6_long$Wavelength = as.numeric(as.character(bb2_B6_long$Wavelength))


#BB2 B9
bb2_B9 <- bb2_raster_list$B9[-c(1,2,329)]

bb2_B9 <- rbind(colMeans = colMeans(bb2_B9, na.rm = TRUE),
                bb2_B9) 

bb2_B9 <- rbind(colnames(bb2_B9),
                bb2_B9)

bb2_B9 <- bb2_B9[-c(3:nrow(bb2_B9)), ]

bb2_B9_long <- as.data.frame(t(bb2_B9))

names(bb2_B9_long)[names(bb2_B9_long) == "1"] <- "Wavelength"
names(bb2_B9_long)[names(bb2_B9_long) == "2"] <- "Reflectance"

bb2_B9_long[ ,2] <- as.numeric(as.character( bb2_B9_long[ ,2]))

bb2_B9_long$Wavelength = as.numeric(as.character(bb2_B9_long$Wavelength))


#BB2 B12
bb2_B12 <- bb2_raster_list$B12[-c(1,2,329)]

bb2_B12 <- rbind(colMeans = colMeans(bb2_B12, na.rm = TRUE),
                bb2_B12) 

bb2_B12 <- rbind(colnames(bb2_B12),
                bb2_B12)

bb2_B12 <- bb2_B12[-c(3:nrow(bb2_B12)), ]

bb2_B12_long <- as.data.frame(t(bb2_B12))

names(bb2_B12_long)[names(bb2_B12_long) == "1"] <- "Wavelength"
names(bb2_B12_long)[names(bb2_B12_long) == "2"] <- "Reflectance"

bb2_B12_long[ ,2] <- as.numeric(as.character( bb2_B12_long[ ,2]))

bb2_B12_long$Wavelength = as.numeric(as.character(bb2_B12_long$Wavelength))

#BB2 B14
bb2_B14 <- bb2_raster_list$B14[-c(1,2,329)]

bb2_B14 <- rbind(colMeans = colMeans(bb2_B14, na.rm = TRUE),
                 bb2_B14) 

bb2_B14 <- rbind(colnames(bb2_B14),
                 bb2_B14)

bb2_B14 <- bb2_B14[-c(3:nrow(bb2_B14)), ]

bb2_B14_long <- as.data.frame(t(bb2_B14))

names(bb2_B14_long)[names(bb2_B14_long) == "1"] <- "Wavelength"
names(bb2_B14_long)[names(bb2_B14_long) == "2"] <- "Reflectance"

bb2_B14_long[ ,2] <- as.numeric(as.character( bb2_B14_long[ ,2]))

bb2_B14_long$Wavelength = as.numeric(as.character(bb2_B14_long$Wavelength))



#BB3 A4

bb3_A4 <- bb3_raster_list$A4[-c(1,2,329)]

bb3_A4 <- rbind(colMeans = colMeans(bb3_A4, na.rm = TRUE),
                bb3_A4) 

bb3_A4 <- rbind(colnames(bb3_A4),
                bb3_A4)

bb3_A4 <- bb3_A4[-c(3:nrow(bb3_A4)), ]

bb3_A4_long <- as.data.frame(t(bb3_A4))

names(bb3_A4_long)[names(bb3_A4_long) == "1"] <- "Wavelength"
names(bb3_A4_long)[names(bb3_A4_long) == "2"] <- "Reflectance"

bb3_A4_long[ ,2] <- as.numeric(as.character( bb3_A4_long[ ,2]))

bb3_A4_long$Wavelength = as.numeric(as.character(bb3_A4_long$Wavelength))


#BB3 A12

bb3_A12 <- bb3_raster_list$A12[-c(1,2,329)]

bb3_A12 <- rbind(colMeans = colMeans(bb3_A12, na.rm = TRUE),
                bb3_A12) 

bb3_A12 <- rbind(colnames(bb3_A12),
                bb3_A12)

bb3_A12 <- bb3_A12[-c(3:nrow(bb3_A12)), ]

bb3_A12_long <- as.data.frame(t(bb3_A12))

names(bb3_A12_long)[names(bb3_A12_long) == "1"] <- "Wavelength"
names(bb3_A12_long)[names(bb3_A12_long) == "2"] <- "Reflectance"

bb3_A12_long[ ,2] <- as.numeric(as.character( bb3_A12_long[ ,2]))

bb3_A12_long$Wavelength = as.numeric(as.character(bb3_A12_long$Wavelength))



#BB3 A13

bb3_A13 <- bb3_raster_list$A13[-c(1,2,329)]

bb3_A13 <- rbind(colMeans = colMeans(bb3_A13, na.rm = TRUE),
                 bb3_A13) 

bb3_A13 <- rbind(colnames(bb3_A13),
                 bb3_A13)

bb3_A13 <- bb3_A13[-c(3:nrow(bb3_A13)), ]

bb3_A13_long <- as.data.frame(t(bb3_A13))

names(bb3_A13_long)[names(bb3_A13_long) == "1"] <- "Wavelength"
names(bb3_A13_long)[names(bb3_A13_long) == "2"] <- "Reflectance"

bb3_A13_long[ ,2] <- as.numeric(as.character( bb3_A13_long[ ,2]))

bb3_A13_long$Wavelength = as.numeric(as.character(bb3_A13_long$Wavelength))



#BB3 A19

bb3_A19 <- bb3_raster_list$A19[-c(1,2,329)]

bb3_A19 <- rbind(colMeans = colMeans(bb3_A19, na.rm = TRUE),
                 bb3_A19) 

bb3_A19 <- rbind(colnames(bb3_A19),
                 bb3_A19)

bb3_A19 <- bb3_A19[-c(3:nrow(bb3_A19)), ]

bb3_A19_long <- as.data.frame(t(bb3_A19))

names(bb3_A19_long)[names(bb3_A19_long) == "1"] <- "Wavelength"
names(bb3_A19_long)[names(bb3_A19_long) == "2"] <- "Reflectance"

bb3_A19_long[ ,2] <- as.numeric(as.character( bb3_A19_long[ ,2]))

bb3_A19_long$Wavelength = as.numeric(as.character(bb3_A19_long$Wavelength))


#BB3 B6

bb3_B6 <- bb3_raster_list$B6[-c(1,2,329)]

bb3_B6 <- rbind(colMeans = colMeans(bb3_B6, na.rm = TRUE),
                 bb3_B6) 

bb3_B6 <- rbind(colnames(bb3_B6),
                 bb3_B6)

bb3_B6 <- bb3_B6[-c(3:nrow(bb3_B6)), ]

bb3_B6_long <- as.data.frame(t(bb3_B6))

names(bb3_B6_long)[names(bb3_B6_long) == "1"] <- "Wavelength"
names(bb3_B6_long)[names(bb3_B6_long) == "2"] <- "Reflectance"

bb3_B6_long[ ,2] <- as.numeric(as.character( bb3_B6_long[ ,2]))

bb3_B6_long$Wavelength = as.numeric(as.character(bb3_B6_long$Wavelength))


#BB3 B9

bb3_B9 <- bb3_raster_list$B9[-c(1,2,329)]

bb3_B9 <- rbind(colMeans = colMeans(bb3_B9, na.rm = TRUE),
                bb3_B9) 

bb3_B9 <- rbind(colnames(bb3_B9),
                bb3_B9)

bb3_B9 <- bb3_B9[-c(3:nrow(bb3_B9)), ]

bb3_B9_long <- as.data.frame(t(bb3_B9))

names(bb3_B9_long)[names(bb3_B9_long) == "1"] <- "Wavelength"
names(bb3_B9_long)[names(bb3_B9_long) == "2"] <- "Reflectance"

bb3_B9_long[ ,2] <- as.numeric(as.character( bb3_B9_long[ ,2]))

bb3_B9_long$Wavelength = as.numeric(as.character(bb3_B9_long$Wavelength))


#BB3 B12

bb3_B12 <- bb3_raster_list$B12[-c(1,2,329)]

bb3_B12 <- rbind(colMeans = colMeans(bb3_B12, na.rm = TRUE),
                bb3_B12) 

bb3_B12 <- rbind(colnames(bb3_B12),
                bb3_B12)

bb3_B12 <- bb3_B12[-c(3:nrow(bb3_B12)), ]

bb3_B12_long <- as.data.frame(t(bb3_B12))

names(bb3_B12_long)[names(bb3_B12_long) == "1"] <- "Wavelength"
names(bb3_B12_long)[names(bb3_B12_long) == "2"] <- "Reflectance"

bb3_B12_long[ ,2] <- as.numeric(as.character( bb3_B12_long[ ,2]))

bb3_B12_long$Wavelength = as.numeric(as.character(bb3_B12_long$Wavelength))



#BB3 B14

bb3_B14 <- bb3_raster_list$B14[-c(1,2,329)]

bb3_B14 <- rbind(colMeans = colMeans(bb3_B14, na.rm = TRUE),
                 bb3_B14) 

bb3_B14 <- rbind(colnames(bb3_B14),
                 bb3_B14)

bb3_B14 <- bb3_B14[-c(3:nrow(bb3_B14)), ]

bb3_B14_long <- as.data.frame(t(bb3_B14))

names(bb3_B14_long)[names(bb3_B14_long) == "1"] <- "Wavelength"
names(bb3_B14_long)[names(bb3_B14_long) == "2"] <- "Reflectance"

bb3_B14_long[ ,2] <- as.numeric(as.character( bb3_B14_long[ ,2]))

bb3_B14_long$Wavelength = as.numeric(as.character(bb3_B14_long$Wavelength))


#bb3_B4_long$Wavelength <- as.factor(bb3_B4_long$Wavelength)
#bb3_B4_long$Reflectance <- as.factor(bb3_B4_long$Reflectance)



# All plots

all_plotdf <- cbind(bb1_A12_long, bb1_A19_long, bb1_B12_long, bb1_B14_long, bb2_A12_long, bb2_A19_long, 
              bb2_B12_long, bb2_B14_long, bb3_A12_long, bb3_A19_long, bb3_B12_long, bb3_B14_long)

all_plotdf <- all_plotdf[, -c(3,5,7,9, 11,13, 15, 17, 19, 21, 23)]

colnames(all_plotdf) <- c('Wavelength', 'PBI', 'PBI2', 'PBNI', 'PBNI2',
                          'GFI', 'GFI2', 'GFNI', 'GFNI2',
                          'CBI', 'CBI2', 'CBNI', 'CBNI2') 


ggplot(data=all_plotdf) + 
  #geom_line(data = bb1_A4_long, aes(x = Wavelength, y = Reflectance), color = "darksalmon", size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = PBI, color = "Peak Bloom Irrigated"), size=.75) +
  #geom_line(data = bb1_A13_long, aes(x = Wavelength, y = Reflectance), color = "darksalmon", size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = PBI2, color = "NA1"), size=.75) +
  #geom_line(data = bb1_B6_long, aes(x = Wavelength, y = Reflectance), color = "deeppink4", size=.75) +
  #geom_line(data = bb1_B9_long, aes(x = Wavelength, y = Reflectance), color = "deeppink4", size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = PBNI, color = "Peak Bloom Non-Irrigated"), size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = PBNI2, color = "NA2"), size=.75) +
  
  #geom_line(data = bb2_A4_long, aes(x = Wavelength, y = Reflectance), color = "chartreuse1", size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = GFI, color = "Green Fruit Irrigated"), size=.75) +
  #geom_line(data = bb2_A13_long, aes(x = Wavelength, y = Reflectance), color = "chartreuse1", size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = GFI2, color = "NA3"), size=.75) +
  #geom_line(data = bb2_B6_long, aes(x = Wavelength, y = Reflectance), color = "darkgreen", size=.75) +
  #geom_line(data = bb2_B9_long, aes(x = Wavelength, y = Reflectance), color = "darkgreen", size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = GFNI, color = "Green Fruit Non-Irrigated"), size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = GFNI2, color = "NA4"), size=.75) +
  
  #geom_line(data = bb3_A4_long, aes(x = Wavelength, y = Reflectance), color = "cyan", size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = CBI, color = "Color Break Irrigated"), size=.75) +
  #geom_line(data = bb3_A13_long, aes(x = Wavelength, y = Reflectance), color = "cyan", size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = CBI2, color = "NA5"), size=.75) +
  #geom_line(data = bb3_B6_long, aes(x = Wavelength, y = Reflectance), color = "darkblue", size=.75) +
  #geom_line(data = bb3_B9_long, aes(x = Wavelength, y = Reflectance), color = "darkblue", size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = CBNI, color = "Color Break Non-Irrigated"), size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = CBNI2, color = "NA6"), size=.75) +
  
  scale_color_manual(values = c('Peak Bloom Irrigated'="darksalmon", 'NA1' = "darksalmon", 'Peak Bloom Non-Irrigated'="deeppink4", 
                               'NA2'="deeppink4",'Green Fruit Irrigated'="chartreuse1", 'NA3'="chartreuse1", 
                                'Green Fruit Non-Irrigated'="darkgreen", 'NA4'="darkgreen", 'Color Break Irrigated'="cyan", 
                                'NA5'="cyan", 'Color Break Non-Irrigated'="darkblue", 'NA6'="darkblue"), 
                                breaks = c('Peak Bloom Irrigated', 'Peak Bloom Non-Irrigated', 'Green Fruit Irrigated', 
                                           'Green Fruit Non-Irrigated', 'Color Break Irrigated', 'Color Break Non-Irrigated'), 

  # darksalmon - deeppink4
  # chartreuse1 - darkgreen
  # cyan - darkblue
  
  labels = function(x) str_wrap(x, width = 18)) +
  
  theme(legend.position = c(.3, .55),
        legend.justification = c("right", "bottom"))+
  
  labs(color='Stage and Category')+
  
  
  
  xlab('Wavelength nm') +
  ylab('Reflectance %') + 
  xlim(500,800)+
  ylim(0, 0.75) +
  #theme(panel.background= element_rect(fill="snow2"),
        #panel.grid.major = element_line(colour = "gray", size=0.6),
        #panel.grid.minor = element_line(colour = "deepskyblue3",
        #                                size=.03,
        #                                linetype = "solid")) +
  
  ggtitle("Blueberry Spectra 500-800nm")
#wvl 500-800

setwd("E:/Blueberries/ROutputs/Spectra_exports")
ggsave("AllBB_allsamps_500_800_signature.jpeg")


# Plotting Airport versus Baxter

# BB1 / BB2/ BB3

ggplot(data=all_plotdf) + 
  #geom_line(mapping = aes(x = Wavelength, y = PBI, color = "Irrigated"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = PBI2, color = "NA1"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = PBNI, color = "Non-Irrigated"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = PBNI2, color = "NA2"), size=.75) +
  
  #geom_line(mapping = aes(x = Wavelength, y = GFI, color = "Irrigated"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = GFI2, color = "NA3"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = GFNI, color = "Non-Irrigated"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = GFNI2, color = "NA4"), size=.75) +
  
  geom_line(mapping = aes(x = Wavelength, y = CBI, color = "Irrigated"), size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = CBI2, color = "NA5"), size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = CBNI, color = "Non-Irrigated"), size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = CBNI2, color = "NA6"), size=.75) +
  
  scale_color_manual(values = c('Irrigated'="cyan", 'NA5' = "cyan", 'Non-Irrigated'="darkblue", 
                                'NA6'="darkblue"), 
                     breaks = c('Irrigated', 'Non-Irrigated'),
                     labels = function(x) str_wrap(x, width = 5)) +
 
   theme(legend.position = c(.35, .75),
         legend.justification = c("right", "bottom"))+
 
  labs(color='Stage and Category')+
  
  xlab('Wavelength nm') +
  ylab('Reflectance %') + 
  xlim(500,800)+
  ylim(0, 0.75) +
 
  ggtitle("Color Break Spectra")
#wvl 500-800

setwd("E:/Blueberries/ROutputs/Spectra_exports")
ggsave("CB_500_800_signature.jpeg")



# Sample (temporal representation)

ggplot(data=all_plotdf) + 
  geom_line(mapping = aes(x = Wavelength, y = PBI, color = "Peak Bloom"), size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = PBI2, color = "NA1"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = PBNI, color = "Peak Bloom"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = PBNI2, color = "NA2"), size=.75) +
  
  geom_line(mapping = aes(x = Wavelength, y = GFI, color = "Green Fruit"), size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = GFI2, color = "NA3"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = GFNI, color = "Green Fruit"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = GFNI2, color = "NA4"), size=.75) +
  
  geom_line(mapping = aes(x = Wavelength, y = CBI, color = "Color Break"), size=.75) +
  geom_line(mapping = aes(x = Wavelength, y = CBI2, color = "NA5"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = CBNI, color = "Color Break"), size=.75) +
  #geom_line(mapping = aes(x = Wavelength, y = CBNI2, color = "NA6"), size=.75) +
  
  scale_color_manual(values = c('Peak Bloom'="darksalmon", 'NA1' = "darksalmon", 
                                'Green Fruit'="chartreuse1", 'NA3'="chartreuse1", 
                                 'Color Break'="cyan", 'NA5'="cyan"), 
                     breaks = c('Peak Bloom', 'Green Fruit', 'Color Break'), 
                    labels = function(x) str_wrap(x, width = 30)) +
  
  theme(legend.position = c(.25, .7),
        legend.justification = c("right", "bottom"))+
  
  labs(color='Stage')+
  # darksalmon - deeppink4
  # chartreuse1 - darkgreen
  # cyan - darkblue
  
  
  xlab('Wavelength nm') +
  ylab('Reflectance %') + 
  xlim(500,800)+
  ylim(0, 0.75) +

  ggtitle("Irrigated Spectra")
#wvl 500-800

setwd("E:/Blueberries/ROutputs/Spectra_exports")
ggsave("Irrigated_500_800_signature.jpeg")




scale_color_manual(values = c('Peak Bloom Irrigated'="darksalmon", 'NA1' = "darksalmon", 'Peak Bloom Non-Irrigated'="deeppink4", 
                              'NA2'="deeppink4",'Green Fruit Irrigated'="chartreuse1", 'NA3'="chartreuse1", 
                              'Green Fruit Non-Irrigated'="darkgreen", 'NA4'="darkgreen", 'Color Break Irrigated'="cyan", 
                              'NA5'="cyan", 'Color Break Non-Irrigated'="darkblue", 'NA6'="darkblue"), 
                   breaks = c('Peak Bloom Irrigated', 'Peak Bloom Non-Irrigated', 'Green Fruit Irrigated', 
                              'Green Fruit Non-Irrigated', 'Color Break Irrigated', 'Color Break Non-Irrigated')) +
