# MASKING OUT UNWANTED CATEGORIES - BLUEBERRIES
library(RStoolbox)
library(raster)
library(rgdal)
library(sf)

library(tidyverse)
library(sp)
library(raster)
library(readxl)

Headwall_wv<-scan("/Blueberries/Headwall_wv",numeric())

# Call in classified/prediction tif layer that acts as mask 
class_mask <- brick("E:/Forests/ROutputs/MaskedImages/atkinson_12915_tree_maskAtkinson_12915_cat_mask.tif/Atkinson_12915_cat_mask.tif_PredLayer.tif")

# Call in image to be masked (hyperspectral data cube)
image <- brick("/Forests/ROutputs/MaskedImages/Atkinson_12915_cat_mask.tif")

#"/Forests/Data_Sub_Images/Atkinson_15366_Sub"

#Remove/mask out tree values so that only the ones we don't want are out of the image:
class_mask[class_mask >1 ] <- NA


masked <- raster::mask(image, class_mask, filename = "/Forests/ROutputs/MaskedImages/Atkinson_12915_ash_mask.tif")


