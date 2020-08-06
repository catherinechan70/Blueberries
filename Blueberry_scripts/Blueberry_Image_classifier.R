# ----------------------------------- Build a model on Spectral Library (Headwall) ------------------------------------
# List of packages to install
library(spectrolab)
library(tidyverse)
library(raster)
library(SpaDES)
library(doParallel)
library(parallel)
library(hsdar)
library(caret)
library(randomForest)
library(ranger)
library(tools)


# --------------------------------------------- Building Models ------------------------------------------------------
# Source the function that will calculate derivatives of our new spectral library
# The function will do the same for a datacube
source("/Blueberries/Blueberry_scripts/Blueberry_LandCoverEstimator.R")

source("/Blueberries/Blueberry_scripts/Blueberry_HyperspecGenFunctionRanger.R")


# Calculate Derivative for spectral libaray
Spectral_Library_hyperspec<-HyperSpec_DerivGeneratorRanger(filename="/Blueberries/ROutputs/allbb_full_Df.csv", 
                                                           out_file="/Blueberries/ROutputs/allbb_full_Df")

Spectral_Library<-LandCoverEstimator(filename= "/Blueberries/ROutputs/bb1_Df.csv", 
                                          out_file= "/Blueberries/ROutputs/bb1", 
                                          datatype = "csv", 
                                          extension = FALSE) 

# Source Function That will Build randomForest model
###source("Functions/Spectral_classifier.R")

# Build Model
###Headwall_RFMOD<-Spectral_classifier("Output/D_002_SpecLib_Derivs.csv",out_file = "Output/")

# Save results
###save(Headwall_RFMOD,file = "Output/Headwall_model.rda")

# --------------------------------------------- Classify Raster ------------------------------------------------------
# Classify Image using the HyperSpec_DerivGenerator function
#"/Forests/OriginalData/Buffers/1_Buffer.tif"
system.time(PredLayer<-HyperSpec_DerivGeneratorRanger(filename = "/Chan_Thesis_Missions/Blueberries_06072019/100304_blueberries_baxter_2_2019_06_07_18_37_52/raw_9510_rd_rf_or",
                                     out_file = "/Blueberries/ROutputs/bb1_9510_categor2",
                                     Classif_Model = "/Blueberries/ROutputs/Best_Model25vars_bb1_categor2_hyperspec.rda"))

#source("/Forests/Functions/LandCoverEstimator")


#LandCoverEstimator<-function(filename,out_file,Classif_Model,datatype,extension){

system.time(PredLayer<-LandCoverEstimator(filename = "/Chan_Thesis_Missions/Blueberries_06072019/100287_blueberries_airport1_2019_06_07_14_59_30/raw_13575_rd_rf_or",
                                                        out_file = "/Blueberries/ROutputs/test",
                                                        Classif_Model = "/Blueberries/ROutputs/Best_Model20vars_bb1.rda",
                                                        datatype = "raster", 
                                                        extension = TRUE))
  


# Classify BUFFER images using the Hyperspec_DerivGenerator function

setwd("/Forests/OriginalData/Buffers/") # Set path for buffer samples
mypath_buffer = "/Forests/OriginalData/Buffers/"
# Import names of shadow sample tif files into character list
buffer_samples = list.files(mypath_buffer, pattern="\\.tif$")

buffer_raster_list<-lapply(buffer_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",buffer_samples))  # Substitutes file name and removes .tif from the name

output_path = "/Forests/ROutputs/ClassifiedBuffers/"

lapply(buffer_samples, function(x) {
  system.time(PredLayer <- HyperSpec_DerivGeneratorRanger(filename = x,
                            out_file =  "/Forests/ROutputs/ClassifiedBuffers/buffer",
                            Classif_Model = "/Forests/ROutputs/Best_Model10vars_ash_shad.rda"))
  
  
})

 
system.time(PredLayer <- lapply(buffer_samples, function(x){
                                                      HyperSpec_DerivGeneratorRanger(filename = x,
                                                      out_file =  "/Forests/ROutputs/ClassifiedBuffers/buffer",
                                                      Classif_Model = "/Forests/ROutputs/Best_Model10vars_ash_shad.rda")
}))
#file.path(dirname(mypath_buffer), gsub(basename(mypath_buffer)))
#"/Forests/ROutputs/ClassifiedBuffers/",
#Add column Tree_numbe to dataframe
#nhti_raster_list <- Map(cbind, nhti_raster_list, Tree_numbe = names(nhti_raster_list))

# E:/Chan_Thesis_Missions/Ash_07262019/100131_ash_atkinson1_2019_07_27_13_58_10/raw_15366_rd_rf_or
# E:/Chan_Thesis_Missions/Ash_07262019/100139_ash_hooksett2_2019_07_27_17_00_31/raw_2575_rd_rf_or
# E:/Forests/ROutputs/MaskedImages/Atkinson_15366_all_mask.tif"


