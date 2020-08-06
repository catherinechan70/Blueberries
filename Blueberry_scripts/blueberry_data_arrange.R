# Arrange extracted tiff samples to create water potential predictors
# Previous step is blueberry_extraction.R 
# Next step image_classifier/HyperspecGenFunctionRanger

library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(readxl)
library(reshape2)


setwd("E:/")

## Read in headwall wavelength bandpasses
Headwall_wv<-scan("Blueberries/Headwall_wv",numeric())

## Read in sheet data with water potential values
sheet_data <- read_excel(path = "Blueberries/WymansFarm_waterpot.xlsx")

names(sheet_data) <- as.matrix(sheet_data[1, ])
sheet_data <- sheet_data[-1, ]
sheet_data[] <- lapply(sheet_data, function(x) type.convert(as.character(x)))


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


#Renames columns to headwall bandpasses
bb1_raster_list <- lapply(bb1_raster_list, function(bb1_df) {
  names(bb1_df)[3:328] <- Headwall_wv
  bb1_df
})

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
bb1_Df <- do.call("rbind", bb1_raster_list)%>% #dim() #[1] 1076  329
  inner_join(sheet_data[ ,c("SN", "Waterpotential1")], by="SN")%>% #dim() #[1] 1076  330
  dplyr::select(x,y,SN,Waterpotential1,everything(),-(`901.276`:`999.42`))%>%
  na.omit()


## Run logical test on dataframe to see if there are any weird values (those values outside of 0 and 2)
## Convert weird values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
bb1_tst1<-lapply(bb1_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
bb1_tst1$V1%>%range()##There are weird values   
bb1_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
bb1_Df<-bb1_Df%>%
  filter_at(vars(-x,-y,-SN,-Waterpotential1),all_vars(. >=0))


### Adding categorical column (irrigated and non-irrigated rather than continuous, water potential)
# BE CAREFUL ABOUT DUPLICATE SAMPLES IN BAXTER (B11a AND B11b)
bb1_Df <- add_column(bb1_Df, "Condition" = NA, .after = 4)
bb1_Df$Condition <- ifelse(grepl("A", bb1_Df$SN), 1, 2)

names(bb1_Df) [4] <- "Waterpotential"


### Write out csv file for water potential / categorical dataframe
#write.csv(bb1_Df,"E:/Blueberries/ROutputs/bb1_cal_Df.csv", row.names = FALSE)



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

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
bb2_Df <- do.call("rbind", bb2_raster_list)%>% #dim() #[1] 1076  329
  inner_join(sheet_data[ ,c("SN", "Waterpotential2")], by="SN")%>% #dim() #[1] 1076  330
  dplyr::select(x,y,SN,Waterpotential2,everything(),-(`901.276`:`999.42`))%>%
  na.omit()


## Run logical test on dataframe to see if there are any weird values (those values outside of 0 and 2)
## Convert weird values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
bb2_tst1<-lapply(bb2_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
bb2_tst1$V1%>%range()##There are weird values   
bb2_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
bb2_Df<-bb2_Df%>%
  filter_at(vars(-x,-y,-SN,-Waterpotential2),all_vars(. >=0))


### Adding categorical column (irrigated and non-irrigated rather than continuous, water potential)
# BE CAREFUL ABOUT DUPLICATE SAMPLES IN BAXTER (B11a AND B11b)
bb2_Df <- add_column(bb2_Df, "Condition" = NA, .after = 4)
bb2_Df$Condition <- ifelse(grepl("A", bb2_Df$SN), 1, 2)

names(bb2_Df) [4] <- "Waterpotential"

### Write out csv file for water potential / categorical dataframe
#write.csv(bb2_Df,"E:/Blueberries/ROutputs/bb2_cal_Df.csv", row.names = FALSE)


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

##Combines all dataframes so we can do a inner join on the sheets dataset later
##We don't have to combine all dataframes here, but this would be necessary for building models later
bb3_Df <- do.call("rbind", bb3_raster_list)%>% #dim() #[1] 1076  329
  inner_join(sheet_data[ ,c("SN", "Waterpotential3")], by="SN")%>% #dim() #[1] 1076  330
  dplyr::select(x,y,SN,Waterpotential3,everything(),-(`901.276`:`999.42`))%>%
  na.omit()


## Run logical test on dataframe to see if there are any weird values (those values outside of 0 and 2)
## Convert weird values to 0s or remove them if needed (check with peter, not sure if we were to convert or remove these values)
##Need an if fuction here
bb3_tst1<-lapply(bb3_Df[-1:-4],range)%>%as.data.frame%>%t()%>%as.data.frame
bb3_tst1$V1%>%range()##There are weird values   
bb3_tst1$V2%>%range()##There are no weird values

##Now lets convert those values to 0s
bb3_Df<-bb3_Df%>%
  filter_at(vars(-x,-y,-SN,-Waterpotential3),all_vars(. >=0))


### Adding categorical column (irrigated and non-irrigated rather than continuous, water potential)
# BE CAREFUL ABOUT DUPLICATE SAMPLES IN BAXTER (B11a AND B11b)
bb3_Df <- add_column(bb3_Df, "Condition" = NA, .after = 4)
bb3_Df$Condition <- ifelse(grepl("A", bb3_Df$SN), 1, 2)

names(bb3_Df) [4] <- "Waterpotential"

### Write out csv file for water potential / categorical dataframe
#write.csv(bb3_Df,"E:/Blueberries/ROutputs/bb3_cal_Df.csv", row.names = FALSE)

names(bb1_Df) [4] <- "Waterpotential"
names(bb2_Df) [4] <- "Waterpotential"
names(bb3_Df) [4] <- "Waterpotential"


allbb_Df <- rbind(bb1_Df, bb2_Df, bb3_Df)

write.csv(allbb_Df,"E:/Blueberries/ROutputs/allbb_full_Df.csv", row.names = FALSE)


#length(bb1_Df[bb1_Df$Condition_==2])

#Df%>%filter(Condition=="2")%>%View()

