# Blueberries
This repository includes the processes and scripts used to predict wild blueberry water stress (via water potential) and irrigation status (irrigated field or non-irrigated). The prediction method entails a random forest algorithm developed by the Nelson Lab. The data and scripts were used in a thesis resulting in a publication in MDPI Remote Sensing journal *Predicting water stress in wild blueberry fields using airborne visible and near infrared imaging spectroscopy.* 

Data can be made available upon request. 

### **Blueberry_scripts**

Polygons were first created in ENVI over the locations of ground-sampled points using the region of interest (ROI) tool. Each image contains one polygon shapefile with multiple polygons (sometimes only one). These are labeled under a naming convention of site name, image number, and ‘samps’ (airport_14844_samps) and are within the **ImageSamples** file under each respective collection date. 

1.	Blueberry_extraction.R
Script contains a loop that extracts all samples from the remote sensing image. The loop must be run for each image. All polygon extractions are saved as their own .tif file. These are stored under **ImageExtractions** with naming convention letter of site and sample number (A15). 

2.	Any of the following 3 scripts (those including ‘arrange’)  
*blueberry_data_arrange.R (arrangement water potential prediction)  
blueberry_data_arrange_categor.R (arrangement irrigated or non-irrigated classification)  
blueberry_data_arrange_val.R (arrangement for validation of water potential prediction)*    
These scripts arrange the extracted data into the correct groupings for training data. 

3.	*Blueberry_predict.R*    
Used to make a prediction and assess prediction accuracy using validation data from the water potential data.

4.	*Blueberry_spectra_plot.R*  
Used to create plots of spectral curves for visual interpretation.

### Random forest classification scripts:
1.	or 2. *Blueberry_Spectral_classifier_categor.R*  
Creates model and generates variable importance information to classify fields as irrigated or non-irrigated. 

2.	or 1. *Blueberry_Spectral_classifier_watpot.R*  
Creates model and generates variable importance information to predict water stress or water potential in fields. 

*Blueberry_hyperspecGenFunctionRanger.R  
Blueberry_Image_classifier.R*  
Classifies image using the generated model.  
*Blueberry_LandCoverEstimator.R*
	
Scripts that are irrelevant and should be removed:  
*Blueberry_mask.R  
ashhealth_data_arrange.R*
