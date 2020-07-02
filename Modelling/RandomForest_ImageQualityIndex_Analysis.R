# Author: Maurits Hilverda
# Date: June 28, 2020
# Note: inspiration for this code was drawn from Interpretable Machine Learning by Molnar (2020)
# and the explanation of the related package on: http://uc-r.github.io/iml-pkg

# Set the desired working directory
setwd("/Users/mauritshilverda/Documents/Studie/MSc/THESIS/R")
# Install and activate package required to read excel file
library("readxl")
# Import data
data <- read_excel("/Users/mauritshilverda/Documents/Studie/MSc/THESIS/DATA/CombinedData_June28.xlsx")
#Remove Property_Type, Listing_Type, Count_Reservation Days LTM, Count_Available days LTM and Count_Blocked days LTM variables
data <- data[,-c(3,4,18,19,20)]
# Rename rownames based on the Property IDs
row.names(data) <- data$Property_ID
# Remove Property_ID and Host_ID variables
data <- data[, -c(1,2)]
# Check data
str(data)
# Convert character variables (Canc_Policy, District and Image_Location) to factors
# Also we choose to convert the Premiumization_Level variable to a factor
data[,c(9,20,28,29)] <- lapply(data[,c(9,20,28, 29)] , factor)
# Check data structure again to see if it worked properly
str(data)
summary(data)
# Name dataset with normal Face_Count variable
data <- data[, -c(18)]
names(data) <- make.names(names(data))

#### Random Forests ####
library(scales)
# Scale photographic metrics to 0-1 range
data$Colorfulness1 <- rescale(data$Colorfulness, to = c(0, 1), from = range(data$Colorfulness, na.rm = TRUE, finite = TRUE))
data$Luminance1 <- rescale(data$Luminance, to = c(0, 1), from = range(data$Luminance, na.rm = TRUE, finite = TRUE))
data$Contrasts1 <- rescale(data$Contrasts, to = c(0, 1), from = range(data$Contrasts, na.rm = TRUE, finite = TRUE))
data$Sharpness1 <- rescale(data$Sharpness, to = c(0, 1), from = range(data$Sharpness, na.rm = TRUE, finite = TRUE))

#Standardize photographic metrics
data$Colorfulness2 <- scale(data$Colorfulness, scale = TRUE)
data$Luminance2 <- scale(data$Luminance, scale = TRUE)
data$Contrasts2 <- scale(data$Contrasts, scale = TRUE)
data$Sharpness2 <- scale(data$Sharpness, scale = TRUE)

#Create new image quality indexes
data$ImageQ1 <- data$Colorfulness1*data$Luminance1*data$Contrasts1*data$Sharpness1
data$ImageQ2 <- data$Colorfulness2*data$Luminance2*data$Contrasts2*data$Sharpness2
str(data)
#Remove all old photographic metrics
data <- data[, -c(20,21,22,23,29,30,31,32,33,34,35,36)]

#### Random Forests ####
library(rsample)   # data splitting
library(ggplot2)   # allows extension of visualizations
library(dplyr)     # basic data transformation
library(h2o)       # machine learning modeling
library(iml) 

# initialize h2o session
h2o.no_progress()
h2o.init()

# Data
# For the interpretation purposes of a random forest, we first have to exclude highly correlated variables
# Delete Max_No_Guests, No_Bathrooms and Unique_Objects from dataset
data <- data[, -c(5,6,21)]
names(data) <- make.names(names(data))
data$ImageQ2 <- as.numeric(data$ImageQ2)
summary(data$ImageQ2)
# Remove ImageQ1
#Or remove ImageQ2
data<- data[, -c(23)]
## Delete Max_No_Guests, NO_Bathrooms and Objectcount from dataset
#data2 <- data[, -c(5,6,24)]
df <- data %>% 
  mutate_if(is.ordered, factor, ordered = FALSE)

# convert to h2o object
df.h2o <- as.h2o(df)

# create train, validation, and test splits
set.seed(123)
splits <- h2o.splitFrame(df.h2o, ratios = c(.75, .15), destination_frames = c("train","valid","test"))
names(splits) <- c("train","valid","test")

# variable names for resonse & features
y <- "Occupancy_Rate_LTM"
x <- setdiff(names(df), y) 

# random forest model
rf <- h2o.randomForest(
  x = x, 
  y = y,
  training_frame = splits$train,
  validation_frame = splits$valid,
  ntrees = 1000,
  stopping_metric = "AUTO",    
  stopping_rounds = 10,         
  stopping_tolerance = 0.005,
  seed = 123
)

# 1. create a data frame with just the features
features <- as.data.frame(splits$valid) %>% select(-Occupancy_Rate_LTM)

# 2. Create a vector with the actual responses
response <- as.numeric(as.vector(splits$valid$Occupancy_Rate_LTM))

# 3. Create custom predict function that returns the predicted values as a vector
pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[3L]])
}

# create predictor object to pass to explainer functions
predictor.rf <- Predictor$new(
  model = rf, 
  data = features, 
  y = response, 
  predict.fun = pred,
  class = "regression"
)

################## Results / Interpretation ######################

# Variable importances
imp.rf <- FeatureImp$new(predictor.rf, loss = "mse")
imp.rf$results
plot(imp.rf) 


# Variables with largest interactions
# This measures if a feature interacts with any other feature
interact.rf  <- Interaction$new(predictor.rf) %>% plot() 
plot(interact.rf)

# No we can measure the two-way interactions of all variables with a specified variable
## ImageQ1
interact.rf.Q1  <- Interaction$new(predictor.rf, feature = "ImageQ1") %>% plot()
plot(interact.rf.Q1)

## No_Reviews
interact.rf.no_reviews  <- Interaction$new(predictor.rf, feature = "No_Reviews") %>% plot()
plot(interact.rf.no_reviews)
## Premiumization_Level
interact.rf.prem_level  <- Interaction$new(predictor.rf, feature = "Premiumization_Level") %>% plot()
plot(interact.rf.prem_level)
## Response_Time
interact.rf.res_time  <- Interaction$new(predictor.rf, feature = "Response_Time") %>% plot()
plot(interact.rf.res_time)
## District
interact.rf.district  <- Interaction$new(predictor.rf, feature = "DiStrict") %>% plot()
plot(interact.rf.district)




# ICE Plots
## No_Reviews
rf.reviews <- Partial$new(predictor.rf, "No_Reviews", ice = TRUE, grid.size = 50)
rf.reviews$center(min(features$No_Reviews))
plot(rf.reviews)
## Face_Count
rf.facecount <- Partial$new(predictor.rf, "Face_Count", ice = TRUE, grid.size = 50)
rf.facecount$center(min(features$Face_Count))
plot(rf.facecount)

## Q1
rf.q1 <- Partial$new(predictor.rf, "ImageQ1", ice = TRUE, grid.size = 50)
rf.q1$center(min(features$ImageQ1))
plot(rf.q1)

## Q2 
rf.q2 <- Partial$new(predictor.rf, "ImageQ2", ice = TRUE, grid.size = 50)
rf.q2$center(min(features$ImageQ2))
plot(rf.q2)


## Object_Count
rf.objectcount <- Partial$new(predictor.rf, "Object_Count", ice = TRUE, grid.size = 50)
rf.objectcount$center(min(features$Object_Count))
plot(rf.objectcount)
## Humanized
rf.humanized <- Partial$new(predictor.rf, "Humanized", ice = TRUE, grid.size = 50)
rf.humanized$center(min(features$Humanized))
plot(rf.humanized)
## Image_Location
rf.im_location <- Partial$new(predictor.rf, "Image_Location", ice = TRUE, grid.size = 50)
rf.im_location$center(min(features$Image_Location))
plot(rf.im_location)
## Premiumization_Level
rf.premlevel <- Partial$new(predictor.rf, "Premiumization_Level", ice = TRUE, grid.size = 50)
rf.premlevel$center(min(features$Premiumization_Level))
plot(rf.premlevel)

