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
data <- data[, -c(5,6,25)]
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
p2 <- plot(imp.rf) 
plot(p2)

# Variables with largest interactions
# This measures if a feature interacts with any other feature
interact.rf  <- Interaction$new(predictor.rf) %>% plot() 
plot(interact.rf)

# No we can measure the two-way interactions of all variables with a specified variable
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
## Sharpness
interact.rf.sharpness  <- Interaction$new(predictor.rf, feature = "Sharpness") %>% plot()
plot(interact.rf.sharpness)



# ICE Plots
## No_Reviews
rf.reviews <- Partial$new(predictor.rf, "No_Reviews", ice = TRUE, grid.size = 50)
rf.reviews$center(min(features$No_Reviews))
plot(rf.reviews)
## Face_Count
rf.facecount <- Partial$new(predictor.rf, "Face_Count", ice = TRUE, grid.size = 50)
rf.facecount$center(min(features$Face_Count))
plot(rf.facecount)
## Colorfulness
rf.color <- Partial$new(predictor.rf, "Colorfulness", ice = TRUE, grid.size = 50)
rf.color$center(min(features$Colorfulness))
plot(rf.color)
## Luminance
rf.luminance <- Partial$new(predictor.rf, "Luminance", ice = TRUE, grid.size = 50)
rf.luminance$center(min(features$Luminance))
plot(rf.luminance)
## Contrasts
rf.contrasts <- Partial$new(predictor.rf, "Contrasts", ice = TRUE, grid.size = 50)
rf.contrasts$center(min(features$Contrasts))
plot(rf.contrasts)
## Sharpness
rf.sharpness <- Partial$new(predictor.rf, "Sharpness", ice = TRUE, grid.size = 50)
rf.sharpness$center(min(features$Sharpness))
plot(rf.sharpness)
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


