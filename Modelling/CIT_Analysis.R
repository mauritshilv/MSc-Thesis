# Author: Maurits Hilverda
# Date: June 28, 2020


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

#Divide data into test and training set
size <- floor(0.75 * nrow(data)) # Define 75% of the sample size
set.seed(123) # set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(data)), size = size)
trainingData <- data[train_ind, ]
testData <- data[-train_ind, ]

##### CIT #####

# Grow Conditional Inference Tree based on training dataset
library("party")
CIT.tree.95 <- ctree(Occupancy_Rate_LTM ~ ., data= trainingData, control = ctree_control(mincriterion = 0.95))
CIT.tree.99 <- ctree(Occupancy_Rate_LTM ~ ., data= trainingData, control = ctree_control(mincriterion = 0.99))

# Plot them
plot(CIT.tree.95, main="Conditional Inference Tree for Airbnb occupancy rates",
     inner_panel=node_inner(CIT.tree.95,abbreviate = FALSE, pval = TRUE, id = FALSE))
plot(CIT.tree.99, main="Conditional Inference Tree for Airbnb occupancy rates",
     inner_panel=node_inner(CIT.tree.99,abbreviate = FALSE, pval = TRUE, id = FALSE))

# Predictions
CIT.tree.pred.95 <- predict(CIT.tree.95, testData)
summary(CIT.tree.pred.95)
CIT.tree.pred.99 <- predict(CIT.tree.99, testData)
summary(CIT.tree.pred.99)

# Check r2
cor(predict(CIT.tree.95, newdata=testData),testData$Occupancy_Rate_LTM)^2
cor(predict(CIT.tree.99, newdata=testData),testData$Occupancy_Rate_LTM)^2

