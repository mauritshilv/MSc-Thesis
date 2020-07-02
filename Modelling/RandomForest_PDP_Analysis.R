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
# For the interpretation purposes of a random forest, we first have to exclude highly correlated variables
# Delete Max_No_Guests, No_Bathrooms and Unique_Objects from dataset
data <- data[, -c(5,6,25)]

# Possibility to transform prem level to numeric 
#data$Premiumization_Level <- as.numeric(data$Premiumization_Level)

library(randomForest) # for randomForest, partialPlot, and varImpPlot functions
set.seed(101) # for reproducibility
new.rf <- randomForest(Occupancy_Rate_LTM ~ ., data = data, importance = TRUE)
varImpPlot(new.rf) 

library(pdp)

# Compute partial dependence data for Luminance and Sharpness
pd <- partial(new.rf, pred.var = c("Luminance", "Sharpness"))
# Default PDP
pdp1 <- plotPartial(pd)
plot(pdp1)
# Add contour lines and use a different color palette
rwb <- colorRampPalette(c("red", "white", "blue"))
pdp2 <- plotPartial(pd, contour = TRUE, col.regions = rwb)
plot(pdp2)
# 3-D surface
pdp3 <- plotPartial(pd, levelplot = FALSE, zlab = "Occ. Rate", drape = TRUE,
                    colorkey = TRUE, screen = list(z = -20, x = -60))
plot(pdp3)
# Figure 3
grid.arrange(pdp1, pdp2, pdp3, ncol = 3)


# Compute partial dependence data for Premiumization_Level and Sharpness
pd2 <- partial(new.rf, pred.var = c("Premiumization_Level", "Sharpness"))
pdp2 <- plotPartial(pd2)
plot(pdp2)

# Compute partial dependence data for Premiumization_Level and Colorfulness
pd3 <- partial(new.rf, pred.var = c("Premiumization_Level", "Colorfulness"))
pdp3 <- plotPartial(pd3)
plot(pdp3)

# Compute partial dependence data for Premiumization_Level and Contrasts
pd4 <- partial(new.rf, pred.var = c("Premiumization_Level", "Contrasts"))
pdp4 <- plotPartial(pd4)
plot(pdp4)

# Compute partial dependence data for Premiumization_Level and Luminance
pd5 <- partial(new.rf, pred.var = c("Premiumization_Level", "Luminance"))
pdp5 <- plotPartial(pd5)
plot(pdp5)

# Compute partial dependence data for Premiumization_Level and Humanized
pd6 <- partial(new.rf, pred.var = c("Premiumization_Level", "Humanized"))
pdp6 <- plotPartial(pd6)
plot(pdp6)

# Compute partial dependence data for Premiumization_Level and Object_Count
pd7 <- partial(new.rf, pred.var = c("Premiumization_Level", "Object_Count"))
pdp7 <- plotPartial(pd7)
plot(pdp7)

