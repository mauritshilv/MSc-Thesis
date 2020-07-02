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
# At this point we do not yet convert the Premiumization_Level variable to a factor but we might do this later
data[,c(9,20,28)] <- lapply(data[,c(9,20,28)] , factor)
#data[,c(9,20,28,29)] <- lapply(data[,c(9,20,28,29)] , factor) (this one also takes the premlevel variable)
# Check data structure again to see if it worked properly
str(data)
summary(data)
# Name dataset with normal Face_Count variable
data1 <- data[, -c(18)]
# Name dataset with Face_Count_Alternative variable
data2 <- data[, -c(17)]
# For interpretation purposes later in this research we rescale the ocupancy rate to a value between 0 and 100 instead of a value between 0 and 1
#data1$Occupancy_Rate_LTM <- data1$Occupancy_Rate_LTM*100
#data2$Occupancy_Rate_LTM <- data2$Occupancy_Rate_LTM*100

# Remove factor variables in order to make a correlation plot
corrplotdata <- data1[,-c(9,19,27)]
# Calculate correlations
correlations <- cor(corrplotdata)
# Create corrplot
library(corrplot)
corrplot(correlations, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.8)

# First check difference in mean occupancy rates between listings with humanized or not-humanized images
library(dplyr)
data1 %>%
  group_by(Humanized) %>%
  summarize(meanocc <- mean(Occupancy_Rate_LTM))
# Run t-test to see if difference is significant
t.test(data1$Occupancy_Rate_LTM ~ data1$Humanized)


# Apply the scaling for all numeric variables (for both dataset 1 and 2, and including premiumization level as a numeric variable)
data1$Average_Daily_Rate <- scale(data1$Average_Daily_Rate, scale = TRUE)
data1$No_Reviews <- scale(data1$No_Reviews, scale = TRUE)
data1$No_Bedrooms <- scale(data1$No_Bedrooms, scale = TRUE)
data1$No_Bathrooms <- scale(data1$No_Bathrooms, scale = TRUE)
data1$Max_No_Guests <- scale(data1$Max_No_Guests, scale = TRUE)
data1$Response_Time <- scale(data1$Response_Time, scale = TRUE)
data1$Sec_Deposit <- scale(data1$Sec_Deposit, scale = TRUE)
data1$Cleaning_Fee <- scale(data1$Cleaning_Fee, scale = TRUE)
data1$`Extra People Fee` <- scale(data1$`Extra People Fee`, scale = TRUE)
data1$`Minimum Stay` <- scale(data1$`Minimum Stay`, scale = TRUE)
data1$`Number of Photos` <- scale(data1$`Number of Photos`, scale = TRUE)
data1$Overall_Rating <- scale(data1$Overall_Rating, scale = TRUE)
data1$Face_Count <- scale(data1$Face_Count, scale = TRUE)
data1$Host_No_Listings <- scale(data1$Host_No_Listings, scale = TRUE)
data1$Colorfulness <- scale(data1$Colorfulness, scale = TRUE)
data1$Luminance <- scale(data1$Luminance, scale = TRUE)
data1$Contrasts <- scale(data1$Contrasts, scale = TRUE)
data1$Sharpness <- scale(data1$Sharpness, scale = TRUE)
data1$Object_Count <- scale(data1$Object_Count, scale = TRUE)
data1$Unique_Objects <- scale(data1$Unique_Objects, scale = TRUE)
data1$Premiumization_Level <- scale(data1$Premiumization_Level, scale = TRUE)

data2$Average_Daily_Rate <- scale(data2$Average_Daily_Rate, scale = TRUE)
data2$No_Reviews <- scale(data2$No_Reviews, scale = TRUE)
data2$No_Bedrooms <- scale(data2$No_Bedrooms, scale = TRUE)
data2$No_Bathrooms <- scale(data2$No_Bathrooms, scale = TRUE)
data2$Max_No_Guests <- scale(data2$Max_No_Guests, scale = TRUE)
data2$Response_Time <- scale(data2$Response_Time, scale = TRUE)
data2$Sec_Deposit <- scale(data2$Sec_Deposit, scale = TRUE)
data2$Cleaning_Fee <- scale(data2$Cleaning_Fee, scale = TRUE)
data2$`Extra People Fee` <- scale(data2$`Extra People Fee`, scale = TRUE)
data2$`Minimum Stay` <- scale(data2$`Minimum Stay`, scale = TRUE)
data2$`Number of Photos` <- scale(data2$`Number of Photos`, scale = TRUE)
data2$Overall_Rating <- scale(data2$Overall_Rating, scale = TRUE)
data2$Face_Count_Alternative <- scale(data2$Face_Count_Alternative, scale = TRUE)
data2$Host_No_Listings <- scale(data2$Host_No_Listings, scale = TRUE)
data2$Colorfulness <- scale(data2$Colorfulness, scale = TRUE)
data2$Luminance <- scale(data2$Luminance, scale = TRUE)
data2$Contrasts <- scale(data2$Contrasts, scale = TRUE)
data2$Sharpness <- scale(data2$Sharpness, scale = TRUE)
data2$Object_Count <- scale(data2$Object_Count, scale = TRUE)
data2$Unique_Objects <- scale(data2$Unique_Objects, scale = TRUE)
data2$Premiumization_Level <- scale(data2$Premiumization_Level, scale = TRUE)

# Fit simple linear regression model
model1 <- lm(Occupancy_Rate_LTM ~., data1)
summary(model1)
# Check VIFs
library(car)
vif(model1)
# Taking the squares of the generalized VIF^(1/(2*Df)), does not indicate excessive multicollinearity
# Therefore removing certain variables from this model is not needed

# Fit moderated regression model
model2 <- lm(Occupancy_Rate_LTM ~. + Colorfulness*Premiumization_Level + Luminance*Premiumization_Level + Contrasts*Premiumization_Level + Sharpness*Premiumization_Level + Humanized*Premiumization_Level + Object_Count*Premiumization_Level, data1)
summary(model2)
# Check VIFs
vif(model2)
# Taking the squares of the generalized VIF^(1/(2*Df)), does not indicate excessive multicollinearity
# Therefore removing certain variables from this model is not needed

# Now with data2
model3 <- lm(Occupancy_Rate_LTM ~. + Colorfulness*Premiumization_Level + Luminance*Premiumization_Level + Contrasts*Premiumization_Level + Sharpness*Premiumization_Level + Humanized*Premiumization_Level + Object_Count*Premiumization_Level, data2)
summary(model3)
# Check VIFs
vif(model3)
# Taking the squares of the generalized VIF^(1/(2*Df)), does not indicate excessive multicollinearity
# Therefore removing certain variables from this model is not needed

# Plot the errors
plot(model3, which = 1, las = 1)

# Rerun model with log transformed dependend variable to assess if heteroscedasticity persists
model4 <- lm(log(Occupancy_Rate_LTM) ~. + Colorfulness*Premiumization_Level + Luminance*Premiumization_Level + Contrasts*Premiumization_Level + Sharpness*Premiumization_Level + Humanized*Premiumization_Level + Object_Count*Premiumization_Level, data2)
summary(model4)
# Check VIFs
vif(model4)
# Taking the squares of the generalized VIF^(1/(2*Df)), does not indicate excessive multicollinearity
# Therefore removing certain variables from this model is not needed
# Check Residuals vs Fitted plot
plot(model4, which = 1, las = 1)

