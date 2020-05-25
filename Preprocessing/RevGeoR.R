# Activate required packages
library(revgeo)
library(dplyr)
library(readxl)
# Load dataset
Data <- read_excel("Documents/Studie/MSc/Thesis/DATA/CombinedData2.xlsx")
# Limit to most relevant columns to reduce computative load
Data <- Data[, c('Property ID', 'Latitude', 'Longitude')]
gc()

# Define empty dataframe to store results
data_all = data.frame()
start <- Sys.time()

# Create while loop that allows running the function over portions of the total dataset
while (nrow(Data)>0) {
  # Subset data to ensure the number of requests does not overload the Photon server
  main_sub_t <-  Data[1:200,]
  # Extract the coordinates from the dataset
  latlong <- main_sub_t %>% 
    select(Latitude, Longitude) %>% 
    unique() %>% 
    mutate(index=row_number())
  
  # Start the reverse geocoding with the revgeo package
  cities <- revgeo(latlong$Longitude, latlong$Latitude, provider =  'photon', output = 'frame') %>% 
    mutate(index = row_number(),country = as.character(country)) %>%
    mutate(postcode = paste(city, zip, sep = ", ")) %>% 
    select(index, postcode) %>% 
    left_join(latlong, by="index") %>% 
    select(-index)

# Add the reversed geocoding outupt to the earlier defined dataframe 
data_new <- main_sub_t %>% 
  left_join(cities, by=c("Latitude","Longitude")) %>% 
  select(`Property ID`, postcode, Latitude, Longitude)

# Input the data_new to the empty data_all dataframe where we combine the output of all subsets
data_all <- rbind(data_all,data_new) %>% 
  na.omit()

# Remove used rows to make sure the while loop afterwards uses the next 200 observations
Data <- anti_join(Data, main_sub_t, by=c("Property ID"))
print(nrow(Data))

# Free up memory space by deleting unneeded data
rm(main_sub_t)
rm(data_new)
rm(cities)
rm(latlong)

# Wait before starting with the next subset to prevent server errors
print('Sleeping for 15 seconds')
Sys.sleep(15)

}
end <- Sys.time()

#export as an excel file
library(writexl)
write_xlsx(data_all,'Documents/Studie/MSc/Thesis/DATA/RevGeoDataset.xlsx')
