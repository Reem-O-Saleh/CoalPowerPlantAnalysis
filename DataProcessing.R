# load the necessary libraries 
library(tidyverse)   
library(ggmap)       
library(leaflet)     
library(scales)      
library(data.table) 
library(RColorBrewer) 
library(openxlsx)

# import datasets
data_2017 <- read.csv("Global-Coal-Plant-Tracker-Jan-2017.csv")
data_2023 <- read.csv("Global-Coal-Plant-Tracker-July-2023.csv")

# print names to verify that the dataset variables are the same  
print(names(data_2017))
print(names(data_2023))

# define columns to keep
columns_to_keep <- c("Tracker.ID", "Unit", "Plant", "Owner", "Parent", "Capacity..MW.", "Status", 
                     "Country", "Region", "Start.year", "Planned.retirement", "Year.retired",
                      "Latitude", "Longitude", "Combustion.technology", "Heat.rate", "Emission.factor", "Capacity.factor", 
                      "Annual.CO2")

# subset the data to keep only necessary columns
data_2017 <- data_2017[, columns_to_keep]
data_2023 <- data_2023[, columns_to_keep]

# view datasets after making changes 
View(data_2023)
View(data_2017)

# review column data types to ensure proper formatting 
str(data_2017)
str(data_2023)

# ensure latitude and longitude columns are numeric 
data_2017$Latitude <- as.numeric(as.character(data_2017$Latitude))
data_2017$Longitude <- as.numeric(as.character(data_2017$Longitude))

data_2023$Latitude <- as.numeric(as.character(data_2023$Latitude))
data_2023$Longitude <- as.numeric(as.character(data_2023$Longitude))

# ensure columns have character type data for 2017 data 
data_2017 <- data_2017 %>%
  mutate(across(c(Status, Start.year, Year.retired, Planned.retirement), as.character))

# ensure columns have character type data for 2023 data
data_2023 <- data_2023 %>%
  mutate(across(c(Status, Start.year, Year.retired, Planned.retirement), as.character))

# remove commas and convert Capacity..MW. column to numeric
data_2017$Capacity..MW. <- as.numeric(gsub(",", "", data_2017$Capacity..MW.))
data_2023$Capacity..MW. <- as.numeric(gsub(",", "", data_2023$Capacity..MW.))

# remove commas for heat rate column 
data_2017$Heat.rate <- as.numeric(gsub(",", "", data_2017$Heat.rate))
data_2023$Heat.rate <- as.numeric(gsub(",", "", data_2023$Heat.rate))

# remove '%' and convert to decimal for capacity factor column 
data_2017$Capacity.factor <- as.numeric(sub("%", "", data_2017$Capacity.factor)) / 100
data_2023$Capacity.factor <- as.numeric(sub("%", "", data_2023$Capacity.factor)) / 100

# converting selected categorical columns to factors for both datasets 
data_2017$Country <- as.factor(data_2017$Country)
data_2017$Owner <- as.factor(data_2017$Owner) 
data_2017$Combustion.technology <- as.factor(data_2017$Combustion.technology) 

data_2023$Country <- as.factor(data_2023$Country)
data_2023$Owner <- as.factor(data_2023$Owner)  
data_2023$Combustion.technology <- as.factor(data_2023$Combustion.technology)  

# capitalize first word in every row for Status column in 2023 dataset 
data_2023$Status <- str_to_title(data_2023$Status)

# view dataset after changes 
View(data_2023)

# change "Pre" to say "Pre-Permit" for Status column in 2017 dataset 
data_2017$Status <- str_replace_all(data_2017$Status, "Pre", "Pre-Permit")

# view dataset after changes 
View(data_2017)

# replace blanks in Start.year column for 2017 dataset

data_2017 <- data_2017 %>%
  mutate(Start.year = case_when(
    # If 'Start.year' is blank and 'Status' is either "Cancelled", "Deactivated", or "Shelved", replace 'Start.year' with "Not Applicable"
    Start.year == "" & Status %in% c("Cancelled", "Deactivated", "Shelved") ~ "Not Applicable",
    
    # If 'Start.year' is blank and 'Status' is one of the following: "Permitted", "Pre-Permit", "Announced", "Operating", "Retired", "Construction", replace 'Start.year' with "Unknown"
    Start.year == "" & Status %in% c("Permitted", "Pre-Permit", "Announced", "Operating", "Retired", "Construction") ~ "Unknown",
    
    # If neither of the above conditions are met, keep the original (or non-blank) value of 'Start.year'
    TRUE ~ Start.year  
  ))

# replace blanks in Year.retired column for 2017 dataset

data_2017 <- data_2017 %>%
  mutate(Year.retired = case_when(
    # Replace all blank entries in 'Year.retired' with "Not Applicable"
    Year.retired == "" ~ "Not Applicable",
    
    # If 'Year.retired' is not blank, retain its current value
    TRUE ~ Year.retired
  ))

# replace blanks in Planned.retirement column for 2017 dataset

data_2017 <- data_2017 %>%
  mutate(Planned.retirement = case_when(
    # If 'Planned.retirement' is blank and 'Status' is either "Cancelled", "Deactivated", "Retired", or "Shelved", replace 'Planned.retirement' with "Not Applicable"
    Planned.retirement == "" & Status %in% c("Cancelled", "Deactivated", "Shelved", "Retired") ~ "Not Applicable",
    
    # If 'Planned.retirement' is blank and 'Status' is one of the following: "Permitted", "Pre-Permit", "Announced", "Operating", "Construction", replace 'Planned.retirement' with "Unknown"
    Planned.retirement == "" & Status %in% c("Permitted", "Pre-Permit", "Announced", "Operating", "Construction") ~ "Unknown",
    
    # If neither of the above conditions are met, keep the original (or non-blank) value of 'Start.year'
    TRUE ~ Planned.retirement
  ))


table(data_2017$Start.year)  # Check the updated values in Start.year column 
table(data_2017$Year.retired)  # Check the updated values in Year.retired column 
table(data_2017$Planned.retirement) # Check the updated values in Planned.retirement column 


# save the processed/cleaned data to Excel files
write.xlsx(data_2017, "Processed_Data_2017.xlsx")
write.xlsx(data_2023, "Processed_Data_2023.xlsx")

#####################################################################
# DEBUGGING 2023 DATASET (because it was not filling out year related data correctly in the Planned.retirement, Year.retired, and Start.year columns)

# Load the dataset
data_2023 <- read_excel("Processed_Data_2023.xlsx")

# Check initial status and types
glimpse(data_2023)

# Convert necessary columns to character to ensure consistency in text handling
data_2023 <- data_2023 %>%
  mutate(across(c(Status, Start.year, Year.retired, Planned.retirement), as.character))

# Clean and trim the Status column to remove any leading/trailing spaces or case issues
data_2023$Status <- tolower(str_trim(data_2023$Status))

# Verify unique statuses to ensure all conditions in case_when are covered
print(unique(data_2023$Status))

# Apply transformations with case_when and an NA handling condition
data_2023 <- data_2023 %>%
  mutate(
    Start.year = case_when(
      (is.na(Start.year) | Start.year == "") & Status %in% c("cancelled", "deactivated", "shelved", "mothballed") ~ "Not Applicable",
      (is.na(Start.year) | Start.year == "") & Status %in% c("announced", "pre-permit", "permitted", "construction", "operating") ~ "Unknown",
      TRUE ~ Start.year
    ),
    Year.retired = case_when(
      is.na(Year.retired) | Year.retired == "" ~ "Not Applicable",
      TRUE ~ Year.retired
    ),
    Planned.retirement = case_when(
      (is.na(Planned.retirement) | Planned.retirement == "") & Status %in% c("cancelled", "deactivated", "shelved", "mothballed", "retired") ~ "Not Applicable",
      (is.na(Planned.retirement) | Planned.retirement == "") & Status %in% c("announced", "pre-permit", "permitted", "construction", "operating") ~ "Unknown",
      TRUE ~ Planned.retirement
    )
  )



# Check output to ensure transformations are applied correctly
print(head(data_2023$Start.year))
print(head(data_2023$Year.retired))
print(head(data_2023$Planned.retirement))

# Filter to see only operating plants
operating_plants <- data_2023 %>% 
  filter(Status == "Operating") %>% 
  select(Status, Planned.retirement)

# View the output to understand what's happening
print(head(operating_plants))

# conditional indexing to format the Status and Planned.retirement for operating plants 
planned_retirement_rows <- data_2023$Status == "operating"
data_2023$Planned.retirement[planned_retirement_rows] <- "Unknown"

# Save cleaned data
write.xlsx(data_2023, "Processed_Data_2023.xlsx")

# reload and check the saved data
processed_data_2023 <- read_excel("Processed_Data_2023.xlsx")
print(head(processed_data_2023$Start.year))

