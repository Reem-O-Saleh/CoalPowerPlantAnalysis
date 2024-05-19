# Load the necessary libraries 
library(tidyverse)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(scales)
library(data.table)
library(RColorBrewer)
library(openxlsx)
library(readxl)

# Import datasets
data_2017 <- read_excel("Processed_Data_2017.xlsx")
data_2023 <- read_excel("Processed_Data_2023.xlsx")

# Add Year_Snapshot column to each dataset
data_2017$Year_Snapshot <- "2017"
data_2023$Year_Snapshot <- "2023"

# Convert Emission.factor column to numeric for 2023 dataset, removing commas
data_2023$Emission.factor <- gsub(",", "", data_2023$Emission.factor)
data_2023$Emission.factor <- as.numeric(data_2023$Emission.factor)

# Convert Capacity..MW. to numeric for both datasets
data_2017$Capacity..MW. <- as.numeric(as.character(data_2017$Capacity..MW.))
data_2023$Capacity..MW. <- as.numeric(as.character(data_2023$Capacity..MW.))

# Combine datasets
combined_data <- bind_rows(data_2017, data_2023)

# Convert Start.year to numeric
combined_data$Start.year <- as.numeric(as.character(combined_data$Start.year))

# capitalize first word in every row for Status column in 2023 dataset 
data_2023$Status <- str_to_title(data_2023$Status)

# Check for duplicates
combined_data %>%
  group_by(Plant, Country, Year_Snapshot) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(Count > 1) %>%
  arrange(desc(Count)) %>%
  print()

# Filter for operating status
operating_data <- combined_data %>%
  filter(Status == "Operating")

# Summarize the data by Country and Year_Snapshot
operating_summary <- operating_data %>%
  group_by(Country, Year_Snapshot) %>%
  summarise(Total_Capacity_MW = sum(Capacity..MW., na.rm = TRUE), .groups = 'drop')

# Create a bar graph to show coal power capacity by country for the 2017 dataset
ggplot(operating_summary, aes(x = Country, y = Total_Capacity_MW, fill = Year_Snapshot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Operating Coal Power Capacity by Country",
       x = "Country",
       y = "Total Capacity (MW)",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +  # Rotate x-axis labels
  theme(panel.grid.major.x = element_line(color = "grey80"),  # Add vertical grid lines
        panel.grid.major.y = element_line(color = "grey80"))  # Add horizontal grid lines


# if you want to get the written values of each column on the bar graph for 2017 dataset
ggplot(operating_summary, aes(x = Country, y = Total_Capacity_MW, fill = Year_Snapshot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Total_Capacity_MW), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +  # Add text labels with values on bars
  labs(title = "Operating Coal Power Capacity by Country",
       x = "Country",
       y = "Total Capacity (MW)",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +  # Rotate x-axis labels
  theme(panel.grid.major.x = element_line(color = "grey80"),  # Add vertical grid lines
        panel.grid.major.y = element_line(color = "grey80"))  # Add horizontal grid lines

##########################################################################



# Import datasets
data_2017 <- read_excel("Processed_Data_2017.xlsx")
data_2023 <- read_excel("Processed_Data_2023.xlsx")

# Add Year_Snapshot column to each dataset
data_2017$Year_Snapshot <- "2017"
data_2023$Year_Snapshot <- "2023"

# Convert Emission.factor column to numeric for 2023 dataset, removing commas
data_2023$Emission.factor <- gsub(",", "", data_2023$Emission.factor)
data_2023$Emission.factor <- as.numeric(data_2023$Emission.factor)

# Convert Capacity..MW. to numeric for both datasets
data_2017$Capacity..MW. <- as.numeric(as.character(data_2017$Capacity..MW.))
data_2023$Capacity..MW. <- as.numeric(as.character(data_2023$Capacity..MW.))

# Standardize country names
data_2017$Country <- str_replace_all(data_2017$Country, "Bosnia & Herzegovina", "Bosnia and Herzegovina")
data_2023$Country <- str_replace_all(data_2023$Country, "Bosnia & Herzegovina", "Bosnia and Herzegovina")

# Capitalize the first word in every row for Status column in 2023 dataset
data_2023$Status <- str_to_title(data_2023$Status)

# Combine datasets
combined_data <- bind_rows(data_2017, data_2023)

# Convert Start.year to numeric
combined_data$Start.year <- as.numeric(as.character(combined_data$Start.year))

# Check for NA values introduced by coercion and handle them
print("Rows with NA Start.year in combined dataset:")
print(combined_data %>% filter(is.na(Start.year)))

# Remove rows with NA Start.year
combined_data <- combined_data %>% filter(!is.na(Start.year))

# Verify the unique values in the Status column
print("Unique Status values in combined dataset:")
print(unique(combined_data$Status))

# Check for duplicates
duplicates <- combined_data %>%
  group_by(Plant, Country, Year_Snapshot) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(Count > 1) %>%
  arrange(desc(Count))

print("Duplicates found:")
print(duplicates)

# Filter for operating status
operating_data <- combined_data %>%
  filter(Status == "Operating")

# Summarize the data by Country and Year_Snapshot
operating_summary <- operating_data %>%
  group_by(Country, Year_Snapshot) %>%
  summarise(Total_Capacity_MW = sum(Capacity..MW., na.rm = TRUE), .groups = 'drop')

# Print the summarized data to check if it looks correct
print("Operating Summary:")
print(operating_summary)

# Create a bar graph to show coal power capacity by country for the combined dataset
ggplot(operating_summary, aes(x = Country, y = Total_Capacity_MW, fill = Year_Snapshot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Total_Capacity_MW, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +  # Add text labels with values on bars
  labs(title = "Operating Coal Power Capacity by Country",
       x = "Country",
       y = "Total Capacity (MW)",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +  # Rotate x-axis labels
  theme(panel.grid.major.x = element_line(color = "grey80"),  # Add vertical grid lines
        panel.grid.major.y = element_line(color = "grey80"))  # Add horizontal grid lines

# Create a bar graph to show coal power capacity by country with a logarithmic scale
ggplot(operating_summary, aes(x = Country, y = Total_Capacity_MW, fill = Year_Snapshot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Operating Coal Power Capacity by Country",
       x = "Country",
       y = "Total Capacity (MW, log scale)",
       fill = "Year") +
  scale_y_log10() +  # Apply logarithmic scale to y-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Rotate and adjust x-axis labels
        panel.grid.major.x = element_line(color = "grey80"),  # Add vertical grid lines
        panel.grid.major.y = element_line(color = "grey80"))  # Add horizontal grid lines

#############################################################################


# Filter for plants under development
development_statuses <- c("Construction", "Permitted", "Pre-Permit", "Announced")
development_data <- combined_data %>%
  filter(Status %in% development_statuses)

# Summarize the data by Country and Year_Snapshot
development_summary <- development_data %>%
  group_by(Country, Year_Snapshot) %>%
  summarise(Total_Capacity_MW = sum(Capacity..MW., na.rm = TRUE), .groups = 'drop')

# Print the summarized data to check if it looks correct
print("Development Summary:")
print(development_summary)

# Create a bar graph to show coal power capacity by country for the combined dataset
ggplot(development_summary, aes(x = Country, y = Total_Capacity_MW, fill = Year_Snapshot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Total_Capacity_MW, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +  # Adjust text label size
  labs(title = "Coal Power Capacity Under Development by Country",
       x = "Country",
       y = "Total Capacity (MW, log scale)",
       fill = "Year") +
  scale_y_log10() +  # Apply logarithmic scale to y-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Rotate and adjust x-axis labels
        panel.grid.major.x = element_line(color = "grey80"),  # Add vertical grid lines
        panel.grid.major.y = element_line(color = "grey80"))  # Add horizontal grid lines

# same graph as above without labels
ggplot(development_summary, aes(x = Country, y = Total_Capacity_MW, fill = Year_Snapshot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Coal Power Capacity Under Development by Country",
       x = "Country",
       y = "Total Capacity (MW, log scale)",
       fill = "Year") +
  scale_y_log10() +  # Apply logarithmic scale to y-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Rotate and adjust x-axis labels
        panel.grid.major.x = element_line(color = "grey80"),  # Add vertical grid lines
        panel.grid.major.y = element_line(color = "grey80"))  # Add horizontal grid lines

# Filter for retired plants
retired_data <- combined_data %>%
  filter(Status == "Retired")

# Summarize the data by Country and Year_Snapshot for retired capacity
retired_summary <- retired_data %>%
  group_by(Country, Year_Snapshot) %>%
  summarise(Total_Capacity_MW = sum(Capacity..MW., na.rm = TRUE), .groups = 'drop')


# Create a bar graph to show retired coal power capacity by country
ggplot(retired_summary, aes(x = Country, y = Total_Capacity_MW, fill = Year_Snapshot)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Retired Coal Power Capacity by Country",
       x = "Country",
       y = "Total Capacity (MW, log scale)",
       fill = "Year") +
  scale_y_log10() +  # Apply logarithmic scale to y-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Rotate and adjust x-axis labels
        panel.grid.major.x = element_line(color = "grey80"),  # Add vertical grid lines
        panel.grid.major.y = element_line(color = "grey80"))  # Add horizontal grid lines

#####################

# Create a Map to visualize where power plants are located

# Filter out rows with missing latitude or longitude
combined_data <- combined_data %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Create a leaflet map
leaflet(data = combined_data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    color = ~ifelse(Status == "Operating", "blue", "red"),
    popup = ~paste("Plant:", Plant, "<br>",
                   "Country:", Country, "<br>",
                   "Status:", Status, "<br>",
                   "Capacity (MW):", Capacity..MW., "<br>",
                   "Start Year:", Start.year),
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.8
  ) %>%
  addLegend(
    "bottomright",
    colors = c("blue", "red"),
    labels = c("Operating", "Non-Operating"),
    title = "Plant Status"
  ) %>%
  addProviderTiles(providers$CartoDB.Positron)
