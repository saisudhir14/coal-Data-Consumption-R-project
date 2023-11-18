
#Loading the Coal Consumption dataset
# Read the CSV file manually
coalConsumptionData <- read.csv("C:/Users/sudhi/Downloads/BIS_581/CoalConsumption_R_Project/annual-coal-consumption-by-country-1980-2009-2.csv")
View(coalConsumptionData)

# Rename the first column to "Region"
colnames(coalConsumptionData)[1] <- "Region"
View(coalConsumptionData)

# Display summary statistics of the dataset
summary(coalConsumptionData)



# Loading the required libraries
# Load the library for pivot_longer and pivot_wider functions.
library(tidyverse)  
library(dplyr)

# Transforming the dataset into a long format using pivot_longer
coalLongData <- pivot_longer(coalConsumptionData, !Region, names_to = "Year", values_to = "Consumption")
coalLongData

View(coalLongData)

# Observing mixed data types in the 'Year' column
# Utilizing transform and gsub to remove "X" from the 'Year' column
coalLongData <- transform(coalLongData, Year = gsub("X", "", Year))
coalLongData

View(coalLongData)

# Converting 'Year' to a numeric format
coalLongData$Year <- as.numeric(as.character(coalLongData$Year))
coalLongData
is.numeric(coalLongData$Year)

# Converting 'Consumption' from character to numeric
# Checking if 'Consumption' is numeric
is.numeric(coalLongData$Consumption)

# Checking if 'Consumption' is in character format
is.character(coalLongData$Consumption)

# Converting 'Consumption' to numeric
coalLongData$Consumption <- as.numeric(coalLongData$Consumption)
is.numeric(coalLongData$Consumption)
summary(coalLongData)
View(coalLongData)

# Reviewing the class types for all columns
sapply(coalLongData, class)
summary(coalLongData)

# Removing NA values
install.packages("janitor")
library(janitor)
View(coalLongData)

tidyCoalData <- coalLongData
View(tidyCoalData)

summary(tidyCoalData)

library(tidyr)

cleanCoalData <- tidyCoalData
cleanCoalData <- cleanCoalData %>%
  drop_na()

tidyCoalData
sum(is.na(cleanCoalData))
summary(cleanCoalData)

processedCoalData <- cleanCoalData

distinct(coalConsumptionData)
# Separating continents, regions, and countries for further analysis

continentLabels <- c("Africa", "Asia", "Europe", "North America", "Central & South America", "Former U.S.S.R.",
                     "Middle East", "Central African Republic", "Asia & Oceania", "Antarctica")

# Using filtering to categorize

continentData <- filter(processedCoalData, Region %in% continentLabels)
continentData

View(continentData)

otherRegionData <- processedCoalData %>%
  filter(!(Region %in% continentLabels))
otherRegionData

countrySpecificData <- otherRegionData[!(otherRegionData$Region %in% "World"),]
View(countrySpecificData)
View(continentData)


library(ggplot2)


ggplot(continentData, aes(x = as.numeric(Year), y = Consumption, color = Region)) +
  geom_line(size = 1.2, alpha = 0.8, linetype = "solid") +  # Adjusting line appearance
  labs(
    title = "Coal Consumption Over Years by Region",
    x = "Year",
    y = "Consumption"
  ) +
  theme_minimal()



# Boxplot of coal consumption by year
#ggplot(otherRegionData, aes(x = as.numeric(Year), y = Consumption)) +
#  geom_boxplot() +
#  labs(
#    title = "Boxplot of Coal Consumption by Year",
#    x = "Year",
#    y = "Consumption"
#  ) +
#  theme_minimal()


# Faceted histograms by region
ggplot(continentData, aes(x = Consumption)) +
  geom_histogram(bins = 10) +
  facet_wrap(~Region) +
  labs(
    title = "Distribution of Coal Consumption by Region",
    x = "Consumption",
    y = "Frequency"
  ) +
  theme_minimal()

# Stacked bar chart of total consumption by year
totalConsumptionByYear <- aggregate(Consumption ~ Year + Region, data = continentData, FUN = sum)

ggplot(totalConsumptionByYear, aes(x = as.factor(Year), y = Consumption, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Stacked bar chart showing Total Coal Consumption by Year",
    x = "Year",
    y = "Total Consumption"
  ) +
  theme_minimal()



ggplot(continentData, aes(x = as.numeric(Year), y = Consumption, fill = as.factor(Year))) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Violin Plot of Coal Consumption by Year",
    x = "Year",
    y = "Consumption"
  ) +
  theme_minimal()


library(ggplot2)
library(dplyr)

# Aggregate total consumption by region
totalConsumptionByRegion <- continentData %>%
  group_by(Region) %>%
  summarise(TotalConsumption = sum(Consumption)) %>%
  arrange(desc(TotalConsumption))

# Create bar chart
ggplot(totalConsumptionByRegion, aes(x = reorder(Region, -TotalConsumption), y = TotalConsumption)) +
  geom_bar(stat = "identity", fill = "violet") +
  labs(
    title = "Total Consumption by Region",
    x = "Region",
    y = "Total Consumption"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#HeatMap
#-- did not work as expected as per ggplot website documentation
# Install tinytex package
install.packages('tinytex')

# Install TinyTeX
tinytex::install_tinytex()

# Confirm that TinyTeX is installed
tinytex:::is_tinytex()




