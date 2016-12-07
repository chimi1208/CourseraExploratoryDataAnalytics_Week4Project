###########################################
#
# Coursera: Exploratory Data Analysis
# Course Project
#
# Plot 3
###########################################



## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

##  NEI Data Fields:
##  fips: A five-digit number (represented as a string) indicating the U.S. county
##  SCC: The name of the source as indicated by a digit string (see source code classification table)
##  Pollutant: A string indicating the pollutant
##  Emissions: Amount of PM2.5 emitted, in tons
##  type: The type of source (point, non-point, on-road, or non-road)
##  year: The year of emissions recorded

## Assess scope:
# Years
# unique(NEI$year) 
#[1] 1999 2002 2005 2008
## ALready limited to the requested years

# Pollutants
# unique(NEI$Pollutant)
# [1] "PM25-PRI"
# Only pm25

# EXtract Baltimore City
NEI_BaltCity <- NEI[NEI$fips == "24510",]


## Create Subsets of each "type" variable from teh Baltimore City Extract
## > unique(NEI$type)
## [1] "POINT"    "NONPOINT" "ON-ROAD"  "NON-ROAD"
NEI_Point <- NEI_BaltCity[NEI_BaltCity$type =="POINT",]
NEI_NonPoint <- NEI_BaltCity[NEI_BaltCity$type =="NONPOINT",]
NEI_OnRoad <- NEI_BaltCity[NEI_BaltCity$type =="ON-ROAD",]
NEI_NonRoad <- NEI_BaltCity[NEI_BaltCity$type =="NON-ROAD",]

# Use Average by year and understand trend line
mean_Emissions_Point <- tapply(NEI_Point$Emissions, NEI_Point$year, mean)
mean_Emissions_NonPoint <- tapply(NEI_NonPoint$Emissions, NEI_NonPoint$year, mean)
mean_Emissions_OnRoad <- tapply(NEI_OnRoad$Emissions, NEI_OnRoad$year, mean)
mean_Emissions_NonRoad <- tapply(NEI_NonRoad$Emissions, NEI_NonRoad$year, mean)

# Transform data for plots
mean_Emission_Complete <- rbind(mean_Emissions_Point, mean_Emissions_NonPoint, mean_Emissions_OnRoad, mean_Emissions_NonRoad)
df_mec_t <- data.frame(t(mean_Emission_Complete))
df_mec_complete <- cbind(df_mec_t, seq(1999,2008,3))
names(df_mec_complete)[5] <- "Year"

df_graphing_Point <- cbind(df_mec_complete[,1], names(df_mec_complete[1]), df_mec_complete$Year)
df_graphing_NonPoint <- cbind(df_mec_complete[,2], names(df_mec_complete[2]), df_mec_complete$Year)
df_graphing_OnRoad <- cbind(df_mec_complete[,3], names(df_mec_complete[3]), df_mec_complete$Year)
df_graphing_NonRoad <- cbind(df_mec_complete[,4], names(df_mec_complete[4]), df_mec_complete$Year)

df_graphing <- data.frame(rbind(df_graphing_NonPoint, df_graphing_NonRoad,df_graphing_Point, df_graphing_OnRoad)) # Creates formatted Data Frame
names(df_graphing) <- c("Mean.Emissions", "Measurement", "Year")

g <- ggplot(df_graphing, aes(Year, Mean.Emissions))
g + geom_point() +facet_grid(.~Measurement) + labs(title = "Mean Emissions by Measurement Type")




