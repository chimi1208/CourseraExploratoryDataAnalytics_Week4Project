###########################################
#
# Coursera: Exploratory Data Analysis
# Course Project
#
# Plot 2
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


# Use Average by year and understand trend line
mean_Emissions <- tapply(NEI_BaltCity$Emissions, NEI_BaltCity$year, mean)
plot(names(mean_Emissions), mean_Emissions, 
     lwd = 1, pch = 15, type = "b", lty = 1, 
     main = "Pollution Trend by Year For Baltimore City Maryland", ylab = "Average Pollution", xlab = "Year")
    axis(1, at = c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008))

regress <- lm(mean_Emissions ~ as.numeric(names(mean_Emissions)))
abline(regress, col = "red")

# Appears to be a downward trend