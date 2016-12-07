###########################################
#
# Coursera: Exploratory Data Analysis
# Course Project
#
# Plot 6
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

# EXtract Baltimore City
NEI_BaltCity <- NEI[NEI$fips == "24510",]

# EXtract Los Angeles
NEI_LA <- NEI[NEI$fips == "06037",]


# Use Average by year and understand trend line
mean_Emissions_BC <- tapply(NEI_BaltCity$Emissions, NEI_BaltCity$year, mean)
mean_Emissions_LA <- tapply(NEI_LA$Emissions, NEI_LA$year, mean)

# Make a 1x1 plot with two lines to compare the cities
par(mfrow = c(1,1))

# Graph LA
plot(names(mean_Emissions_LA), mean_Emissions_LA, 
     lwd = 1, pch = 15, type = "b", lty = 1, 
     main = "Pollution Trend by Year", 
     ylab = "Average Pollution", xlab = "Year")
axis(1, at = c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008))
axis(2, at = c(0,20,40,60,80,100,120))
regress <- lm(mean_Emissions_LA ~ as.numeric(names(mean_Emissions_LA)))
abline(regress, col = "red")

# Add Baltimore
points(names(mean_Emissions_BC), mean_Emissions_BC, 
       type = "b", lwd = 1, pch = 3, lty = 1)

regress <- lm(mean_Emissions_BC ~ as.numeric(names(mean_Emissions_BC)))
abline(regress, col = "blue")

legend('topright', c("Los Angeles", "Baltimore City"), col = c("red", "blue"), lty = 1, lwd = 2)

## Los Angeles decreased a much higher rate and absolute amount compared to BC