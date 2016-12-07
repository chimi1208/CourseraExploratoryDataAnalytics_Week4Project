###########################################
#
# Coursera: Exploratory Data Analysis
# Course Project
#
# Plot 4
###########################################


## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Extract Coal SCC Codes and restrict NEI data to those codes
coal <- SCC[grepl("Coal",SCC$EI.Sector),]
mrg <- merge(NEI, coal)


# Look for outliers.
summary(mrg$Emissions)
## Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.000     0.002     0.177    70.760     3.840 14270.000
### 75% below 3.8 << Checked pattern and consistent across all years
# Data set is heavily skewed to low side with some very large outliers.
# 70% of data emissions data < 2 and 1% > 1000 

# Use Average and Line chart to show

mean_Emissions <- tapply(mrg$Emissions, mrg$year, mean)
plot(names(mean_Emissions), mean_Emissions, 
     lwd = 1, pch = 15, type = "b", lty = 1, 
     main = "Pollution Trend by Year for Coal Sources", ylab = "Average Pollution", xlab = "Year")
axis(1, at = c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008))

# Average pollution is approximated steady from 2002 - 2008 but much lower level than 1999
