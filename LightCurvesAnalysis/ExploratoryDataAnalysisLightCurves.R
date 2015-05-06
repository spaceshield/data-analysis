# Clear Workspace
rm(list = ls())

# Necessary libraries
library(caret)
library(plyr)
library(Metrics)
library(mice)
library(RANN)
library(RColorBrewer)

# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("C:/Users/User/Documents/R/SpaceApps1/")

# Load Data
load(file="DiameterTrain.rda")
load(file="DiameterTest.rda")
load("RawData.rda")
load("ProcessedData.rda")
load("TrainData.rda")
load("TestData.rda")

# Exploratory Data Analysis of spins
# Plotting min amplitude
png(filename = "SpinsMin.png", width = 1000, height = 1000)
qplot(spin_min_amplitude, data=RawData, geom="histogram")
dev.off()

# Plotting max amplitude
png(filename = "SpinsMax.png", width = 1000, height = 1000)
qplot(spin_max_amplitude, data=RawData, geom="histogram")
dev.off()

# Plotting difference between max and min amplitudes
SpinDiff = RawData$spin_max_amplitude - RawData$spin_min_amplitude
png(filename = "SpinsDifference.png", width = 1000, height = 1000)
qplot(SpinDiff, geom="histogram")
dev.off()

# Plotting spin period. Not very informative plot, better use summary
png(filename = "SpinsPeriod.png", width = 1000, height = 1000)
qplot(spin_period, data=RawData, geom="histogram")
dev.off()
# Summary
summary(RawData$spin_period)

# There are 275 asteroids with unknown spin_period:
length(unique(RawData$objectnumber[is.na(RawData$spin_period)]))



# Exploratory Data Analysis of Diameters
# Plotting diameter distribution
png(filename = "DiameterDist.png", width = 1000, height = 1000)
qplot(diamNotNormalised, data=DiameterTrain, geom="histogram")
dev.off()

png(filename = "DiameterLogDist.png", width = 1000, height = 1000)
qplot(diamLOG, data=DiameterTrain, geom="histogram")
dev.off()


# ExploratoryDataAnalysis of variables important for orbit type prediction
# Plotting Orbit Vs Neo Vs Perihelion
TrainData$neo = factor(TrainData$neo, levels = c(TRUE, FALSE))

# Create a custom color scale
myColors <- brewer.pal(2,"Set2")
names(myColors) <- levels(TrainData$pha)
colScale <- scale_colour_manual(name = "pha",values = myColors)

# Plot
png(filename = "OrbitVsNeoVsPerihelion.png", width = 600, height = 600)
q <- qplot(orbit_type, perihelion_distance, data = TrainData,
           facets = .~neo, color=pha, geom="point", size=20,legend.key.size = 30) 
q + colScale
dev.off()