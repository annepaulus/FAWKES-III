############################################################################
### Purpose of this skript module 01 is to:
### 
### 01.1. load all libraries needed for subsequent analysis
### 01.2. load all self-written functions needed for subsequent analysis
###
### Authors: MB ...
############################################################################

############################################################################
### 01.1. load all libraries needed for subsequent analysis, 
### automatically installs if libraries are missing
### Please add all libraries needed here
############################################################################

needed_libs <- c("devtools",# needed for library googlesheets
                 "googlesheets",# for loading data directly from google
                 "ggplot2",# For plotting
                 "raster",# for adding map data
                 "rgdal", # for loading map data
                 "rgeos", # dependency for rgdal
                 "letsR", # for macroecological data analysis (IUCN status)
                 "tm", # for text mining
                 "SnowballC", # for text mining
                 "XLConnect", # for reading in data from online Excel files (pressures data)
                 "igraph", # for network diagrams
                 "treemap", # for visualising the threats and services
                 "cooccur", # for calculating co-occurrence statistics for services
                 "ordinal" # for multinomial modelling
)
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
sapply(needed_libs,usePackage)

rm(needed_libs)

############################################################################
### 01.2. load all self-written functions needed for subsequent analysis
###
### to be added
############################################################################