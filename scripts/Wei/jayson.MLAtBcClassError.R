## ---------------------------
##
## MLAtBcClassError
##
## Displays the AtBc ML summary
##
## Author: Jayson Sia
##
## Date Created: 2022-06-14
##

## Copyright (c)
## Email: jsia@usc.edu
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

#### Clear environment
rm(list=ls())

#### Set working directory
setwd("/Users/jayson/Research/Projects/AtBotML/scripts/Wei/")

#### Load libraries

#### Source
source("jayson.MLAtBcFunctions.R")

#### Execute
df <- MLAtBcClassError.data()

# MLAtBcClass.Stat()

MLAtBcClassError.plot()