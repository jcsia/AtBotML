#### Clear environment
rm(list=ls())

#### Set working directory
setwd("/Users/jayson/Research/Projects/AtBotML/scripts/Wei/")

#### Load libraries

#### Source
source("jayson.MLApplyAtPsyFunctions.R")

#### Execute
d <- AtPsy.ClassError.data()

AtPsy.ClassError.plot()
