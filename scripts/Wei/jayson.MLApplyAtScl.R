#############################################################
## Plant-Pathogen Interaction Prediction ML
## jsia@usc.edu - Jun 2022
#############################################################

#### Clear environment
rm(list=ls())

#### Set working directory
setwd("/Users/jayson/Research/Projects/AtBotML/scripts/Wei/")

#### Load libraries

#### Source
source("jayson.MLApplyAtSclFunctions.R")

#### Execute
d <- AtScl.ClassError.data()

AtScl.ClassError.plot()



