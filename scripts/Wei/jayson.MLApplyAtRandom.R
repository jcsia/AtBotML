#### Clear environment
rm(list=ls())

#### Set working directory
setwd("/Users/jayson/Research/Projects/AtBotML/scripts/Wei/")

#### Load libraries
# libraries for plots
library(ggplot2)
library(ggpubr)
library(patchwork)
library(reshape2)
library(RColorBrewer)
# display.brewer.all()

theme_set(theme_pubr())

#libraries for statistical analysis
# library("agricolae")

#### Functions

#### Source
# source("jayson.MLApplyAtPsyFunctions.R")

#### Execute
# d <- AtPsy.ClassError.data()
d <- read.csv("/Users/jayson/Research/Projects/AtBotML/results/03_random/f100/AtOnly_f100_avg_ce_data1.csv")
d

p <- ggballoonplot(d, x="Model", y="ClassError", size="Count",
                   fill="Count", yticks.by = 2,
                   ggtheme = theme_bw(base_size = 12)) +
     scale_fill_viridis_c()
p
pdf(file="results/MLSummary/MLApply/AtRandom/AtRandomClassErrorHostBalloon_Data1.pdf",
    pointsize=10,height=4, width=6)
p
dev.off()

# Scl
d <- read.csv("/Users/jayson/Research/Projects/AtBotML/results/03_random/f100/AtOnly_f100_avg_ce_data2.csv")
d

p <- ggballoonplot(d, x="Model", y="ClassError", size="Count",
                   fill="Count", yticks.by = 2,
                   ggtheme = theme_bw(base_size = 12)) +
     scale_fill_viridis_c()
p
pdf(file="results/MLSummary/MLApply/AtRandom/AtRandomClassErrorHostBalloon_Data2.pdf",
    pointsize=10,height=4, width=6)
p
dev.off()

# Bacteria
d <- read.csv("/Users/jayson/Research/Projects/AtBotML/results/03_random/f100/AtOnly_f100_avg_ce_data3.csv")
d

p <- ggballoonplot(d, x="Model", y="ClassError", size="Count",
                   fill="Count", yticks.by = 2,
                   ggtheme = theme_bw(base_size = 12)) +
     scale_fill_viridis_c()
p
pdf(file="results/MLSummary/MLApply/AtRandom/AtRandomClassErrorHostBalloon_Data3.pdf",
    pointsize=10,height=4, width=6)

p
dev.off()

# boxplot Bc
d <- read.csv("/Users/jayson/Research/Projects/AtBotML/results/03_random/f100/random100_acc_data1.csv")
paste("mean accuracy At-Bc", mean(d$Accuracy))

relabel <- c('1_SVM' = 'SVM', '2_SVM_LIN' = 'linSVM', '3_RF' = 'RF', '4_XGB' = 'XGB',
             '5_DNN' = 'DNN')

p1 <- ggplot(d, aes(x=Model, y=Accuracy,fill=Model)) +
    geom_boxplot(color="black",width=0.5,outlier.shape = NA,alpha=0.3) +
    geom_jitter(shape=16, size=0.5,position=position_jitter(0.15),alpha=0.6) +
    # scale_x_discrete(name ="Model", limits=c("SVM","SVM_LIN","RF", "XGB", "DNN")) +
    ylim(0.4, 0.7) +
    scale_fill_brewer(palette="Dark2") +
    scale_x_discrete(name ="Model", labels = relabel) +
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    theme(axis.title.x = element_blank())
p1
pdf(file="results/MLSummary/MLApply/AtRandom/AtRandomAccModel_Data1.pdf",
    pointsize=10, height=2.5, width=2)
p1
dev.off()

# boxplot Scl
d <- read.csv("/Users/jayson/Research/Projects/AtBotML/results/03_random/f100/random100_acc_data2-new.csv")
paste("mean accuracy At-Scl", mean(d$Accuracy))


relabel <- c('1_SVM' = 'SVM', '2_SVM_LIN' = 'linSVM', '3_RF' = 'RF', '4_XGB' = 'XGB',
             '5_DNN' = 'DNN')

p1 <- ggplot(d, aes(x=Model, y=Accuracy,fill=Model)) +
    geom_boxplot(color="black",width=0.5,outlier.shape = NA,alpha=0.3) +
    geom_jitter(shape=16, size=0.5,position=position_jitter(0.15),alpha=0.6) +
    ylim(0, 1) +
    scale_fill_brewer(palette="Dark2") +
    scale_x_discrete(name ="Model", labels = relabel) +
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    theme(axis.title.x = element_blank())
p1
pdf(file="results/MLSummary/MLApply/AtRandom/AtRandomAccModel_Data2.pdf",
    pointsize=10, height=2.5, width=2)
p1
dev.off()

# boxplot Psy
d <- read.csv("/Users/jayson/Research/Projects/AtBotML/results/03_random/f100/random100_acc_data3.csv")
paste("mean accuracy At-Psy", mean(d$Accuracy))

relabel <- c('1_SVM' = 'SVM', '2_SVM_LIN' = 'linSVM', '3_RF' = 'RF', '4_XGB' = 'XGB',
             '5_DNN' = 'DNN')

p1 <- ggplot(d, aes(x=Model, y=Accuracy,fill=Model)) +
    geom_boxplot(color="black",width=0.5,outlier.shape = NA,alpha=0.3) +
    geom_jitter(shape=16, size=0.5,position=position_jitter(0.15),alpha=0.6) +
    ylim(0, 0.8) +
    scale_fill_brewer(palette="Dark2") +
    scale_x_discrete(name ="Model", labels = relabel) +
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    theme(axis.title.x=element_blank())
p1
pdf(file="results/MLSummary/MLApply/AtRandom/AtRandomAccModel_Data3.pdf",
    pointsize=10,height=2.5, width=2)
p1
dev.off()

# AtPsy.ClassError.plot()
