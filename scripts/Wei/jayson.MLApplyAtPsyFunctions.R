## --------
# libraries for plots
library(ggplot2)
library(ggpubr)
library(patchwork)
library(reshape2)
library(RColorBrewer)
# display.brewer.all()

theme_set(theme_pubr())

#libraries for statistical analysis
library("agricolae")

AtPsy.ClassError.data <- function(){
  d1 <- read.csv("results/MLSummary/MLApply/AtPsy/test3_dnn_predictions.csv")
  d1 <- d1[,-1]
  colnames(d1)
  rownames(d1) <- d1[,4]
  c <- c("Pathosystem","Strain","HostGeno","Sample","BacGrowth","Class","HostImmunityRank","PathoVirulenceRank",
         "Tier1","Dual_29101","Host_20340","XGB_100","RF_100","Degree_100","Btwns_100","NFD_100","FDC_100","Bipartite_711",
         "Defence_130","GOBiotic_912","PosCorr_786","NegCorr_762","TopCorr_1548","XGB_best_500","RF_best_100","Bipartite_best_2226",
         "Degree_best_500","Degree_10","Btwns_best_500","Btwns_10","NFD_10","NFD_best_3000","FDC_best_3000","FDC_10",
         "PosCorr_100","NegCorr_100","TopCorr_100")
  colnames(d1)

  x1 <- d1[,-c(1,2,3,4,5,7,8,9)]
  colnames(x1)
  x1[2:29] <- x1[2:29]-x1[,1]

  d1 <- merge(x1,d1,by= "row.names")
  d1 <- d1[,-1]
  colnames(d1)

  res1 <- NULL
  for (i in 2:29) {
    x <- cbind(d1[,c(30:34,36:38)],Model="DNN",FS_Method=colnames(d1[i]),ObserveClass=d1[,1],PredictClass=d1[,i+37],ClassError=d1[,i])
    res1 <-rbind(res1,x)
  }
  dat1 <- res1

  d2 <- read.csv("results/MLSummary/MLApply/AtPsy/test3_rf_predictions.csv")
  d2 <- d2[,-1]
  colnames(d2)
  rownames(d2) <- d2[,4]
  c <- c("Pathosystem","Strain","HostGeno","Sample","BacGrowth","Class","HostImmunityRank","PathoVirulenceRank",
         "Tier1","Dual_29101","Host_20340","XGB_100","RF_100","Degree_100","Btwns_100","NFD_100","FDC_100","Bipartite_711",
         "Defence_130","GOBiotic_912","PosCorr_786","NegCorr_762","TopCorr_1548","XGB_best_500","RF_best_100","Bipartite_best_2226",
         "Degree_best_500","Degree_10","Btwns_best_500","Btwns_10","NFD_10","NFD_best_3000","FDC_best_3000","FDC_10",
         "PosCorr_100","NegCorr_100","TopCorr_100")
  colnames(d2) <- c

  x2 <- d2[,-c(1,2,3,4,5,7,8,9)]
  colnames(x2)
  x2[2:29] <- x2[2:29]-x2[,1]

  d2 <- merge(x2,d2,by= "row.names")
  d2 <- d2[,-1]
  colnames(d2)

  res2 <- NULL
  for (i in 2:29) {
    x <- cbind(d2[,c(30:34,36:38)],Model="RF",FS_Method=colnames(d2[i]),ObserveClass=d2[,1],PredictClass=d2[,i+37],ClassError=d2[,i])
    res2 <-rbind(res2,x)
  }
  dat2 <- res2

  d3 <- read.csv("results/MLSummary/MLApply/AtPsy/test3_svm_lin_predictions.csv")
  d3 <- d3[,-1]
  colnames(d3)
  rownames(d3) <- d3[,4]
  c <- c("Pathosystem","Strain","HostGeno","Sample","BacGrowth","Class","HostImmunityRank","PathoVirulenceRank",
         "Tier1","Dual_29101","Host_20340","XGB_100","RF_100","Degree_100","Btwns_100","NFD_100","FDC_100","Bipartite_711",
         "Defence_130","GOBiotic_912","PosCorr_786","NegCorr_762","TopCorr_1548","XGB_best_500","RF_best_100","Bipartite_best_2226",
         "Degree_best_500","Degree_10","Btwns_best_500","Btwns_10","NFD_10","NFD_best_3000","FDC_best_3000","FDC_10",
         "PosCorr_100","NegCorr_100","TopCorr_100")
  colnames(d3) <- c

  x3 <- d3[,-c(1,2,3,4,5,7,8,9)]
  colnames(x3)
  x3[2:29] <- x3[2:29]-x3[,1]

  d3 <- merge(x3,d3,by= "row.names")
  d3 <- d3[,-1]
  colnames(d3)

  res3 <- NULL
  for (i in 2:29) {
    x <- cbind(d3[,c(30:34,36:38)],Model="linSVM",FS_Method=colnames(d3[i]),ObserveClass=d3[,1],PredictClass=d3[,i+37],ClassError=d3[,i])
    res3 <-rbind(res3,x)
  }
  dat3 <- res3

  d4 <- read.csv("results/MLSummary/MLApply/AtPsy/test3_svm_predictions.csv")
  d4 <- d4[,-1]
  colnames(d4)
  rownames(d4) <- d4[,4]
  c <- c("Pathosystem","Strain","HostGeno","Sample","BacGrowth","Class","HostImmunityRank","PathoVirulenceRank",
         "Tier1","Dual_29101","Host_20340","XGB_100","RF_100","Degree_100","Btwns_100","NFD_100","FDC_100","Bipartite_711",
         "Defence_130","GOBiotic_912","PosCorr_786","NegCorr_762","TopCorr_1548","XGB_best_500","RF_best_100","Bipartite_best_2226",
         "Degree_best_500","Degree_10","Btwns_best_500","Btwns_10","NFD_10","NFD_best_3000","FDC_best_3000","FDC_10",
         "PosCorr_100","NegCorr_100","TopCorr_100")
  colnames(d4) <- c

  x4 <- d4[,-c(1,2,3,4,5,7,8,9)]
  colnames(x4)
  x4[2:29] <- x4[2:29]-x4[,1]

  d4 <- merge(x4,d4,by= "row.names")
  d4 <- d4[,-1]
  colnames(d4)

  res4 <- NULL
  for (i in 2:29) {
    x <- cbind(d4[,c(30:34,36:38)],Model="SVM",FS_Method=colnames(d4[i]),ObserveClass=d4[,1],PredictClass=d4[,i+37],ClassError=d4[,i])
    res4 <-rbind(res4,x)
  }
  dat4 <- res4

  d5 <- read.csv("results/MLSummary/MLApply/AtPsy/test3_xgb_predictions.csv")
  d5 <- d5[,-1]
  colnames(d5)
  rownames(d5) <- d5[,4]
  c <- c("Pathosystem","Strain","HostGeno","Sample","BacGrowth","Class","HostImmunityRank","PathoVirulenceRank",
         "Tier1","Dual_29101","Host_20340","XGB_100","RF_100","Degree_100","Btwns_100","NFD_100","FDC_100","Bipartite_711",
         "Defence_130","GOBiotic_912","PosCorr_786","NegCorr_762","TopCorr_1548","XGB_best_500","RF_best_100","Bipartite_best_2226",
         "Degree_best_500","Degree_10","Btwns_best_500","Btwns_10","NFD_10","NFD_best_3000","FDC_best_3000","FDC_10",
         "PosCorr_100","NegCorr_100","TopCorr_100")
  colnames(d5) <- c

  x5 <- d5[,-c(1,2,3,4,5,7,8,9)]
  colnames(x5)
  x5[2:29] <- x5[2:29]-x5[,1]

  d5 <- merge(x5,d5,by= "row.names")
  d5 <- d5[,-1]
  colnames(d5)

  res5 <- NULL
  for (i in 2:29) {
    x <- cbind(d5[,c(30:34,36:38)],Model="XGB",FS_Method=colnames(d5[i]),ObserveClass=d5[,1],PredictClass=d5[,i+37],ClassError=d5[,i])
    res5 <-rbind(res5,x)
  }
  dat5 <- res5

  d <- rbind(dat1,dat2,dat3,dat4,dat5)
  colnames(d)
  d$FS_Method <- gsub(".x","",d$FS_Method)
  d <- d[!d$FS_Method=="Dual_29101",]


  write.csv(d,"results/MLSummary/MLApply/AtPsy/ClassErroSummary.csv",row.names = FALSE)

  ########Import class error data frame
  d <- read.csv("results/MLSummary/MLApply/AtPsy/ClassErroSummary.csv")
  colnames(d)
  colnames(d)[8] <- "HI"

  d <- d[!(d$FS_Method== "Degree_10"|d$FS_Method== "Btwns_10"|d$FS_Method== "NFD_10"|d$FS_Method== "FDC_10"),]
  colnames(d)

  d$Strain <- factor(d$Strain, levels = c("Pto","Pto-AvrRpt2"))
  d$HostGeno <- factor(d$HostGeno, levels = c("Col-0 flg22 treatment","Col-0 Mock treatment","Col-0",
                                              "pad4-1","sid2-2","npr1-1","rpm1-3 rps2-101C","dde2-2 ein2-1",
                                              "pad4-1 sid2-2","dde2-2 ein2-1 pad4-1 sid2-2"))
  d$HI <- factor(d$HI,levels=c("flg22","Col-0","Single_mutant","Double_mutant","Quadruple_mutant"))
  d$Model <- factor(d$Model, levels = c("SVM","linSVM","RF","XGB","DNN"))
  d$FS_Method <- factor(d$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
                                                "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
                                                "RF_100","RF_best_100","XGB_100","XGB_best_500",
                                                "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
                                                "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
                                                "Bipartite_711","Bipartite_best_2226"))
  #Accuracy
  # model <- aov(d$ClassError ~ d$HI+d$Strain+d$Model+d$FS_Method)
  model <- aov(d$ClassError ~ d$HI*d$Strain*d$Model*d$FS_Method)
  summary(model)

  return (d)


}

AtPsy.ClassError.plot <- function () {
  ##############Machine learning methods by Class error
  p <- ggplot(d, aes(x=Model, y=ClassError,fill=Model)) +
    geom_violin(width=1.2,alpha=0.5,position = position_dodge(0.9)) +
    geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.9)) +
    geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
    geom_hline(yintercept=1,linetype="dashed", color = "blue") +
    scale_fill_manual(values=c("#F8766C", "#39B600",
                               "#9591FF","#FF62BC","#13B0F5"))+
    scale_y_continuous(n.breaks = 10) +
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.title.x=element_blank())
  p

  # Strain
  p <- ggplot(d, aes(x=Strain, y=ClassError,fill=HI)) +
    geom_violin(width=1.2,alpha=0.5,position = position_dodge(0.9)) +
    geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.9)) +
    geom_hline(yintercept=-1,linetype="dashed", color = "black") +
    geom_hline(yintercept=1,linetype="dashed", color = "black") +
    scale_y_continuous(n.breaks = 12) +
    scale_fill_brewer(palette="Set1",
                      labels = c("flg22", "WT", "Single","Double","Quad")) +
    theme_classic() +
    theme(legend.position="top") +
    theme(axis.title.x=element_blank())

  pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyClassErrorHost.pdf", pointsize=10,height=3, width=5)
  p
  dev.off()

  # Balloon - by strain
  ct <- with(d, table(Strain, HI, ClassError))
  ct.m <- melt(ct)
  ct.m2 <- ct.m[ct.m$value != 0, ]

  # write.csv(ct.m2, "test_psy1.csv")

  # ct.m2$HI <- gsub('Col-0', 'WT', ct.m2$HI)
  # ct.m2$HI <- gsub('Single_mutant', 'Single', ct.m2$HI)
  # ct.m2$HI <- gsub('Double_mutant', 'Double', ct.m2$HI)
  # ct.m2$HI <- gsub('Quadruple_mutant', 'Quad', ct.m2$HI)

  p <- ggballoonplot(ct.m2, x="HI", y="ClassError", size="value",
                       facet.by = "Strain", fill="value", yticks.by = 2,
                       ggtheme = theme_bw(base_size = 12)) +
         scale_fill_viridis_c()
  p
  pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyClassErrorHostBalloon.pdf",
      pointsize=10,height=4, width=6)
  p
  dev.off()


  p <- ggplot(d, aes(x=FS_Method, y=ClassError,fill=FS_Method)) +
    geom_boxplot(width=0.5,outlier.shape = NA,alpha=0.5) +
    geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
    geom_hline(yintercept=1,linetype="dashed", color = "blue") +
    scale_y_continuous(n.breaks = 10) +
    theme_classic() +
    theme(legend.position="top") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    theme(axis.title.x=element_blank())
  p

  p <- ggplot(d, aes(x=FS_Method, y=ClassError,fill=Strain)) +
    geom_boxplot(width=0.5,outlier.shape = NA,alpha=0.5) +
    geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
    geom_hline(yintercept=1,linetype="dashed", color = "blue") +
    scale_y_continuous(n.breaks = 10) +
    theme_classic() +
    theme(legend.position="top") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    theme(axis.title.x=element_blank())
  p
}

AtPsy.ConfusionMatrix <- function() {
  data <- read.csv("results/MLSummary/MLApply/AtPsy/AtPsy_FeatureSeleAll_Test3.csv")
  colnames(data)
  data$FS_Method
  data <- data[!(data$FS_Method=="Dual_29101"|data$FS_Method=="Pathogen_8761"
               |data$FS_Method== "Degree_10"|data$FS_Method== "Btwns_10"|data$FS_Method== "NFD_10"|data$FS_Method== "FDC_10"),]

  ########Plots
  data$Model <- factor(data$Model, levels = c("SVM","linSVM","RF","XGB","DNN"))
  data$FS_Method <- factor(data$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
                                              "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
                                              "RF_100","RF_best_100","XGB_100","XGB_best_500",
                                              "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
                                              "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
                                              "Bipartite_711","Bipartite_best_2226"))

  p <- ggplot(data, aes(x=Model, y=Accuracy,fill=Model)) +
    geom_boxplot(color="black",width=0.3,outlier.shape = NA,alpha=0.5) +
    geom_jitter(shape=16, size=2.5,position=position_jitter(0.15),alpha=0.5) +
    scale_fill_manual(values=c("#F8766C", "#39B600",
                                "#9591FF","#FF62BC","#13B0F5"))+
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.title.x=element_blank())
  p

  p <- ggplot(data, aes(x=FS_Method, y=Accuracy,fill=FS_Method)) +
    geom_boxplot(color="black",width=0.3,outlier.shape = NA,alpha=0.7) +
    geom_jitter(shape=16, size=2.5,position=position_jitter(0.15),alpha=0.7) +
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    theme(axis.title.x=element_blank())
  p
}

AtPsy.ConfusionMatrix.separate <- function() {
  x1 <- read.csv("results/MLSummary/MLApply/AtPsy/AtPsy_col-0_FeatureSelectAll_Test3.csv")
  x1 <- data.frame(x1, Immunity ="Col-0")

  x2 <- read.csv("results/MLSummary/MLApply/AtPsy/AtPsy_flg22_FeatureSelectAll_Test3.csv")
  x2 <- data.frame(x2, Immunity ="flg22")

  x3 <- read.csv("results/MLSummary/MLApply/AtPsy/AtPsy_mutant_1_FeatureSelectAll_Test3.csv")
  x3 <- data.frame(x3, Immunity ="Single_mutant")

  x4 <- read.csv("results/MLSummary/MLApply/AtPsy/AtPsy_mutant_2_FeatureSelectAll_Test3.csv")
  x4 <- data.frame(x4, Immunity ="Double_mutant")

  x5 <- read.csv("results/MLSummary/MLApply/AtPsy/AtPsy_mutant_4_FeatureSelectAll_Test3.csv")
  x5 <- data.frame(x5, Immunity ="Quadruple_mutant")
  x <- rbind(x1,x2,x3,x4,x5)
  unique(x$FS_Method)
  x <- x[!(x$FS_Method=="Dual_29101"|x$FS_Method== "Degree_10"|x$FS_Method== "Btwns_10"|x$FS_Method== "NFD_10"|x$FS_Method== "FDC_10"),]
  colnames(x)[1] <- "Model"

  ########Plots
  colnames(x)
  x$Model <- factor(x$Model, levels = c("SVM","linSVM","RF","XGB","DNN"))
  x$Immunity <- factor(x$Immunity, levels = c("flg22","Col-0","Single_mutant","Double_mutant","Quadruple_mutant"))
  x$FS_Method <- factor(x$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
                                                 "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
                                                      "RF_100","RF_best_100","XGB_100","XGB_best_500",
                                                      "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
                                                      "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
                                                      "Bipartite_711","Bipartite_best_2226"))

  p1 <- ggplot(x, aes(x=Immunity, y=Accuracy,fill=Immunity)) +
    # geom_violin(width=1,alpha=0.5,trim = FALSE, position = position_dodge(0.9)) +
    geom_violin(width=1, trim = FALSE, fill='#FFFFFF', position = position_dodge(0.9)) +
    geom_boxplot(width=0.15, fill='gray', outlier.shape = NA, position = position_dodge(0.9)) +
    scale_y_continuous(n.breaks = 10,limits = c(0, 1)) +
    # scale_fill_brewer(palette="Set1") +
    scale_x_discrete(labels=c("flg22"="flg22", "Col-0"="Col-0","Single_mutant"="Single",
                              "Double_mutant"="Double","Quadruple_mutant"="Quad")) +
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.title.x=element_blank()) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
  p1

  pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyAccuracyHostImmu.pdf", pointsize=10,height=2.5, width=3)
  p1
  dev.off()

  ##########Accuracy by pathogen
  y1 <- read.csv("results/MLSummary/MLApply/AtPsy/AtPsy_Pto_FeatureSelectAll_Test3.csv")
  y1 <- data.frame(y1, Virulence ="Pto")

  y2 <- read.csv("results/MLSummary/MLApply/AtPsy/AtPsy_Pto-AvrRpt2_FeatureSelectAll_Test3.csv")
  y2 <- data.frame(y2, Virulence ="Pto-AvrRpt2")

  y <- rbind(y1, y2)
  unique(y$FS_Method)
  y <- y[!(y$FS_Method=="Dual_29101"|y$FS_Method== "Degree_10"|y$FS_Method== "Btwns_10"|y$FS_Method== "NFD_10"|y$FS_Method== "FDC_10"),]

  colnames(y)[1] <- "Model"

  ########Plots
  y$Model <- factor(y$Model, levels = c("SVM","linSVM","RF","XGB","DNN"))
  y$Virulence <- factor(y$Virulence, levels = c("Pto","Pto-AvrRpt2"))
  y$FS_Method <- factor(y$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
                                                "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
                                                "RF_100","RF_best_100","XGB_100","XGB_best_500",
                                                "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
                                                "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
                                                "Bipartite_711","Bipartite_best_2226"))
  display.brewer.all()

  p2 <- ggplot(y, aes(x=Virulence, y=Accuracy,fill=Virulence)) +
    geom_violin(width=1,fill='#FFFFFF', trim = FALSE, position = position_dodge(0.9)) +
    geom_boxplot(width=0.15,outlier.shape = NA, fill='gray', position = position_dodge(0.9)) +
    scale_y_continuous(n.breaks = 10,limits = c(0, 1)) +
    # scale_fill_manual(values=c("#D95F02", "#E6AB02"))+
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.title.x=element_blank())
  p2
  pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyAccuracyPathogen.pdf", pointsize=10,height=2.2, width=2)
  p2
  dev.off()
}