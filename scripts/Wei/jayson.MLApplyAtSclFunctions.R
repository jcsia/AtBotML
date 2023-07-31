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

AtScl.ClassError.data <- function(){
    # dnn
    d1 <- read.csv("results/MLSummary/MLApply/AtScl/test2_dnn_predictions.csv")
    colnames(d1)[1] <- "Sample"
    rownames(d1) <- d1[,1]
    colnames(d1)
    c <- c("Sample","Dual_29101","Host_20340","XGB_100","RF_100","Degree_100","Btwns_100","NFD_100","FDC_100","Bipartite_711",
           "Defence_130","GOBiotic_912","PosCorr_786","NegCorr_762","TopCorr_1548","XGB_best_500","RF_best_100","Bipartite_best_2226",
           "Degree_best_500","Degree_10","Btwns_best_500","Btwns_10","NFD_best_3000","NFD_10","FDC_best_3000","FDC_10",
           "PosCorr_100","NegCorr_100","TopCorr_100","Pathogen_8761","Class")
    colnames(d1) <- c

    # (JCS) Limit to only 13 FS
    c <- c("Host_20340", "Defence_130", "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
           "RF_best_100", "XGB_best_500", "Degree_best_500", "Btwns_best_500", "NFD_best_3000", "FDC_best_3000",
           "Bipartite_best_2226", "Class")
    d1 <- d1[c]
    d1[1:13] <- d1[1:13]-d1[,14]
    d1 <-d1[1:13]

    # d1 <- d1[,-1]
    # d1[1:29] <- d1[1:29]-d1[,30]
    # d1 <-d1[1:29]
    d1 <- as.data.frame(t(d1))
    colnames(d1) <-c("Ctrl_1","Ctrl_2","Ctrl_3","Infection_1","Infection_2","Infection_3")
    d1$FS_Method <-rownames(d1)
    d1$Model <- "DNN"

    s1 <- cbind(d1[,c(1,7,8)],"Ctrl_1")
    colnames(s1) <- c("ClassError","FS_Method","Model","Treat")

    s2 <- cbind(d1[,c(2,7,8)],"Ctrl_2")
    colnames(s2) <- c("ClassError","FS_Method","Model","Treat")

    s3 <- cbind(d1[,c(3,7,8)],"Ctrl_3")
    colnames(s3) <- c("ClassError","FS_Method","Model","Treat")

    s4 <- cbind(d1[,c(4,7,8)],"Infection_1")
    colnames(s4) <- c("ClassError","FS_Method","Model","Treat")

    s5 <- cbind(d1[,c(5,7,8)],"Infection_2")
    colnames(s5) <- c("ClassError","FS_Method","Model","Treat")

    s6 <- cbind(d1[,c(6,7,8)],"Infection_3")
    colnames(s6) <- c("ClassError","FS_Method","Model","Treat")

    dat1 <- rbind(s1,s2,s3,s4,s5,s6)

    # rf
    d2 <- read.csv("results/MLSummary/MLApply/AtScl/test2_rf_predictions.csv")
    colnames(d2)[1] <- "Sample"
    rownames(d2) <- d2[,1]

    colnames(d2)
    c <- c("Sample","Dual_29101","Host_20340","XGB_100","RF_100","Degree_100","Btwns_100","NFD_100","FDC_100","Bipartite_711",
           "Defence_130","GOBiotic_912","PosCorr_786","NegCorr_762","TopCorr_1548","XGB_best_500","RF_best_100","Bipartite_best_2226",
           "Degree_best_500","Degree_10","Btwns_best_500","Btwns_10","NFD_best_3000","NFD_10","FDC_best_3000","FDC_10",
           "PosCorr_100","NegCorr_100","TopCorr_100","Pathogen_8761","Class")
    colnames(d2) <- c

    # (JCS) Limit to only 13 FS

    c <- c("Host_20340", "Defence_130", "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
           "RF_best_100", "XGB_best_500", "Degree_best_500", "Btwns_best_500", "NFD_best_3000", "FDC_best_3000",
           "Bipartite_best_2226", "Class")
    d2 <- d2[c]
    d2[1:13] <- d2[1:13]-d2[,14]
    d2 <-d2[1:13]

    # d2 <- d2[,-1]
    # d2[1:29] <- d2[1:29]-d2[,30]
    # d2 <-d2[1:29]
    d2 <- as.data.frame(t(d2))
    colnames(d2) <-c("Ctrl_1","Ctrl_2","Ctrl_3","Infection_1","Infection_2","Infection_3")
    d2$FS_Method <-rownames(d2)
    d2$Model <- "RF"

    s1 <- cbind(d2[,c(1,7,8)],"Ctrl_1")
    colnames(s1) <- c("ClassError","FS_Method","Model","Treat")

    s2 <- cbind(d2[,c(2,7,8)],"Ctrl_2")
    colnames(s2) <- c("ClassError","FS_Method","Model","Treat")

    s3 <- cbind(d2[,c(3,7,8)],"Ctrl_3")
    colnames(s3) <- c("ClassError","FS_Method","Model","Treat")

    s4 <- cbind(d2[,c(4,7,8)],"Infection_1")
    colnames(s4) <- c("ClassError","FS_Method","Model","Treat")

    s5 <- cbind(d2[,c(5,7,8)],"Infection_2")
    colnames(s5) <- c("ClassError","FS_Method","Model","Treat")

    s6 <- cbind(d2[,c(6,7,8)],"Infection_3")
    colnames(s6) <- c("ClassError","FS_Method","Model","Treat")

    dat2 <- rbind(s1,s2,s3,s4,s5,s6)

    # svm_lin
    d3 <- read.csv("results/MLSummary/MLApply/AtScl/test2_svm_lin_predictions.csv")

    colnames(d3)[1] <- "Sample"
    rownames(d3) <- d3[,1]

    colnames(d3)
    c <- c("Sample","Dual_29101","Host_20340","XGB_100","RF_100","Degree_100","Btwns_100","NFD_100","FDC_100","Bipartite_711",
           "Defence_130","GOBiotic_912","PosCorr_786","NegCorr_762","TopCorr_1548","XGB_best_500","RF_best_100","Bipartite_best_2226",
           "Degree_best_500","Degree_10","Btwns_best_500","Btwns_10","NFD_best_3000","NFD_10","FDC_best_3000","FDC_10",
           "PosCorr_100","NegCorr_100","TopCorr_100","Pathogen_8761","Class")
    colnames(d3) <- c


    # (JCS) Limit to only 13 FS

    c <- c("Host_20340", "Defence_130", "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
           "RF_best_100", "XGB_best_500", "Degree_best_500", "Btwns_best_500", "NFD_best_3000", "FDC_best_3000",
           "Bipartite_best_2226", "Class")
    d3 <- d3[c]
    d3[1:13] <- d3[1:13]-d3[,14]
    d3 <-d3[1:13]

    # d3 <- d3[,-1]
    # d3[1:29] <- d3[1:29]-d3[,30]
    # d3 <-d3[1:29]
    d3 <- as.data.frame(t(d3))
    colnames(d3) <-c("Ctrl_1","Ctrl_2","Ctrl_3","Infection_1","Infection_2","Infection_3")
    d3$FS_Method <-rownames(d3)
    d3$Model <- "linSVM"

    s1 <- cbind(d3[,c(1,7,8)],"Ctrl_1")
    colnames(s1) <- c("ClassError","FS_Method","Model","Treat")

    s2 <- cbind(d3[,c(2,7,8)],"Ctrl_2")
    colnames(s2) <- c("ClassError","FS_Method","Model","Treat")

    s3 <- cbind(d3[,c(3,7,8)],"Ctrl_3")
    colnames(s3) <- c("ClassError","FS_Method","Model","Treat")

    s4 <- cbind(d3[,c(4,7,8)],"Infection_1")
    colnames(s4) <- c("ClassError","FS_Method","Model","Treat")

    s5 <- cbind(d3[,c(5,7,8)],"Infection_2")
    colnames(s5) <- c("ClassError","FS_Method","Model","Treat")

    s6 <- cbind(d3[,c(6,7,8)],"Infection_3")
    colnames(s6) <- c("ClassError","FS_Method","Model","Treat")

    dat3 <- rbind(s1,s2,s3,s4,s5,s6)

    # svm
    d4 <- read.csv("results/MLSummary/MLApply/AtScl/test2_svm_predictions.csv")
    colnames(d4)[1] <- "Sample"
    rownames(d4) <- d4[,1]

    colnames(d4)
    c <- c("Sample","Dual_29101","Host_20340","XGB_100","RF_100","Degree_100","Btwns_100","NFD_100","FDC_100","Bipartite_711",
           "Defence_130","GOBiotic_912","PosCorr_786","NegCorr_762","TopCorr_1548","XGB_best_500","RF_best_100","Bipartite_best_2226",
           "Degree_best_500","Degree_10","Btwns_best_500","Btwns_10","NFD_best_3000","NFD_10","FDC_best_3000","FDC_10",
           "PosCorr_100","NegCorr_100","TopCorr_100","Pathogen_8761","Class")
    colnames(d4) <- c

    # (JCS) Limit to only 13 FS
    # d4 <- d4[,-1]
    c <- c("Host_20340", "Defence_130", "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
           "RF_best_100", "XGB_best_500", "Degree_best_500", "Btwns_best_500", "NFD_best_3000", "FDC_best_3000",
           "Bipartite_best_2226", "Class")
    d4 <- d4[c]
    d4[1:13] <- d4[1:13]-d4[,14]
    d4 <-d4[1:13]

    # d4 <- d4[,-1]
    # d4[1:29] <- d4[1:29]-d4[,30]
    # d4 <-d4[1:29]
    d4 <- as.data.frame(t(d4))
    colnames(d4) <-c("Ctrl_1","Ctrl_2","Ctrl_3","Infection_1","Infection_2","Infection_3")
    d4$FS_Method <-rownames(d4)
    d4$Model <- "SVM"

    s1 <- cbind(d4[,c(1,7,8)],"Ctrl_1")
    colnames(s1) <- c("ClassError","FS_Method","Model","Treat")

    s2 <- cbind(d4[,c(2,7,8)],"Ctrl_2")
    colnames(s2) <- c("ClassError","FS_Method","Model","Treat")

    s3 <- cbind(d4[,c(3,7,8)],"Ctrl_3")
    colnames(s3) <- c("ClassError","FS_Method","Model","Treat")

    s4 <- cbind(d4[,c(4,7,8)],"Infection_1")
    colnames(s4) <- c("ClassError","FS_Method","Model","Treat")

    s5 <- cbind(d4[,c(5,7,8)],"Infection_2")
    colnames(s5) <- c("ClassError","FS_Method","Model","Treat")


    s6 <- cbind(d4[,c(6,7,8)],"Infection_3")
    colnames(s6) <- c("ClassError","FS_Method","Model","Treat")

    dat4 <- rbind(s1,s2,s3,s4,s5,s6)

    # xgb
    d5 <- read.csv("results/MLSummary/MLApply/AtScl/test2_xgb_predictions.csv")
    colnames(d5)[1] <- "Sample"
    rownames(d5) <- d5[,1]

    colnames(d5)
    c <- c("Sample","Dual_29101","Host_20340","XGB_100","RF_100","Degree_100","Btwns_100","NFD_100","FDC_100","Bipartite_711",
           "Defence_130","GOBiotic_912","PosCorr_786","NegCorr_762","TopCorr_1548","XGB_best_500","RF_best_100","Bipartite_best_2226",
           "Degree_best_500","Degree_10","Btwns_best_500","Btwns_10","NFD_best_3000","NFD_10","FDC_best_3000","FDC_10",
           "PosCorr_100","NegCorr_100","TopCorr_100","Pathogen_8761","Class")
    colnames(d5) <- c

    # (JCS) Limit to only 13 FS

    c <- c("Host_20340", "Defence_130", "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
           "RF_best_100", "XGB_best_500", "Degree_best_500", "Btwns_best_500", "NFD_best_3000", "FDC_best_3000",
           "Bipartite_best_2226", "Class")
    d5 <- d5[c]
    d5[1:13] <- d5[1:13]-d5[,14]
    d5 <-d5[1:13]

    # d5 <- d5[,-1]
    # d5[1:29] <- d5[1:29]-d5[,30]
    # d5 <-d5[1:29]
    d5 <- as.data.frame(t(d5))
    colnames(d5) <-c("Ctrl_1","Ctrl_2","Ctrl_3","Infection_1","Infection_2","Infection_3")
    d5$FS_Method <-rownames(d5)
    d5$Model <- "XGB"

    s1 <- cbind(d5[,c(1,7,8)],"Ctrl_1")
    colnames(s1) <- c("ClassError","FS_Method","Model","Treat")

    s2 <- cbind(d5[,c(2,7,8)],"Ctrl_2")
    colnames(s2) <- c("ClassError","FS_Method","Model","Treat")

    s3 <- cbind(d5[,c(3,7,8)],"Ctrl_3")
    colnames(s3) <- c("ClassError","FS_Method","Model","Treat")

    s4 <- cbind(d5[,c(4,7,8)],"Infection_1")
    colnames(s4) <- c("ClassError","FS_Method","Model","Treat")

    s5 <- cbind(d5[,c(5,7,8)],"Infection_2")
    colnames(s5) <- c("ClassError","FS_Method","Model","Treat")

    s6 <- cbind(d5[,c(6,7,8)],"Infection_3")
    colnames(s6) <- c("ClassError","FS_Method","Model","Treat")

    dat5 <- rbind(s1,s2,s3,s4,s5,s6)

    d <- rbind(dat1,dat2,dat3,dat4,dat5)
    colnames(d)
    colnames(d)[4] <- "Sample"
    d$Treat <- d[,4]
    d$Treat <- gsub("_","",d$Treat)
    d$Treat <- gsub("[0-9]+","",d$Treat)

    d <- d[!(d$FS_Method=="Dual_29101"|d$FS_Method=="Pathogen_8761"|d$FS_Method== "Degree_10"
             |d$FS_Method== "Btwns_10"|d$FS_Method== "NFD_10"|d$FS_Method== "FDC_10"),]

    d$Model <- factor(d$Model, levels = c("SVM","linSVM","RF","XGB","DNN"))
    d$FS_Method <- factor(d$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
                                                  "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
                                                  "RF_100","RF_best_100","XGB_100","XGB_best_500",
                                                  "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
                                                  "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
                                                  "Bipartite_711","Bipartite_best_2226"))
    d$Treat <- factor(d$Treat,levels = c("Ctrl","Infection"))


    ######Statistical analysis on class error impacted by Feature selection method, machine learning models, and treatment
    #ClassError
    # model <- aov(d$ClassError ~ d$FS_Method*d$Model*d$Treat)
    # summary(model)

    #Turkey post hoc test for multiple comparison of class error on Feature selection methods
    # out <- HSD.test(model,"d$FS_Method", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    #Turkey post hoc test for multiple comparison of Class Error by machine learning Models
    # out <- HSD.test(model,"d$Model", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    #Turkey post hoc test for multiple comparison of Class Error by treatments
    # out <- HSD.test(model,"d$Treat", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    ##############Feature selection methods by Class error
    #colourCount = length(unique(d$FS_Method))
    # display.brewer.all()
    #getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    #brewer.pal(n = 8, name = "Dark2")
    #brewer.pal(n = 9, name = "Set1")
    #brewer.pal(n = 12, name = "Paired")
    #colorkey = c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666",
                #   "#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#999999",
                #   "#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#CAB2D6",
                 #  "#6A3D9A","#FFFF99") #,"#B15928""

    return(d)

  }

AtScl.ClassError.plot <- function () {

    ct <- with(d, table(FS_Method, ClassError))
    ct.m <- melt(ct)
    ct.m2 <- ct.m[ct.m$value != 0, ]

    write.csv(ct.m2, "test.csv")

    p <- ggplot(d, aes(x=FS_Method, y=ClassError,color=FS_Method)) +
      geom_violin(width=1.2,alpha=0.5,position = position_dodge(0.9),trim=FALSE) +
      geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.9)) +
      geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
      geom_hline(yintercept=1,linetype="dashed", color = "blue") +
      scale_y_continuous(n.breaks = 10) +
      scale_color_manual(values=c("black", "#FDE725FF","#FDE725FF", "#B8DE29FF","#B8DE29FF", "#B8DE29FF","#B8DE29FF","#B8DE29FF",
                                  "#B8DE29FF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#287D8EFF","#287D8EFF",
                                  "#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#440154FF","#440154FF")) +
      theme_classic() +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
      theme(axis.title.x=element_blank())
    p

    pdf(file="results/MLSummary/MLApply/AtScl/ClassErrorAtSclFeatureSelection.pdf", pointsize=10,height=3, width=6)
    p
    dev.off()

    # Balloon plot 1 -- FS_methods
    fs_list <- c("Host_20340", "Defence_130",
        "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
        "RF_best_100", "XGB_best_500", "Degree_best_500",
        "Btwns_best_500", "NFD_best_3000", "FDC_best_3000", "Bipartite_best_2226")
    relabel <- c("RF_best_100" = "RF_100", "XGB_best_500" = "XGB_500",
                 "Degree_best_500" = "Degree_500", "Btwns_best_500" = "Btwns_500",
                 "NFD_best_3000" = "NFD_3000", "FDC_best_3000" = "FDC_3000",
                 "Bipartite_best_2226" = "Bipartite_2226")


    p <- ggballoonplot(ct.m2, x="FS_Method", y="ClassError", size="value",
                       fill="value", yticks.by = 2,
                       ggtheme = theme_bw(base_size = 12)) +
         scale_x_discrete(limits = fs_list, labels = relabel) +
         scale_fill_viridis_c()
    p
    pdf(file="results/MLSummary/MLApply/AtScl/ClassErrorAtSclFeatureSelectionBaloon.pdf", pointsize=10,height=5, width=8)
    p
    dev.off()

    # Change to balloon plot
    ct <- with(d, table(Model, Treat, ClassError))
    ct.m <- melt(ct)
    ct.m2 <- ct.m[ct.m$value != 0, ]

    p <- ggplot(d, aes(x=Model, y=ClassError,fill=Treat)) +
      geom_violin(width=1.2,alpha=0.,position = position_dodge(0.8),trim=FALSE) +
      geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.8)) +
      geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
      geom_hline(yintercept=1,linetype="dashed", color = "blue") +
      scale_y_continuous(n.breaks = 10) +
      scale_fill_manual(values=c("#5ec962","#440154")) +
      theme_classic() +
      theme(legend.position="top") +
      theme(axis.title.x=element_blank())

    pdf(file="results/MLSummary/MLApply/AtScl/ClassErrorAtSclModel.pdf")
    p
    dev.off()

    # Balloon plot 2 --
    p <- ggballoonplot(ct.m2, x="Model", y="ClassError", size="value",
                   facet.by = "Treat", fill="value", yticks.by = 2,
                   ggtheme = theme_bw(base_size = 12)) +
     scale_fill_viridis_c()
    p

    pdf(file="results/MLSummary/MLApply/AtScl/Fig3d_ClassErrorAtSclModelBalloon.pdf",
        pointsize=10,height=4, width=6)
    p
    dev.off()

    # Random 100 only
    fs_list <- c("Host_20340", "Defence_130",
        "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
        "RF_best_100", "XGB_best_500", "Degree_best_500",
        "Btwns_best_500", "NFD_best_3000", "FDC_best_3000", "Bipartite_best_2226")

    data <- read.csv("results/MLSummary/MLApply/AtScl/At_FeatureSeleAll_Test2.csv")
    data <- data[data$FS_Method %in% fs_list,]
    # data <- data[!(data$FS_Method=="Dual_29101"|data$FS_Method=="Pathogen_8761"|data$FS_Method== "Degree_10"
    #              |data$FS_Method== "Btwns_10"|data$FS_Method== "NFD_10"|data$FS_Method== "FDC_10"),]
    colnames(data)

    data_rand100 <- data[data$FS_Method=="Rand_100",]

    p1 <- ggplot(data_rand100, aes(x=FS_Method, y=Accuracy,color=Model)) +
    geom_boxplot(color="black",width=0.25,outlier.shape = NA) +
    geom_jitter(shape=16, size=3,position=position_jitter(0.15),alpha=0.7) +
    scale_color_brewer(palette="Dark2") +
    theme_classic() +
    theme(legend.position = "right") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    theme(axis.title.x=element_blank())
    p1
    pdf(file="results/MLSummary/MLApply/AtRandom/AccuracyAtRandomFeatureSelectionAcc.pdf",
        pointsize=10,height=3, width=2.2)
    p1
    dev.off()

}

AtSc.ConfusionMatrix <- function () {
    data <- read.csv("results/MLSummary/MLApply/AtScl/At_FeatureSeleAll_Test2.csv")
    data <- data[!(data$FS_Method=="Dual_29101"|data$FS_Method=="Pathogen_8761"|data$FS_Method== "Degree_10"
             |data$FS_Method== "Btwns_10"|data$FS_Method== "NFD_10"|data$FS_Method== "FDC_10"
             |data$FS_Method== "RF_best_100"|data$FS_Method== "XGB_100"
             |data$FS_Method== "Btwns_100"|data$FS_Method== "NFD_100"|data$FS_Method== "FDC_100"
             |data$FS_Method== "Degree_100"|data$FS_Method== "Bipartite_711"
             |data$FS_Method== "NegCorr_100"|data$FS_Method== "Rand_100"
             |data$FS_Method== "PosCorr_100"|data$FS_Method== "TopCorr_100"),]
    # data <- data[!(data$FS_Method=="Dual_29101"|data$FS_Method=="Pathogen_8761"|data$FS_Method== "Degree_10"
    #              |data$FS_Method== "Btwns_10"|data$FS_Method== "NFD_10"|data$FS_Method== "FDC_10"),]
    colnames(data)

    ########Statistical analysis
    # Accuracy
    model <- aov(data$Accuracy ~ data$FS_Method+data$Model)
    summary(model)

    #Turkey post hoc test for multiple comparison of Accuracy on Feature selection methods
    out <- HSD.test(model,"data$FS_Method", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)
    "                    data$Accuracy groups
NegCorr_762             0.9000000      a
XGB_best_500            0.7333333      a
Bipartite_best_2226     0.7000000      a
Degree_best_500         0.7000000      a
TopCorr_1548            0.7000000      a
PosCorr_786             0.6333333      a
GOBiotic_912            0.6333333      a
Host_20340              0.6000000      a
Btwns_best_500          0.5666667      a
FDC_best_3000           0.5666667      a
NFD_best_3000           0.5333333      a
RF_100                  0.5000000      a
Defence_130             0.4333333      a"

    #Turkey post hoc test for multiple comparison of Accuracy on Models
    out <- HSD.test(model,"data$Model", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    "       data$Accuracy groups
RF         0.8618627      a
SVM        0.6605882      b
XGB        0.6238235      b
DNN        0.5098039      b
linSVM     0.4727451      b"

    # #Precision
    # model <- aov(data$Precision ~ data$FS_Method + data$Model)
    # summary(model)

    # #Turkey post hoc test for multiple comparison of Precision on Feature selection methods
    # out <- HSD.test(model,"data$FS_Method", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    # #Turkey post hoc test for multiple comparison of Precision on Models
    # out <- HSD.test(model,"data$Model", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    # #Recall
    # model <- aov(data$Recall ~ data$FS_Method+data$Model)
    # summary(model)

    # #Turkey post hoc test for multiple comparison of Recall on Feature selection methods
    # out <- HSD.test(model,"data$FS_Method", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    # #Turkey post hoc test for multiple comparison of Recall on Models
    # out <- HSD.test(model,"data$Model", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    # #F1_Score
    # model <- aov(data$F1_Score ~ data$FS_Method+data$Model)
    # summary(model)

    # #Turkey post hoc test for multiple comparison of F1_Score on Feature selection methods
    # out <- HSD.test(model,"data$FS_Method", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    # #Turkey post hoc test for multiple comparison of F1_Score on Models
    # out <- HSD.test(model,"data$Model", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    # #MSE
    # model <- aov(data$MSE ~ data$FS_Method+data$Model)
    # summary(model)

    # #Turkey post hoc test for multiple comparison of MSE on Feature selection methods
    # out <- HSD.test(model,"data$FS_Method", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    # #Turkey post hoc test for multiple comparison of MSE on Models
    # out <- HSD.test(model,"data$Model", group=TRUE,console=FALSE)
    # print(out$statistics)
    # print(out$groups)

    ########Plots
    data$Model<- factor(data$Model, levels = c("SVM","linSVM","RF","XGB","DNN"))
    data$FS_Method <- factor(data$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
                                                               "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
                                                               "RF_100","RF_best_100","XGB_100","XGB_best_500",
                                                               "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
                                                               "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
                                                               "Bipartite_711","Bipartite_best_2226","Rand_100"))
    #####Models
    p1 <- ggplot(data, aes(x=Model, y=Accuracy,fill=Model)) +
        geom_boxplot(color="black",width=0.5,outlier.shape = NA,alpha=0.3) +
        geom_jitter(shape=16, size=1,position=position_jitter(0.15),alpha=0.6) +
        # ylim(0, 1) +
        scale_fill_brewer(palette="Dark2") +
        theme_classic() +
        theme(legend.position="none") +
        theme(axis.title.x=element_blank())
    p1

    pdf(file="results/MLSummary/MLApply/AtScl/Fig3e_AccuracyAtSclModel.pdf", pointsize=10,height=2, width=2.5)
    p1
    dev.off()

    ### Accuracy all models

    ### Accuracy all feature selectio models
    fs_list <- c("Host_20340", "Defence_130",
        "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
        "RF_best_100", "XGB_best_500", "Degree_best_500",
        "Btwns_best_500", "NFD_best_3000", "FDC_best_3000", "Bipartite_best_2226")
    relabel <- c("RF_best_100" = "RF_100", "XGB_best_500" = "XGB_500",
                 "Degree_best_500" = "Degree_500", "Btwns_best_500" = "Btwns_500",
                 "NFD_best_3000" = "NFD_3000", "FDC_best_3000" = "FDC_3000",
                 "Bipartite_best_2226" = "Bipartite_2226")

    p1 <- ggplot(data, aes(x=FS_Method, y=Accuracy,color=Model)) +
        geom_boxplot(color=c("black"),width=0.5,outlier.shape = NA) +
        geom_jitter(shape=16, size=1.5,position=position_jitter(0.15),alpha=0.7) +
        scale_color_brewer(palette="Dark2") +
        scale_x_discrete(limits = fs_list, labels = relabel) +
        theme_classic() +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        theme(axis.title.x=element_blank())
    p1

    pdf(file="results/MLSummary/MLApply/AtScl/AccuracyAtSclFeatureSelection.pdf",
        pointsize=10, height=3, width=6.2)
    p1
    dev.off()


}

jayson.ClassError.plot <- function () {
    df <- read.csv("results/MLSummary/MLApply/AtScl/AtOnly_Scl_ClassError_FS.csv")

    # ct <- with(df, table(FS_Method, ClassError))
    # ct.m <- melt(df)
    ct.m2 <- df[df$value != 0, ]

    fs_list <- c("Host_20340", "Defence_130",
        "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
        "RF_best_100", "XGB_best_500", "Degree_best_500",
        "Btwns_best_500", "NFD_best_3000", "FDC_best_3000", "Bipartite_best_2226")

    p <- ggballoonplot(ct.m2, x="FS_Method", y="ClassError", size="value",
                       fill="value", yticks.by = 2, size.range = c(1, 10),
                       ggtheme = theme_bw(base_size = 12)) +
         ylab('Class Error') +
         scale_x_discrete(limits = fs_list) +
         scale_fill_viridis_c()
    # "Dual_29101",  "Pathogen_8761",
    p

    pdf(file="results/MLSummary/MLApply/AtScl/Fig3a_ClassErrorAtSclFeatureSelectionBalloon.pdf",
        pointsize=10,height=4, width=6)
    p
    dev.off()

 # Class Error by Model
    df <- read.csv("results/MLSummary/MLApply/AtScl/AtOnly_Scl_ClassError_FS_Model.csv")
    ct.m2 <- df[df$value != 0, ]

    model_list <- c("SVM", "SVM_LIN", "RF", "XGB", "DNN" )
    relabel <- c("SVM_LIN" = "linSVM")

    p <- ggballoonplot(ct.m2, x="Model", y="ClassError", size="value",
                       fill="value", yticks.by = 2, size.range = c(1, 10),
                       ggtheme = theme_bw(base_size = 12)) +
         scale_x_discrete(limits = model_list, labels = relabel) +
         scale_fill_viridis_c() +
         theme(axis.text.x = element_text(size=12))
    # "Dual_29101",  "Pathogen_8761",
    p

    pdf(file="results/MLSummary/MLApply/AtPsy/Fig3d_ClassErrorAtSclRandomFeatureSelectionByModelBalloon.pdf",
        pointsize=10,height=4, width=6)

    p
    dev.off()
}

jayson.ConfusionMatrix <- function () {
    data <- read.csv("results/MLSummary/MLApply/AtScl/AtOnly_Scl_Acc_byModel.csv")

    ########Plots
    data$Model<- factor(data$Model, levels = c("SVM","linSVM","RF","XGB","DNN"))
    data$FS_Method <- factor(data$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
                                                               "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
                                                               "RF_100","RF_best_100","XGB_100","XGB_best_500",
                                                               "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
                                                               "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
                                                               "Bipartite_711","Bipartite_best_2226","Rand_100"))

    # 2022.07.12 Accuracy analysis
    hist(data$Accuracy)

    model <- aov(data$Accuracy ~ data$Model + data$FS_Method)
    summary(model)

    #Turkey post hoc test for multiple comparison of Accuracy on Models
    out <- HSD.test(model,"data$Model", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    #Turkey post hoc test for multiple comparison of Accuracy on Models
    out <- HSD.test(model,"data$FS_Method", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    ## Accuracy by Model
    p1 <- ggplot(data, aes(x=Model, y=Accuracy,fill=Model)) +
        geom_boxplot(color="black",width=0.5,outlier.shape = NA,alpha=0.3) +
        # geom_jitter()
        geom_jitter(shape=16, size=1,position=position_jitter(0.25),alpha=0.6) +
        # ylim(0, 1) +
        scale_fill_brewer(palette="Dark2") +
        theme_classic() +
        theme(legend.position="none") +
        theme(axis.title.x=element_blank())
    p1

    pdf(file="results/MLSummary/MLApply/AtScl/Fig3e_AccuracyAtSclModel.pdf", pointsize=10,height=2, width=2.5)
    p1
    dev.off()


    ### Accuracy all feature selectio models
    fs_list <- c("Host_20340", "Defence_130",
        "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
        "RF_best_100", "XGB_best_500", "Degree_best_500",
        "Btwns_best_500", "NFD_best_3000", "FDC_best_3000", "Bipartite_best_2226")
    relabel <- c("RF_best_100" = "RF_100", "XGB_best_500" = 'XGB_500',
                 "Degree_best_500" = "Degree_500", "Btwns_best_500" = "Btwns_500",
                 "NFD_best_3000" = "NFD_3000", "FDC_best_3000" = "FDC_3000",
                 "Bipartite_best_2226" = "Bipartite_2226")

    ## x-labels without feature numbers
    # relabel <- c("Host_20340" = "Host", "Defence_130" = "Defence",
    #              "GOBiotic_912" = "GOBiotic", "TopCorr_1548" = "TopCorr",
    #              "PosCorr_786" = "PosCorr", "NegCorr_762" = "NegCorr",
    #              "RF_best_100" = "RF", "XGB_500" = "XGB",
    #              "Degree_best_500" = "Degree", "Btwns_best_500" = "Btwns",
    #              "NFD_best_3000" = "NFD", "FDC_best_3000" = "FDC",
    #              "Bipartite_best_2226" = "Bipartite")

    p1 <- ggplot(data, aes(x=FS_Method, y=Accuracy,color=Model)) +
        geom_boxplot(color=c("black"),width=0.5,outlier.shape = NA) +
        geom_jitter(shape=16, size=1.5,position=position_jitter(w = 0.25, h = 0.0),alpha=0.7) +
        geom_hline(yintercept=0.638,linetype="dashed", color = "dark gray") +
        # ylim(0, 1) +
        ylim(0, 1) +
        scale_color_brewer(palette="Dark2") +
        scale_x_discrete(limits = fs_list, labels = relabel) +
        theme_classic() +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        theme(axis.title.x=element_blank())
    p1

    pdf(file="results/MLSummary/MLApply/AtScl/Fig3b_AccuracyAtSclFeatureSelection.pdf", pointsize=10,height=3, width=6.2)
    p1
    dev.off()
}