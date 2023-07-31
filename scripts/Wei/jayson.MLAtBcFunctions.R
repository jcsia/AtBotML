##
## MLAtBcFunctions
##
## Functions for ML AtBc data summary and plotting
##
## Author: Jayson Sia
##
## Date Created: 2022-06-14
##
## Copyright (c)
## Email: jsia@usc.edu
##
##
## Notes:
##
##

# libraries for plots
library(ggplot2)
library(ggpubr)
library(patchwork)
library(reshape2)
library(RColorBrewer)
# display.brewer.all()

# theme_set(theme_pubr())

#libraries for statistical analysis
library("agricolae")

MLAtBcClassError.data <- function() {

    #import data, total samples 1092, train=764, test=328
    df1 <- read.csv("results/MLSummary/train_test1_dnn_predictions.csv")
    df1 <- data.frame(df1,Model="DNN")

    df2 <- read.csv("results/MLSummary/train_test1_rf_predictions.csv")
    df2 <- data.frame(df2,Model="RF")

    df3 <- read.csv("results/MLSummary/train_test1_svm_lin_predictions.csv")
    df3 <- data.frame(df3,Model="linSVM")

    df4 <- read.csv("results/MLSummary/train_test1_svm_predictions.csv")
    df4 <- data.frame(df4,Model="SVM")

    df5 <- read.csv("results/MLSummary/train_test1_xgb_predictions.csv")
    df5 <- data.frame(df5,Model="XGB")

    df <- rbind(df1,df2,df3,df4,df5)
    colnames(df)
    df <- df[,c(3:7,39,9,8,10,11,38)]
    df <- df[df$Test.Train=="Test",]

    return (df)
}

MLAtBcClass.Stat <- function(){
    # sink(file = "MLAtBcClassEror.txt")
    ### Statistical test on observed lesions
    # print("Statistical test on observed lesions")
    model <- aov(df$Lesion ~ df$HostGenoType*df$Isolate)
    print(summary(model))

    # Turkey post hoc test for multiple comparison of Lesion by Host genotype
    out <- HSD.test(model,"df$HostGenoType", group=TRUE,console=FALSE)
    print(out$statistics)
    print("")
    print(out$groups)

    # Turkey post hoc test for multiple comparison of Lesion by Pathogen isolates
    out <- HSD.test(model,"df$Isolate", group=FALSE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    ### Statistical test on observed disease classes
    model <- aov(df$Class ~ df$HostGenoType*df$Isolate)
    summary(model)

    # Turkey post hoc test for multiple comparison of Lesion by Host genotype
    out <- HSD.test(model,"df$HostGenoType", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    # Turkey post hoc test for multiple comparison of Lesion by Pathogen isolates
    out <- HSD.test(model,"df$Isolate", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    x1 <- data.frame(df[,c(1:7)],Species="Dual",ObserClass=df$Class,PrediClass=df$Dual,ClassError=df$Dual-df$Class)
    x2 <- data.frame(df[,c(1:7)],Species="Host",ObserClass=df$Class,PrediClass=df$At.All,ClassError=df$At.All-df$Class)
    x3 <- data.frame(df[,c(1:7)],Species="Pathogen",ObserClass=df$Class,PrediClass=df$Pathogen,ClassError=df$Pathogen-df$Class)
    x<- rbind(x1,x2,x3)

    hist(x1$ClassError)
    hist(x2$ClassError)
    hist(x3$ClassError)

    ### Statistical test on predicted disease classes
    model <- aov(x$PrediClass ~ x$HostGenoType*x$Isolate*x$Species*x$Model)
    print(summary(model))

    # Turkey post hoc test for multiple comparison of Predicted disease classes by Host genotype
    out <- HSD.test(model,"x$HostGenoType", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    # Turkey post hoc test for multiple comparison of Predicted disease classes by Pathogen isolates
    out <- HSD.test(model,"x$Isolate", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    # Turkey post hoc test for multiple comparison of Predicted disease classes by data species
    out <- HSD.test(model,"x$Species", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    # Turkey post hoc test for multiple comparison of Predicted disease classes by machine learning models
    out <- HSD.test(model,"x$Model", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    ### Statistical test on class error
    model <- aov(x$ClassError ~ x$HostGenoType*x$Isolate*x$Species*x$Model)
    summary(model)

    # Turkey post hoc test for multiple comparison of class error by Host genotype
    out <- HSD.test(model,"x$HostGenoType", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    # Turkey post hoc test for multiple comparison of class error by Pathogen isolates
    out <- HSD.test(model,"x$Isolate", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    # Turkey post hoc test for multiple comparison of class error by data species
    out <- HSD.test(model,"x$Species", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    # Turkey post hoc test for multiple comparison of class error by machine learning models
    out <- HSD.test(model,"x$Model", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    # sink(file = NULL)
}


MLAtBcClassError.plot <- function() {
    # display.brewer.all()
    brewer.pal(n = 9, name = "Set1")

    x1 <- data.frame(df[,c(1:7)],Species="Dual",ObserClass=df$Class,PrediClass=df$Dual,ClassError=df$Dual-df$Class)
    x2 <- data.frame(df[,c(1:7)],Species="Host",ObserClass=df$Class,PrediClass=df$At.All,ClassError=df$At.All-df$Class)
    x3 <- data.frame(df[,c(1:7)],Species="Pathogen",ObserClass=df$Class,PrediClass=df$Pathogen,ClassError=df$Pathogen-df$Class)
    x<- rbind(x1,x2,x3)

    # hist(x1$ClassError)
    # hist(x2$ClassError)
    # hist(x3$ClassError)

    # contigency table (Species vs ClassError)
    ct <- with(x, table(Species, HostGenoType, ClassError))
    ct.m <- melt(ct)
    ct.m2 <- ct.m[ct.m$value != 0, ]

    ## Violin
    # p1 <- ggplot(x, aes(x=Species, y=ClassError,fill=HostGenoType)) +
    #     geom_violin(width=1,alpha=0.5,position = position_dodge(0.9)) +
    #     geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.9)) +
    #     geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
    #     geom_hline(yintercept=1,linetype="dashed", color = "blue") +
    #     scale_y_continuous(n.breaks = 10) +
    #     scale_fill_brewer(palette="Set1",labels = c("coi1-1", "Col-0", "npr1-1")) +
    #     theme_classic() +
    #     theme(legend.position="top") +
    #     theme(axis.title.x=element_blank())
    #
    # pdf(file="results/MLSummary/AtBcClassError.pdf", pointsize=10,height=3, width=4)
    # p1
    # dev.off()

    p2 <- ggballoonplot(ct.m2, x="HostGenoType", y="ClassError", size="value",
                        facet.by = "Species",  fill="value", yticks.by = 2,
                        ggtheme = theme_bw(base_size = 12)) +
          annotate("rect", xmin=-Inf, xmax=Inf,
                   ymin=-1.5, ymax=1.5, fill = "blue", alpha=0.1) +
          scale_fill_viridis_c()
    p2

    # pdf(file="results/MLSummary/AtBcClassError_Balloon.pdf", pointsize=10,height=3, width=4)
    pdf(file="results/MLSummary/AtBcClassError_Balloon.pdf", height=3, width=5)
    p2
    dev.off()

    ## Class error by feature selection
    # contigency table (Species vs ClassError)
    ct <- with(x, table(Species, HostGenoType, ClassError))
    ct.m <- melt(ct)
    ct.m2 <- ct.m[ct.m$value != 0, ]

}

MLAtBc.ClassErrorFeaturesSelection.plot <- function () {

    df <- read.csv("results/MLSummary/AtOnly_ClassError_FS.csv")

    # ct <- with(df, table(FS_Method, ClassError))
    # ct.m <- melt(df)
    ct.m2 <- df[df$value != 0, ]

    fs_list <- c("Host_20340", "Defence_130",
        "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
        "RF_best_100", "XGB_best_500", "Degree_best_500",
        "Btwns_best_500", "NFD_best_3000", "FDC_best_3000", "Bipartite_best_2226")

    p <- ggballoonplot(ct.m2, x="FS_Method", y="ClassError", size="value",
                       fill="value", yticks.by = 2,
                       size.range = c(0, 7),
                       ggtheme = theme_bw(base_size = 12)) +
         # geom_hline(aes(yintercept=1.5),linetype="dashed", color = "blue") +
         # geom_hline(aes(yintercept=-1.5),linetype="dashed", color = "blue") +
         annotate("rect", xmin=-Inf, xmax=Inf, ymin=-1.5, ymax=1.5, fill = "blue", alpha=0.1) +
         ylab('Class Error') +
         scale_x_discrete(limits = fs_list) +
         scale_fill_viridis_c()
    # "Dual_29101",  "Pathogen_8761",
    p

    pdf(file="results/MLSummary/Fig2a_ClassErrorAtBcFeatureSelectionBalloon.pdf",
        pointsize=10,height=4, width=6)
    p
    dev.off()

    # Class Error by Model
    df <- read.csv("results/MLSummary/AtOnly_ClassError_FS_Model.csv")
    ct.m2 <- df[df$value != 0, ]

    model_list <- c("SVM", "SVM_LIN", "RF", "XGB", "DNN" )
    relabel <- c("SVM_LIN" = "linSVM")

    p <- ggballoonplot(ct.m2, x="Model", y="ClassError", size="value",
                       fill="value", yticks.by = 2, size.range = c(1, 7),
                       ggtheme = theme_bw(base_size = 12)) +
         annotate("rect", xmin=-Inf, xmax=Inf, ymin=-1.5, ymax=1.5, fill = "blue", alpha=0.1) +
         scale_x_discrete(limits = model_list, labels = relabel) +
         scale_fill_viridis_c() +
         theme(axis.text.x = element_text(size=12))
    # "Dual_29101",  "Pathogen_8761",
    p

    pdf(file="results/MLSummary/Fig2b_ClassErrorAtBcRandomFeatureSelectionByModelBalloon.pdf",
        pointsize=10,height=4, width=6)
    p
    dev.off()
}

MLAtBc.ConfusionMatrix.plot <- function () {
    # OK!
    # data <- read.csv("results/MLSummary/AtBot_FeatureSeleAll_Test1_TestOnly1.csv")
    data <- read.csv("results/MLSummary/AtOnly_Acc_byModel.csv")

    # data <- data[!(data$FS_Method=="Dual_29101"|data$FS_Method=="Pathogen_8761"|data$FS_Method== "Degree_10"
    #          |data$FS_Method== "Btwns_10"|data$FS_Method== "NFD_10"|data$FS_Method== "FDC_10"
    #          |data$FS_Method== "RF_best_100"|data$FS_Method== "XGB_100"
    #          |data$FS_Method== "Btwns_100"|data$FS_Method== "NFD_100"|data$FS_Method== "FDC_100"
    #          |data$FS_Method== "Degree_100"|data$FS_Method== "Bipartite_711"),]
    # data <- data[!(data$FS_Method=="Dual_29101"|data$FS_Method=="Pathogen_8761"|data$FS_Method== "Degree_10"
    #          |data$FS_Method== "Btwns_10"|data$FS_Method== "NFD_10"|data$FS_Method== "FDC_10"),]
    colnames(data)


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
    "       data$Accuracy groups
SVM        0.6477486      a
RF         0.6259381     ab
XGB        0.6210131     ab
linSVM     0.6099906      b
DNN        0.5928705      b"

    #Turkey post hoc test for multiple comparison of Accuracy on Models
    out <- HSD.test(model,"data$FS_Method", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    "                    data$Accuracy groups
Host_20340              0.6896341      a
FDC_best_3000           0.6835366      a
NFD_best_3000           0.6792683      a
RF_100                  0.6743902      a
XGB_best_500            0.6713415      a
Defence_130             0.6701220      a
Degree_best_500         0.6621951      a
Bipartite_best_2226     0.6481707      a
Btwns_best_500          0.6432927      a
PosCorr_786             0.5731707      b
GOBiotic_912            0.5060976     bc
NegCorr_762             0.4914634      c
TopCorr_1548            0.4609756      c"


    ######

    ## Accuracy by Model
    p1 <- ggplot(data, aes(x=Model, y=Accuracy,fill=Model)) +
        geom_boxplot(color="black",width=0.5,outlier.shape = NA,alpha=0.3) +
        geom_jitter(shape=16, size=1,position=position_jitter(0.15),alpha=0.6) +
        scale_fill_brewer(palette="Dark2") +
        theme_classic() +
        theme(legend.position="none") +
        theme(axis.title.x=element_blank())
    p1

    pdf(file="results/MLSummary/AccuracyAtBcModel.pdf", pointsize=10,height=2, width=2.5)
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
        geom_jitter(shape=16, size=1.5,position=position_jitter(0.15),alpha=0.7) +
        geom_hline(yintercept=0.632,linetype="dashed", color = "dark gray") +
        # ylim(0, 1) +
        scale_color_brewer(palette="Dark2") +
        scale_x_discrete(limits = fs_list, labels = relabel) +
        theme_classic() +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        theme(axis.title.x=element_blank())
    p1

    pdf(file="results/MLSummary/Fig2b_AccuracyAtBcFeatureSelection.pdf", pointsize=10,height=3, width=6.2)
    p1
    dev.off()

    # Random 100 only
    fs_list <- c("Host_20340", "Defence_130",
        "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
        "RF_best_100", "XGB_best_500", "Degree_best_500",
        "Btwns_best_500", "NFD_best_3000", "FDC_best_3000", "Bipartite_best_2226")

    data <- read.csv("results/MLSummary/MLApply/AtScl/At_FeatureSeleAll_Test1.csv")
    data <- data[data$FS_Method %in% fs_list,]
    # data <- data[!(data$FS_Method=="Dual_29101"|data$FS_Method=="Pathogen_8761"|data$FS_Method== "Degree_10"
    #              |data$FS_Method== "Btwns_10"|data$FS_Method== "NFD_10"|data$FS_Method== "FDC_10"),]
    colnames(data)

    data_rand100 <- data[data$FS_Method=="Rand_100",]

    p1 <- ggplot(data_rand100, aes(x=FS_Method, y=Accuracy,color=Model)) +
    geom_boxplot(color="black",width=0.25,outlier.shape = NA) +
    geom_jitter(shape=16, size=3,position=position_jitter(0.15),alpha=0.7) +
    # geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
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