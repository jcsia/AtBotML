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

MLApplyAtPsyColPtoClassError <- function(){

    ########Class Error
    dat <- read.csv("results/MLSummary/MLApply/AtPsy/ClassErroSummary.csv")
    dat <-dat[dat$Strain=="Pto",]
    dat <-dat[dat$HostGeno=="Col-0"|dat$HostGeno=="Col-0 Mock treatment",]
    dat <- dat[!(dat$FS_Method=="Dual_29101"|dat$FS_Method== "Degree_10"|dat$FS_Method== "Btwns_10"|dat$FS_Method== "NFD_10"|dat$FS_Method== "FDC_10"),]

    hist(dat$ClassError)
    hist(dat$ObserveClass)
    hist(dat$PredictClass)

    dat$Model <- factor(dat$Model, levels = c("SVM","linSVM","RF","XGB","DNN"))
    dat$FS_Method <- factor(dat$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
                                                      "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
                                                      "RF_100","RF_best_100","XGB_100","XGB_best_500",
                                                      "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
                                                      "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
                                                      "Bipartite_711","Bipartite_best_2226"))


    ##############Feature selection methods by Class error
    #colourCount = length(unique(d$FS_Method))
    # display.brewer.all()
    #getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    brewer.pal(n = 8, name = "Dark2")
    brewer.pal(n = 9, name = "Set1")
    brewer.pal(n = 12, name = "Paired")
    colorkey <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666",
                  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999",
                  "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#CAB2D6",
                  "#6A3D9A", "#FFFF99")
    #,"#B15928"

    p5 <- ggplot(dat, aes(x=FS_Method, y=ClassError,color=FS_Method)) +
      geom_violin(width=1.2,alpha=0.5,trim = FALSE, position = position_dodge(0.9)) +
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

    pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyClassErrorColPtoFSAll.pdf", pointsize=10,height=3, width=6)
    p5
    dev.off()

    # balloon plot class error features
    fs_list <- c("Host_20340", "Defence_130",
        "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
        "RF_best_100", "XGB_best_500", "Degree_best_500",
        "Btwns_best_500", "NFD_best_3000", "FDC_best_3000", "Bipartite_best_2226")
    relabel <- c("RF_best_100" = "RF_100", "XGB_best_500" = "XGB_500",
                 "Degree_best_500" = "Degree_500", "Btwns_best_500" = "Btwns_500",
                 "NFD_best_3000" = "NFD_3000", "FDC_best_3000" = "FDC_3000",
                 "Bipartite_best_2226" = "Bipartite_2226")

    ct <- with(dat, table(FS_Method, ClassError))
    ct.m <- melt(ct)
    ct.m2 <- ct.m[ct.m$value != 0, ]

    p <- ggballoonplot(ct.m2, x="FS_Method", y="ClassError", size="value",
                       fill="value", yticks.by = 2,
                       ggtheme = theme_bw(base_size = 12)) +
         scale_x_discrete(limits = fs_list, labels = relabel) +
         scale_fill_viridis_c()
    p

    pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyClassErrorColPtoFSAllBalloon.pdf",
        pointsize=10,height=4, width=8)
    p
    dev.off()

    p6 <- ggplot(dat, aes(x=Model, y=ClassError,fill=Model)) +
      geom_violin(width=1.2,alpha=0.3,position = position_dodge(0.9)) +
      geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.9)) +
      geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
      geom_hline(yintercept=1,linetype="dashed", color = "blue") +
      scale_fill_brewer(palette="Dark2") +
      scale_y_continuous(n.breaks = 10) +
      theme_classic() +
      theme(legend.position="none") +
      theme(axis.title.x=element_blank())

    pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyClassErrorColPtoModel.pdf", pointsize=10,height=2, width=3.2)
    p6
    dev.off()

    # balloon plot class error models
    ct <- with(dat, table(Model, ClassError))
    ct.m <- melt(ct)
    ct.m2 <- ct.m[ct.m$value != 0, ]

    p <- ggballoonplot(ct.m2, x="Model", y="ClassError", size="value",
                       fill="value", yticks.by = 2,
                       ggtheme = theme_bw(base_size = 12)) +
         scale_fill_viridis_c()
    p

    pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyClassErrorColPtoModelBalloon.pdf",
        pointsize=10,height=3.5, width=4)
    p
    dev.off()

    dat.rf <- dat[dat$Model=="RF",]
    p11 <- ggplot(dat.rf, aes(x=FS_Method, y=ClassError,color=FS_Method)) +
      geom_violin(width=1.2,alpha=0.5,trim = FALSE, position = position_dodge(0.9)) +
      geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.9)) +
      geom_jitter(shape=16, size=0.6,position=position_jitter(0.2),alpha=0.5) +
      geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
      geom_hline(yintercept=1,linetype="dashed", color = "blue") +
      scale_color_manual(values=c("black", "#FDE725FF","#FDE725FF", "#B8DE29FF","#B8DE29FF", "#B8DE29FF","#B8DE29FF","#B8DE29FF",
                                  "#B8DE29FF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#287D8EFF","#287D8EFF",
                                  "#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#440154FF","#440154FF")) +
      scale_y_continuous(n.breaks = 10) +
      theme_classic() +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
      theme(axis.title.x=element_blank())

    pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyColPtoRFFSAll.pdf", pointsize=10,height=3, width=6)
    p11
    dev.off()

    dat.xgb <- dat[dat$Model=="XGB",]
    p12 <- ggplot(dat.xgb, aes(x=FS_Method, y=ClassError,color=FS_Method)) +
      geom_violin(width=1.2,alpha=0.5,trim = FALSE, position = position_dodge(0.9)) +
      geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.9)) +
      geom_jitter(shape=16, size=0.6,position=position_jitter(0.2),alpha=0.5) +
      geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
      geom_hline(yintercept=1,linetype="dashed", color = "blue") +
      scale_color_manual(values=c("black", "#FDE725FF","#FDE725FF", "#B8DE29FF","#B8DE29FF", "#B8DE29FF","#B8DE29FF","#B8DE29FF",
                                  "#B8DE29FF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#287D8EFF","#287D8EFF",
                                  "#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#440154FF","#440154FF")) +
      scale_y_continuous(n.breaks = 10) +
      theme_classic() +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
      theme(axis.title.x=element_blank())

    pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyColPtoXGBFSAll.pdf", pointsize=10,height=3, width=6)
    p12
    dev.off()

    dat.svm <- dat[dat$Model=="SVM",]
    p13 <- ggplot(dat.svm, aes(x=FS_Method, y=ClassError,color=FS_Method)) +
      geom_violin(width=1.2,alpha=0.5,trim = FALSE, position = position_dodge(0.9)) +
      geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.9)) +
      geom_jitter(shape=16, size=0.6,position=position_jitter(0.2),alpha=0.5) +
      geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
      geom_hline(yintercept=1,linetype="dashed", color = "blue") +
      scale_color_manual(values=c("black", "#FDE725FF","#FDE725FF", "#B8DE29FF","#B8DE29FF", "#B8DE29FF","#B8DE29FF","#B8DE29FF",
                                  "#B8DE29FF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#287D8EFF","#287D8EFF",
                                  "#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#440154FF","#440154FF")) +
      scale_y_continuous(n.breaks = 10) +
      theme_classic() +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
      theme(axis.title.x=element_blank())

    pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyColPtoSVMFSAll.pdf", pointsize=10,height=3, width=6)
    p13
    dev.off()

    dat.linSVM <- dat[dat$Model=="linSVM",]
    p14 <- ggplot(dat.linSVM, aes(x=FS_Method, y=ClassError,color=FS_Method)) +
      geom_violin(width=1.2,alpha=0.5,trim = FALSE, position = position_dodge(0.9)) +
      geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.9)) +
      geom_jitter(shape=16, size=0.6,position=position_jitter(0.2),alpha=0.5) +
      geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
      geom_hline(yintercept=1,linetype="dashed", color = "blue") +
      scale_color_manual(values=c("black", "#FDE725FF","#FDE725FF", "#B8DE29FF","#B8DE29FF", "#B8DE29FF","#B8DE29FF","#B8DE29FF",
                                  "#B8DE29FF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#287D8EFF","#287D8EFF",
                                  "#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#440154FF","#440154FF")) +
      scale_y_continuous(n.breaks = 10) +
      theme_classic() +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
      theme(axis.title.x=element_blank())

    pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyColPtoLinSVMFSAll.pdf", pointsize=10,height=3, width=6)
    p14
    dev.off()

    dat.dnn <- dat[dat$Model=="DNN",]
    p15 <- ggplot(dat.dnn, aes(x=FS_Method, y=ClassError,color=FS_Method)) +
      geom_violin(width=1.2,alpha=0.5,trim = FALSE, position = position_dodge(0.9)) +
      geom_boxplot(width=0.15,outlier.shape = NA,alpha=0.2,position = position_dodge(0.9)) +
      geom_jitter(shape=16, size=0.6,position=position_jitter(0.2),alpha=0.5) +
      geom_hline(yintercept=-1,linetype="dashed", color = "blue") +
      geom_hline(yintercept=1,linetype="dashed", color = "blue") +
      scale_color_manual(values=c("black", "#FDE725FF","#FDE725FF", "#B8DE29FF","#B8DE29FF", "#B8DE29FF","#B8DE29FF","#B8DE29FF",
                                  "#B8DE29FF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#29AF7FFF","#287D8EFF","#287D8EFF",
                                  "#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#287D8EFF","#440154FF","#440154FF")) +
      scale_y_continuous(n.breaks = 10) +
      theme_classic() +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
      theme(axis.title.x=element_blank())

    pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyColPtoDNNFSAll.pdf", pointsize=10,height=3, width=6)
    p15
    dev.off()





    #### Feature selection method
    df <- read.csv("results/MLSummary/MLApply/AtPsy/AtPsy_col-0_Pto_FeatureSelectAll_Test3.csv")
    df <- df[!(df$FS_Method=="Dual_29101"|df$FS_Method== "Degree_10"|df$FS_Method== "Btwns_10"|df$FS_Method== "NFD_10"|df$FS_Method== "FDC_10"),]
    colnames(df)[1] <- "Model"
    hist(df$Accuracy)

    data_rand100 <- df[df$FS_Method=="Rand_100",]


    p9 <- ggplot(data_rand100, aes(x=FS_Method, y=Accuracy,color=Model)) +
    geom_boxplot(color="black",width=0.25,outlier.shape = NA) +
    geom_jitter(shape=16, size=3,position=position_jitter(0.15),alpha=0.7) +
    scale_color_brewer(palette="Dark2") +
    scale_y_continuous(n.breaks = 10) + ylim(0.3,0.82) +
    theme_classic() +
    theme(legend.position="right") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    theme(axis.title.x=element_blank())
    p9
    pdf(file="results/MLSummary/MLApply/AtRandom/AtRandomColPtoAccuracyFeatureSelectionRand.pdf",
        pointsize=10,height=3, width=2.2)
    p9
    dev.off()

  }

jayson.MLApplyAtPsyColPtoClassError <- function() {
    df <- read.csv("results/MLSummary/MLApply/AtPsy/AtOnly_Psy_ClassError_FS.csv")

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

    pdf(file="results/MLSummary/MLApply/AtPsy/Fig5a_ClassErrorAtPsyFeatureSelectionBalloonXX.pdf",
        pointsize=10,height=4, width=6)
    p
    dev.off()

 # Class Error by Model
    df <- read.csv("results/MLSummary/MLApply/AtPsy/AtOnly_Psy_ClassError_FS_Model.csv")
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

    pdf(file="results/MLSummary/MLApply/AtPsy/Fig5d_ClassErrorAtPsyRandomFeatureSelectionByModelBalloon.pdf",
        pointsize=10,height=4, width=6)

    p
    dev.off()

    # ########Class Error
    # dat <- read.csv("results/MLSummary/MLApply/AtPsy/AtOnly_Psy_ClassError_FS.csv")
    # dat <-dat[dat$Strain=="Pto",]
    # dat <-dat[dat$HostGeno=="Col-0"|dat$HostGeno=="Col-0 Mock treatment",]
    # dat <- dat[!(dat$FS_Method=="Dual_29101"|dat$FS_Method== "Degree_10"|dat$FS_Method== "Btwns_10"|dat$FS_Method== "NFD_10"|dat$FS_Method== "FDC_10"),]
    #
    # hist(dat$ClassError)
    # hist(dat$ObserveClass)
    # hist(dat$PredictClass)
    #
    # dat$Model <- factor(dat$Model, levels = c("SVM","linSVM","RF","XGB","DNN"))
    # dat$FS_Method <- factor(dat$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
    #                                                   "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
    #                                                   "RF_100","RF_best_100","XGB_100","XGB_best_500",
    #                                                   "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
    #                                                   "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
    #                                                   "Bipartite_711","Bipartite_best_2226"))

}

wei.MLApplyAtPsyColPtoAccuracy <- function () {
    #######Accuracy analysis on At-Psy Col-0* Pto
    df <- read.csv("results/MLSummary/MLApply/AtPsy/AtPsy_col-0_Pto_FeatureSelectAll_Test3.csv")
    df <- df[!(df$FS_Method=="Dual_29101"|df$FS_Method=="Pathogen_8761"|df$FS_Method== "Degree_10"
             |df$FS_Method== "Btwns_10"|df$FS_Method== "NFD_10"|df$FS_Method== "FDC_10"
             |df$FS_Method== "RF_best_100"|df$FS_Method== "XGB_100"
             |df$FS_Method== "Btwns_100"|df$FS_Method== "NFD_100"|df$FS_Method== "FDC_100"
             |df$FS_Method== "Degree_100"|df$FS_Method== "Bipartite_711"
             |df$FS_Method== "NegCorr_100"|df$FS_Method== "Rand_100"
             |df$FS_Method== "PosCorr_100"|df$FS_Method== "TopCorr_100"),]
    df <- df[!(df$FS_Method=="Dual_29101"|df$FS_Method== "Degree_10"|df$FS_Method== "Btwns_10"
              |df$FS_Method== "NFD_10"|df$FS_Method== "FDC_10"),]
    colnames(df)[1] <- "Model"
    hist(df$Accuracy)

    ########Statistical analysis
    # Accuracy
    model <- aov(df$Accuracy ~ df$FS_Method+df$Model)
    summary(model)

    #Turkey post hoc test for multiple comparison of Accuracy on Feature selection methods
    out <- HSD.test(model,"df$FS_Method", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    "                    df$Accuracy groups
Btwns_best_500        0.7481481      a
RF_100                0.6814815      a
Defence_130           0.5333333      a
PosCorr_786           0.5333333      a
Bipartite_best_2226   0.4962963      a
TopCorr_1548          0.4740741      a
Host_20340            0.4370370      a
XGB_best_500          0.4000000      a
GOBiotic_912          0.3703704      a
FDC_best_3000         0.3555556      a
NFD_best_3000         0.3185185      a
Degree_best_500       0.3037037      a
NegCorr_762           0.1777778      a"

    #Turkey post hoc test for multiple comparison of Accuracy on Models
    out <- HSD.test(model,"df$Model", group=TRUE,console=FALSE)
    print(out$statistics)
    print(out$groups)

    "       df$Accuracy groups
XGB      0.7008547      a
RF       0.6666667      a
SVM      0.3732194     ab
linSVM   0.2649573      b
DNN      0.2364672      b"

    #######Plots
    df$Model <- factor(df$Model, levels = c("SVM","linSVM","RF","XGB","DNN"))
    df$FS_Method <- factor(df$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
                                                  "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
                                                  "RF_100","RF_best_100","XGB_100","XGB_best_500",
                                                  "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
                                                  "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
                                                  "Bipartite_711","Bipartite_best_2226","Rand_100"))


    # display.brewer.all()
    #getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    brewer.pal(n = 8, name = "Dark2")
    brewer.pal(n = 9, name = "Set1")
    brewer.pal(n = 12, name = "Paired")
    colorkey <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666",
                "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999",
                "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#CAB2D6",
                "#6A3D9A", "#FFFF99", "#B15928")
    #,"#B15928"

    p4 <- ggplot(df, aes(x=Model, y=Accuracy,fill=Model)) +
        geom_boxplot(color="black",width=0.5,outlier.shape = NA,alpha=0.3) +
        geom_jitter(shape=16, size=1,position=position_jitter(0.15),alpha=0.6) +
        scale_fill_brewer(palette="Dark2") +
        theme_classic() +
        theme(legend.position="none") +
        theme(axis.title.x=element_blank())
    p4

    #### Feature selection method
    p9 <- ggplot(df, aes(x=FS_Method, y=Accuracy,color=Model)) +
        geom_boxplot(color=c("black"),width=0.5,outlier.shape = NA) +
        geom_jitter(shape=16, size=1.5,position=position_jitter(0.15),alpha=0.7) +
        scale_color_brewer(palette="Dark2") +
        scale_y_continuous(n.breaks = 10) +
        scale_x_discrete(limits = fs_list, labels = relabel) +
        theme_classic() +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        theme(axis.title.x=element_blank())
    p9

    pdf(file="results/MLSummary/MLApply/AtPsy/AtPsyColPtoAccuracyFeatureSelectionAll.pdf", pointsize=10,height=3, width=6.2)
    p9
    dev.off()
}

jayson.MLApplyAtPsyColPtoAccuracy <- function () {
    data <- read.csv("results/MLSummary/MLApply/AtPsy/AtOnly_Psy_Acc_byModel.csv")

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
        geom_jitter(shape=16, size=1,position=position_jitter(0.15),alpha=0.6) +
        scale_fill_brewer(palette="Dark2") +
        theme_classic() +
        theme(legend.position="none") +
        theme(axis.title.x=element_blank())
    p1

    pdf(file="results/MLSummary/MLApply/AtPsy/Fig5e_AccuracyAtPsyModel.pdf", pointsize=10,height=2, width=2.5)
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
        geom_hline(yintercept=0.366,linetype="dashed", color = "dark gray") +
        # ylim(0, 1) +
        scale_color_brewer(palette="Dark2") +
        scale_x_discrete(limits = fs_list, labels = relabel) +
        theme_classic() +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
        theme(axis.title.x=element_blank())
    p1

    pdf(file="results/MLSummary/MLApply/AtPsy/Fig5b_AccuracyAtPsyFeatureSelection.pdf",
        pointsize=10, height=3, width=6.2)
    p1
    dev.off()
}