#### Clear environment
rm(list=ls())


#### At-Bc data
#### Set working directory
setwd("/Users/jayson/Research/Projects/AtBotML/scripts/Wei/")

#### Source
source("jayson.MLAtBcFunctions.R")

#### Execute
# df_Bc <- MLAtBcClassError.data()

df_Bc_acc <- read.csv("results/MLSummary/AtOnly_Acc_byModel.csv")
df_Scl_acc <- read.csv("results/MLSummary/MLApply/AtScl/AtOnly_Scl_Acc_byModel.csv")
df_Psy_acc <- read.csv("results/MLSummary/MLApply/AtPsy/AtOnly_Psy_Acc_byModel.csv")

df_Bc_acc$Pathogen <- 'Bc'
df_Scl_acc$Pathogen <- 'Scl'
df_Psy_acc$Pathogen <- 'Psy'

df_AllData_acc <- df_Bc_acc[df_Bc_acc$Model %in% c('XGB','RF'),]
df_AllData_acc <- rbind(df_AllData_acc, df_Scl_acc[df_Scl_acc$Model %in% c('XGB','RF'),])
df_AllData_acc <- rbind(df_AllData_acc, df_Psy_acc[df_Psy_acc$Model %in% c('XGB','RF'),])

#### Stats
df_AllData_acc$Model<- factor(df_AllData_acc$Model, levels = c("RF","XGB"))
df_AllData_acc$FS_Method <- factor(df_AllData_acc$FS_Method, levels = c("Host_20340","Defence_130","GOBiotic_912",
                                                           "TopCorr_1548","TopCorr_100","PosCorr_786","PosCorr_100","NegCorr_762","NegCorr_100",
                                                           "RF_100","RF_best_100","XGB_100","XGB_best_500",
                                                           "Degree_100","Degree_best_500","Btwns_100","Btwns_best_500",
                                                           "NFD_100","NFD_best_3000","FDC_100","FDC_best_3000",
                                                           "Bipartite_711","Bipartite_best_2226","Rand_100"))



hist(df_AllData_acc$Accuracy)

model <- aov(df_AllData_acc$Accuracy ~ df_AllData_acc$Model + df_AllData_acc$FS_Method)
summary(model)

out <- HSD.test(model,"df_AllData_acc$Model", group=TRUE,console=FALSE)
print(out$statistics)
print(out$groups)


out <- HSD.test(model,"df_AllData_acc$FS_Method", group=TRUE,console=FALSE)
print(out$statistics)
print(out$groups)
#### Plot

# p1 <- ggplot(df_AllData_acc, aes(x=Model, y=Accuracy,fill=Model)) +
#     geom_boxplot(color="black",width=0.5,outlier.shape = NA,alpha=0.3) +
#     geom_jitter(shape=16, size=1,position=position_jitter(0.15),alpha=0.6) +
#     scale_fill_brewer(palette="Dark2") +
#     theme_classic() +
#     theme(legend.position="none") +
#     theme(axis.title.x=element_blank())
# p1

fs_list <- c("Host_20340", "Defence_130",
    "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
    "RF_best_100", "XGB_best_500", "Degree_best_500",
    "Btwns_best_500", "NFD_best_3000", "FDC_best_3000", "Bipartite_best_2226")
relabel <- c("RF_best_100" = "RF_100", "XGB_best_500" = 'XGB_500',
             "Degree_best_500" = "Degree_500", "Btwns_best_500" = "Btwns_500",
             "NFD_best_3000" = "NFD_3000", "FDC_best_3000" = "FDC_3000",
             "Bipartite_best_2226" = "Bipartite_2226")

p2 <- ggplot(df_AllData_acc, aes(x=FS_Method, y=Accuracy)) + # , color=Pathogen
    geom_boxplot(aes(x=FS_Method, y=Accuracy), color=c("black"),width=0.5,outlier.shape = NA) +
    geom_jitter(aes(x=FS_Method, y=Accuracy, color=Pathogen, shape=Model), size=1.5, position=position_jitter(0.15),alpha=0.7) + # shape=16,
    # geom_hline(yintercept=0.632,linetype="dashed", color = "dark gray") +
    # ylim(0, 1) +
    scale_color_brewer(palette="Dark2") +
    scale_x_discrete(limits = fs_list, labels = relabel) +
    theme_classic() +
    theme(legend.position = "right") +
    theme(axis.text.x = element_blank()) +
    theme(axis.title.x=element_blank())
p2

# p3 <- ggplot(df_AllData_acc, aes(x=Pathogen, y=Accuracy, fill=Model)) + # , shape=Model
#     geom_boxplot(color="black",width=0.5,outlier.shape = NA,alpha=0.3) +
#     geom_jitter(shape=16, size=1,position=position_jitter(0.15),alpha=0.6) +
#     scale_fill_brewer(palette="Dark2") +
#     theme_classic() +
#     theme(legend.position="right") +
#     theme(axis.title.x=element_blank())
# p3

####
df_logTPM1 <- read.csv("results/MLSummary/MLApply/At_logTPM_FS.csv")
data_frame_mod <- data_frame[data_frame$col3==TRUE,]

df_logTPM1_bc <- df_logTPM1[df_logTPM1$Pathogen=='Bc',]
df_logTPM1_scl <- df_logTPM1[df_logTPM1$Pathogen=='Scl',]
df_logTPM1_psy <- df_logTPM1[df_logTPM1$Pathogen=='Psy',]

fs_list <- c("Host_20340", "Defence_130",
    "GOBiotic_912", "TopCorr_1548", "PosCorr_786", "NegCorr_762",
    "RF_100", "XGB_500", "Degree_500",
    "Btwns_500", "NFD_3000", "FDC_3000", "Bipartite_2226")
relabel <- c("RF_best_100" = "RF_100", "XGB_best_500" = 'XGB_500',
             "Degree_best_500" = "Degree_500", "Btwns_best_500" = "Btwns_500",
             "NFD_best_3000" = "NFD_3000", "FDC_best_3000" = "FDC_3000",
             "Bipartite_best_2226" = "Bipartite_2226")


p4 <- ggplot(df_logTPM1_bc, aes(x=FS_Method, y=logTPM.1)) +
    geom_boxplot(aes(fill=Treatment), color=c("black"), width=0.5, outlier.shape = 1) +
    # geom_jitter(size=1.5,position=position_jitter(0.15),alpha=0.7) + # shape=16,
    # geom_hline(yintercept=0.632,linetype="dashed", color = "dark gray") +
    # ylim(c(0.9, 1.2)) +
    scale_color_brewer(palette="Dark2") +
    scale_fill_manual(values = c("#00bec3")) +
    scale_x_discrete(limits = fs_list, labels = relabel) +
    theme_classic() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_blank()) +
    theme(axis.title.x=element_blank()) +
    ylab("At-Bc\nlog(TPM+1)")
# p4

p5 <- ggplot(df_logTPM1_scl, aes(x=FS_Method, y=logTPM.1)) +
    geom_boxplot(aes(fill=Treatment), color=c("black"), width=0.5, outlier.shape = 1) +
    # geom_jitter(size=1.5,position=position_jitter(0.15),alpha=0.7) + # shape=16,
    # geom_hline(yintercept=0.632,linetype="dashed", color = "dark gray") +
    # ylim(c(0.9, 1.2)) +
    scale_color_brewer(palette="Dark2") +
    scale_x_discrete(limits = fs_list, labels = relabel) +
    theme_classic() +
    theme(legend.position = "right") +
    theme(axis.text.x =element_blank()) +
    theme(axis.title.x=element_blank()) +
    ylab("At-Scl\nlog(TPM+1)")
# p5

p6 <- ggplot(df_logTPM1_psy, aes(x=FS_Method, y=logTPM.1)) +
    geom_boxplot(aes(fill=Treatment), color=c("black"), width=0.5, outlier.shape = 1) +
    # geom_jitter(size=1.5,position=position_jitter(0.15),alpha=0.7) + # shape=16,
    # geom_hline(yintercept=0.632,linetype="dashed", color = "dark gray") +
    # ylim(c(0.9, 1.2)) +
    scale_color_brewer(palette="Dark2") +
    scale_x_discrete(limits = fs_list, labels = relabel) +
    theme_classic() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    theme(axis.title.x=element_blank()) +
    ylab("At-Psy\nlog(TPM+1)")

# p6

p <- p2 + p4 + p5 + p6  + plot_layout(ncol = 1)
p

pdf(file="results/MLSummary/MLApply/Fig6_AccuracyTPMs.pdf",
        pointsize=10, height=8, width=7)
p
dev.off()

#### Upset Plot
# install.packages("remotes")
# remotes::install_github("jokergoo/ComplexHeatmap")
#### Set working directory
# library(ComplexHeatmap)


rm(list=ls())
setwd("/Users/jayson/Research/Projects/AtBotML/scripts/Wei/")
library(UpSetR)
## files
xgb_ft <- read.csv("../../results/upset/03a01_At_xgb_list_500.csv", header=FALSE)
xgb_ft <- unlist(xgb_ft, use.names = FALSE)

rf_ft <- read.csv("../../results/upset/03a02_At_rf_list_100.csv", header=FALSE)
rf_ft <- unlist(rf_ft, use.names = FALSE)

deg_ft <- read.csv("../../results/upset/03a03b_At_degree_list_500.csv", header=FALSE)
deg_ft <- unlist(deg_ft, use.names = FALSE)

btwns_ft <- read.csv("../../results/upset/03a04b_At_betweenness_list_500.csv", header=FALSE)
btwns_ft <- unlist(btwns_ft, use.names = FALSE)

nfd_ft <- read.csv("../../results/upset/03a05b_At_nfd_list_3000.csv", header=FALSE)
nfd_ft <- unlist(nfd_ft, use.names = FALSE)

fdc_ft <- read.csv("../../results/upset/03a06c_At_fdc_list_3000.csv", header=FALSE)
fdc_ft <- unlist(fdc_ft, use.names = FALSE)

AtSim_ft <- read.csv("../../results/upset/03a07_AtSim_list_2226.csv", header=FALSE)
AtSim_ft <- unlist(AtSim_ft, use.names = FALSE)

def_ft <- read.csv("../../results/upset/03a08_defense_list_130.csv", header=FALSE)
def_ft <- unlist(def_ft, use.names = FALSE)

Go_ft <- read.csv("../../results/upset/03a09_GObiotic_list_912.csv", header=FALSE)
Go_ft <- unlist(Go_ft, use.names = FALSE)

PosCorr_ft <- read.csv("../../results/upset/03a10_TopCorr_Pos_list_786.csv", header=FALSE)
PosCorr_ft <- unlist(PosCorr_ft, use.names = FALSE)

NegCorr_ft <- read.csv("../../results/upset/03a10c_TopCorr_Neg_list_762.csv", header=FALSE)
NegCorr_ft <- unlist(NegCorr_ft, use.names = FALSE)

TopCorr_ft <- read.csv("../../results/upset/03a10_TopCorr_PosNeg_list_1548.csv", header=FALSE)
TopCorr_ft <- unlist(TopCorr_ft, use.names = FALSE)

GWAS_ft <- read.csv("../../results/upset/GWAS.95_Used.csv", header=TRUE)
GWAS_ft <- unlist(GWAS_ft, use.names = FALSE)

Bjornson_ft <- read.csv("../../results/upset/Bjornson.csv", header=FALSE)
Bjornson_ft <- unlist(Bjornson_ft, use.names = FALSE)

listInput <- list(XGB = xgb_ft, RF = rf_ft, Degree = deg_ft, Betweenness = btwns_ft,
                  NFD = nfd_ft, FDC = fdc_ft, Bipartite = AtSim_ft, Defense = def_ft,
                  Go_Biotic = Go_ft, PosCorr = PosCorr_ft, NegCorr = NegCorr_ft, TopCorr = TopCorr_ft,
                  GWAS = GWAS_ft, Bjornson = Bjornson_ft)


p_upset <- upset(fromList(listInput), nsets = 12, number.angles = 15, order.by = "freq",
      mainbar.y.label = "Gene Intersections" ) # sets.x.label = "Movies Per Genre",
p_upset

# Ordered sets
sets <- c("GWAS", "Bjornson", "TopCorr", "NegCorr", "PosCorr", "Go_Biotic", "Defense",
          "Bipartite", "FDC", "NFD", "Betweenness", "Degree", "XGB" ) # "RF",
p_upset <- upset(fromList(listInput), nsets = 12, sets = sets, mb.ratio = c(0.55, 0.45),
                 number.angles = 15, order.by = "freq",
                 mainbar.y.label = "Gene Intersections", keep.order = TRUE)

p_upset

pdf(file="results/MLSummary/MLApply/Fig7_UpSet_plot.pdf")
p_upset
dev.off()

# Ordered sets
sets <- c("GWAS", "Bjornson", "TopCorr", "NegCorr", "PosCorr", "Go_Biotic", "Defense",
          "Bipartite", "FDC", "NFD", "Betweenness", "Degree", "XGB" ) # "RF",
p_upset <- upset(fromList(listInput), nsets = 12, nintersects = 30, sets = sets,
                 group.by = "sets", cutoff = 15,
                 mb.ratio = c(0.55, 0.45),
                 number.angles = 15, order.by = "freq",
                 mainbar.y.label = "Gene Intersections", keep.order = TRUE)

p_upset

pdf(file="results/MLSummary/MLApply/Fig7c_UpSet_plot.pdf")
p_upset
dev.off()


# movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"),
#     header = T, sep = ";")
#
# upset(movies, nsets = 6, number.angles = 30, point.size = 3.5, line.size = 2,
#     mainbar.y.label = "Genre Intersections", sets.x.label = "Movies Per Genre",
#     text.scale = c(1.3, 1.3, 1, 1, 2, 0.75))
#
#
# # example of list input (list of named vectors)
# listInput <- list(one = c(1, 2, 3, 5, 7, 8, 11, 12, 13), two = c(1, 2, 4, 5,
#     10), three = c(1, 5, 6, 7, 8, 9, 10, 12, 13))
#
# upset(fromList(listInput), nsets = 9, order.by = "freq")

library(UpSetR)
library(grid)
library(gridExtra)
library(ggplot2)

NoAttBasePlot <- function (legend, size_plot_height, Main_bar_plot, Matrix_plot,
    hratios, Size_plot, query_legend, set_metadata, set_metadata_plots,
    newpage) {
    top <- 1
    bottom <- 100
    if ((!is.null(legend)) && (query_legend != tolower("none"))) {
        if (query_legend == tolower("top")) {
            top <- 3
            bottom <- 102
            legend_top <- 1
            legend_bottom <- 3
            size_plot_height <- (size_plot_height + 2)
        }
        else if (query_legend == tolower("bottom")) {
            legend_top <- 101
            legend_bottom <- 103
        }
    }
    if (is.null(set_metadata)) {
        matrix_and_mainbar_right <- 100
        matrix_and_mainbar_left <- 21
        size_bar_right <- 20
        size_bar_left <- 1
    }
    else if (!is.null(set_metadata)) {
        matrix_and_mainbar_right <- set_metadata$ncols + 100
        matrix_and_mainbar_left <- set_metadata$ncols + 21
        size_bar_right <- set_metadata$ncols + 20
        size_bar_left <- set_metadata$ncols + 1
        metadata_right <- set_metadata$ncols
        metadata_left <- 1
    }
    if (newpage) {
        grid::grid.newpage()
    }
    if ((!is.null(legend)) && (query_legend != tolower("none"))) {
        if (query_legend == tolower("top")) {
            pushViewport(viewport(layout = grid.layout(102, matrix_and_mainbar_right)))
        }
        else if (query_legend == tolower("bottom")) {
            pushViewport(viewport(layout = grid.layout(103, matrix_and_mainbar_right)))
        }
    }
    else if ((is.null(legend)) || (query_legend == tolower("none"))) {
        pushViewport(viewport(layout = grid.layout(100, matrix_and_mainbar_right)))
    }
    # Modified
    vp = UpSetR:::vplayout(top:bottom, 1:(matrix_and_mainbar_right-matrix_and_mainbar_left))
    pushViewport(vp)
    grid.draw(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios))
    popViewport()
    # Modified
    vp = UpSetR:::vplayout(size_plot_height:bottom, (matrix_and_mainbar_right-matrix_and_mainbar_left-1):96)
    pushViewport(vp)
    grid.draw(arrangeGrob(Size_plot))
    popViewport()
    if (!is.null(set_metadata)) {
        for (i in 1:length(set_metadata_plots)) {
            if (i != 1) {
                metadata_left <- 1 + metadata_right
                metadata_right <- metadata_right + set_metadata$plots[[i]]$assign
            }
            else {
                metadata_left <- 1
                metadata_right <- set_metadata$plots[[i]]$assign
            }
            vp = UpSetR:::vplayout(size_plot_height:bottom, metadata_left:metadata_right)
            pushViewport(vp)
            grid.draw(arrangeGrob(set_metadata_plots[[i]]))
            popViewport()
        }
    }
    if ((!is.null(legend)) && (query_legend != tolower("none"))) {
        vp = UpSetR:::vplayout(legend_top:legend_bottom, matrix_and_mainbar_left:matrix_and_mainbar_right)
        pushViewport(vp)
        grid.draw(arrangeGrob(legend))
        popViewport()
    }
}

Make_size_plot <- function (Set_size_data, sbar_color, ratios, ylabel, scale_sets,
    text_scale, set_size_angle, set_size.show, set_size.scale_max,
    set_size.number_size) {
    if (length(text_scale) > 1 && length(text_scale) <= 6) {
        x_axis_title_scale <- text_scale[3]
        x_axis_tick_label_scale <- text_scale[4]
    }
    else {
        x_axis_title_scale <- text_scale
        x_axis_tick_label_scale <- text_scale
    }
    if (ylabel == "Set Size" && scale_sets != "identity") {
        ylabel <- paste("Set Size", paste0("( ",
            scale_sets, " )"))
        if (scale_sets == "log2") {
            Set_size_data$y <- log2(Set_size_data$y)
        }
        if (scale_sets == "log10") {
            Set_size_data$y <- log10(Set_size_data$y)
        }
    }
    if (!is.null(set_size.number_size)) {
        num.size <- (set_size.number_size/2.845276) * x_axis_tick_label_scale
    }
    else {
        num.size <- (7/2.845276) * x_axis_tick_label_scale
    }
    Size_plot <- (ggplot(data = Set_size_data, aes_string(x = "x",
        y = "y")) + geom_bar(stat = "identity", colour = sbar_color,
        width = 0.4, fill = sbar_color, position = "identity") +
        scale_x_continuous(limits = c(0.5, (nrow(Set_size_data) +
            0.5)), breaks = c(0, max(Set_size_data)), expand = c(0,
            0)) + theme(panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(-0.11, -1.3, 0.5, 0.5), "lines"),
        axis.title.x = element_text(size = 8.3 * x_axis_title_scale),
        axis.text.x = element_text(size = 7 * x_axis_tick_label_scale,
            vjust = 1, hjust = 0.5), axis.line = element_line(colour = "gray0"),
        axis.line.y = element_blank(), axis.line.x = element_line(colour = "gray0",
            size = 0.3), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
        xlab(NULL) + ylab(ylabel) + coord_flip())
    if (set_size.show == TRUE) {
        Size_plot <- (Size_plot + geom_text(aes(label = y, vjust = 0.5,
            hjust = 1.2, angle = set_size_angle), size = num.size))
    }
    if (scale_sets == "log10") {
        if (!is.null(set_size.scale_max)) {
            Size_plot <- (Size_plot + scale_y_continuous(limits = c(set_size.scale_max,
                0), trans = log10_reverse_trans()))
        }
        else {
            Size_plot <- (Size_plot + scale_y_continuous(trans = log10_reverse_trans()))
        }
    }
    else if (scale_sets == "log2") {
        if (!is.null(set_size.scale_max)) {
            Size_plot <- (Size_plot + scale_y_continuous(limits = c(set_size.scale_max,
                0), trans = log2_reverse_trans()))
        }
        else {
            Size_plot <- (Size_plot + scale_y_continuous(trans = log2_reverse_trans()))
        }
    }
    else {
        if (!is.null(set_size.scale_max)) {
            Size_plot <- (Size_plot + scale_y_continuous(limits = c(set_size.scale_max,
                0), trans = "reverse"))
        }
        else {
            # Modified
            #Size_plot <- (Size_plot + scale_y_continuous(trans = "reverse"))
        }
    }
    Size_plot <- ggplot_gtable(ggplot_build(Size_plot))
    return(Size_plot)
}

assignInNamespace(x="NoAttBasePlot", value=NoAttBasePlot, ns="UpSetR")
assignInNamespace(x="Make_size_plot", value=Make_size_plot, ns="UpSetR")

movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"),
                   header=TRUE, sep=";")
# p_upset <- upset(movies, nsets = 7, nintersects = 30, mb.ratio = c(0.5, 0.5),
#       order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE))

p_upset <- upset(movies, nsets = 7, nintersects = 30, mb.ratio = c(0.5, 0.5),
      order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE))

# Ordered sets
sets <- c("GWAS", "Bjornson", "TopCorr", "NegCorr", "PosCorr", "Go_Biotic", "Defense",
          "Bipartite", "FDC", "NFD", "Betweenness", "Degree", "XGB" ) # "RF",
p_upset <- upset(fromList(listInput), nsets = 12, nintersects = 30, sets = sets,
                 group.by = "sets", cutoff = 15,
                 mb.ratio = c(0.55, 0.45),
                 number.angles = 15, order.by = "freq",
                 mainbar.y.label = "Gene Intersections", keep.order = TRUE)

p_upset

pdf(file="results/MLSummary/MLApply/Fig7b_UpSet_plot.pdf",  pointsize=10,height=8, width=8)
p_upset
dev.off()