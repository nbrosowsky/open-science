library(dplyr)
library(ggplot2)
library(cowplot)

vjout <- function(x) {
    xm <- mean(x)
    xsize <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
               25, 30, 35, 50, 100)
    stds <- c(3, 1.458, 1.68, 1.841, 1.961, 2.05, 2.12, 2.173, 
              2.22, 2.246, 2.274, 2.31, 2.326, 2.391, 2.41, 2.4305, 
              2.45, 2.48, 2.5)
    stdindex <- length(xsize[xsize <= length(x)])
    removed <- x[x < xm+sd(x)*stds[stdindex]]
    removed <- removed[removed > xm - (sd(x)*stds[stdindex])]
    proportionRemoved <- length(removed)/length(x)
    list(data = removed, pr = proportionRemoved)
}


########### Exp 1. #################

load("data/data_E1.Rda")

## pre-process data 
data_E1<-data_E1[data_E1$RTs>0,]
data_E1<-data_E1[data_E1$RTs<5000,]
levels(data_E1$Proportion) <- c("25% (Invalid)", "75% (Valid)")
levels(data_E1$TargetType) <- c("Color", "Letter")
data_E1$Proportion <- factor(data_E1$Proportion, levels = c("75% (Valid)", "25% (Invalid)"))

E1.ACC.summary <- data_E1 %>%
    mutate(Subject = factor(Subject)) %>%
    group_by(Subject,Proportion,TargetType) %>%
    summarise(
        ACC = mean(ACC)*100
    ) %>%
    group_by(Proportion,TargetType) %>%
    summarise(
        N    = length(ACC),
        mean = mean(ACC),
        sd   = sd(ACC),
        se   = sd / sqrt(N)
    ) %>%
    select(TargetType,Proportion,N,mean,sd,se) %>%
    arrange(TargetType,Proportion)


#Exp 1
limits <- aes(ymax = mean + se, ymin = mean - se)
E1_figure <- ggplot(E1.ACC.summary,aes(x=TargetType, y=mean,fill=Proportion))+
    theme(text = element_text(family="Times")) +
    geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
    scale_fill_manual(values=c("gray60", "white"))+
    geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
    coord_cartesian(ylim = c(0,100))+
    scale_y_continuous(breaks=seq(0, 100, 25), expand = c(0,0))+
    theme(axis.text = element_text(size=7.5),
          axis.title =element_text(size=10,face="bold"),
          axis.line = element_line(size = .5),
          panel.background = element_blank()) +
    ylab("Accuracy (%)")+
    xlab("Task") +
    theme(       legend.position=c(1,1),
                 legend.justification = c(1,1),
                 #legend.direction = "horizontal",
                 legend.background = element_rect(colour = "black", fill = "white", size=.5),
                 legend.box.background = element_rect(colour = "black"),
                 legend.margin = margin(t = .2, r = .2, b = .2, l = .2, unit = "cm"),
                 legend.text = element_text(size = 8, margin =  margin(t = .05, r = .05, b = .2, l = .2, unit = "cm")),
                 legend.title = element_blank()
    ) + guides(fill=guide_legend(title.position = "top"))


ggsave(filename="IC-figure2.pdf",device="pdf",path="figures/",width = 3.5, height = 3, dpi = 600)

rm(data_E1)
rm(E1.ACC.summary)
rm(E1_figure)

##################### END EXP 1 ##################

#################### Exp 2 #######################

load("data/data_E2.Rda")

## pre-process data 
data_E2<-data_E2[data_E2$RTs>0,]
data_E2<-data_E2[data_E2$RTs<5000,]
levels(data_E2$Proportion) <- c("100%", "25% (Invalid)", "75% (Valid)")
levels(data_E2$TargetType) <- c("Color", "Letter")

data_E2$Proportion <- factor(data_E2$Proportion, levels = c("100%", "75% (Valid)", "25% (Invalid)"))

E2.ACC.summary<-data_E2 %>%
    group_by(Subject,Proportion,TargetType) %>%
    summarise(ACC = mean(ACC)*100) %>%
    group_by(TargetType,Proportion) %>%
    summarise(
        N    = length(ACC),
        mean = mean(ACC),
        sd   = sd(ACC),
        se   = sd / sqrt(N)
    )


##### CREATE FIGURE ######
limits <- aes(ymax = mean + se, ymin = mean - se)
E2_figureA <- ggplot(E2.ACC.summary[E2.ACC.summary$Proportion == "100%",],aes(x=TargetType, y=mean,fill=TargetType))+
    theme(text = element_text(family="Times")) +
    geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
    scale_fill_manual(values=c("gray60", "white"))+
    geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
    coord_cartesian(ylim = c(0,100))+
    scale_y_continuous(breaks=seq(0, 100, 25), expand = c(0,0))+
    theme(axis.text = element_text(size=7.5),
          axis.title =element_text(size=10,face="bold"),
          axis.line = element_line(size = .5),
          panel.background = element_blank()) +
    ylab("Accuracy (%)")+
    xlab("Task") +
    theme(legend.position="none")

limits <- aes(ymax = mean + se, ymin = mean - se)
E2_figureB <- ggplot(E2.ACC.summary[E2.ACC.summary$Proportion != "100%",],aes(x=TargetType, y=mean,fill=Proportion))+
    theme(text = element_text(family="Times")) +
    geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
    scale_fill_manual(values=c("gray60", "white"))+
    geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
    coord_cartesian(ylim = c(0,100))+
    scale_y_continuous(breaks=seq(0, 100, 25), expand = c(0,0))+
    theme(axis.text = element_text(size=7.5),
          axis.title =element_text(size=10,face="bold"),
          axis.line = element_line(size = .5),
          panel.background = element_blank()) +
    ylab("Accuracy (%)")+
    xlab("Task") +
    theme(       legend.position=c(1,1),
                 legend.justification = c(1,1),
                 #legend.direction = "horizontal",
                 legend.background = element_rect(colour = "black", fill = "white", size=.5),
                 legend.box.background = element_rect(colour = "black"),
                 legend.margin = margin(t = .2, r = .2, b = .2, l = .2, unit = "cm"),
                 legend.text = element_text(size = 8, margin =  margin(t = .05, r = .05, b = .2, l = .2, unit = "cm")),
                 legend.title = element_blank()
    ) + guides(fill=guide_legend(title.position = "top"))

E2_figure<-plot_grid(E2_figureA,NULL,E2_figureB, 
                     nrow = 1, 
                     rel_widths = c(.8, 0.05, 1),
                      label_fontfamily = "Times", labels = c("A", "", "B"))


# clean up
rm(data_E2)
rm(E2.ACC.summary)
rm(E2_figureA)
rm(E2_figureB)

ggsave(filename="IC-figure3.pdf",device="pdf",path="figures/",width = 5, height = 3, dpi = 600)
rm(E2_figure)

##################### END EXP 2 #######################

#################### EXP 3 ############################

load("data/data_E3.Rda")

## remove participants with low task accuracy
data_E3 <- data_E3 %>%
    filter(
        Subject != 1,
        Subject != 4,
        Subject != 13
    )

levels(data_E3$Proportion) <- c("100%", "25% (Invalid)", "75% (Valid)")
levels(data_E3$TargetType) <- c("Color", "Letter")

data_E3$Proportion <- factor(data_E3$Proportion, levels = c("100%", "75% (Valid)", "25% (Invalid)"))

E3.ACC.summary<-data_E3 %>%
    group_by(Subject,Proportion,TargetType) %>%
    summarise(ACC = mean(ACC)*100) %>%
    group_by(TargetType,Proportion) %>%
    summarise(
        N    = length(ACC),
        mean = mean(ACC),
        sd   = sd(ACC),
        se   = sd / sqrt(N)
    )

##### CREATE FIGURE ######
limits <- aes(ymax = mean + se, ymin = mean - se)
E3_figureA <- ggplot(E3.ACC.summary[E3.ACC.summary$Proportion == "100%",],aes(x=TargetType, y=mean,fill=TargetType))+
    theme(text = element_text(family="Times")) +
    geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
    scale_fill_manual(values=c("gray60", "white"))+
    geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
    coord_cartesian(ylim = c(0,100))+
    scale_y_continuous(breaks=seq(0, 100, 25), expand = c(0,0))+
    theme(axis.text = element_text(size=7.5),
          axis.title =element_text(size=10,face="bold"),
          axis.line = element_line(size = .5),
          panel.background = element_blank()) +
    ylab("Accuracy (%)")+
    xlab("Task") +
    theme(legend.position="none")

limits <- aes(ymax = mean + se, ymin = mean - se)
E3_figureB <- ggplot(E3.ACC.summary[E3.ACC.summary$Proportion != "100%",],aes(x=TargetType, y=mean,fill=Proportion))+
    theme(text = element_text(family="Times")) +
    geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
    scale_fill_manual(values=c("gray60", "white"))+
    geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
    coord_cartesian(ylim = c(0,100))+
    scale_y_continuous(breaks=seq(0, 100, 25), expand = c(0,0))+
    theme(axis.text = element_text(size=7.5),
          axis.title =element_text(size=10,face="bold"),
          axis.line = element_line(size = .5),
          panel.background = element_blank()) +
    ylab("Accuracy (%)")+
    xlab("Task") +
    theme(       legend.position=c(1,1),
                 legend.justification = c(1,1),
                 #legend.direction = "horizontal",
                 legend.background = element_rect(colour = "black", fill = "white", size=.5),
                 legend.box.background = element_rect(colour = "black"),
                 legend.margin = margin(t = .2, r = .2, b = .2, l = .2, unit = "cm"),
                 legend.text = element_text(size = 8, margin =  margin(t = .05, r = .05, b = .2, l = .2, unit = "cm")),
                 legend.title = element_blank()
    ) + guides(fill=guide_legend(title.position = "top"))

E3_figure<-plot_grid(E3_figureA,NULL,E3_figureB, 
                     nrow = 1, 
                     rel_widths = c(.8, 0.05, 1),
                      label_fontfamily = "Times", labels = c("A", "", "B"))

# clean up
rm(data_E3)
rm(E3.ACC.summary)
rm(E3_figureA)
rm(E3_figureB)

ggsave(filename="IC-figure4.pdf",device="pdf",path="figures/",width = 5, height = 3, dpi = 600)
rm(E3_figure)


#################### EXP 4 ############################

load("data/data_E4.Rda")

lowACC<-c(6,16,23,26,3,14)
## remove participants with low task accuracy
data_E4 <- data_E4 %>%
    filter(!Subject%in%lowACC)
rm(lowACC)

levels(data_E4$Proportion) <- c("100%", "75% (Valid)", "25% (Invalid)")
levels(data_E4$TargetType) <- c("Color", "Letter")


E4.ACC.summary<-data_E4 %>%
    group_by(Subject,block.type,Proportion,TargetType) %>%
    summarise(ACC = mean(ACC)*100) %>%
    group_by(block.type,TargetType,Proportion) %>%
    summarise(
        N    = length(ACC),
        mean = mean(ACC),
        sd   = sd(ACC),
        se   = sd / sqrt(N)
    ) %>%
    arrange(desc(block.type),TargetType,Proportion)

##### CREATE FIGURE ######
limits <- aes(ymax = mean + se, ymin = mean - se)
E4_figureA <- ggplot(E4.ACC.summary[E4.ACC.summary$Proportion == "100%",],aes(x=TargetType, y=mean,fill=TargetType))+
    theme(text = element_text(family="Times")) +
    geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
    scale_fill_manual(values=c("gray60", "white"))+
    geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
    coord_cartesian(ylim = c(0,100))+
    scale_y_continuous(breaks=seq(0, 100, 25), expand = c(0,0))+
    theme(text = element_text(family="Times"),
          axis.text = element_text(size=7.5),
          axis.title =element_text(size=10,face="bold"),
          axis.line = element_line(size = .5),
          panel.background = element_blank()) +
    ylab("Accuracy (%)")+
    xlab("Task") +
    theme(legend.position="none")


E4.ACC.summary$block.type <- factor(E4.ACC.summary$block.type, levels = c("T.Context", "R.Context"))
levels(E4.ACC.summary$block.type)<-c("Trained Context","Reversed Context")
limits <- aes(ymax = mean + se, ymin = mean - se)
E4_figureB <- ggplot(E4.ACC.summary[E4.ACC.summary$Proportion != "100%",],aes(x=TargetType, y=mean,fill=Proportion))+
    theme(text = element_text(family="Times")) +
    geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
    scale_fill_manual(values=c("gray60", "white"))+
    geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
    coord_cartesian(ylim = c(0,100))+
    scale_y_continuous(breaks=seq(0, 100, 25), expand = c(0,0))+
    theme(axis.text = element_text(size=7.5),
          axis.title =element_text(size=10,face="bold"),
          axis.line = element_line(size = .5),
          panel.background = element_blank(),
          strip.background = element_rect(fill = "white", colour = "black"),
          strip.text = element_text(size = 10)) +
    ylab("Accuracy (%)")+
    xlab("Task") +
    theme(       legend.position=c(.975,.9),
                 legend.justification = c(1,1),
                 #legend.direction = "horizontal",
                 #legend.background = element_rect(colour = "black", fill = "white", size=.5),
                 #legend.box.background = element_rect(colour = "black"),
                 legend.margin = margin(t = .2, r = .2, b = .2, l = .2, unit = "cm"),
                 legend.text = element_text(size = 8, margin =  margin(t = .05, r = .05, b = .2, l = .2, unit = "cm")),
                 legend.title = element_blank()
    ) + guides(fill=guide_legend(title.position = "top")) +
    facet_wrap(~block.type)

E4_figure<-plot_grid(E4_figureA,NULL,E4_figureB, 
                     nrow = 1, 
                     rel_widths = c(.333, 0.05, 1),
                      label_fontfamily = "Times", labels = c("A", "", "B"))


ggsave(filename="IC-figure5.pdf",device="pdf",path="figures/",width = 6, height = 3, dpi = 600)
rm(E4_figure)


#### block analysis 
byBlock <- data_E4 %>%
    filter(Proportion != "100%") %>%
    mutate(Subject = factor(Subject)) %>%
    group_by(Subject,block,block.type,Proportion,TargetType) %>%
    summarise(
        ACC = mean(ACC)*100
    ) %>%
    group_by(block,block.type,Proportion,TargetType) %>%
    summarise(
        N    = length(ACC),
        mean = mean(ACC),
        sd   = sd(ACC),
        se   = sd / sqrt(N)
    )

byBlock$block.type <- factor(byBlock$block.type, levels = c("T.Context", "R.Context"))
levels(byBlock$block.type)<-c("Trained Context","Reversed Context")

limits <- aes(ymax = mean + se, ymin = mean - se)
E4_block_figureA <- ggplot(byBlock[byBlock$block == 1,],aes(x=TargetType, y=mean,fill=Proportion))+
    theme(text = element_text(family="Times")) +
    geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
    scale_fill_manual(values=c("gray60", "white"))+
    geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
    coord_cartesian(ylim = c(0,100))+
    scale_y_continuous(breaks=seq(0, 100, 25), expand = c(0,0))+
    theme(axis.text = element_text(size=7.5),
          axis.title =element_text(size=10,face="bold"),
          axis.line = element_line(size = .5),
          panel.background = element_blank(),
          strip.background = element_rect(fill = "white", colour = "black"),
          strip.text = element_text(size = 10)) +
    ylab("Accuracy (%)")+
    xlab("Task") +
    labs(title = "First Half") +
    theme(       legend.position=c(.975,.925),
                 legend.justification = c(1,1),
                 #legend.direction = "horizontal",
                 #legend.background = element_rect(colour = "black", fill = "white", size=.5),
                 #legend.box.background = element_rect(colour = "black"),
                 legend.margin = margin(t = .2, r = .2, b = .2, l = .2, unit = "cm"),
                 legend.text = element_text(size = 8, margin =  margin(t = .05, r = .05, b = .2, l = .2, unit = "cm")),
                 legend.title = element_blank()
    ) + guides(fill=guide_legend(title.position = "top", plot.title = element_text(size = 10))) +
    facet_wrap(~block.type)

E4_block_figureB <- ggplot(byBlock[byBlock$block == 2,],aes(x=TargetType, y=mean,fill=Proportion))+
    theme(text = element_text(family="Times")) +
    geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
    scale_fill_manual(values=c("gray60", "white"))+
    geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
    coord_cartesian(ylim = c(0,100))+
    scale_y_continuous(breaks=seq(0, 100, 25), expand = c(0,0))+
    theme(axis.text = element_text(size=7.5),
          axis.title =element_text(size=10,face="bold"),
          axis.line = element_line(size = .5),
          panel.background = element_blank(),
          strip.background = element_rect(fill = "white", colour = "black"),
          strip.text = element_text(size = 10)) +
    ylab("Accuracy (%)")+
    xlab("Task") +
    labs(title = "Second Half") +
    theme(       legend.position=c(.975,.925),
                 legend.justification = c(1,1),
                 #legend.direction = "horizontal",
                 #legend.background = element_rect(colour = "black", fill = "white", size=.5),
                 #legend.box.background = element_rect(colour = "black"),
                 legend.margin = margin(t = .2, r = .2, b = .2, l = .2, unit = "cm"),
                 legend.text = element_text(size = 8, margin =  margin(t = .05, r = .05, b = .2, l = .2, unit = "cm")),
                 legend.title = element_blank()
    ) + guides(fill=guide_legend(title.position = "top", plot.title = element_text(size = 10))) +
    facet_wrap(~block.type)


E4_block_figure<-plot_grid(E4_block_figureA,NULL,E4_block_figureB, 
                           nrow = 1, 
                           rel_widths = c(1, 0.05, 1),
                           label_fontfamily = "Times", labels = c("", "", ""))

ggsave(filename="IC-figure6.pdf",device="pdf",path="figures/",width = 6.5, height = 3, dpi = 600)
rm(E4_block_figure)


# clean up
rm(data_E4)
rm(E4.ACC.summary)
rm(byBlock)
rm(E4_figureA)
rm(E4_figureB)
rm(E4_block_figureA)
rm(E4_block_figureB)
rm(limits)
