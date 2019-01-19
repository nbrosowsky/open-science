library(afex)
library(dplyr)
library(ggplot2)
library(cowplot)
#apa_print requires development version of papaja (2018-12-16)
library(papaja)



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

## add data for table
forTable<-c()

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


### E1 RT Analysis
E1.RT.summary <- data_E1 %>%
    filter(ACC == 1) %>%
    mutate(Subject = factor(Subject)) %>%
    group_by(Subject,Proportion,TargetType) %>%
    summarise(
        RTs=mean(vjout(RTs)$data)
    ) %>%
    group_by(Proportion,TargetType) %>%
    summarise(
        N    = length(RTs),
        mean = mean(RTs),
        sd   = sd(RTs),
        se   = sd / sqrt(N)
    ) %>%
    select(TargetType,Proportion,N,mean,sd,se) %>%
    arrange(TargetType,Proportion)


# Exp 1 color
forTable<-rbind(forTable,
                c("-","-",
                  round(E1.ACC.summary$mean[1],digits=2),round(E1.ACC.summary$se[1],digits=2),
                  round(E1.ACC.summary$mean[2],digits=2),round(E1.ACC.summary$se[2],digits=2))
)

forTable<-rbind(forTable,
                c("-","-",
                  round(E1.RT.summary$mean[1],digits=0),round(E1.RT.summary$se[1],digits=0),
                  round(E1.RT.summary$mean[2],digits=0),round(E1.RT.summary$se[2],digits=0))
)


# Exp 1 letter
forTable<-rbind(forTable,
                c("-","-",
                  round(E1.ACC.summary$mean[3],digits=2),round(E1.ACC.summary$se[3],digits=2),
                  round(E1.ACC.summary$mean[4],digits=2),round(E1.ACC.summary$se[4],digits=2))
)
forTable<-rbind(forTable,
                c("-","-",
                  round(E1.RT.summary$mean[3],digits=0),round(E1.RT.summary$se[3],digits=0),
                  round(E1.RT.summary$mean[4],digits=0),round(E1.RT.summary$se[4],digits=0))
)

rm(data_E1)
rm(E1.ACC.summary)
rm(E1.RT.summary)

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

### E2 RT Analysis
E2.RT.summary <- data_E2 %>%
    filter(ACC == 1) %>%
    mutate(Subject = factor(Subject)) %>%
    group_by(Subject,Proportion,TargetType) %>%
    summarise(
        RTs=mean(vjout(RTs)$data)
    ) %>%
    group_by(Proportion,TargetType) %>%
    summarise(
        N    = length(RTs),
        mean = mean(RTs),
        sd   = sd(RTs),
        se   = sd / sqrt(N)
    ) %>%
    select(TargetType,Proportion,N,mean,sd,se) %>%
    arrange(TargetType,Proportion)


#### ADD TO TABLE DATA ####
# Exp 2 color
forTable<-rbind(forTable,
                c(round(E2.ACC.summary$mean[1],digits=2),round(E2.ACC.summary$se[1],digits=2),
                  round(E2.ACC.summary$mean[2],digits=2),round(E2.ACC.summary$se[2],digits=2),
                  round(E2.ACC.summary$mean[3],digits=2),round(E2.ACC.summary$se[3],digits=2)
                )
)

forTable<-rbind(forTable,
                c(round(E2.RT.summary$mean[1],digits=0),round(E2.RT.summary$se[1],digits=0),
                  round(E2.RT.summary$mean[2],digits=0),round(E2.RT.summary$se[2],digits=0),
                  round(E2.RT.summary$mean[3],digits=0),round(E2.RT.summary$se[3],digits=0)
                )
)


# Exp 2 letter
forTable<-rbind(forTable,
                c(round(E2.ACC.summary$mean[4],digits=2),round(E2.ACC.summary$se[4],digits=2),
                  round(E2.ACC.summary$mean[5],digits=2),round(E2.ACC.summary$se[5],digits=2),
                  round(E2.ACC.summary$mean[6],digits=2),round(E2.ACC.summary$se[6],digits=2)
                )
)
forTable<-rbind(forTable,
                c(round(E2.RT.summary$mean[4],digits=0),round(E2.RT.summary$se[4],digits=0),
                  round(E2.RT.summary$mean[5],digits=0),round(E2.RT.summary$se[5],digits=0),
                  round(E2.RT.summary$mean[6],digits=0),round(E2.RT.summary$se[6],digits=0)
                )
)


# clean up
rm(data_E2)
rm(E2.ACC.summary)
rm(E2.RT.summary)


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

### E3 RT Analysis
E3.RT.summary <- data_E3 %>%
    filter(ACC == 1) %>%
    mutate(Subject = factor(Subject)) %>%
    group_by(Subject,Proportion,TargetType) %>%
    summarise(
        RTs=mean(vjout(RTs)$data)
    ) %>%
    group_by(Proportion,TargetType) %>%
    summarise(
        N    = length(RTs),
        mean = mean(RTs),
        sd   = sd(RTs),
        se   = sd / sqrt(N)
    ) %>%
    select(TargetType,Proportion,N,mean,sd,se) %>%
    arrange(TargetType,Proportion)


#### ADD TO TABLE DATA ####
# Exp 2 color
forTable<-rbind(forTable,
                c(round(E3.ACC.summary$mean[1],digits=2),round(E3.ACC.summary$se[1],digits=2),
                  round(E3.ACC.summary$mean[2],digits=2),round(E3.ACC.summary$se[2],digits=2),
                  round(E3.ACC.summary$mean[3],digits=2),round(E3.ACC.summary$se[3],digits=2)
                )
)

forTable<-rbind(forTable,
                c(round(E3.RT.summary$mean[1],digits=0),round(E3.RT.summary$se[1],digits=0),
                  round(E3.RT.summary$mean[2],digits=0),round(E3.RT.summary$se[2],digits=0),
                  round(E3.RT.summary$mean[3],digits=0),round(E3.RT.summary$se[3],digits=0)
                )
)


# Exp 2 letter
forTable<-rbind(forTable,
                c(round(E3.ACC.summary$mean[4],digits=2),round(E3.ACC.summary$se[4],digits=2),
                  round(E3.ACC.summary$mean[5],digits=2),round(E3.ACC.summary$se[5],digits=2),
                  round(E3.ACC.summary$mean[6],digits=2),round(E3.ACC.summary$se[6],digits=2)
                )
)
forTable<-rbind(forTable,
                c(round(E3.RT.summary$mean[4],digits=0),round(E3.RT.summary$se[4],digits=0),
                  round(E3.RT.summary$mean[5],digits=0),round(E3.RT.summary$se[5],digits=0),
                  round(E3.RT.summary$mean[6],digits=0),round(E3.RT.summary$se[6],digits=0)
                )
)

# clean up
rm(data_E3)
rm(E3.ACC.summary)
rm(E3.RT.summary)


#################### EXP 4 ############################

load("data/data_E4.Rda")

lowACC<-c(6,16,23,26,3,14)
## remove participants with low task accuracy
data_E4 <- data_E4 %>%
    filter(!Subject%in%lowACC)
rm(lowACC)

levels(data_E4$Proportion) <- c("100%", "75% (Invalid)", "25% (Valid)")
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

### E4 RT Analysis

### There's an error here in the published table...
### To replicate the published table I need to use < 2500 ms for non-training.. and not for training
### Also did not apply van selst outlier for some reason

E4.RT.training <- data_E4 %>%
    filter(ACC == 1,
           Proportion == "100%") %>%
    mutate(Subject = factor(Subject)) %>%
    group_by(Subject,block.type,Proportion,TargetType) %>%
    summarise(
        RTs=mean(RTs)
    ) %>%
    group_by(block.type,Proportion,TargetType) %>%
    summarise(
        N    = length(RTs),
        mean = mean(RTs),
        sd   = sd(RTs),
        se   = sd / sqrt(N)
    ) %>%
    select(block.type,TargetType,Proportion,N,mean,sd,se) %>%
    arrange(desc(block.type,TargetType,Proportion))

E4.RT.summary <- data_E4 %>%
    filter(ACC == 1,
           Proportion != "100%",
           RTs < 2500) %>%
    mutate(Subject = factor(Subject)) %>%
    group_by(Subject,block.type,Proportion,TargetType) %>%
    summarise(
        RTs=mean(RTs)
    ) %>%
    group_by(block.type,Proportion,TargetType) %>%
    summarise(
        N    = length(RTs),
        mean = mean(RTs),
        sd   = sd(RTs),
        se   = sd / sqrt(N)
    ) %>%
    select(block.type,TargetType,Proportion,N,mean,sd,se) %>%
    arrange(desc(block.type,TargetType,Proportion))

E4.RT.summary<-rbind(E4.RT.summary,E4.RT.training)
rm(E4.RT.training)

E4.RT.summary <- E4.RT.summary %>%
    arrange(desc(block.type),TargetType,Proportion)

#### ADD TO TABLE DATA ####
# Exp 2 color
forTable<-rbind(forTable,
                c(round(E4.ACC.summary$mean[1],digits=2),round(E4.ACC.summary$se[1],digits=2),
                  round(E4.ACC.summary$mean[2],digits=2),round(E4.ACC.summary$se[2],digits=2),
                  round(E4.ACC.summary$mean[3],digits=2),round(E4.ACC.summary$se[3],digits=2)
                )
)

forTable<-rbind(forTable,
                c(round(E4.RT.summary$mean[1],digits=0),round(E4.RT.summary$se[1],digits=0),
                  round(E4.RT.summary$mean[2],digits=0),round(E4.RT.summary$se[2],digits=0),
                  round(E4.RT.summary$mean[3],digits=0),round(E4.RT.summary$se[3],digits=0)
                )
)


# Exp 2 letter
forTable<-rbind(forTable,
                c(round(E4.ACC.summary$mean[4],digits=2),round(E4.ACC.summary$se[4],digits=2),
                  round(E4.ACC.summary$mean[5],digits=2),round(E4.ACC.summary$se[5],digits=2),
                  round(E4.ACC.summary$mean[6],digits=2),round(E4.ACC.summary$se[6],digits=2)
                )
)
forTable<-rbind(forTable,
                c(round(E4.RT.summary$mean[4],digits=0),round(E4.RT.summary$se[4],digits=0),
                  round(E4.RT.summary$mean[5],digits=0),round(E4.RT.summary$se[5],digits=0),
                  round(E4.RT.summary$mean[6],digits=0),round(E4.RT.summary$se[6],digits=0)
                )
)

# Exp 2 REVERSED CONTEXT
forTable<-rbind(forTable,
                c("-","-",
                  round(E4.ACC.summary$mean[7],digits=2),round(E4.ACC.summary$se[7],digits=2),
                  round(E4.ACC.summary$mean[8],digits=2),round(E4.ACC.summary$se[8],digits=2)
                )
)
forTable<-rbind(forTable,
                c("-","-",
                  round(E4.RT.summary$mean[7],digits=0),round(E4.RT.summary$se[7],digits=0),
                  round(E4.RT.summary$mean[8],digits=0),round(E4.RT.summary$se[8],digits=0)
                )
)

forTable<-rbind(forTable,
                c("-","-",
                  round(E4.ACC.summary$mean[9],digits=2),round(E4.ACC.summary$se[9],digits=2),
                  round(E4.ACC.summary$mean[10],digits=2),round(E4.ACC.summary$se[10],digits=2)
                )
)
forTable<-rbind(forTable,
                c("-","-",
                  round(E4.RT.summary$mean[9],digits=0),round(E4.RT.summary$se[9],digits=0),
                  round(E4.RT.summary$mean[10],digits=0),round(E4.RT.summary$se[10],digits=0)
                )
)


# clean up
rm(data_E4)
rm(E4.ACC.summary)
rm(E4.RT.summary)

IC_Table<-paste(
    "\\begin{table}[htbp]",
    "\\caption{Mean correct color and letter identification response latencies, standard errors, and accuracy rates for all experiments.}",
    "\\label{IC_table}",
    "\\centering",
    "\\begin{tabular}{cccccccccc}",
    " & & & & \\multicolumn{2}{c}{Training Phase} & \\multicolumn{4}{c}{Mixed Phase} \\\\", 
    "\\cmidrule(rl){5-6}",
    "\\cmidrule(rl){7-10}",
    " & & & & \\multicolumn{2}{c}{100\\%} & \\multicolumn{2}{c}{75\\%} & \\multicolumn{2}{c}{25\\%}  \\\\",
    "\\cmidrule(rl){5-6}",
    "\\cmidrule(rl){7-8}",
    "\\cmidrule(rl){9-10}",
    " & & \\multicolumn{1}{c}{Task} & & \\multicolumn{1}{c}{M} & \\multicolumn{1}{c}{SE} & \\multicolumn{1}{c}{M} & \\multicolumn{1}{c}{SE} & \\multicolumn{1}{c}{M} & \\multicolumn{1}{c}{SE} \\\\",
    "\\midrule", 
    
    "\\multicolumn{2}{l}{\\textbf{Exp. 1}}  &   &    &     &     &    &  & & \\\\",
    paste0("& & \\multicolumn{1}{c}{Color} & \\multicolumn{1}{c}{ACC} & \\multicolumn{1}{c}{", paste(forTable[1,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{", paste(forTable[2,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{c}{Letter} & \\multicolumn{1}{c}{ACC} & \\multicolumn{1}{c}{", paste(forTable[3,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{", paste(forTable[4,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    "\\cmidrule(l){2-10}",
    " &  & & & & & & & & \\\\",
    
    "\\multicolumn{2}{l}{\\textbf{Exp. 2}}  &   &    &     &     &    &  & & \\\\",
    paste0("& & \\multicolumn{1}{c}{Color} & \\multicolumn{1}{c}{ACC} & \\multicolumn{1}{c}{", paste(forTable[5,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{", paste(forTable[6,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{c}{Letter} & \\multicolumn{1}{c}{ACC} & \\multicolumn{1}{c}{", paste(forTable[7,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{", paste(forTable[8,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    "\\cmidrule(l){2-10}",
    "& & & & & & & & & \\\\",
    
    "\\multicolumn{2}{l}{\\textbf{Exp. 3}}  &   &    &     &     &    &  & & \\\\",
    paste0("& & \\multicolumn{1}{c}{Color} & \\multicolumn{1}{c}{ACC} & \\multicolumn{1}{c}{", paste(forTable[9,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{", paste(forTable[10,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{c}{Letter} & \\multicolumn{1}{c}{ACC} & \\multicolumn{1}{c}{", paste(forTable[11,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{", paste(forTable[12,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    "\\cmidrule(l){2-10}",
    " & & & & & & & & & \\\\",
    
    "\\multicolumn{2}{l}{\\textbf{Exp. 4}}  &   &    &     &     &    &  & & \\\\",
    paste0("& \\multicolumn{1}{l}{Trained Context} & \\multicolumn{1}{c}{Color} & \\multicolumn{1}{c}{ACC} & \\multicolumn{1}{c}{", paste(forTable[13,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{", paste(forTable[14,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{c}{Letter} & \\multicolumn{1}{c}{ACC} & \\multicolumn{1}{c}{", paste(forTable[15,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{", paste(forTable[16,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    "\\cmidrule(rl){3-10}",
    
    paste0("& \\multicolumn{1}{l}{Reversed Context} & \\multicolumn{1}{c}{Color} & \\multicolumn{1}{c}{ACC} & \\multicolumn{1}{c}{", paste(forTable[17,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{", paste(forTable[18,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{c}{Letter} & \\multicolumn{1}{c}{ACC} & \\multicolumn{1}{c}{", paste(forTable[19,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    paste0("& & & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{", paste(forTable[20,],collapse="} & \\multicolumn{1}{c}{"), "} \\\\"),
    " & & & & & & & & & \\\\",
    "\\bottomrule",
    "\\multicolumn{10}{l}{\\textit{Note}: RT = Reaction Time (ms);  ACC = Accuracy (\\%);  M = Mean; SE = } \\\\",
    "\\multicolumn{10}{l}{Standard Error;  100\\%\\textbackslash 75\\%\\textbackslash 25\\% = Cue Validity.} \\\\",
    "\\end{tabular}%",
    "\\end{table}",
    sep = "\n"
)