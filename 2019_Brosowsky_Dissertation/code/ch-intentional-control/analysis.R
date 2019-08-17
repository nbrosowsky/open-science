print(getwd())

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

load("../data/ch-intentional-control/data_E1.Rda")

## pre-process data 
data_E1<-data_E1[data_E1$RTs>0,]
data_E1<-data_E1[data_E1$RTs<5000,]
levels(data_E1$Proportion) <- c("25% (Invalid)", "75% (Valid)")
levels(data_E1$TargetType) <- c("Color", "Letter")
data_E1$Proportion <- factor(data_E1$Proportion, levels = c("75% (Valid)", "25% (Invalid)"))

## 2x2 ANOVA 
aov.out<-aov_car(ACC ~ TargetType * Proportion + Error(Subject/(TargetType*Proportion)), 
                 data = data_E1 %>%
                     mutate(Subject = factor(Subject)) %>%
                     group_by(Subject,Proportion,TargetType) %>%
                     summarise(
                         ACC = mean(ACC)*100
                     ) 
                 )


E1.ACC.results<-apa_print(aov.out, es = "pes")$full_result
rm(aov.out)

rm(data_E1)

##################### END EXP 1 ##################

#################### Exp 2 #######################

load("../data/ch-intentional-control/data_E2.Rda")

## pre-process data 
data_E2<-data_E2[data_E2$RTs>0,]
data_E2<-data_E2[data_E2$RTs<5000,]
levels(data_E2$Proportion) <- c("100%", "25% (Invalid)", "75% (Valid)")
levels(data_E2$TargetType) <- c("Color", "Letter")

data_E2$Proportion <- factor(data_E2$Proportion, levels = c("100%", "75% (Valid)", "25% (Invalid)"))

## 2x2 ANOVA 
aov.out<-aov_car(ACC ~ TargetType + Error(Subject/(TargetType)), 
                 data = data_E2 %>%
                     filter(TrialType == "Training") %>%
                     mutate(Subject = factor(Subject)) %>%
                     group_by(Subject,Proportion,TargetType) %>%
                     summarise(
                         ACC = mean(ACC)*100
                     ) 
)


E2.ACC.training.results<-apa_print(aov.out, es = "pes")$full_result
rm(aov.out)

aov.out<-aov_car(ACC ~ TargetType * Proportion + Error(Subject/(TargetType*Proportion)), 
                 data = data_E2 %>%
                     filter(TrialType != "Training") %>%
                     mutate(Subject = factor(Subject)) %>%
                     group_by(Subject,Proportion,TargetType) %>%
                     summarise(
                         ACC = mean(ACC)
                     ) 
)


E2.ACC.mixed.results<-apa_print(aov.out, es = "pes")$full_result
rm(aov.out)


ACC.training<-data_E2[data_E2$TrialType == "Training",]
ACC.training$TrialType<-factor(ACC.training$TrialType)
ACC.training<-ACC.training %>%
    group_by(Subject,TrialType,TargetType) %>%
    summarise(ACC = mean(ACC))

ACC.training$Phase<-"Training"

ACC.mixed<-data_E2[data_E2$TrialType != "Training",]
ACC.mixed$TrialType<-factor(ACC.mixed$TrialType)
ACC.mixed<-ACC.mixed %>%
    group_by(Subject,TrialType,TargetType) %>%
    summarise(ACC = mean(ACC))
ACC.mixed$Phase<-"Mixed"
ACC.all<-rbind(ACC.training,ACC.mixed)


ACC.all$Phase<-factor(ACC.all$Phase)
ACC.all$Subject<-factor(ACC.all$Subject)
ACC.all$TrialType<-factor(ACC.all$TrialType)

ACC.all <- ACC.all %>%
    group_by(Subject,Phase) %>%
    summarise(ACC = mean(ACC)*100)

E2.ACC.ttest<-apa_print(t.test(ACC ~ Phase, data = ACC.all, paired = TRUE))$statistic
rm(ACC.all)
rm(ACC.mixed)
rm(ACC.training)


# clean up
rm(data_E2)

##################### END EXP 2 #######################

#################### EXP 3 ############################

load("../data/ch-intentional-control/data_E3.Rda")

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

## 2x2 ANOVA 
aov.out<-aov_car(ACC ~ TargetType + Error(Subject/(TargetType)), 
                 data = data_E3 %>%
                     filter(Proportion == "100%") %>%
                     mutate(Subject = factor(Subject)) %>%
                     group_by(Subject,Proportion,TargetType) %>%
                     summarise(
                         ACC = mean(ACC)*100
                     ) 
)


E3.ACC.training.results<-apa_print(aov.out, es = "pes")$full_result
rm(aov.out)

aov.out<-aov_car(ACC ~ TargetType * Proportion + Error(Subject/(TargetType*Proportion)), 
                 data = data_E3 %>%
                     filter(Proportion != "100%") %>%
                     mutate(Subject = factor(Subject)) %>%
                     group_by(Subject,Proportion,TargetType) %>%
                     summarise(
                         ACC = mean(ACC)
                     ) 
)


E3.ACC.mixed.results<-apa_print(aov.out, es = "pes")$full_result
rm(aov.out)

ACC.training<-data_E3[data_E3$TrialType == "Training",]
ACC.training$TrialType<-factor(ACC.training$TrialType)
ACC.training<-ACC.training %>%
    group_by(Subject,TrialType,TargetType) %>%
    summarise(ACC = mean(ACC))

ACC.training$Phase<-"Training"

ACC.mixed<-data_E3[data_E3$TrialType != "Training",]
ACC.mixed$TrialType<-factor(ACC.mixed$TrialType)
ACC.mixed<-ACC.mixed %>%
    group_by(Subject,TrialType,TargetType) %>%
    summarise(ACC = mean(ACC))
ACC.mixed$Phase<-"Mixed"
ACC.all<-rbind(ACC.training,ACC.mixed)


ACC.all$Phase<-factor(ACC.all$Phase)
ACC.all$Subject<-factor(ACC.all$Subject)
ACC.all$TrialType<-factor(ACC.all$TrialType)

ACC.all <- ACC.all %>%
    group_by(Subject,Phase) %>%
    summarise(ACC = mean(ACC)*100)

E3.ACC.ttest<-apa_print(t.test(ACC ~ Phase, data = ACC.all, paired = TRUE))$statistic
rm(ACC.all)
rm(ACC.mixed)
rm(ACC.training)

# clean up
rm(data_E3)

#################### EXP 4 ############################

load("../data/ch-intentional-control/data_E4.Rda")

lowACC<-c(6,16,23,26,3,14)
## remove participants with low task accuracy
data_E4 <- data_E4 %>%
    filter(!Subject%in%lowACC)
rm(lowACC)

levels(data_E4$Proportion) <- c("100%", "75% (Invalid)", "25% (Valid)")
levels(data_E4$TargetType) <- c("Color", "Letter")

#data_E4$Proportion <- factor(data_E4$Proportion, levels = c("100%", "75% (Valid)", "25% (Invalid)"))

## 2x2 ANOVA 
aov.out<-aov_car(ACC ~ TargetType + Error(Subject/(TargetType)), 
                 data = data_E4 %>%
                     filter(Proportion == "100%") %>%
                     mutate(Subject = factor(Subject)) %>%
                     group_by(Subject,Proportion,TargetType) %>%
                     summarise(
                         ACC = mean(ACC)*100
                     ) 
)


E4.ACC.training.results<-apa_print(aov.out, es = "pes")$full_result
rm(aov.out)

aov.out<-aov_car(ACC ~ block.type * TargetType * Proportion + Error(Subject/(block.type*TargetType*Proportion)), 
                 data = data_E4 %>%
                     filter(Proportion != "100%") %>%
                     mutate(Subject = factor(Subject)) %>%
                     group_by(Subject,block.type,Proportion,TargetType) %>%
                     summarise(
                         ACC = mean(ACC)
                     ) 
)


E4.ACC.mixed.results<-apa_print(aov.out, es = "pes")$full_result
rm(aov.out)

E4.ACC.ttest1<-apa_print(t.test(ACC ~ Proportion, 
                      data = data_E4 %>%
                          filter(Proportion != "100%",
                                 block.type == "T.Context") %>%
                          group_by(Subject,Proportion) %>%
                          summarise(
                              ACC = mean(ACC)
                          ),
                      paired = TRUE
                      ))$statistic


E4.ACC.ttest2<-apa_print(t.test(ACC ~ Proportion, 
                                data = data_E4 %>%
                                    filter(Proportion != "100%",
                                           block.type == "R.Context") %>%
                                    group_by(Subject,Proportion) %>%
                                    summarise(
                                        ACC = mean(ACC)
                                    ),
                                paired = TRUE
))$statistic

#### block analysis ##

aov.out<-aov_car(ACC ~ block * block.type * TargetType * Proportion + Error(Subject/(block*block.type*TargetType*Proportion)), 
                 data = data_E4 %>%
                     filter(Proportion != "100%") %>%
                     mutate(Subject = factor(Subject)) %>%
                     group_by(Subject,block,block.type,Proportion,TargetType) %>%
                     summarise(
                         ACC = mean(ACC)
                     ) 
)


E4.ACC.block.results<-apa_print(aov.out, es = "pes")$full_result
E4.ACC.block.results<-E4.ACC.block.results[c(12,15)]
rm(aov.out)


aov.out<-aov_car(ACC ~ block.type * Proportion + Error(Subject/(block.type*Proportion)), 
                 data = data_E4 %>%
                     filter(Proportion != "100%",
                            block == "1") %>%
                     mutate(Subject = factor(Subject)) %>%
                     group_by(Subject,block.type,Proportion) %>%
                     summarise(
                         ACC = mean(ACC)
                     ) 
)


E4.ACC.block1.results<-apa_print(aov.out, es = "pes")$full_result
rm(aov.out)


E4.ACC.ttest3<-apa_print(t.test(ACC ~ Proportion, 
                                data = data_E4 %>%
                                    filter(Proportion != "100%",
                                           block == "1",
                                           block.type == "T.Context") %>%
                                    group_by(Subject,Proportion) %>%
                                    summarise(
                                        ACC = mean(ACC)
                                    ),
                                paired = TRUE
))$statistic


E4.ACC.ttest4<-apa_print(t.test(ACC ~ Proportion, 
                                data = data_E4 %>%
                                    filter(Proportion != "100%",
                                           block == "1",
                                           block.type == "R.Context") %>%
                                    group_by(Subject,Proportion) %>%
                                    summarise(
                                        ACC = mean(ACC)
                                    ),
                                paired = TRUE
))$statistic



aov.out<-aov_car(ACC ~ block.type * Proportion + Error(Subject/(block.type*Proportion)), 
                 data = data_E4 %>%
                     filter(Proportion != "100%",
                            block == "2") %>%
                     mutate(Subject = factor(Subject)) %>%
                     group_by(Subject,block.type,Proportion) %>%
                     summarise(
                         ACC = mean(ACC)
                     ) 
)


E4.ACC.block2.results<-apa_print(aov.out, es = "pes")$full_result
rm(aov.out)

# clean up
rm(data_E4)
