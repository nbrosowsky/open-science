## Helper Functions ----

## Van Selst & Jolicoeur, 1994
## Non-recursive outlier removal function

vjoutNR <- function(x,n) {
    # vjoutNR(data, 1) returns mean with outliers removed #
    # vjoutNR(data, 2) returns percentage of trials removed #
    xm <- mean(x)
    xsize <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
               25, 30, 35, 50, 100)
    stds <- c(1.3, 1.458, 1.68, 1.841, 1.961, 2.05, 2.12, 2.173, 
              2.22, 2.246, 2.274, 2.31, 2.326, 2.391, 2.41, 2.4305, 
              2.45, 2.48, 2.5)
    stdindex <- length(xsize[xsize <= length(x)])
    removed <- x[x < xm+sd(x)*stds[stdindex]]
    removed <- removed[removed > xm - (sd(x)*stds[stdindex])]
    proportionRemoved <- length(removed)/length(x)
    finaldata<-c(mean(removed),1-proportionRemoved)
    return(finaldata[[n]])
}

print_apa_ci <- function(aov_table){
    print_eta_ci <- function(Fval, conf = .90, df1, df2){
        limits <- apaTables::get.ci.partial.eta.squared(F.value=Fval, df1=df1, df2=df2, conf.level=conf)
        return(paste0(", 90\\% CI $[",round(limits$LL, 2),"$, $",round(limits$UL, 2),"]$"))
    }
    
    pap <- apa_print(aov_table, es = "pes", correction = "none")$full_result
    
    for(i in 1:length(pap)){
        pap[i] <- paste0(pap[i], print_eta_ci(Fval = aov_table$anova_table$`F`[i], df1 = aov_table$anova_table$`num Df`[i], df2 = aov_table$anova_table$`den Df`[i]))
        pap[i] <- gsub("p = .000", "p < .001", pap[i])
        pap[i] <- gsub(") = 0.00", ") < 0.01", pap[i])
    }
    return(pap)
}
# Function takes the experiment df and returns all analyses in latex formatting
run_E1E2_analyses <- function(df){
    
    RT.DF<- df %>%
        filter(ACC == TRUE, RT < 5000, RT > 0, PrimeProbe == "Probe") %>%
        group_by(Subject,Exp,block,PrimeCon,ProbeCon) %>%
        summarise(vjoutRT = vjoutNR(RT,1))
    
    ## Remove low acc subjects
    ExploreDF <- df %>%
        filter(RT < 5000, RT > 0) %>%
        group_by(Subject,Exp,PrimeProbe,PrimeCon,ProbeCon) %>%
        summarise(meanRT = mean(RT), 
                  meanAccuracy = mean(ACC), 
                  TrialsCompleted = length(RT),
                  maxRT = max(RT),
                  vjoutRT = mean(vjoutNR(RT,1)),
                  removed = vjoutNR(RT,2))
    
    LowACC <- unique(ExploreDF %>%
                         filter(meanAccuracy <.8) %>%
                         .$Subject)
    
    if(levels(df$Exp)[1] == "E3A"){
        RT.DF<- df %>%
            filter(ACC == TRUE, RT < 5000, RT > 0, PrimeProbe == "Probe") %>%
            group_by(Subject,Exp,PrimeCon,ProbeCon) %>%
            summarise(vjoutRT = vjoutNR(RT,1))
    }
    
    
    RT.DF <- RT.DF %>%
        filter(Subject%in%LowACC == FALSE)
    ####
    
    ## Summarize Data 
    RT.DF <- RT.DF %>%
        group_by(Subject,Exp,PrimeCon,ProbeCon) %>%
        summarise(vjoutRT = mean(vjoutRT))
    
    
    RT.Diff<-RT.DF %>%
        group_by(Exp,Subject,PrimeCon,ProbeCon) %>%
        summarise(RT = mean(vjoutRT)) %>%
        spread(ProbeCon,RT) %>%
        mutate(Diff = Inc - Con) %>%
        select(-Con:-Inc)%>%
        group_by(Exp,PrimeCon) %>%
        summarise(
            N = n_distinct(Subject), 
            Flanker = mean(Diff), 
            sd = sd(Diff), 
            SE = sd/sqrt(N)
        ) 
    
    LT_RT <- aov_car(vjoutRT ~ Exp*ProbeCon*PrimeCon + Error(Subject/ProbeCon*PrimeCon), data = RT.DF)
    LT_RT <- print_apa_ci(LT_RT)
    
    ACC.DF <- df %>%
        filter(Subject%in%LowACC == FALSE, PrimeProbe == "Probe") %>%
        group_by(Subject,Exp,PrimeCon,ProbeCon) %>%
        summarise(mACC = (1-mean(ACC))*100)
    
    LT_ACC <- aov_car(mACC ~ PrimeCon*ProbeCon*Exp + Error(Subject/PrimeCon*ProbeCon), data = ACC.DF)
    LT_ACC <- print_apa_ci(LT_ACC)
    
    ## SUMMARIZE RT Data ##
    RT.DF<- df %>%
        filter(trial != 1, RT < 5000, RT > 0, ACC == TRUE) %>%
        group_by(Subject,Exp,block,Congruency,N1Congruency) %>%
        summarise(vjoutRT = vjoutNR(RT,1))
    
    # FIND LOW ACC SUBS AND REMOVE (Same as above) ##
    ExploreDF <- df %>%
        filter(RT < 5000, RT > 0) %>%
        group_by(Subject,Exp,PrimeProbe,PrimeCon,ProbeCon) %>%
        summarise(meanRT = mean(RT), 
                  meanAccuracy = mean(ACC), 
                  TrialsCompleted = length(RT),
                  maxRT = max(RT),
                  vjoutRT = mean(vjoutNR(RT,1)),
                  removed = vjoutNR(RT,2))
    
    LowACC <- unique(ExploreDF %>%
                         filter(meanAccuracy <.8) %>%
                         .$Subject)
    ####
    RT.DF <- RT.DF %>%
        filter(Subject%in%LowACC == FALSE)
    
    ## RT analysis
    RT.DF <- RT.DF %>%
        group_by(Subject,Exp,Congruency,N1Congruency) %>%
        summarise(vjoutRT = mean(vjoutRT))
    
    N1_RT <- print_apa_ci(aov_car(vjoutRT ~ Exp*Congruency*N1Congruency + Error(Subject/(Congruency*N1Congruency)), data = RT.DF))

    N1_FU <- list()
    ### FOLLOW UP T-TESTS ###
    for(i in 1:length(unique(df$Exp))){
            N1_FU[[levels(RT.DF$Exp)[i]]] <- print_apa_ci(aov_car(vjoutRT ~ Congruency*N1Congruency + Error(Subject/(Congruency*N1Congruency)), data = RT.DF[RT.DF$Exp == (levels(RT.DF$Exp)[i]),]))
        }
  
    
    ## ACC analysis
    ACC.DF <- df %>%
        filter(Subject%in%LowACC == FALSE,!is.na(N1Congruency)) %>%
        group_by(Subject,Exp,N1Congruency,Congruency) %>%
        summarise(mACC = (1-mean(ACC))*100)
    
    N1_ACC <- aov_car(mACC ~ N1Congruency*Congruency*Exp + Error(Subject/N1Congruency*Congruency), data = ACC.DF)
    N1_ACC <- print_apa_ci(N1_ACC)
    

    out <- list(
            LT_RT = LT_RT,
            LT_ACC = LT_ACC,
            N1_RT  = N1_RT,
            N1_ACC = N1_ACC,
            N1_FU = N1_FU)
    

    return(out)
}

run_E3_analyses <- function(df){
    
    RT.DF<- df %>%
        filter(ACC == TRUE, RT < 5000, RT > 0, PrimeProbe == "Probe") %>%
        group_by(Subject,Exp,PrimeCon,ProbeCon) %>%
        summarise(vjoutRT = vjoutNR(RT,1))
    
    ## Remove low acc subjects
    ExploreDF <- df %>%
        filter(RT < 5000, RT > 0) %>%
        group_by(Subject,Exp,PrimeProbe,PrimeCon,ProbeCon) %>%
        summarise(meanRT = mean(RT), 
                  meanAccuracy = mean(ACC), 
                  TrialsCompleted = length(RT),
                  maxRT = max(RT),
                  vjoutRT = mean(vjoutNR(RT,1)),
                  removed = vjoutNR(RT,2))
    
    LowACC <- unique(ExploreDF %>%
                         filter(meanAccuracy <.8) %>%
                         .$Subject)
    
    RT.DF <- RT.DF %>%
        filter(Subject%in%LowACC == FALSE)
    ####
    
    ## Summarize Data 
    RT.DF <- RT.DF %>%
        group_by(Subject,Exp,PrimeCon,ProbeCon) %>%
        summarise(vjoutRT = mean(vjoutRT))
    
    
    RT.Diff<-RT.DF %>%
        group_by(Exp,Subject,PrimeCon,ProbeCon) %>%
        summarise(RT = mean(vjoutRT)) %>%
        spread(ProbeCon,RT) %>%
        mutate(Diff = Inc - Con) %>%
        select(-Con:-Inc)%>%
        group_by(Exp,PrimeCon) %>%
        summarise(
            N = n_distinct(Subject), 
            Flanker = mean(Diff), 
            sd = sd(Diff), 
            SE = sd/sqrt(N)
        ) 
    
    EA_LT_RT <- aov_car(vjoutRT ~ ProbeCon*PrimeCon + Error(Subject/ProbeCon*PrimeCon), data = RT.DF[RT.DF$Exp == "E3A",])
    EA_LT_RT <- print_apa_ci(EA_LT_RT)
    
    ACC.DF <- df %>%
        filter(Subject%in%LowACC == FALSE, PrimeProbe == "Probe") %>%
        group_by(Subject,Exp,PrimeCon,ProbeCon) %>%
        summarise(mACC = (1-mean(ACC))*100)
    
    EA_LT_ACC <- aov_car(mACC ~ PrimeCon*ProbeCon + Error(Subject/PrimeCon*ProbeCon), data = ACC.DF[ACC.DF$Exp == "E3A",])
    EA_LT_ACC <- print_apa_ci(EA_LT_ACC)
    
    EB_LT_RT <- aov_car(vjoutRT ~ ProbeCon*PrimeCon + Error(Subject/ProbeCon*PrimeCon), data = RT.DF[RT.DF$Exp == "E3B",])
    EB_LT_RT <- print_apa_ci(EB_LT_RT)
    
    ACC.DF <- df %>%
        filter(Subject%in%LowACC == FALSE, PrimeProbe == "Probe") %>%
        group_by(Subject,Exp,PrimeCon,ProbeCon) %>%
        summarise(mACC = (1-mean(ACC))*100)
    
    EB_LT_ACC <- aov_car(mACC ~ PrimeCon*ProbeCon + Error(Subject/PrimeCon*ProbeCon), data = ACC.DF[ACC.DF$Exp == "E3B",])
    EB_LT_ACC <- print_apa_ci(EB_LT_ACC)
    
    ## SUMMARIZE RT Data ##
    RT.DF<- df %>%
        filter(trial != 1, RT < 5000, RT > 0, ACC == TRUE) %>%
        group_by(Subject,Exp,Congruency,N1Congruency) %>%
        summarise(vjoutRT = vjoutNR(RT,1))
    
    # FIND LOW ACC SUBS AND REMOVE (Same as above) ##
    ExploreDF <- df %>%
        filter(RT < 5000, RT > 0) %>%
        group_by(Subject,Exp,PrimeProbe,PrimeCon,ProbeCon) %>%
        summarise(meanRT = mean(RT), 
                  meanAccuracy = mean(ACC), 
                  TrialsCompleted = length(RT),
                  maxRT = max(RT),
                  vjoutRT = mean(vjoutNR(RT,1)),
                  removed = vjoutNR(RT,2))
    
    LowACC <- unique(ExploreDF %>%
                         filter(meanAccuracy <.8) %>%
                         .$Subject)
    ####
    RT.DF <- RT.DF %>%
        filter(Subject%in%LowACC == FALSE)
    
    ## RT analysis
    RT.DF <- RT.DF %>%
        group_by(Subject,Exp,Congruency,N1Congruency) %>%
        summarise(vjoutRT = mean(vjoutRT))
    
    EA_N1_RT <- print_apa_ci(aov_car(vjoutRT ~ Congruency*N1Congruency + Error(Subject/(Congruency*N1Congruency)), data = RT.DF[RT.DF$Exp == "E3A",]))
    EB_N1_RT <- print_apa_ci(aov_car(vjoutRT ~ Congruency*N1Congruency + Error(Subject/(Congruency*N1Congruency)), data = RT.DF[RT.DF$Exp == "E3B",]))
    
    
    ## ACC analysis
    ACC.DF <- df %>%
        filter(Subject%in%LowACC == FALSE,!is.na(N1Congruency)) %>%
        group_by(Subject,Exp,N1Congruency,Congruency) %>%
        summarise(mACC = (1-mean(ACC))*100)
    
    EA_N1_ACC <- print_apa_ci(aov_car(mACC ~ N1Congruency*Congruency + Error(Subject/N1Congruency*Congruency), data = ACC.DF[ACC.DF$Exp == "E3A",]))
    EB_N1_ACC <- print_apa_ci(aov_car(mACC ~ N1Congruency*Congruency + Error(Subject/N1Congruency*Congruency), data = ACC.DF[ACC.DF$Exp == "E3B",]))
    
    
    out <- list(
        EA_LT_RT = EA_LT_RT,
        EA_LT_ACC = EA_LT_ACC,
        EA_N1_RT = EA_N1_RT,
        EA_N1_ACC = EA_N1_ACC,
        EB_LT_RT = EB_LT_RT,
        EB_LT_ACC = EB_LT_ACC,
        EB_N1_RT = EB_N1_RT,
        EB_N1_ACC = EB_N1_ACC
    )
    
    
    return(out)
    
}

repeat_analysis <- function(df){
    RT.DF<- df %>%
        filter(ACC == TRUE, RT < 5000, RT > 0, PrimeProbe == "Probe") %>%
        group_by(Subject,Exp,RespRepeat,PrimeCon,ProbeCon) %>%
        summarise(vjoutRT = vjoutNR(RT,1))
    
    ## Remove low acc subjects
    ExploreDF <- df %>%
        filter(RT < 5000, RT > 0) %>%
        group_by(Subject,Exp,PrimeProbe,PrimeCon,ProbeCon) %>%
        summarise(meanRT = mean(RT), 
                  meanAccuracy = mean(ACC), 
                  TrialsCompleted = length(RT),
                  maxRT = max(RT),
                  vjoutRT = mean(vjoutNR(RT,1)),
                  removed = vjoutNR(RT,2))
    
    LowACC <- unique(ExploreDF %>%
                         filter(meanAccuracy <.8) %>%
                         .$Subject)
    
    RT.DF <- RT.DF %>%
        filter(Subject%in%LowACC == FALSE,
               PrimeCon != "odd")
    ####
    
    ## Summarize Data 
    RT.DF <- RT.DF %>%
        group_by(Subject,RespRepeat,PrimeCon,ProbeCon) %>%
        summarise(vjoutRT = mean(vjoutRT))
    
    LT <- print_apa_ci(aov_car(vjoutRT ~ RespRepeat*PrimeCon*ProbeCon + Error(Subject/RespRepeat*PrimeCon*ProbeCon), data = RT.DF))
    
    RT.DF<- df %>%
        filter(trial != 1, RT < 5000, RT > 0, ACC == TRUE) %>%
        group_by(Subject,Exp,N1RespRepeat,Congruency,N1Congruency) %>%
        summarise(vjoutRT = vjoutNR(RT,1))
    
    RT.DF <- RT.DF %>%
        filter(Subject%in%LowACC == FALSE,
               Subject != 77) # missing data
    ####
    
    ## Summarize Data 
    RT.DF <- RT.DF %>%
        group_by(Subject,N1RespRepeat,N1Congruency,Congruency) %>%
        summarise(vjoutRT = mean(vjoutRT))
    
    N1 <- print_apa_ci(aov_car(vjoutRT ~ N1RespRepeat*N1Congruency*Congruency + Error(Subject/N1RespRepeat*N1Congruency*Congruency), data = RT.DF))
    
    N1_FU <- list()
    N1_FU[["Change"]] <- print_apa_ci(aov_car(vjoutRT ~ Congruency*N1Congruency + Error(Subject/(Congruency*N1Congruency)), data = RT.DF[RT.DF$N1RespRepeat == "Change",]))
    N1_FU[["Repeat"]] <- print_apa_ci(aov_car(vjoutRT ~ Congruency*N1Congruency + Error(Subject/(Congruency*N1Congruency)), data = RT.DF[RT.DF$N1RespRepeat == "Repeat",]))
    
    out <- list(
        LT = LT,
        N1 = N1,
        N1_FU = N1_FU
    )
    
    return(out)
}


longshort_analysis <- function(df){
    RT.DF<- df %>%
        filter(ACC == TRUE, RT < 5000, RT > 0, PrimeProbe == "Probe") %>%
        group_by(Subject,Exp,PrimeCon,N1Congruency,ProbeCon) %>%
        summarise(vjoutRT = vjoutNR(RT,1))
    
    ## Remove low acc subjects
    ExploreDF <- df %>%
        filter(RT < 5000, RT > 0) %>%
        group_by(Subject,Exp,PrimeProbe,PrimeCon,ProbeCon) %>%
        summarise(meanRT = mean(RT), 
                  meanAccuracy = mean(ACC), 
                  TrialsCompleted = length(RT),
                  maxRT = max(RT),
                  vjoutRT = mean(vjoutNR(RT,1)),
                  removed = vjoutNR(RT,2))
    
    LowACC <- unique(ExploreDF %>%
                         filter(meanAccuracy <.8) %>%
                         .$Subject)
    
    RT.DF <- RT.DF %>%
        filter(Subject%in%LowACC == FALSE,
               PrimeCon != "odd")
    ####
    
    ## Summarize Data 
    RT.DF <- RT.DF %>%
        group_by(Subject,N1Congruency,PrimeCon,ProbeCon) %>%
        summarise(vjoutRT = mean(vjoutRT))
    
    LS <- print_apa_ci(aov_car(vjoutRT ~ N1Congruency*PrimeCon*ProbeCon + Error(Subject/N1Congruency*PrimeCon*ProbeCon), data = RT.DF))
    
    
    return(LS)
}


## RUN ANALYSES ----

## EXPERIMENT 1 ----

load("../data/ch-memory-guided/E1.Rda")
E1_results <- run_E1E2_analyses(E1)

#### EXPERIMENT 2 ----

load("../data/ch-memory-guided/E2.Rda")
E2_results <- run_E1E2_analyses(E2)


#### EXPERIMENT 3 ----

load("../data/ch-memory-guided/E3.Rda")
E3_results <- run_E3_analyses(E3)

#### REPEAT ANALYSIS ----
E1$Subject <- as.numeric(as.character(E1$Subject))
E2$Subject <- as.numeric(as.character(E2$Subject))
E2$Subject <- E2$Subject + max(E1$Subject)

E12 <- rbind(E1,E2)
E12$Subject <- factor(E12$Subject)

rep_results <- repeat_analysis(E12)

#### LONG/SHORT ANALYSIS ----

ls_results <- longshort_analysis(E12)

rm(E1, E2, E3, E12)
