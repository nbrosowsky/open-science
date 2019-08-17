## Van Selst & Jolicoeur, 1994 ----
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

# Function takes the experiment dataframe and returns a figure of CSE effects
create_plot <- function(df){
    
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
        ungroup() %>%
        filter(PrimeCon != "odd") %>%
        mutate(PrimeCon = factor(PrimeCon)) %>%
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
    
    limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
    LT_Plot <- ggplot(RT.Diff,aes(x=Exp, y=Flanker,fill=PrimeCon))+
        geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
        scale_fill_manual(values=c("gray60", "white"))+
        geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
        coord_cartesian(ylim = c(0,90))+
        scale_y_continuous(breaks=seq(0, 90, 10), expand = c(0,0))+
        theme(axis.text=element_text(size=7.5),
              axis.title=element_text(size=10,face="bold")) +
        theme_classic(base_size=12)+
        labs(title="Long-Term CSE")+
        ylab("Congruency Effect (ms)")+
        theme(legend.position="none",legend.direction="horizontal",legend.title = element_blank(),
              axis.title.x = element_blank())
    
    
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
    
    RT.DF <- RT.DF %>%
        filter(Subject%in%LowACC == FALSE)
    
    ############## SUMMARIZE DATA ################
    RT.DF <- RT.DF %>%
        group_by(Subject,Exp,Congruency,N1Congruency) %>%
        summarise(vjoutRT = mean(vjoutRT))
    
    
    RT.Diff<-RT.DF %>%
        group_by(Exp,Subject,Congruency,N1Congruency) %>%
        summarise(
            RT = mean(vjoutRT)
        ) %>%
        spread(Congruency,RT) %>%
        mutate(Diff = Inc - Con) %>%
        select(-Con:-Inc)%>%
        group_by(Exp,N1Congruency) %>%
        summarise(
            N = n_distinct(Subject), 
            Flanker = mean(Diff), 
            sd = sd(Diff), 
            SE = sd/sqrt(N)
        ) 
    

    limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
    N1_Plot <- ggplot(RT.Diff,aes(x=Exp, y=Flanker,fill=N1Congruency))+
        geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
        scale_fill_manual(values=c("gray60", "white"))+
        geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
        coord_cartesian(ylim = c(0,90))+
        scale_y_continuous(breaks=seq(0, 90, 10), expand = c(0,0))+
        theme(axis.text=element_text(size=7.5),
              axis.title=element_text(size=10,face="bold")) +
        theme_classic(base_size=12)+
        labs(title="N-1 CSE")+
        ylab("Congruency Effect (ms)")+
        theme(legend.position="none",legend.direction="horizontal",legend.title = element_blank(),
              axis.title.x = element_blank())
    
    ########## ARRANGE #########
    figure <- plot_grid(LT_Plot,NULL, N1_Plot, 
                         nrow = 1, 
                         rel_widths = c(1, 0.05, 1),
                         labels = c("A.", "", "B."))
    
    return(figure)   
}

create_repeat_plot <- function(df){
    
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
    
    
    RT.Diff<-RT.DF %>%
        ungroup() %>%
        filter(PrimeCon != "odd") %>%
        mutate(PrimeCon = factor(PrimeCon)) %>%
        group_by(Subject,RespRepeat, PrimeCon,ProbeCon) %>%
        summarise(RT = mean(vjoutRT)) %>%
        spread(ProbeCon,RT) %>%
        mutate(Diff = Inc - Con) %>%
        select(-Con:-Inc)%>%
        group_by(RespRepeat,PrimeCon) %>%
        summarise(
            N = n_distinct(Subject), 
            Flanker = mean(Diff), 
            sd = sd(Diff), 
            SE = sd/sqrt(N)
        ) 
    
    limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
    LT_Plot <- ggplot(RT.Diff,aes(x=RespRepeat, y=Flanker,fill=PrimeCon))+
        geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
        scale_fill_manual(values=c("gray60", "white"))+
        geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
        coord_cartesian(ylim = c(0,90))+
        scale_y_continuous(breaks=seq(0, 90, 10), expand = c(0,0))+
        theme(axis.text=element_text(size=7.5),
              axis.title=element_text(size=10,face="bold")) +
        theme_classic(base_size=12)+
        labs(title="Long-Term CSE")+
        ylab("Congruency Effect (ms)")+
        theme(legend.position="none",legend.direction="horizontal",legend.title = element_blank(),
              axis.title.x = element_blank())
    
    
    ## SUMMARIZE RT Data ##
    RT.DF<- df %>%
        filter(trial != 1, RT < 5000, RT > 0, ACC == TRUE) %>%
        group_by(Subject,N1RespRepeat,Congruency,N1Congruency) %>%
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
    
    RT.DF <- RT.DF %>%
        filter(Subject%in%LowACC == FALSE,
               Subject != 77)
    
    ############## SUMMARIZE DATA ################
    RT.DF <- RT.DF %>%
        group_by(Subject,N1RespRepeat,Congruency,N1Congruency) %>%
        summarise(vjoutRT = mean(vjoutRT))
    
    
    RT.Diff<-RT.DF %>%
        group_by(N1RespRepeat,Subject,Congruency,N1Congruency) %>%
        summarise(
            RT = mean(vjoutRT)
        ) %>%
        spread(Congruency,RT) %>%
        mutate(Diff = Inc - Con) %>%
        select(-Con:-Inc)%>%
        group_by(N1RespRepeat,N1Congruency) %>%
        summarise(
            N = n_distinct(Subject), 
            Flanker = mean(Diff), 
            sd = sd(Diff), 
            SE = sd/sqrt(N)
        ) 
    
    
    limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
    N1_Plot <- ggplot(RT.Diff,aes(x=N1RespRepeat, y=Flanker,fill=N1Congruency))+
        geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
        scale_fill_manual(values=c("gray60", "white"))+
        geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
        coord_cartesian(ylim = c(0,90))+
        scale_y_continuous(breaks=seq(0, 90, 10), expand = c(0,0))+
        theme(axis.text=element_text(size=7.5),
              axis.title=element_text(size=10,face="bold")) +
        theme_classic(base_size=12)+
        labs(title="N-1 CSE")+
        ylab("Congruency Effect (ms)")+
        theme(legend.position="none",legend.direction="horizontal",legend.title = element_blank(),
              axis.title.x = element_blank())
    
    ########## ARRANGE #########
    figure <- plot_grid(LT_Plot,NULL, N1_Plot, 
                        nrow = 1, 
                        rel_widths = c(1, 0.05, 1),
                        labels = c("A.", "", "B."))
    
    return(figure) 
    
    
}

## CREATE & SAVE PLOTS ---

## EXPERIMENT 1 ----

load(file="../data/ch-memory-guided/E1.Rda")
levels(E1$Exp) <- c("Exp. 1A", "Exp. 1B", "Exp. 1C")
figure1 <- create_plot(E1)

ggsave("../figures/MGfigure2.pdf", device = "pdf", dpi = 600,
       width = 6.875, height = 4, units = "in") 


#### EXPERIMENT 2 ----

load(file="../data/ch-memory-guided/E2.Rda")
levels(E2$Exp) <- c("Exp. 2A", "Exp. 2B")
figure2 <- create_plot(E2)

ggsave("../figures/MGfigure3.pdf", device = "pdf", dpi = 600,
       width = 6.875, height = 4, units = "in") 


#### EXPERIMENT 3 ----

load(file="../data/ch-memory-guided/E3.Rda")
levels(E3$Exp) <- c("Exp. 3A", "Exp. 3B")
figure3 <- create_plot(E3)

ggsave("../figures/MGfigure4.pdf", device = "pdf", dpi = 600,
       width = 6.875, height = 4, units = "in") 

#### REPEAT ANALYSIS ----
load(file="../data/ch-memory-guided/E1.Rda")
load(file="../data/ch-memory-guided/E2.Rda")

E1$Subject <- as.numeric(as.character(E1$Subject))
E2$Subject <- as.numeric(as.character(E2$Subject))
E2$Subject <- E2$Subject + max(E1$Subject)

E12 <- rbind(E1,E2)
E12$Subject <- factor(E12$Subject)

figure4 <- create_repeat_plot(E12)

ggsave("../figures/MGfigure5.pdf", device = "pdf", dpi = 600,
       width = 6.875, height = 4, units = "in") 
