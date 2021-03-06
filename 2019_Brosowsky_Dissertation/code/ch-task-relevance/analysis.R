vjoutNR <- function(x,n) {
  xm <- mean(x)
  xsize <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
             25, 30, 35, 50, 80)
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



load("../data/ch-task-relevance/raw_data.Rda")


# Find subs with < 75% accuracy
low_acc <- raw_data %>%
  group_by(Subject,Condition,Task_Relevant_Context,Frequency,Congruency) %>%
  summarise(meanAccuracy = mean(ACC)) %>%
  filter(meanAccuracy < .75) %>%
  .$Subject

N_subjects_removed<-length(unique(low_acc))

## summarise by subject
RT.DF <- raw_data %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Condition,Subject,Order,Phase,Block,Frequency,Task_Relevant_Context,Congruency)%>%
  summarise(vjoutRT = vjoutNR(RT,1))

## Find percentage of trials removed 
percent_removed <- raw_data %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Condition,Subject,Order,Phase,Frequency,Task_Relevant_Context,Congruency)%>%
  summarise(percent_removed = vjoutNR(RT,2)) %>%
  ungroup() %>%
  summarise(avg_removed = mean(percent_removed)) %>%
  .$avg_removed

## Analyze RTs for frequency unbiased items
RT.Analysis <- RT.DF %>%
  filter(
    Frequency == "unbiased"
  ) %>%
  group_by(Condition,Subject,Task_Relevant_Context,Congruency) %>%
  summarise(meanRT = mean(vjoutRT))

RT_ANOVA <- aov_car(meanRT ~ Condition*Task_Relevant_Context*Congruency + Error(Subject/Task_Relevant_Context*Congruency), data = RT.Analysis)
RT_ANOVA <- apa_print(RT_ANOVA, es = "pes")$full_result

## Analyze ACC for frequency unbiased items
ACC.analysis <- raw_data %>%
  mutate(error = (1-ACC)*100) %>%
  filter(
    Frequency == "unbiased",
    Subject%in%low_acc == FALSE)  %>%
  group_by(Subject,Condition,Task_Relevant_Context,Congruency) %>%
  summarise(error = mean(error))

ACC_ANOVA <- aov_car(error ~ Condition*Task_Relevant_Context*Congruency + Error(Subject/Task_Relevant_Context*Congruency), data = ACC.analysis)
ACC_ANOVA <- apa_print(ACC_ANOVA, es = "pes")$full_result

####### GRAPH FLANKER EFFECTS #########
RT.Diff<-RT.DF %>%
  filter(Frequency == "unbiased") %>%
  group_by(Condition,Subject,Task_Relevant_Context,Congruency) %>%
  summarise(
    RT = mean(vjoutRT)
  ) %>%
  spread(Congruency,RT) %>%
  mutate(Diff = inc - con) %>%
  select(-con:-inc)%>%
  group_by(Condition,Task_Relevant_Context) %>%
  summarise(
    N = n_distinct(Subject), 
    Flanker = mean(Diff), 
    sd = sd(Diff), 
    SE = sd/sqrt(N)
  ) 


#names(RT.Diff)[names(RT.Diff) == "Task_Relevant_Context"] <- 'Task Relevant\nContext'
levels(RT.Diff$Task_Relevant_Context) <- c("100% PC", "0% PC")
levels(RT.Diff$Condition) <- c("Object", "Social", "Social (NR)")


limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
RT.graph <- ggplot(RT.Diff,aes(x=Condition, y=Flanker,fill=Task_Relevant_Context))+
  geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
  scale_fill_manual(values=c("gray60", "white"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,150))+
  scale_y_continuous(breaks=seq(0, 150, 10), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  theme_classic(base_size=12)+
  ylab("Congruency Effect (ms)")+
  theme(       legend.position=c(1,1),
               legend.justification = c(1,1),
               legend.direction = "horizontal",
               #legend.background = element_rect(colour = "black", fill = "white", size=1),
               #legend.box.background = element_rect(colour = "black"),
               legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8)
  ) +
  guides(fill=guide_legend(title="Task-Relevant Context",title.position = "top"))


ACC.Diff <- raw_data %>%
  mutate(error = (1-ACC)*100) %>%
  filter(
    Frequency == "unbiased",
    Subject%in%low_acc == FALSE)  %>%
  group_by(Subject,Condition,Task_Relevant_Context,Congruency) %>%
  summarise(error = mean(error)) %>%
  group_by(Condition,Subject,Task_Relevant_Context,Congruency) %>%
  summarise(
    error = mean(error)
  ) %>%
  spread(Congruency,error) %>%
  mutate(Diff = inc - con) %>%
  select(-con:-inc)%>%
  group_by(Condition,Task_Relevant_Context) %>%
  summarise(
    N = n_distinct(Subject), 
    Flanker = mean(Diff), 
    sd = sd(Diff), 
    SE = sd/sqrt(N)
  ) 


levels(ACC.Diff$Task_Relevant_Context) <- c("100% PC", "0% PC")
levels(ACC.Diff$Condition) <- c("Object", "Social", "Social (NR)")

limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
ACC.graph <- ggplot(ACC.Diff,aes(x=Condition, y=Flanker,fill=Task_Relevant_Context))+
  geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
  scale_fill_manual(values=c("gray60", "white"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,10))+
  scale_y_continuous(breaks=seq(0, 10, 1), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  theme_classic(base_size=12)+
  ylab("Congruency Effect (ms)")+
  theme(       legend.position=c(1,1),
               legend.justification = c(1,1),
               legend.direction = "horizontal",
               #legend.background = element_rect(colour = "black", fill = "white", size=1),
               #legend.box.background = element_rect(colour = "black"),
               legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8)
  ) +
  guides(fill=guide_legend(title="Task-Relevant Context",title.position = "top"))

#ACC.graph

########## ARRANGE #########
figure2<-plot_grid(RT.graph,NULL,ACC.graph, 
                   nrow = 1, 
                   rel_widths = c(1, 0.05, 1),
                   labels = c("A", "", "B"))

figure2

ggsave("../figures/TRfigure2.pdf", device = "pdf", dpi = 600,
      width = 6.875, height = 4, units = "in") 

#clean up environment
#rm(list=c("ACC.analysis","aov.out","raw_data","RT.Analysis","RT.DF","low_acc","vjoutNR", "RT.Diff"))
