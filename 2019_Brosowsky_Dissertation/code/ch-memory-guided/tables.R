# Helper Functions ----
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

# 
# vj_NR <- function(x,n) {
#     # vjoutNR(data, 1) returns mean with outliers removed #
#     # vjoutNR(data, 2) returns percentage of trials removed #
#     xm <- mean(x)
#     xsize <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
#                25, 30, 35, 50, 100)
#     stds <- c(1.3, 1.458, 1.68, 1.841, 1.961, 2.05, 2.12, 2.173, 
#               2.22, 2.246, 2.274, 2.31, 2.326, 2.391, 2.41, 2.4305, 
#               2.45, 2.48, 2.5)
#     stdindex <- length(xsize[xsize <= length(x)])
#     
#     out <- list()
#     
#     out$cutoff        <- stds[stdindex]
#     out$min           <- xm-sd(x)*out$cutoff
#     out$max           <- xm+sd(x)*out$cutoff
#     out$original_RTs  <- x
#     out$removed_RTs   <- x[x > out$max | x < out$min]
#     out$remaining_RTs <- x[x < out$max & x > out$min]
#     out$prop_Removed  <- length(out$removed_RTs)/length(out$original_RTs)
#     out$mean          <- mean(x)
#     out$mean_vj       <- mean(out$remaining_RTs)
#     
#     return(out)
# }

# Function takes in the experiment dataframe and returns a matrix formatted like the table
get_lt_matrix <- function(df){
    RT.DF<- df %>%
        filter(ACC == TRUE, RT < 5000, RT > 0, PrimeProbe == "Probe") %>%
        group_by(Subject,Exp,block,PrimeCon,ProbeCon) %>%
        summarise(vjoutRT = vjoutNR(RT,1))
    
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
    
    
    # Get summary RTs
    RT.DF <- RT.DF %>%
        group_by(Subject,Exp,PrimeCon,ProbeCon) %>%
        summarise(vjoutRT = mean(vjoutRT))
    
    
    
    #RT Summary Table
    RT.Sum <- RT.DF %>%
        group_by(Exp,PrimeCon,ProbeCon) %>%
        summarise(
            N = n_distinct(Subject), 
            RT = mean(vjoutRT), 
            sd = sd(vjoutRT), 
            SE = sd/sqrt(N)
        ) 
    
    RT.Sum <- matrix(paste0(round(RT.Sum$RT, digits = 0), " (", round(RT.Sum$SE, digits = 0) , ")"),nrow = length(unique(df$Exp))*2, byrow = TRUE)
    
    # Get Flanker RTs
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
    
    Diff.Sum <- matrix(paste0(round(RT.Diff$Flanker, digits = 0), " (", round(RT.Diff$SE, digits = 0) , ")"),nrow = length(unique(df$Exp))*2, byrow = TRUE)
    
    # Get CSEs
    RT.Gratton <-RT.DF %>%
        group_by(Exp,Subject,ProbeCon,PrimeCon) %>%
        summarise(
            RT = mean(vjoutRT)
        ) %>%
        spread(ProbeCon,RT) %>%
        mutate(Diff = Inc - Con) %>%
        select(-Con:-Inc)%>%
        spread(PrimeCon,Diff) %>%
        mutate(Diff = Con - Inc) %>%
        select(-Con:-Inc)%>%
        group_by(Exp) %>%
        summarise(
            N = n_distinct(Subject), 
            N8 = mean(Diff), 
            sd = sd(Diff), 
            SE = sd/sqrt(N)
        ) 
    
    temp <-paste0(round(RT.Gratton$N8, digits = 0), " (", round(RT.Gratton$SE, digits = 0) , ")")
    Gratton.Sum <- c()
    for (i in 1:length(temp)){
        Gratton.Sum <- c(Gratton.Sum, temp[i], " ")
    }
    Gratton.Sum <- matrix(Gratton.Sum,nrow = length(unique(df$Exp))*2, byrow = TRUE)
    
    rt.data <- cbind(RT.Sum, Diff.Sum, Gratton.Sum)
    
    # Get Error rates
    ACC.DF <- df %>%
        filter(Subject%in%LowACC == FALSE, PrimeProbe == "Probe") %>%
        group_by(Subject,Exp,PrimeCon,ProbeCon) %>%
        summarise(mACC = (1-mean(ACC))*100)
    
    #Error Rate Summary Table
    ACC.Sum <- ACC.DF %>%
        group_by(Exp,PrimeCon,ProbeCon) %>%
        summarise(
            N = n_distinct(Subject), 
            error = mean(mACC), 
            sd = (sd(mACC)), 
            SE = (sd/sqrt(N))
        ) 
    
    # create matrix that matches table format
    er.data <- matrix(paste0(round(ACC.Sum$error, digits = 2), " (", round(ACC.Sum$SE, digits = 2) , ")"),nrow = length(unique(df$Exp))*2, byrow = TRUE)
    t.data <- cbind(rt.data[,1],er.data[,1],rt.data[,2],er.data[,2],rt.data[,3:4])
    
    return(t.data)
}

get_n1_matrix <- function(df){
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
    
    if(levels(df$Exp)[1] == "E3A"){
        RT.DF<- df %>%
            filter(trial != 1, RT < 5000, RT > 0, ACC == TRUE) %>%
            group_by(Subject,Exp,N1Congruency,Congruency) %>%
            summarise(vjoutRT = vjoutNR(RT,1))
    }
    
    
    RT.DF <- RT.DF %>%
        filter(Subject%in%LowACC == FALSE)
    
    ## RT analysis
    RT.DF <- RT.DF %>%
        group_by(Subject,Exp,Congruency,N1Congruency) %>%
        summarise(vjoutRT = mean(vjoutRT))
   
    
    #RT Summary Table
    RT.Sum <- RT.DF %>%
        group_by(Exp,N1Congruency,Congruency) %>%
        summarise(
            N = n_distinct(Subject), 
            RT = mean(vjoutRT), 
            sd = sd(vjoutRT), 
            SE = sd/sqrt(N)
        ) 
    
    RT.Sum <- matrix(paste0(round(RT.Sum$RT, digits = 0), " (", round(RT.Sum$SE, digits = 0) , ")"),nrow = length(unique(df$Exp))*2, byrow = TRUE)
    
    # Get Flanker RTs
    RT.Diff<-RT.DF %>%
        ungroup() %>%
        mutate(N1Congruency = factor(N1Congruency)) %>%
        group_by(Exp,Subject,N1Congruency,Congruency) %>%
        summarise(RT = mean(vjoutRT)) %>%
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
    
    Diff.Sum <- matrix(paste0(round(RT.Diff$Flanker, digits = 0), " (", round(RT.Diff$SE, digits = 0) , ")"),nrow = length(unique(df$Exp))*2, byrow = TRUE)
    
    # Get CSEs
    RT.Gratton <-RT.DF %>%
        group_by(Exp,Subject,Congruency,N1Congruency) %>%
        summarise(
            RT = mean(vjoutRT)
        ) %>%
        spread(Congruency,RT) %>%
        mutate(Diff = Inc - Con) %>%
        select(-Con:-Inc)%>%
        spread(N1Congruency,Diff) %>%
        mutate(Diff = Con - Inc) %>%
        select(-Con:-Inc)%>%
        group_by(Exp) %>%
        summarise(
            N = n_distinct(Subject), 
            N8 = mean(Diff), 
            sd = sd(Diff), 
            SE = sd/sqrt(N)
        ) 
    
    temp <-paste0(round(RT.Gratton$N8, digits = 0), " (", round(RT.Gratton$SE, digits = 0) , ")")
    Gratton.Sum <- c()
    for (i in 1:length(temp)){
        Gratton.Sum <- c(Gratton.Sum, temp[i], " ")
    }
    Gratton.Sum <- matrix(Gratton.Sum,nrow = length(unique(df$Exp))*2, byrow = TRUE)
    
    rt.data <- cbind(RT.Sum, Diff.Sum, Gratton.Sum)
    
    ## ACC analysis
    ACC.DF <- df %>%
        filter(Subject%in%LowACC == FALSE, !is.na(N1Congruency)) %>%
        group_by(Subject,Exp,N1Congruency,Congruency) %>%
        summarise(mACC = (1-mean(ACC))*100)

    
    #Error Rate Summary Table
    ACC.Sum <- ACC.DF %>%
        group_by(Exp,N1Congruency,Congruency) %>%
        summarise(
            N = n_distinct(Subject), 
            error = mean(mACC), 
            sd = sd(mACC), 
            SE = (sd/sqrt(N))
        ) 
    
    # create matrix that matches table format
    er.data <- matrix(paste0(round(ACC.Sum$error, digits = 2), " (", round(ACC.Sum$SE, digits = 2) , ")"),nrow = length(unique(df$Exp))*2, byrow = TRUE)
    t.data <- cbind(rt.data[,1],er.data[,1],rt.data[,2],er.data[,2],rt.data[,3:4])
    
    return(t.data)
}

## LT-Matrix
load(file="../data/ch-memory-guided/E1.Rda")
E1.matrix <- get_lt_matrix(E1)

load("../data/ch-memory-guided/E2.Rda")
E2.matrix <- get_lt_matrix(E2)

load("../data/ch-memory-guided/E3.Rda")
E3.matrix <- get_lt_matrix(E3)

lt.data <- rbind(E1.matrix, E2.matrix, E3.matrix)

##  N1-Matrix ----
load(file="../data/ch-memory-guided/E1.Rda")
E1.matrix <- get_n1_matrix(E1)

load("../data/ch-memory-guided/E2.Rda")
E2.matrix <- get_n1_matrix(E2)

load("../data/ch-memory-guided/E3.Rda")
E3.matrix <- get_n1_matrix(E3)

n1.data <- rbind(E1.matrix, E2.matrix, E3.matrix)

## Stitch the table together in latex
MG_table1 <- paste("\\begin{table}[htpb]",
                 "\\centering",
                 "\\caption{Long-Term Congruency Sequence Effects for Experiments 1-3}",
                 "\\label{MG_table1}",
                 "\\resizebox{\\textwidth}{!}{",
                  "\\begin{tabular}{rrcccccc}",
                  "\\toprule & " ,
                    ## HEADERS
                paste0("& \\multicolumn{4}{c}{Probe} ","& \\multicolumn{1}{l}{ Congruency Effect } ", "& \\multicolumn{1}{l}{ Long-term CSE} \\\\"),
                "\\cmidrule{3-6} &",       
                paste0("& \\multicolumn{2}{l}{Con} & \\multicolumn{2}{l}{Inc} ","& \\multicolumn{1}{l}{ $(I-C)$} ","& \\multicolumn{1}{l}{ ($C_{(I-C)} - I_{(I-C)})$} \\\\"),
                "\\cmidrule{3-8}", 
                paste0("\\multicolumn{2}{c}{Prime} & \\multicolumn{1}{l}{", paste(c("RT","ER","RT","ER","RT","RT"),collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                "\\midrule",
                ## E1 DATA
                "\\multicolumn{2}{l}{\\textbf{Exp. 1A}}  &   &    &     &     &    &  \\\\",
                paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(lt.data[1,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(lt.data[2,], collapse="} & \\multicolumn{1}{l}{"), "}  \\\\"),
        
                "\\multicolumn{2}{l}{\\textbf{Exp. 1B}} &    &     &     &     &     &  \\\\",
                paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(lt.data[3,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(lt.data[4,], collapse="} & \\multicolumn{1}{l}{"), "}  \\\\"),
  
                "\\multicolumn{2}{l}{\\textbf{Exp. 1C}} &    &     &     &     &     &  \\\\",
                paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(lt.data[5,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(lt.data[6,], collapse="} & \\multicolumn{1}{l}{"), "}  \\\\"),
                
                "&       &       &       &       &       &       &  \\\\",
                "\\midrule",
                
                ## E2 DATA
                "\\multicolumn{2}{l}{\\textbf{Exp. 2A}}  &   &    &     &     &    &  \\\\",
                paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(lt.data[7,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(lt.data[8,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                
                "\\multicolumn{2}{l}{\\textbf{Exp. 2B}} &    &     &     &     &     &  \\\\",
                paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(lt.data[9,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(lt.data[10,], collapse="} & \\multicolumn{1}{l}{"), "}  \\\\"),
                
                "&       &       &       &       &       &       &  \\\\",
                "\\midrule",
                
                ## E3 DATA
                "\\multicolumn{2}{l}{\\textbf{Exp. 3A}}  &   &    &     &     &    &  \\\\",
                paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(lt.data[11,], collapse="} & \\multicolumn{1}{l}{"), "}  \\\\"),
                paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(lt.data[12,], collapse="} & \\multicolumn{1}{l}{"), "}  \\\\"),
                
                "\\multicolumn{2}{l}{\\textbf{Exp. 3B}} &    &     &     &     &     &  \\\\",
                paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(lt.data[13,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(lt.data[14,], collapse="} & \\multicolumn{1}{l}{"), "}  \\\\"),
                
                "&       &       &       &       &       &       &  \\\\",
                "\\bottomrule",
                "\\multicolumn{8}{l}{\\textit{Note}: RT = Reaction Time (ms);  ER = Error Rates (\\%);  } \\\\",
                "\\multicolumn{8}{l}{Con/C  congruent; Inc/I  incongruent; standard errors are presented in parentheses.} \\\\",
                "\\end{tabular}",
                "}%",
                "\\end{table}", 
                sep = "\n")
   
   


## Stitch the table together in latex
MG_table2 <- paste("\\begin{table}[htpb]",
                   "\\centering",
                   "\\caption{$\\textit{N}-1$ Congruency Sequence Effects for Experiments 1-3}",
                   "\\label{MG_table2}",
                   "\\resizebox{\\textwidth}{!}{",
                   "\\begin{tabular}{rrcccccc}",
                   "\\toprule & " ,
                   ## HEADERS
                   paste0("& \\multicolumn{4}{c}{Trial $\\textit{n}$} ","& \\multicolumn{1}{l}{Congruency Effect } ", "& \\multicolumn{1}{l}{$\\textit{N}-1$ CSE} \\\\"),
                   "\\cmidrule{3-6} &",       
                   paste0("& \\multicolumn{2}{l}{Con} & \\multicolumn{2}{l}{ Inc} ","& \\multicolumn{1}{l}{ $(I-C)$} ","& \\multicolumn{1}{l}{ ($C_{(I-C)} - I_{(I-C)})$} \\\\"),
                   "\\cmidrule{3-8}", 
                   paste0("\\multicolumn{2}{c}{Trial \\textit{n}-1} & \\multicolumn{1}{l}{", paste(c("RT","ER","RT","ER","RT","RT"),collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   "\\midrule",
                   ## E1 DATA
                   "\\multicolumn{2}{l}{\\textbf{Exp. 1A}}  &   &    &     &     &    &  \\\\",
                   paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(n1.data[1,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(n1.data[2,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   
                   "\\multicolumn{2}{l}{\\textbf{Exp. 1B}} &    &     &     &     &     &  \\\\",
                   paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(n1.data[3,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(n1.data[4,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   
                   "\\multicolumn{2}{l}{\\textbf{Exp. 1C}} &    &     &     &     &     &  \\\\",
                   paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(n1.data[5,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(n1.data[6,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   
                   "&       &       &       &       &       &       &  \\\\",
                   "\\midrule",
                   
                   ## E2 DATA
                   "\\multicolumn{2}{l}{\\textbf{Exp. 2A}}  &   &    &     &     &    &  \\\\",
                   paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(n1.data[7,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(n1.data[8,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   
                   "\\multicolumn{2}{l}{\\textbf{Exp. 2B}} &    &     &     &     &     &  \\\\",
                   paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(n1.data[9,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(n1.data[10,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   
                   "&       &       &       &       &       &       &  \\\\",
                   "\\midrule",
                   
                   ## E3 DATA
                   "\\multicolumn{2}{l}{\\textbf{Exp. 3A}}  &   &    &     &     &    &  \\\\",
                   paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(n1.data[11,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(n1.data[12,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   
                   "\\multicolumn{2}{l}{\\textbf{Exp. 3B}} &    &     &     &     &     &  \\\\",
                   paste0("& \\multicolumn{1}{l}{Con} & \\multicolumn{1}{l}{", paste(n1.data[13,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   paste0("& \\multicolumn{1}{l}{Inc} & \\multicolumn{1}{l}{", paste(n1.data[14,], collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
                   
                   "&       &       &       &       &       &       &  \\\\",
                   "\\bottomrule",
                   "\\multicolumn{8}{l}{\\textit{Note}: RT = Reaction Time (ms);  ER = Error Rates (\\%);  } \\\\",
                   "\\multicolumn{8}{l}{Con/C  congruent; Inc/I  incongruent; standard errors are presented in parentheses.} \\\\",
                   "\\end{tabular}",
                   "}%",
                   "\\end{table}",
                   sep = "\n")


