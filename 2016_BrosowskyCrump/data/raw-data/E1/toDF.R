require(ggplot2)
require(plyr)
require(pander)
require(knitr)
require(xtable)
require(car)
require(Crump)
require(lsr)
require(afex)


subject_data<-c()
subject_data <- list.files(pattern=".*txt")

AllData<-data.frame()
single_sub<-c()
input<-c()
data_info<-c()
alldata_info<-c()

x<-c(1,2,3,6,7,9,10,11,13:20)

for (i in x){
    input<-scan(file=subject_data[i], what = "character",skip=(0))
    
    
    datatable<-matrix(input,ncol=12,byrow=T)
    Subject<-c(rep(i,nrow(datatable)))
    
    data_info<-c(subject_data[i],length(input),nrow(datatable))
    
    #make labels consistent with other experiments
    Proportion<-c()
    for (n in seq(1:nrow(datatable))){
        if (datatable[n,7]=="HighLet" && datatable[n,8]=="Let"){
            Proportion[n]<-c("75P")}
        if (datatable[n,7]=="HighLet" && datatable[n,8]=="Col"){
            Proportion[n]<-c("25P")}
        if (datatable[n,7]=="HighCol" && datatable[n,8]=="Col"){
            Proportion[n]<-c("75P")}
        if (datatable[n,7]=="HighCol" && datatable[n,8]=="Let"){
            Proportion[n]<-c("25P")}
    }
    
    TargetType<-c(datatable[,8])
    TargetPosition<-c(datatable[,9])
    RTs<-c(as.numeric(datatable[,11])-as.numeric(datatable[,10]))
    
    #separate letter targets
    Letters<-c(unlist(strsplit(datatable[,1],"")))
    LettersM<-matrix(Letters,ncol=4,byrow=T)
    
    #record responses to match targets
    Response<-recode(datatable[,12],"c('a','A')= 'a';c('s','S')='o';c('d','D')='e';c('f','F')='c';c('h','H')='r';c('j','J')='b';c('k','K')='g';c('l','L')='y'")
    Trial<-c(seq(1:nrow(datatable)))
    
    #recode color words to single letters
    ColorP1<-recode(datatable[,2],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorP2<-recode(datatable[,3],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorP3<-recode(datatable[,4],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorP4<-recode(datatable[,5],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorsM<-cbind(ColorP1,ColorP2,ColorP3,ColorP4)
    
    
    c.response<-c()
    ACC<-c()
    for (i in seq(1:nrow(datatable))){
        for (n in seq(1:4)){
            if (TargetType[i]=="Let" && TargetPosition[i]==n){
                c.response[i]<-LettersM[i,n]
                if (Response[i]==LettersM[i,n]){
                    ACC[i]<-1
                }else
                {ACC[i]<-0}
            } 
        }
        for (n in seq(1:4)){
            if (TargetType[i]=="Col" && TargetPosition[i]==n){
                c.response[i]<-ColorsM[i,n]
                if (Response[i]==ColorsM[i,n]){
                    ACC[i]<-1
                }else
                {ACC[i]<-0}
            } 
        }
    }
    
    task.ACC<-c()
    for (i in seq(1:nrow(datatable))){
        for (n in seq(1:4)){
            if (TargetType[i]=="Let"){
                if (Response[i]=="a" | Response[i]=="o" | Response[i]=="e" | Response[i]=="c"){
                    task.ACC[i]<-1
                }else
                {task.ACC[i]<-0}
            } 
        }
        for (n in seq(1:4)){
            if (TargetType[i]=="Col"){
                if (Response[i]=="r" | Response[i]=="b" | Response[i]=="g" | Response[i]=="y"){
                    task.ACC[i]<-1
                }else
                {task.ACC[i]<-0}
            } 
        }
    }
    
    Response2<-recode(datatable[,12],"c('a','A')= 'a';c('s','S')='e';c('d','D')='o';c('f','F')='c';c('h','H')='r';c('k','K')='b';c('j','J')='g';c('l','L')='y'")
    
    ACC2<-c()
    for (i in seq(1:nrow(datatable))){
        for (n in seq(1:4)){
            if (TargetType[i]=="Let" && TargetPosition[i]==n){
                if (Response2[i]==LettersM[i,n]){
                    ACC2[i]<-1
                }else
                {ACC2[i]<-0}
            } 
        }
        for (n in seq(1:4)){
            if (TargetType[i]=="Col" && TargetPosition[i]==n){
                if (Response2[i]==ColorsM[i,n]){
                    ACC2[i]<-1
                }else
                {ACC2[i]<-0}
            } 
        }
    }
    
    
    #code change and repeat trials
    Task.switch<-c("First")
    Loc.switch<-c("First")
    
    for (n in seq((nrow(datatable)-1))){
        if (datatable[n+1,7]==datatable[n,7]){
            Loc.switch[n+1]<-c("L.Repeat")
        }else{
            Loc.switch[n+1]<-c("L.Change")
        }
        if (datatable[n+1,8]==datatable[n,8]){
            Task.switch[n+1]<-c("T.Repeat")
        }else{
            Task.switch[n+1]<-c("T.Change")
        }
    }
    
    TrialType<-datatable[,7]
    block<-c(rep("B1",nrow(datatable)/2),rep("B2",ceiling(nrow(datatable)/2)))
    
    single_sub<-data.frame(Subject,Proportion,TargetType,TargetPosition,RTs,ACC,Task.switch,Loc.switch,block,TrialType,c.response,Response,ACC2,task.ACC)
    AllData<-rbind(AllData,single_sub)
    alldata_info<-rbind(alldata_info,data_info)
}

subject_data<-c()
subject_data <- list.files(pattern=".*txt")

single_sub<-c()
input<-c()

#subjects 7,8,12,16
x<-c(4,5,8,12)

for (i in x){
    input<-scan(file=subject_data[i], what = "character",skip=(0))
    print(length(input))
    
    datatable<-matrix(input,ncol=12,byrow=T)
    Subject<-c(rep(i,nrow(datatable)))
    
    data_info<-c(subject_data[i],length(input),nrow(datatable))
    
    #make labels consistent with other experiments
    Proportion<-c()
    for (n in seq(1:nrow(datatable))){
        if (datatable[n,7]=="HighLet" && datatable[n,8]=="Let"){
            Proportion[n]<-c("75P")}
        if (datatable[n,7]=="HighLet" && datatable[n,8]=="Col"){
            Proportion[n]<-c("25P")}
        if (datatable[n,7]=="HighCol" && datatable[n,8]=="Col"){
            Proportion[n]<-c("75P")}
        if (datatable[n,7]=="HighCol" && datatable[n,8]=="Let"){
            Proportion[n]<-c("25P")}
    }
    
    TargetType<-c(datatable[,8])
    TargetPosition<-c(datatable[,9])
    RTs<-c(as.numeric(datatable[,11])-as.numeric(datatable[,10]))
    
    #separate letter targets
    Letters<-c(unlist(strsplit(datatable[,1],"")))
    LettersM<-matrix(Letters,ncol=4,byrow=T)
    
    #record responses to match targets
    Response<-recode(datatable[,12],"c('a','A')= 'a';c('s','S')='o';c('d','D')='e';c('f','F')='c';c('h','H')='r';c('k','K')='b';c('j','J')='g';c('l','L')='y'")
    Trial<-c(seq(1:nrow(datatable)))
    
    
    #recode color words to single letters
    ColorP1<-recode(datatable[,2],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorP2<-recode(datatable[,3],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorP3<-recode(datatable[,4],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorP4<-recode(datatable[,5],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorsM<-cbind(ColorP1,ColorP2,ColorP3,ColorP4)
    
    c.response<-c()
    ACC<-c()
    for (i in seq(1:nrow(datatable))){
        for (n in seq(1:4)){
            if (TargetType[i]=="Let" && TargetPosition[i]==n){
                c.response[i]<-LettersM[i,n]
                if (Response[i]==LettersM[i,n]){
                    ACC[i]<-1
                }else
                {ACC[i]<-0}
            } 
        }
        for (n in seq(1:4)){
            if (TargetType[i]=="Col" && TargetPosition[i]==n){
                c.response[i]<-ColorsM[i,n]
                if (Response[i]==ColorsM[i,n]){
                    ACC[i]<-1
                }else
                {ACC[i]<-0}
            } 
        }
    }
    
    Response2<-recode(datatable[,12],"c('a','A')= 'a';c('s','S')='e';c('d','D')='o';c('f','F')='c';c('h','H')='r';c('k','K')='g';c('j','J')='b';c('l','L')='y'")
    
    ACC2<-c()
    for (i in seq(1:nrow(datatable))){
        for (n in seq(1:4)){
            if (TargetType[i]=="Let" && TargetPosition[i]==n){
                if (Response2[i]==LettersM[i,n]){
                    ACC2[i]<-1
                }else
                {ACC2[i]<-0}
            } 
        }
        for (n in seq(1:4)){
            if (TargetType[i]=="Col" && TargetPosition[i]==n){
                if (Response2[i]==ColorsM[i,n]){
                    ACC2[i]<-1
                }else
                {ACC2[i]<-0}
            } 
        }
    }
    
    task.ACC<-c()
    for (i in seq(1:nrow(datatable))){
        for (n in seq(1:4)){
            if (TargetType[i]=="Let"){
                if (Response[i]=="a" | Response[i]=="o" | Response[i]=="e" | Response[i]=="c"){
                    task.ACC[i]<-1
                }else
                {task.ACC[i]<-0}
            } 
        }
        for (n in seq(1:4)){
            if (TargetType[i]=="Col"){
                if (Response[i]=="r" | Response[i]=="b" | Response[i]=="g" | Response[i]=="y"){
                    task.ACC[i]<-1
                }else
                {task.ACC[i]<-0}
            } 
        }
    }
    
    #code change and repeat trials
    Task.switch<-c("First")
    Loc.switch<-c("First")
    
    for (n in seq((nrow(datatable)-1))){
        if (datatable[n+1,7]==datatable[n,7]){
            Loc.switch[n+1]<-c("L.Repeat")
        }else{
            Loc.switch[n+1]<-c("L.Change")
        }
        if (datatable[n+1,8]==datatable[n,8]){
            Task.switch[n+1]<-c("T.Repeat")
        }else{
            Task.switch[n+1]<-c("T.Change")
        }
    }
    
    TrialType<-datatable[,7]
    block<-c(rep("B1",nrow(datatable)/2),rep("B2",ceiling(nrow(datatable)/2)))
    
    single_sub<-data.frame(Subject,Proportion,TargetType,TargetPosition,RTs,ACC,Task.switch,Loc.switch,block,TrialType,c.response,Response,ACC2,task.ACC)
    AllData<-rbind(AllData,single_sub)
    alldata_info<-rbind(alldata_info,data_info)
}

#check<-ddply(AllData,.(Subject,c.response),summarise,ACC=mean(ACC),ACC2=mean(ACC2))
#check<-check[check$c.response=="b" | check$c.response=="g",]
#check

#check<-ddply(AllData,.(Subject,c.response),summarise,ACC=mean(ACC),ACC2=mean(ACC2))
#check<-check[check$c.response=="o" | check$c.response=="e",]
#check

#Print table of means
#print(alldata_info)

data_E1<-AllData
save(data_E1,file="data_E1.Rda")