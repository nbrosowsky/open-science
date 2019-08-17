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

for (i in seq(1:length(subject_data))){
    
    input<-scan(file=subject_data[i], what = "character",skip=(0))
    print(length(input))
    
    datatable<-matrix(input,ncol=14,byrow=T)
    Subject<-c(rep(i,nrow(datatable)))
    
    data_info<-c(subject_data[i],length(input),nrow(datatable))  
    
    block.type<-c(datatable[,11])
    Proportion<-c(datatable[,10])
    TrialType<-c(datatable[,8])
    TargetType<-c(datatable[,7])
    TargetPosition<-c(datatable[,9])
    RTs<-c(as.numeric(datatable[,13])-as.numeric(datatable[,12]))
    
    #separate letter targets
    Letters<-c(unlist(strsplit(datatable[,1],"")))
    LettersM<-matrix(Letters,ncol=4,byrow=T)
    
    #record responses to match targets
    Response<-recode(datatable[,14],"c('a','A')= 'a';c('s','S')='e';c('d','D')='o';c('f','F')='c';c('h','H')='r';c('j','J')='b';c('k','K')='g';c('l','L')='y'")
    
    #number of trials
    Trial<-c(seq(1:nrow(datatable)))
    
    #recode color words to single letters
    ColorP1<-recode(datatable[,2],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorP2<-recode(datatable[,3],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorP3<-recode(datatable[,4],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorP4<-recode(datatable[,5],"'red'= 'r';'blue'='b';'green'='g';'yellow'='y'")
    ColorsM<-cbind(ColorP1,ColorP2,ColorP3,ColorP4)
    
    P75Recode<-c(datatable[1,6],datatable[1,7],datatable[129,6],datatable[129,7])
    #recode the location / proportion label
    
    #code the accuracy 
    ACC<-c()
    for (i in seq(1:nrow(datatable))){
        for (n in seq(1:4)){
            if (TargetType[i]=="letter" && TargetPosition[i]==n){
                if (Response[i]==LettersM[i,n]){
                    ACC[i]<-1
                }else
                {ACC[i]<-0}
            } 
        }
        for (n in seq(1:4)){
            if (TargetType[i]=="color" && TargetPosition[i]==n){
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
            if (TargetType[i]=="letter"){
                if (Response[i]=="a" | Response[i]=="o" | Response[i]=="e" | Response[i]=="c"){
                    task.ACC[i]<-1
                }else
                {task.ACC[i]<-0}
            } 
        }
        for (n in seq(1:4)){
            if (TargetType[i]=="color"){
                if (Response[i]=="r" | Response[i]=="b" | Response[i]=="g" | Response[i]=="y"){
                    task.ACC[i]<-1
                }else
                {task.ACC[i]<-0}
            } 
        }
    }
    
    Response2<-recode(datatable[,14],"c('a','A')= 'a';c('s','S')='o';c('d','D')='e';c('f','F')='c';c('h','H')='r';c('j','J')='g';c('k','K')='b';c('l','L')='y'")  
    #number of trials
    
    c.response<-c()
    ACC2<-c()
    for (i in seq(1:nrow(datatable))){
        for (n in seq(1:4)){
            if (TargetType[i]=="letter" && TargetPosition[i]==n){
                c.response[i]<-LettersM[i,n]
                if (Response2[i]==LettersM[i,n]){
                    ACC2[i]<-1
                }else
                {ACC2[i]<-0}
            } 
        }
        for (n in seq(1:4)){
            if (TargetType[i]=="color" && TargetPosition[i]==n){
                c.response[i]<-ColorsM[i,n]
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
        if (datatable[n+1,6]==datatable[n,6]){
            Loc.switch[n+1]<-c("L.Repeat")
        }else{
            Loc.switch[n+1]<-c("L.Change")
        }
        if (datatable[n+1,7]==datatable[n,7]){
            Task.switch[n+1]<-c("T.Repeat")
        }else{
            Task.switch[n+1]<-c("T.Change")
        }
    }
    
    Task.switch[257]<-c("First")
    Loc.switch[257]<-c("First")
    block<-c(rep("T",96),rep("1",96),rep("1",96),rep("2",96),rep("2",96))
    single_sub<-data.frame(Subject,block,block.type,Proportion,TrialType,TargetPosition,RTs,ACC,Task.switch,Loc.switch,TargetType,ACC2,c.response,task.ACC)
    AllData<-rbind(AllData,single_sub)
    alldata_info<-rbind(alldata_info,data_info)
}

AllData$Subject<-factor(AllData$Subject)

data_E4<-AllData
save(data_E4,file="data_E4.Rda")