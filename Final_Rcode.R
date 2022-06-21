
library(knitr)
library(e1071)
library(ggplot2)
library(foreign)
library(readr)
library(survey)
library(viridis)
library(scales)
library(plyr)

# 1. Introduction
# LOADING AND CLEANING THE DATA

setwd("C:/Users/carme/Desktop/TFM")

# dataset 2020
dat20<-read.spss("NUEVO150720.sav", to.data.frame = TRUE)
dat20<-dat20[,-c(262,317,367:370)]
dat20t <- read_delim("NUEVO150720.dat", delim = "\t", 
                     escape_double = FALSE, trim_ws = TRUE)
dat20t<-dat20t[,-c(367:370)]

# dataset 2021
dat21<-read.spss("NUEVO150021.sav", to.data.frame = TRUE)
dat21t <- read_delim("NUEVO150021.dat", delim = "\t", 
                     escape_double = FALSE, trim_ws = TRUE)
dat21<-dat21[,-448]
dat21t<-dat21t[,-448]

# data set variables selected
var<-read.csv("var.csv", sep=";", header=F)
var<-var[1:44,1:2]
var[1,]<-c(1,1)
var[,1]<-as.numeric(var[,1])
names(var)<-c(2020,2021)

dat20<-dat20[,c(var[,1],241,245,249)]
dat21<-dat21[,c(var[,2],297,301,305)]
dat20t<-dat20t[,c(var[,1],241,245,249)]
dat21t<-dat21t[,c(var[,2],297,301,305)]

dat21$ID3<-as.factor(as.numeric(as.character(dat21$ID3)))
levels(dat21$ID3)<-levels(dat20$ID3)
 


## Designing the survey
#To design the survey we use the library survey. 

data20 = svydesign(ids = ~ 1, data=dat20, weights = dat20$POND)
data20t= svydesign(ids = ~ 1, data=dat20t, weights = dat20$POND)
data21 = svydesign(ids = ~ 1, data=dat21, weights = dat21$POND)
data21t= svydesign(ids = ~ 1, data=dat21t, weights = dat21$POND)
 

# Indicators 
## Health indicator
## Self physical health subindicator

selfperceive<-ifelse(dat20t$P35>2,1,0)
healthnow<-ifelse(dat20t$P36>2,1,0)
covid<-ifelse(dat20t$P39A_2==1,1,0)
hosp_covid<-ifelse(dat20t$P39A_3==1,1,0)
physical<-selfperceive+healthnow+covid+hosp_covid
dat20$physical=ifelse(physical>=2,2,ifelse(physical==0,0,1))

selfperceive1<-ifelse(dat21t$P35>2,1,0)
healthnow1<-ifelse(dat21t$P36>2,1,0)
covid1<-ifelse(dat21t$P39A_2==1,1,0)
hosp_covid1<-ifelse(dat21t$P39A_3==1,1,0)
physical1<-selfperceive1+healthnow1+covid1+hosp_covid1
dat21$physical=ifelse(physical1>=2,2,ifelse(physical1==0,0,1))

 

## Mental health subindicator

psico<-ifelse(dat20t$NUM_PROBL_PSICO==0,0,1)
noone<-ifelse(dat20t$NUM_SIT_P52_NADIE==0,0,1)
dat20$mental<-psico+noone

psico1<-ifelse(dat21t$NUM_PROBL_PSICO==0,0,1)
noone1<-ifelse(dat21t$NUM_SIT_P52_NADIE==0,0,1)
dat21$mental<-psico1+noone1
 
## Social Environment Health subindicator

discap_fam<-ifelse(dat20t$P37==1,1,0)
fam_worse<-ifelse(dat20t$P38==1,1,0)
hosp_covidfam<-ifelse(dat20t$P39B_1==1,1,0)
covidfam<-ifelse(dat20t$P39C_1==1,1,0)
covid_death<-ifelse(dat20t$P39C_2==1,1,0)
covidfam[is.na(covidfam)] <- 0
covid_death[is.na(covid_death)] <- 0
hosp_covidfam[is.na(hosp_covidfam)] <- 0
envir<-discap_fam+fam_worse+hosp_covidfam+covidfam+covid_death
dat20$envir=ifelse(envir>=2,2,ifelse(envir==0,0,1))

discap_fam1<-ifelse(dat21t$P37==1,1,0)
fam_worse1<-ifelse(dat21t$P38==1,1,0)
hosp_covidfam1<-ifelse(dat21t$P39B_1==1,1,0)
covidfam1<-ifelse(dat21t$P39C_1==1,1,0)
covid_death1<-ifelse(dat21t$P39C_2==1,1,0)
covidfam1[is.na(covidfam1)] <- 0
covid_death1[is.na(covid_death1)] <- 0
hosp_covidfam1[is.na(hosp_covidfam1)] <- 0
envir1<-discap_fam1+fam_worse1+hosp_covidfam1+covidfam1+covid_death1
dat21$envir=ifelse(envir1>=2,2,ifelse(envir1==0,0,1))
 

par(mfrow=c(2,3),oma = c(0, 0, 2, 0))
barplot(svytable(~dat20$physical,design=data20)/1507*100, col = plasma(3,begin=.1, end=0.95),
        sub="Physical 2020", ylim=c(0,80))
barplot(svytable(~dat20$mental,design=data20)/1507*100, col= plasma(3,begin=.1, end=0.95),
        sub="Mental 2020", ylim=c(0,80))
barplot(svytable(~dat20$envir,design=data20)/1507*100, col = plasma(3,begin=.1, end=0.95),
        sub="Surroundings 2020", ylim=c(0,80))
barplot(svytable(~dat21$physical,design=data21)/1500*100, col = plasma(3,begin=.1, end=0.95),sub="Physical 2021", ylim=c(0,80))
barplot(svytable(~dat21$mental,design=data21)/1500*100, col = plasma(3,begin=.1, end=0.95),sub="Mental 2021", ylim=c(0,80))
barplot(svytable(~dat21$envir,design=data21)/1500*100, col = plasma(3,begin=.1, end=0.95),
        sub="Surroundings 2021", ylim=c(0,80))
 
## Health Indicator design

mental=rescale(dat20$mental, to=c(0,10))
physical=rescale(dat20$physical, to=c(0,10))
envir=rescale(dat20$envir, to=c(0,10))
envir[is.na(envir)] <- 0
dat20$health= mental + envir + physical
dat20$health=rescale(dat20$health, to=c(0,10))

mental1=rescale(dat21$mental, to=c(0,10))
physical1=rescale(dat21$physical, to=c(0,10))
envir1=rescale(dat21$envir, to=c(0,10))
envir1[is.na(envir1)] <- 0
dat21$health= mental1 + envir1 + physical1
dat21$health=rescale(dat21$health, to=c(0,10))

barplot(svytable(~dat20$health, data20)/1507*100, sub="2020",col=plasma(1,begin=0.95),
        names.arg = c("0","1.6","3.3","5","6.6","8.3","10"),
        ylab="Percentage of population (%)", ylim=c(0,30))
barplot(svytable(~dat21$health, data21)/1500*100, sub="2021",col=plasma(1,begin=0.95),
        names.arg = c("0","1.6","3.3","5","6.6","8.3","10"),
        ylab="Percentage of population (%)",ylim=c(0,30))

svymean(~dat20$health, data20)
svyquantile(~dat20$health, data20, c(.25,.5,.75))

svymean(~dat21$health, data21)
svyquantile(~dat21$health, data21, c(.25,.5,.75))
 


## Problems indicator

home_pro<-ifelse(dat20t$P28==3,0,1)
total_pro<-ifelse(dat20t$NUM_PROBL_TOT==0,0,ifelse(dat20t$NUM_PROBL_TOT>4,2,1))
home_pro=rescale(home_pro, to=c(0,10))
total_pro=rescale(total_pro, to=c(0,10))
dat20$problems<-home_pro+total_pro
dat20$problems=rescale(dat20$problems, to=c(0,10))

home_pro1<-ifelse(dat21t$P28==3,0,1)
total_pro1<-ifelse(dat21t$NUM_PROBL_TOT==0,0,ifelse(dat21t$NUM_PROBL_TOT>4,2,1))
home_pro1=rescale(home_pro1, to=c(0,10))
total_pro1=rescale(total_pro1, to=c(0,10))
dat21$problems<-home_pro1+total_pro1
dat21$problems=rescale(dat21$problems, to=c(0,10))

svytable(~dat20$problems, data20)
par(mfrow=c(1,2))
barplot(svytable(~dat20$problems,design=data20)/1507*100, col=plasma(1,begin=0.95), 
        sub="2020", ylab="Percentage of population (%)", ylim=c(0,50))
barplot(svytable(~dat21$problems,design=data21)/1500*100, col=plasma(1,begin=0.95),
        sub="2021", ylab="Percentage of population (%)", ylim=c(0,50))


svymean(~dat20$problems, data20)
svyquantile(~dat20$problems, data20, c(.25,.5,.75))

svymean(~dat21$problems, data21)
svyquantile(~dat21$problems, data21, c(.25,.5,.75))


 
## Help indicator

imv<-ifelse(dat20t$P49==1,1,0)
imv[is.na(imv)] <- 0
p45_1<-ifelse(dat20$P45_1_1==0,1,0)
p45_2<-ifelse(dat20$P45_2_1==0,1,0)
p45_3<-ifelse(dat20$P45_3_1==0,1,0)
dat20$help<-imv+p45_2+p45_1+p45_3
dat20$help=rescale(dat20$help, to=c(0,10))

imv1<-ifelse(dat21t$P49==1,1,0)
imv1[is.na(imv1)] <- 0
p45_11<-ifelse(dat21$P45_1_1==0,1,0)
p45_21<-ifelse(dat21$P45_2_1==0,1,0)
p45_31<-ifelse(dat21$P45_3_1==0,1,0)
dat21$help<-imv1+p45_21+p45_11+p45_31
dat21$help=rescale(dat21$help, to=c(0,10))

par(mfrow=c(1,2))
barplot(svytable(~dat20$help,design=data20)/1507*100, col = plasma(1,begin=0.95), 
        sub="2020", ylab="Percentage of population (%)", 
        names.arg = c("0","3.3","6.6","10"),ylim=c(0,100))
barplot(svytable(~dat21$help,design=data21)/1500*100, col = plasma(1,begin=0.95),
        sub="2021", ylab="Percentage of population (%)",
        names.arg = c("0","3.3","6.6","10"),ylim=c(0,100))


svymean(~dat20$help, data20)
svyquantile(~dat20$help, data20, c(.25,.5,.75))

svymean(~dat21$help, data21)
svyquantile(~dat21$help, data21, c(.25,.5,.75))
 

## IPM indicator

dat20$IPM1<-dat20$IPM
dat21$IPM1<-dat21$IPM

dat20$IPM1=rescale(dat20$IPM1, to=c(0,10))
dat21$IPM1=rescale(dat21$IPM1, to=c(0,10))

par(mfrow=c(1,2))
barplot(svytable(~dat20$IPM1,design=data20)/1507*100, col = plasma(1,begin=0.95), 
        sub="2020", ylab="Percentage of population (%)",ylim=c(0,25))
barplot(svytable(~dat21$IPM1,design=data21)/1500*100, col = plasma(1,begin=0.95),
        sub="2021", ylab="Percentage of population (%)",ylim=c(0,25))


svymean(~dat20$IPM1, data20)
svyquantile(~dat20$IPM1, data20, c(.25,.5,.75))

svymean(~dat21$IPM1, data21)
svyquantile(~dat21$IPM1, data21, c(.25,.5,.75))


  
  # Descriptive analysis 
  
  # FCPCA function

#' @title Flury's Common Principal Component Analysis
#' 
#' @description 
#' Common principal component Analysis 
#' 
#' @param Data a numeric matrix or data frame
#' @param Group a vector of factors associated with group structure
#' @param Scale scaling variables, by default is False. By default data are centered within groups. 
#' @param graph should loading and component be plotted
#' @return list with the following results:
#' @return \item{Data}{Original data}
#' @return \item{Con.Data}{Concatenated centered data}
#' @return \item{split.Data}{Group centered data}
#' @return \item{Group}{Group as a factor vector}
#' @return \item{loadings.common}{Matrix of common loadings}
#' @return \item{lambda}{The specific variances of group}
#' @return \item{exp.var}{Percentages of total variance recovered associated with each dimension }

FCPCA1 <-function(Data, Group, Scale=FALSE, graph=FALSE){
  #============================================================================
  #                             1. Checking the inputs
  #============================================================================
  
  Data=Data[,-5]
  data=Data
  #============================================================================
  #                              2. preparing Data
  #============================================================================
  if (is.data.frame(Data) == TRUE) {
    Data=as.matrix(Data)
  }
  if(is.null(colnames(Data))) {
    colnames(Data) = paste('V', 1:ncol(Data), sep='')
  }
  Group = as.factor(Group)
  
  #Group.w = svydesign(ids= ~1,data=Group, weights = Data$POND)
  
  
  
  rownames(Data) = Group                 #---- rownames of data=groups
  M = length(levels(Group))              #----number of groups: M
  P = dim(Data)[2]                       #----number of variables: P
  n = as.vector(table(Group))            #----number of individuals in each group
  N = sum(n)                             #----number of individuals
  split.Data = split(Data,Group)         #----split Data to M parts 
  
  # centering and scaling if TRUE
  for(m in 1:M){  
    split.Data[[m]] = matrix(split.Data[[m]], nrow=n[m])
    split.Data[[m]] = scale(split.Data[[m]], center=TRUE, scale=Scale)
  }
  
  
  # concatinated dataset by row as groups
  Con.Data = split.Data[[1]]  
  for(m in 2:M) {
    Con.Data = rbind(Con.Data, split.Data[[m]])
  }
  rownames(Con.Data) = Group
  colnames(Con.Data) = colnames(Data)
  
  
  # Variance-covariance matrix for each group
  
  data1<-data[1:1507,-6]
  data2<-data[1508:3007,-6]
  Data1.w = svydesign(ids= ~1,data=data1, weights = data1$POND)
  Data2.w = svydesign(ids= ~1,data=data2, weights = data2$POND)
  
  library(jtools)
  R1=svycor(~health+problems+help+IPM1, design = Data1.w)
  R1=R1$cors
  c1=svysd(~health+problems+help+IPM1, design = Data1.w)
  c1=as.vector(c1)
  D1=diag(c1)
  S1 = D1%*%R1%*%D1
  
  
  R2=svycor(~health+problems+help+IPM1, design = Data2.w)
  R2=R2$cors
  c2=svysd(~health+problems+help+IPM1, design = Data2.w)
  c2=as.vector(c2)
  D2=diag(c2)
  S2 = D2%*%R2%*%D2
  
  cov<-list(S1,S2)
  
  #============================================================================
  #      		                      3. FG algorithm  
  #============================================================================
  FG <- function(cov,L=15,P,M){
    #------ F step
    k = M
    K = M
    B = diag(P)
    Bold = diag(P)
    C = cov
    T = vector("list",k)
    d1 = c(rep(0,k))
    d2 = c(rep(0,k))
    for(l in 1:L){
      for(p in 1:(P-1)){
        for(e in (p+1):P){
          Q=diag(2)
          #------ G step    
          M=diag(2)
          for(k in 1:K){
            H=B[,c(p,e)]
            T[[k]]=t(H)%*%C[[k]]%*%H
            d1[k]=t(Q[,1])%*%T[[k]]%*%Q[,1]
            d2[k]=t(Q[,2])%*%T[[k]]%*%Q[,2]
            M = M + (d1[k]-d2[k])/(d1[k]*d2[k])*T[[k]]
          }
          
          eig <- eigen(M)
          Q=eig$vectors
          B[,c(p,e)]=H%*%Q
        }
      }
    }
    W=B
    res=W
    return(W)
  }
  
  
  W =FG(cov,15,P,M)
  #============================================================================
  #                              4.  Explained variance 
  #                              variance of each loading 
  #                lambda = t(common loading)*(t(Xm)* Xm) * common loading
  #============================================================================
  lambda = matrix(0, nrow=M, ncol=P)
  for(m in 1:M){
    lambda[m,] = round(diag(t(W) %*% cov[[m]] %*% W),3)
  }
  
  
  
  #============================================================================
  #    			                      5. Outputs
  #============================================================================
  res <- list(
    Data = Data,
    Con.Data   = Con.Data,
    split.Data = split.Data,
    Group=Group)
  
  
  
  res$loadings.common = W  
  rownames(res$loadings.common) = colnames(Data)
  colnames(res$loadings.common) = paste("Dim", 1:P, sep="")
  
  res$lambda = lambda
  rownames(res$lambda) = levels(Group)
  colnames(res$lambda) = paste("Dim", 1:P, sep="")
  
  
  ncomp = ncol(res$lambda) 
  exp.var = matrix(0,M,ncomp)
  for(m in 1:M){
    exp.var[m,] = 100 * lambda[m,]/ sum(diag(cov[[m]]))
  }
  res$exp.var = exp.var
  rownames(res$exp.var) = levels(Group)
  colnames(res$exp.var) = paste("Dim", 1:ncomp, sep="")
  
  if(graph) {plot.mg(res)}
  
  class(res) = c("FCPCA","mg")
  return(res)
}
# =============================================================================

#Creating the global indicators

attach(dat21)
data21_ind<-data.frame(cbind("health"=health,"problems"=problems,"help"=help,
                             "IPM1"=IPM1,"POND"=POND,"group"=rep("2021",1500)))
detach(dat21)

attach(dat20)
data20_ind<-data.frame(cbind("health"=health,"problems"=problems,"help"=help,
                             "IPM1"=IPM1,"POND"=POND,"group"=rep("2020",1507)))
detach(dat20)


data<-rbind(data20_ind,data21_ind)
data[,1:5]<-lapply(data[,1:5],as.numeric)

library(multigroup)
library(ggfortify)
Data = data[,-6]
Group = data[,6]
res.FCPCA = FCPCA1(Data, Group, graph=F)


scoreplot(res.FCPCA, axes=c(1,2))
autoplot(res.FCPCA$loadings.common)

loadingsplot(res.FCPCA, axes=c(1,2))

par(mfrow=c(1,2))
barplot(res.FCPCA$exp.var[1,],col = plasma(1, begin=0.4),
        sub="2020", ylab="Explained variance (%)", ylim=c(0,50))
barplot(res.FCPCA$exp.var[2,],col = plasma(1, begin=0.4),
        sub="2021", ylim=c(0,50))

res.FCPCA$loadings.common

TT = res.FCPCA$Con.Data %*% res.FCPCA$loadings.common[,c(1,2)]
dat20$Y1=TT[1:1507,1]
dat20$Y2=TT[1:1507,2]
dat21$Y1=TT[1508:3007,1]
dat21$Y2=TT[1508:3007,2]

dat20$Y<-(dat20$Y1+dat20$Y2)/2
dat21$Y<-(dat21$Y1+dat21$Y2)/2

 
# Gender

levels(dat20$ID5)<-c("Men","Women")
levels(dat21$ID5)<-c("Men","Women")

par(mfrow=c(2,2))
boxplot(dat20$Y1~dat20$ID5, design=data20, xlab="2020", ylab="First PC", col=plasma(2, begin=0.1,end=0.95), ylim=c(-10,10))
boxplot(dat21$Y1~dat21$ID5, design=data21, xlab="2021", ylab="First PC",col=plasma(2, begin=0.1,end=0.95),ylim=c(-10,10))
boxplot(dat20$Y2~dat20$ID5, design=data20, xlab="2020", ylab="Second PC", col=plasma(2, begin=0.1,end=0.95),ylim=c(-10,10))
boxplot(dat21$Y2~dat21$ID5, design=data21, xlab="2021", ylab="Second PC",col=plasma(2, begin=0.1,end=0.95),ylim=c(-10,10))

# AROPE

par(mfrow=c(2,2))
levels(dat20$AROPE)<-c("No"," Yes")
levels(dat21$AROPE)<-c("No"," Yes")
boxplot(dat20$Y1~dat20$AROPE, design=data20, xlab="2020", ylab="First PC", col=plasma(2, begin=0.1,end=0.95),ylim=c(-10,10))
boxplot(dat21$Y1~dat21$AROPE, design=data21, xlab="2021", ylab="First PC",col=plasma(2, begin=0.1,end=0.95),ylim=c(-10,10))
boxplot(dat20$Y2~dat20$AROPE, design=data20, xlab="2020", ylab="Second PC", col=plasma(2, begin=0.1,end=0.95),ylim=c(-10,10))
boxplot(dat21$Y2~dat21$AROPE, design=data21, xlab="2021", ylab="Second PC",col=plasma(2, begin=0.1,end=0.95),ylim=c(-10,10))


# Level of studies 

levels(dat20$P12_1)<-c("None","Elementary","High School","Graduate", "Post graduate")
levels(dat21$P12_1)<-c("None","Elementary","High School","Graduate", "Post graduate")
par(mfrow=c(1,2))
boxplot(dat20$Y1~dat20$P12_1, xlab=" ",design=data20,ylab="First PC", col=plasma(5, begin=0.1,end=0.95),las=2,cex.axis=0.7,main="2020",ylim=c(-8,10))
boxplot(dat21$Y1~dat21$P12_1, design=data21,cex.axis=0.7,xlab=" ",ylab=" ",col=plasma(5, begin=0.1,end=0.95),las=2,main="2021",ylim=c(-8,10))



# Group of Age

dat20$age1=ifelse(dat20$ID6<=60,1,dat20$ID6)
dat20$age1=ifelse(dat20$ID6>60 & dat20$ID6<=70,2,dat20$age1)
dat20$age1=ifelse(dat20$ID6>70 & dat20$ID6<=80,3,dat20$age1)
dat20$age1=ifelse(dat20$ID6>80,4,dat20$age1)
dat20$age1=as.factor(dat20$age1)
levels(dat20$age1) = c("<60", "61-70","71-80","80<")

dat21$age1=ifelse(dat21$ID6<=60,1,dat21$ID6)
dat21$age1=ifelse(dat21$ID6>60 & dat21$ID6<=70,2,dat21$age1)
dat21$age1=ifelse(dat21$ID6>70 & dat21$ID6<=80,3,dat21$age1)
dat21$age1=ifelse(dat21$ID6>80,4,dat21$age1)
dat21$age1=as.factor(dat21$age1)
levels(dat21$age1) = c("<60", "61-70","71-80","80<")

par(mfrow=c(2,2))
boxplot(dat20$Y1~dat20$age1, design=data20,cex.axis=0.7, xlab="2020", ylab="First PC", col=plasma(4, begin=0.1,end=0.95),ylim=c(-10,10))
boxplot(dat21$Y1~dat21$age1, design=data21,cex.axis=0.7, xlab="2021", ylab="First PC",col=plasma(4, begin=0.1,end=0.95),ylim=c(-10,10))
boxplot(dat20$Y2~dat20$age1, design=data20,cex.axis=0.7, xlab="2020", ylab="Second PC", col=plasma(4, begin=0.1,end=0.95),ylim=c(-8,9))
boxplot(dat21$Y2~dat21$age1, design=data21,cex.axis=0.7, xlab="2021", ylab="Second PC",col=plasma(4, begin=0.1,end=0.95),ylim=c(-8,9))
 

#### Mean value of Y1 per ccaa
ccaa= unique(dat20$ID2)
ccaamean1 = NULL
for (i in ccaa){
  ccaamean1[i]=svymean(~dat20$Y1[dat20$ID2==i], design=subset(data20, dat20$ID2==i))
}

ccaamean11 = NULL
for (i in ccaa){
  ccaamean11[i]=svymean(~dat21$Y1[dat21$ID2==i], design=subset(data21, dat21$ID2==i))
}

barplot(sort(ccaamean1),las=2,col = plasma(20),ylim=c(-2,4))
barplot(sort(ccaamean11), las=2,col = plasma(20))

#### Mean value of Y2 per ccaa
ccaamean2 = NULL

for (i in ccaa){
  ccaamean2[i]=svymean(~dat20$Y2[dat20$ID2==i], design=subset(data20, dat20$ID2==i))
} 
ccaamean21 = NULL
for (i in ccaa){
  ccaamean21[i]=svymean(~dat21$Y2[dat21$ID2==i], design=subset(data21, dat21$ID2==i))
}


barplot(sort(ccaamean2),las=2,col = plasma(20),ylim=c(-3,2))
barplot(sort(ccaamean21),las=2,col = plasma(20),ylim=c(-3,2))


#### Mean value of Y1 per province
prov= unique(dat20$ID3)

prov<-prov[c(7, 16,17, 25, 48, 50, 12, 46, 20, 32, 19,14, 30, 37, 43, 38, 6,9,15,22, 11,33,34,1,24,35,51,13,21,42,8,3,41,28,45,40,39,23,4,18,47,49,29,5,26,36,31,44,10,52,2,27)]

provmean1 = NULL
for (i in prov){
  provmean1[i]=svymean(~dat20$Y1[dat20$ID3==i], design=subset(data20, dat20$ID3==i))
}

provmean11 = NULL
for (i in prov){
  provmean11[i]=svymean(~dat21$Y1[dat21$ID3==i], design=subset(data21, dat21$ID3==i))
}

range(provmean1)
#-3.343203  1.920057
range(provmean11)
#-2.384220  5.528425

mean(provmean1)
mean(provmean11)

 
# Plots of Y1 in Spain

library(highcharter)
require(xgboost)
require(jsonlite)
library(tidyverse)

mydata <- fromJSON("https://code.highcharts.com/mapdata/countries/es/es-all.geo.json", 
                   flatten = TRUE)

provmean1<-c(provmean1,NA)
provincias <- tibble(clave = mydata$features$`properties.hc-key`,valor = provmean1)
provmean11<-c(provmean11,NA)
provincias1 <- tibble(clave = mydata$features$`properties.hc-key`,valor = provmean11)

mes1 <- hcmap("countries/es/es-all", data = provincias,
              name = "clave", value = "valor", joinBy = c("hc-key", "clave"),
              borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(seq(-4,6,by=1))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0)
mes11 <- hcmap("countries/es/es-all", data = provincias1,
               name = "clave", value = "valor", joinBy = c("hc-key", "clave"),
               borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(seq(-4,6,by=1))) %>%
  hc_legend(main= "Y1",layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0)


mes1
mes11

 



 

 


