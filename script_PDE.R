rm(list=ls())
# graphics.off()
setwd("C:/Users/virgillitc/Desktop/Dottorato/pde/script e database/cloris")
 
library(ggplot2)
library(tidyverse)
library(mvtnorm)
library(gridExtra)
library(latex2exp)
library(pracma)

#mortality function
mort <- function(x,y){
  e<-exp(-x*y)
  rbinom(n=1, size=1, prob=e)
}

mort2  <- function(N,x,y){ 
  rbinom(n=1, size=N, prob=exp(-x*y))
}

#function to calculate the radius
raggio <- function(x){   
  sqrt(sum(x[1]^2+x[2]^2))
}


#########################MRR DATA################
#importing data for first,second, and third release
db_MRR1=read.csv(file="dataset_MR1.csv",sep = ";",header=TRUE,na.strings ="NA",dec=".")
db_MRR2=read.csv(file="dataset_MR2.csv",sep = ";",header=TRUE,na.strings ="NA",dec=".")
db_MRR3=read.csv(file="dataset_MR3.csv",sep = ";",header=TRUE,na.strings ="NA",dec=".")

db1<-db_MRR1 %>% filter(day_after_release!="07-nov") %>% 
  mutate(day_after_release = as.numeric(as.character(day_after_release))-1)%>%
  group_by(day_after_release,ST,range) %>% 
  summarise(capture=sum(marked.unmarked))

db2<-db_MRR2 %>% filter(day_after_release!="07-nov" & day_after_release!="dic-16") %>% 
  mutate(day_after_release = as.numeric(as.character(day_after_release))-1)%>%
  group_by(day_after_release,ST,range) %>% 
  summarise(capture=sum(marked.unmarked))

db3<-db_MRR3 %>% filter(day_after_release!="07-nov" &day_after_release!="dic-16") %>% 
  mutate(day_after_release = as.numeric(as.character(day_after_release))-1)%>%
  group_by(day_after_release,ST,range) %>% 
  summarise(capture=sum(marked.unmarked))

#created a list with the 10 annulli first release
anello_1_db1<-subset(db1,db1$range=="0-50") 
anello_2_db1<-subset(db1,db1$range=="50-100")
anello_3_db1<-subset(db1,db1$range=="100-150")
anello_4_db1<-subset(db1,db1$range=="150-200")
anello_5_db1<-subset(db1,db1$range=="200-250")
anello_6_db1<-subset(db1,db1$range=="250-300")
anello_7_db1<-subset(db1,db1$range=="300-350")
anello_8_db1<-subset(db1,db1$range=="350-400")
anello_9_db1<-subset(db1,db1$range=="400-450")
anello_10_db1<-subset(db1,db1$range=="450-500")
lista_anelli_db1=list(anello_1_db1,anello_2_db1,anello_3_db1,anello_4_db1,anello_5_db1,anello_6_db1,anello_7_db1,
                    anello_8_db1,anello_9_db1,anello_10_db1)

#matrix with observed data first release
osservate_db1 <- matrix(NA,nrow=length(lista_anelli_db1),ncol=5)

for(day in 1:5){
  for( s in 1:10){
    osservate_db1[s,day]<-sum(lista_anelli_db1[[s]]$capture[lista_anelli_db1[[s]]$day_after_release==day])
  }
}

#created a list with the 10 annulli second release
anello_1_db2<-subset(db2,db2$range=="0-50") 
anello_2_db2<-subset(db2,db2$range=="50-100")
anello_3_db2<-subset(db2,db2$range=="100-150")
anello_4_db2<-subset(db2,db2$range=="150-200")
anello_5_db2<-subset(db2,db2$range=="200-250")
anello_6_db2<-subset(db2,db2$range=="250-300")
anello_7_db2<-subset(db2,db2$range=="300-350")
anello_8_db2<-subset(db2,db2$range=="350-400")
anello_9_db2<-subset(db2,db2$range=="400-450")
anello_10_db2<-subset(db2,db2$range=="450-500")
lista_anelli_db2=list(anello_1_db2,anello_2_db2,anello_3_db2,anello_4_db2,anello_5_db2,anello_6_db2,anello_7_db2,
                      anello_8_db2,anello_9_db2,anello_10_db2)

#matrix with observed data second release
osservate_db2 <- matrix(NA,nrow=length(lista_anelli_db2),ncol=5)

for(day in 1:5){
  for( s in 1:10){
    osservate_db2[s,day]<-sum(lista_anelli_db2[[s]]$capture[lista_anelli_db2[[s]]$day_after_release==day])
  }
}

#created a list with the 10 annulli third release
anello_1_db3<-subset(db3,db3$range=="0-50") 
anello_2_db3<-subset(db3,db3$range=="50-100")
anello_3_db3<-subset(db3,db3$range=="100-150")
anello_4_db3<-subset(db3,db3$range=="150-200")
anello_5_db3<-subset(db3,db3$range=="200-250")
anello_6_db3<-subset(db3,db3$range=="250-300")
anello_7_db3<-subset(db3,db3$range=="300-350")
anello_8_db3<-subset(db3,db3$range=="350-400")
anello_9_db3<-subset(db3,db3$range=="400-450")
anello_10_db3<-subset(db3,db3$range=="450-500")
lista_anelli_db3=list(anello_1_db3,anello_2_db3,anello_3_db3,anello_4_db3,anello_5_db3,anello_6_db3,anello_7_db3,
                      anello_8_db3,anello_9_db3,anello_10_db3)

#matrix with observed data third release
osservate_db3 <- matrix(NA,nrow=length(lista_anelli_db3),ncol=5)

for(day in 1:5){
  for( s in 1:10){
    osservate_db3[s,day]<-sum(lista_anelli_db3[[s]]$capture[lista_anelli_db3[[s]]$day_after_release==day])
  }
}

#########################Capture rate################

#database for the Sticky traps first release
db1_trap=read.csv( file= "Sticky_MRR1.csv",sep = ";",header=TRUE,na.strings ="NA",dec=".")
db1_trap$range <- factor(db1_trap$range,levels = levels(db1_trap$range)[c(1:2,11,3:10)])
trappole_attive_db1 <- db1_trap %>% group_by(range) %>% 
  summarise(day2 = sum(X2d.act),
            day3 = sum(X3d.act),
            day4 = sum(X4d.act),
            day5 = sum(X5d.act),
            day6 = sum(X6d.act))
trappole_attive_db1
trappole_attive_matrix_db1 <- as.matrix(trappole_attive_db1[-1,2:6])
for (i in 1:nrow(trappole_attive_matrix_db1)){
  if(trappole_attive_matrix_db1[1,1]){
    trappole_attive_matrix_db1[1,1]<-0} 
      else {trappole_attive_matrix_db1[i,1]<-trappole_attive_matrix_db1[i,1]/2}
}
trappole_attive_matrix_db1

#database for the Sticky traps second release
db_trap_db2=read.csv( file= "Sticky_MRR2.csv",sep = ";",header=TRUE,na.strings ="NA",dec=".")
db_trap_db2$range <- factor(db_trap_db2$range,levels =c("0-50","50-100","100-150","150-200","200-250",
                                                "250-300","300-350","350-400","400-450","450-500"))
trappole_attive_db2 <- db_trap_db2 %>% group_by(range) %>% 
  summarise(day2 = sum(X2d.act,na.rm=T),
            day3 = sum(X3d.act,na.rm=T),
            day4 = sum(X4d.act,na.rm=T),
            day5 = sum(X5d.act,na.rm=T),
            day6 = sum(X6d.act,na.rm=T))
trappole_attive_matrix_db2 <- as.matrix(trappole_attive_db2[1:10,-1])

for (i in 1:nrow(trappole_attive_matrix_db2)){
  if(trappole_attive_matrix_db2[1,1]){
    trappole_attive_matrix_db2[1,1]<-0} 
  else {trappole_attive_matrix_db2[i,1]<-trappole_attive_matrix_db2[i,1]/2}
}
trappole_attive_matrix_db2

#database for the Sticky traps third release
db_trap_db3=read.csv( file= "Sticky_MRR3.csv",sep = ";",header=TRUE,na.strings ="NA",dec=".")
db_trap_db3$range <- factor(db_trap_db3$range,levels =c("0-50","50-100","100-150","150-200","200-250",
                                                "250-300","300-350","350-400","400-450","450-500"))
trappole_attive_db3 <- db_trap_db3 %>% group_by(range) %>% 
  summarise(day2 = sum(X2d.act),
            day3 = sum(X3d.act),
            day4 = sum(X4d.act),
            day5 = sum(X5d.act),
            day6 = sum(X6d.act))
trappole_attive_matrix_db3 <- as.matrix(trappole_attive_db3[1:10,-1])

for (i in 1:nrow(trappole_attive_matrix_db3)){
  if(trappole_attive_matrix_db3[1,1]){
    trappole_attive_matrix_db3[1,1]<-0} 
  else {trappole_attive_matrix_db3[i,1]<-trappole_attive_matrix_db3[i,1]/2}
}

M1<-1049 #initial number of mosquitoes first release
M2<-1600 #initial number of mosquitoes second release
M3<-1210 #initial number of mosquitoes third release
anelli<-10 #number of annulli

#calculate of capture rate in each traps.
radius_1 = seq(50,500,by=50) #define 10 radius "50","100" ect
area_anello = pi*(radius_1^2 - c(0,radius_1[-10])^2)# area in m2
area_di_cattura<-50*50*pi #suppose that the recapure area is only 50m.
capture.rate<-0.000124245 #capture rate estimate in another experiment in Rome

#capture rate for each traps in firt release
capture.anello_db1<-matrix(NA,ncol=day,nrow=anelli) #matrix to calculate the capture rate for each traps.
for(d in 1:day){
  for(i in 1:anelli){
    capture.anello_db1[i,d] = (capture.rate * trappole_attive_matrix_db1[i,d] * area_di_cattura)/area_anello[i]
  }
}

#capture rate for each traps in second release
capture.anello_db2<-matrix(NA,ncol=day,nrow=anelli)
for(d in 1:day){
  for(i in 1:anelli){
    capture.anello_db2[i,d] = (capture.rate * trappole_attive_matrix_db2[i,d] * area_di_cattura)/area_anello[i]
  }
}

#capture rate for each traps in third release
capture.anello_db3<-matrix(NA,ncol=day,nrow=anelli)
for(d in 1:day){
  for(i in 1:anelli){
    capture.anello_db3[i,d] = (capture.rate * trappole_attive_matrix_db3[i,d] * area_di_cattura)/area_anello[i]
  }
}
#########################Simulation D1,D2,D3 and lambda################

set.seed(890)
sim2 <- 10 #number of "life" of mosquitoes
max_iter_starting=2000 #number of simulation
mu_1<- 0.02190 #mortality rate
mu_2<- 0.01831
mu_3<- 0.012609
day<-5 #number of experimets day
day.sim<-10 # number of simulation days
lambda<-rep(NA,max_iter_starting)

#Initialing parameters first release
likelihood_db1<-D_db1<-rep(NA,max_iter_starting)#created a 3 vector for the simulation
mosquitoes_db1<-matrix(NA, ncol=anelli, nrow=sim2)#matrix for number of mosquitoes recapture in 10 days in the simulation 
mosquitoesavg_db1<-matrix(NA, ncol=anelli, nrow=day.sim) #matrix for average mosquitoes
mosquitoes24_db1<- matrix(NA, ncol=5, nrow=anelli) #matrix for the number of mosquitoes recapture in 5 days
zanzare_db1<- matrix(NA,ncol=(day.sim+1),nrow=sim2) #matrix for the alive or dead mosquitoes
zanzare_db1[,1] <- M1 #the first row of matrix is th initial number of mosquitoes released 

#Initialing parameters second release
likelihood_db2<-D_db2<-rep(NA,max_iter_starting)
mosquitoes_db2<-matrix(NA, ncol=anelli, nrow=sim2)
mosquitoesavg_db2<-matrix(NA, ncol=anelli, nrow=day.sim) 
mosquitoes24_db2<- matrix(NA, ncol=5, nrow=anelli) 
zanzare_db2<- matrix(NA,ncol=(day.sim+1),nrow=sim2) 
zanzare_db2[,1] <- M2  

#Initialing parameters third release
likelihood_db3<-D_db3<-rep(NA,max_iter_starting)
mosquitoes_db3<-matrix(NA, ncol=anelli, nrow=sim2)
mosquitoesavg_db3<-matrix(NA, ncol=anelli, nrow=day.sim) 
mosquitoes24_db3<- matrix(NA, ncol=5, nrow=anelli) 
zanzare_db3<- matrix(NA,ncol=(day.sim+1),nrow=sim2) 
zanzare_db3[,1] <- M3

tot_likelihood<-rep(NA,max_iter_starting)

#Simulation for  D1,D2,D2, lambda (correction factor) 
for (iter in 1:max_iter_starting){

  D_db1[iter]      = runif(n=1,min=0,max=100)
  D_db2[iter]      = runif(n=1,min=0,max=100)
  D_db3[iter]      = runif(n=1,min=0,max=100)
  lambda[iter] = runif(n=1,min=0,max=200)
  
  for(day in 1:day.sim){
    
    stand_dev_db1<-2* D_db1[iter]*day*12
    stand_dev_db2<-2* D_db2[iter]*day*12
    stand_dev_db3<-2* D_db3[iter]*day*12
    
    cov.matrix_sim_db1<-matrix(c(stand_dev_db1,0,0,stand_dev_db1),ncol=2)
    cov.matrix_sim_db2<-matrix(c(stand_dev_db2,0,0,stand_dev_db2),ncol=2)
    cov.matrix_sim_db3<-matrix(c(stand_dev_db3,0,0,stand_dev_db3),ncol=2)
    
    for (g in 1:sim2){
      zanzare_db1[g,(day+1)]  <-  mort2(zanzare_db1[g,day],mu_1,1)
      zanzare_db2[g,(day+1)]  <-  mort2(zanzare_db2[g,day],mu_2,1)
      zanzare_db3[g,(day+1)]  <-  mort2(zanzare_db3[g,day],mu_3,1)
      
      pos_db1<- rmvnorm(n=zanzare_db1[g,day] ,mean= c(0,0),sigma=cov.matrix_sim_db1 , method = "eigen") 
      pos_db2<- rmvnorm(n=zanzare_db2[g,day] ,mean= c(0,0),sigma=cov.matrix_sim_db2 , method = "eigen") 
      pos_db3<- rmvnorm(n=zanzare_db3[g,day] ,mean= c(0,0),sigma=cov.matrix_sim_db3 , method = "eigen") 
      
      radius_db1<- apply(pos_db1,1,raggio)
      radius_db2<- apply(pos_db2,1,raggio)
      radius_db3<- apply(pos_db3,1,raggio)
      
      where_db1<- findInterval(radius_db1, seq(0,450,50))
      where_db2<- findInterval(radius_db2, seq(0,450,50))
      where_db3<- findInterval(radius_db3, seq(0,450,50))
      
      dummy_db1<- data.frame(anelli = factor(names(table(where_db1)),
                                          levels=c(1:10)),
                          valori = as.numeric(table(where_db1)))
      
      dummy_db2<- data.frame(anelli = factor(names(table(where_db2)),
                                             levels=c(1:10)),
                             valori = as.numeric(table(where_db2)))
      
      dummy_db3<- data.frame(anelli = factor(names(table(where_db3)),
                                             levels=c(1:10)),
                             valori = as.numeric(table(where_db3)))
      
      mosquitoes_db1[g,] <- tapply(dummy_db1$valori,dummy_db1$anelli,sum,na.rm=T)
      mosquitoes_db1[g,is.na(mosquitoes_db1[g,])]<-0   
      
      mosquitoes_db2[g,] <- tapply(dummy_db2$valori,dummy_db2$anelli,sum,na.rm=T)
      mosquitoes_db2[g,is.na(mosquitoes_db2[g,])]<-0
      
      mosquitoes_db3[g,] <- tapply(dummy_db3$valori,dummy_db3$anelli,sum,na.rm=T)
      mosquitoes_db3[g,is.na(mosquitoes_db3[g,])]<-0
    }
    mosquitoesavg_db1[day,] <- apply(mosquitoes_db1,2,mean)
    mosquitoesavg_db2[day,] <- apply(mosquitoes_db2,2,mean)
    mosquitoesavg_db3[day,] <- apply(mosquitoes_db3,2,mean)
    
    
  }
  
  h24 <- seq(2,10,by=2)
  
  for( YY in 1:5){
    mosquitoes24_db1[,YY] <- mosquitoesavg_db1[h24[YY],]  +  mosquitoesavg_db1[(h24[YY]-1),] 
  }
  for( YY in 1:5){
    mosquitoes24_db2[,YY] <- mosquitoesavg_db2[h24[YY],]  +  mosquitoesavg_db2[(h24[YY]-1),] 
  }
  for( YY in 1:5){
    mosquitoes24_db3[,YY] <- mosquitoesavg_db3[h24[YY],]  +  mosquitoesavg_db3[(h24[YY]-1),] 
  }
  
  a <- 0 
  for(day in 1:5){
    for( s in 1:anelli){
      lmd_db1<- lambda[iter]*capture.anello_db1[s,day]*mosquitoes24_db1[s,day]
      a<- a + dpois(x=osservate_db1[s,day],lambda = lmd_db1,log=T)
    }
  }
  likelihood_db1[iter] <- a
  
  aa <- 0 
  for(day in 1:5){
    for( s in 1:anelli){
      lmd_db2<- lambda[iter]*capture.anello_db2[s,day]*mosquitoes24_db2[s,day]
      aa<- aa + dpois(x=osservate_db2[s,day],lambda = lmd_db2,log=T)
    }
  }
  likelihood_db2[iter] <- aa
  
  aaa <- 0 
  for(day in 1:5){
    for( s in 1:anelli){
      lmd_db3 <- lambda[iter]*capture.anello_db3[s,day]*mosquitoes24_db3[s,day]
      aaa <- aaa + dpois(x=osservate_db3[s,day],lambda = lmd_db3,log=T)
    }
  }
  likelihood_db3[iter] <- aaa

  tot_likelihood[iter]<-sum(likelihood_db1[iter],likelihood_db2[iter],likelihood_db3[iter])
   print(iter)
}

#plot for correction factor and Diffusion coefficients
par(mfcol=c(1,1))
plot(lambda,tot_likelihood,main="Simulation of capture rate",xlab="",ylab="")
plot(D_db1,tot_likelihood,main="Simulation of D1",xlab="",ylab="") #
plot(D_db2,tot_likelihood,main="Simulation of D2",xlab="",ylab="") #
plot(D_db3,tot_likelihood,main="Simulation of D3",xlab="",ylab="") #


#values of D and Lambda for max likelihood 
dif_db1<- D_db1[which.max(tot_likelihood)] # 68.96509
dif_db2<- D_db2[which.max(tot_likelihood)] # 37.64955
dif_db3<- D_db3[which.max(tot_likelihood)] # 61.27361
lambda_dif<- lambda[which.max(tot_likelihood)] #64.75319


#########################MCMC################
max_iter_starting=15000  #numbers of runs 
lambda_new<-rep(NA,max_iter_starting) #vector of correction factor for MCMC
D_new_db1<-D_new_db2<-D_new_db3<-rep(NA,max_iter_starting) #vectors of diffusion coefficents  for MCMC

new_likelihood<-rep(NA,max_iter_starting) #vector of likelihood for MCMC

lambda_new[1] = lambda_dif #first value to start MCMC for lambda
D_new_db1[1] = dif_db1 #first value to start MCMC for D1
D_new_db2[1] = dif_db2 #first value to start MCMC for D2
D_new_db3[1] = dif_db3 #first value to start MCMC for D3
new_likelihood[1]=max(tot_likelihood) #first value to start MCMC for likelihood

c = 0 #Initialing of the acceptance rate
sim2=20 #number of "life" of mosquitoes

#Initiling of parameters for MCMC first release
mosquitoes_db1<-matrix(NA, ncol=anelli, nrow=sim2)#matrix for number of mosquitoes recapture in 10 days in the simulation 
mosquitoesavg_db1<-matrix(NA, ncol=anelli, nrow=day.sim) #matrix for average mosquitoes
mosquitoes24_db1<- matrix(NA, ncol=5, nrow=anelli) #matrix for the number of mosquitoes recapture in 5 days
zanzare_db1<- matrix(NA,ncol=(day.sim+1),nrow=sim2) #matrix for the alive or dead mosquitoes
zanzare_db1[,1] <- M1 #the first row of matrix is th initial number of mosquitoes released 

#Initiling of parameters for MCMC second release
mosquitoes_db2<-matrix(NA, ncol=anelli, nrow=sim2)
mosquitoesavg_db2<-matrix(NA, ncol=anelli, nrow=day.sim) 
mosquitoes24_db2<- matrix(NA, ncol=5, nrow=anelli) 
zanzare_db2<- matrix(NA,ncol=(day.sim+1),nrow=sim2) 
zanzare_db2[,1] <- M2  

#Initiling of parameters for MCMC third release
mosquitoes_db3<-matrix(NA, ncol=anelli, nrow=sim2)
mosquitoesavg_db3<-matrix(NA, ncol=anelli, nrow=day.sim) 
mosquitoes24_db3<- matrix(NA, ncol=5, nrow=anelli) 
zanzare_db3<- matrix(NA,ncol=(day.sim+1),nrow=sim2) 
zanzare_db3[,1] <- M3


k=0

for (iter in 2:max_iter_starting){
  
  # pesca centrato nel "migliore"
  D_new_db1[iter]      = rnorm(n=1,mean=D_new_db1[iter-1],sd=0.5) 
  D_new_db2[iter]      = rnorm(n=1,mean=D_new_db2[iter-1],sd=0.5) 
  D_new_db3[iter]      = rnorm(n=1,mean=D_new_db3[iter-1],sd=0.5) 
  
  lambda_new[iter] = rnorm(n=1,mean=lambda_new[iter-1],sd=1)
  
  for(day in 1:day.sim){
    
    stand_dev_db1<-2* D_new_db1[iter]*day*12
    stand_dev_db2<-2* D_new_db2[iter]*day*12
    stand_dev_db3<-2* D_new_db3[iter]*day*12
    
    cov.matrix_sim_db1<-matrix(c(stand_dev_db1,0,0,stand_dev_db1),ncol=2)
    cov.matrix_sim_db2<-matrix(c(stand_dev_db2,0,0,stand_dev_db2),ncol=2)
    cov.matrix_sim_db3<-matrix(c(stand_dev_db3,0,0,stand_dev_db3),ncol=2)
    
    for (g in 1:sim2){
        zanzare_db1[g,(day+1)]  <-  mort2(zanzare_db1[g,day],mu_1,1)
        zanzare_db2[g,(day+1)]  <-  mort2(zanzare_db2[g,day],mu_2,1)
        zanzare_db3[g,(day+1)]  <-  mort2(zanzare_db3[g,day],mu_3,1)
        
        pos_db1<- rmvnorm(n=zanzare_db1[g,day] ,mean= c(0,0),sigma=cov.matrix_sim_db1 , method = "eigen") 
        pos_db2<- rmvnorm(n=zanzare_db2[g,day] ,mean= c(0,0),sigma=cov.matrix_sim_db2 , method = "eigen") 
        pos_db3<- rmvnorm(n=zanzare_db3[g,day] ,mean= c(0,0),sigma=cov.matrix_sim_db3 , method = "eigen") 
        
        radius_db1<- apply(pos_db1,1,raggio)
        radius_db2<- apply(pos_db2,1,raggio)
        radius_db3<- apply(pos_db3,1,raggio)
        
        where_db1<- findInterval(radius_db1, seq(0,450,50))
        where_db2<- findInterval(radius_db2, seq(0,450,50))
        where_db3<- findInterval(radius_db3, seq(0,450,50))
        
        dummy_db1<- data.frame(anelli = factor(names(table(where_db1)),
                                               levels=c(1:10)),
                               valori = as.numeric(table(where_db1)))
        
        dummy_db2<- data.frame(anelli = factor(names(table(where_db2)),
                                               levels=c(1:10)),
                               valori = as.numeric(table(where_db2)))
        
        dummy_db3<- data.frame(anelli = factor(names(table(where_db3)),
                                               levels=c(1:10)),
                               valori = as.numeric(table(where_db3)))
        
        mosquitoes_db1[g,] <- tapply(dummy_db1$valori,dummy_db1$anelli,sum,na.rm=T)
        mosquitoes_db1[g,is.na(mosquitoes_db1[g,])]<-0   
        
        mosquitoes_db2[g,] <- tapply(dummy_db2$valori,dummy_db2$anelli,sum,na.rm=T)
        mosquitoes_db2[g,is.na(mosquitoes_db2[g,])]<-0
        
        mosquitoes_db3[g,] <- tapply(dummy_db3$valori,dummy_db3$anelli,sum,na.rm=T)
        mosquitoes_db3[g,is.na(mosquitoes_db3[g,])]<-0
      }
      mosquitoesavg_db1[day,] <- apply(mosquitoes_db1,2,mean)
      mosquitoesavg_db2[day,] <- apply(mosquitoes_db2,2,mean)
      mosquitoesavg_db3[day,] <- apply(mosquitoes_db3,2,mean)
      
      
    }
    
  h24 <- seq(2,10,by=2)
  
  for( YY in 1:5){
    mosquitoes24_db1[,YY] <- mosquitoesavg_db1[h24[YY],]  +  mosquitoesavg_db1[(h24[YY]-1),] 
  }
  
  for( YY in 1:5){
    mosquitoes24_db2[,YY] <- mosquitoesavg_db2[h24[YY],]  +  mosquitoesavg_db2[(h24[YY]-1),] 
  }
  
  for( YY in 1:5){
    mosquitoes24_db3[,YY] <- mosquitoesavg_db3[h24[YY],]  +  mosquitoesavg_db3[(h24[YY]-1),] 
  }
  
  likelihood1 <- 0 
  for(day in 1:5){
    for( s in 1:anelli){
      lmd_db1<- lambda_new[iter]*capture.anello_db1[s,day]*mosquitoes24_db1[s,day]
      likelihood1 <- likelihood1 + dpois(x=osservate_db1[s,day],lambda = lmd_db1,log=T)
    }
  }
  
  likelihood2 <- 0 
  for(day in 1:5){
    for( s in 1:anelli){
      lmd_db2<- lambda_new[iter]*capture.anello_db2[s,day]*mosquitoes24_db2[s,day]
      likelihood2<- likelihood2 + dpois(x=osservate_db2[s,day],lambda = lmd_db2,log=T)
    }
  }
  
  likelihood3 <- 0 
  for(day in 1:5){
    for( s in 1:anelli){
      lmd_db3<- lambda_new[iter]*capture.anello_db3[s,day]*mosquitoes24_db3[s,day]
      likelihood3<- likelihood3 + dpois(x=osservate_db3[s,day],lambda = lmd_db3,log=T)
    }
  }
  
a<-sum(likelihood1,likelihood2,likelihood3)  
  
# criterio accettazione likelihood ed eventuale aggionramento parametri
  if( exp(a) > exp(new_likelihood[iter-1]) & a!="-Inf"){
    new_likelihood[iter] =  a
    c = c+1
    k=0
  }
  else { 
    if(a!="-Inf"){
      b <- exp(a)/exp(new_likelihood[iter-1])
      if( runif(1,0,1) < b ){
        new_likelihood[iter] =  a
        c = c+1
        k=0
      } 
      else{
        new_likelihood[iter]<-new_likelihood[iter-1]
        D_new_db1[iter]         <-D_new_db1[iter-1]
        D_new_db2[iter]         <-D_new_db2[iter-1]
        D_new_db3[iter]         <-D_new_db3[iter-1]
        lambda_new[iter]    <-lambda_new[iter-1]
        k=k+1
      }
    }
    else{
      new_likelihood[iter]<-new_likelihood[iter-1]
      D_new_db1[iter]         <-D_new_db1[iter-1]
      D_new_db2[iter]         <-D_new_db2[iter-1]
      D_new_db3[iter]         <-D_new_db3[iter-1]
      lambda_new[iter]    <-    lambda_new[iter-1]
 k=k+1
      }
  }
if(k==100){
  k=0
  
  for(day in 1:day.sim){
    
    stand_dev_db1<-2* D_new_db1[iter]*day*12
    stand_dev_db2<-2* D_new_db2[iter]*day*12
    stand_dev_db3<-2* D_new_db3[iter]*day*12
    
    cov.matrix_sim_db1<-matrix(c(stand_dev_db1,0,0,stand_dev_db1),ncol=2)
    cov.matrix_sim_db2<-matrix(c(stand_dev_db2,0,0,stand_dev_db2),ncol=2)
    cov.matrix_sim_db3<-matrix(c(stand_dev_db3,0,0,stand_dev_db3),ncol=2)
    
    for (g in 1:sim2){
      zanzare_db1[g,(day+1)]  <-  mort2(zanzare_db1[g,day],mu_1,1)
      zanzare_db2[g,(day+1)]  <-  mort2(zanzare_db2[g,day],mu_2,1)
      zanzare_db3[g,(day+1)]  <-  mort2(zanzare_db3[g,day],mu_3,1)
      
      pos_db1<- rmvnorm(n=zanzare_db1[g,day] ,mean= c(0,0),sigma=cov.matrix_sim_db1 , method = "eigen") 
      pos_db2<- rmvnorm(n=zanzare_db2[g,day] ,mean= c(0,0),sigma=cov.matrix_sim_db2 , method = "eigen") 
      pos_db3<- rmvnorm(n=zanzare_db3[g,day] ,mean= c(0,0),sigma=cov.matrix_sim_db3 , method = "eigen") 
      
      radius_db1<- apply(pos_db1,1,raggio)
      radius_db2<- apply(pos_db2,1,raggio)
      radius_db3<- apply(pos_db3,1,raggio)
      
      where_db1<- findInterval(radius_db1, seq(0,450,50))
      where_db2<- findInterval(radius_db2, seq(0,450,50))
      where_db3<- findInterval(radius_db3, seq(0,450,50))
      
      dummy_db1<- data.frame(anelli = factor(names(table(where_db1)),
                                             levels=c(1:10)),
                             valori = as.numeric(table(where_db1)))
      
      dummy_db2<- data.frame(anelli = factor(names(table(where_db2)),
                                             levels=c(1:10)),
                             valori = as.numeric(table(where_db2)))
      
      dummy_db3<- data.frame(anelli = factor(names(table(where_db3)),
                                             levels=c(1:10)),
                             valori = as.numeric(table(where_db3)))
      
      mosquitoes_db1[g,] <- tapply(dummy_db1$valori,dummy_db1$anelli,sum,na.rm=T)
      mosquitoes_db1[g,is.na(mosquitoes_db1[g,])]<-0   
      
      mosquitoes_db2[g,] <- tapply(dummy_db2$valori,dummy_db2$anelli,sum,na.rm=T)
      mosquitoes_db2[g,is.na(mosquitoes_db2[g,])]<-0
      
      mosquitoes_db3[g,] <- tapply(dummy_db3$valori,dummy_db3$anelli,sum,na.rm=T)
      mosquitoes_db3[g,is.na(mosquitoes_db3[g,])]<-0
    }
    mosquitoesavg_db1[day,] <- apply(mosquitoes_db1,2,mean)
    mosquitoesavg_db2[day,] <- apply(mosquitoes_db2,2,mean)
    mosquitoesavg_db3[day,] <- apply(mosquitoes_db3,2,mean)
    
    
  }
  
  h24 <- seq(2,10,by=2)
  
  for( YY in 1:5){
    mosquitoes24_db1[,YY] <- mosquitoesavg_db1[h24[YY],]  +  mosquitoesavg_db1[(h24[YY]-1),] 
  }
  
  for( YY in 1:5){
    mosquitoes24_db2[,YY] <- mosquitoesavg_db2[h24[YY],]  +  mosquitoesavg_db2[(h24[YY]-1),] 
  }
  
  for( YY in 1:5){
    mosquitoes24_db3[,YY] <- mosquitoesavg_db3[h24[YY],]  +  mosquitoesavg_db3[(h24[YY]-1),] 
  }
  
  likelihood1 <- 0 
  for(day in 1:5){
    for( s in 1:anelli){
      lmd_db1<- lambda_new[iter]*capture.anello_db1[s,day]*mosquitoes24_db1[s,day]
      likelihood1 <- likelihood1 + dpois(x=osservate_db1[s,day],lambda = lmd_db1,log=T)
    }
  }
  
  likelihood2 <- 0 
  for(day in 1:5){
    for( s in 1:anelli){
      lmd_db2<- lambda_new[iter]*capture.anello_db2[s,day]*mosquitoes24_db2[s,day]
      likelihood2<- likelihood2 + dpois(x=osservate_db2[s,day],lambda = lmd_db2,log=T)
    }
  }
  
  likelihood3 <- 0 
  for(day in 1:5){
    for( s in 1:anelli){
      lmd_db3<- lambda_new[iter]*capture.anello_db3[s,day]*mosquitoes24_db3[s,day]
      likelihood3<- likelihood3 + dpois(x=osservate_db3[s,day],lambda = lmd_db3,log=T)
    }
  }
  
  a<-sum(likelihood1,likelihood2,likelihood3) 
          new_likelihood[iter]<-a
}
print(iter)
  
}

no_accettazione<-k/max_iter_starting #no acceptance rate  
accetazione<-c/max_iter_starting #acceptance rate 
plot(new_likelihood,type="l") #plot likelihood estimate with MCMC method

par(mfcol=c(2,2))
#plot of distriution of D1, D2,D3 and lambda obtained by MCMC methods
plot(D_new_db1,type="l",xlab="Runs",ylab="D1",main="sigma 0.5")
plot(D_new_db3,type="l",xlab="Runs",ylab="D3")
plot(D_new_db2,type="l",xlab="Runs",ylab="D2")
plot(lambda_new,type="l",xlab="Runs",ylab=TeX("correction factor $\\zeta$"))


#elimination of the first 5.000 iter of the diffusion coefficents and lambda 
#obtained by MCMC method
D_new_db1_1<-D_new_db1[-c(1:5000)] 
D_new_db2_2<-D_new_db2[-c(1:5000)]
D_new_db3_3<-D_new_db3[-c(1:5000)]
lambda_new_1<-lambda_new[-c(1:5000)]

#mean, sd, quantile D1
mean(D_new_db1_1) #75.30963
sd(D_new_db1_1) #  2.275145
quantile(D_new_db1_1,c(0.025,0.975))#71.50542/79.49015 

#mean, sd, quantile D2
mean(D_new_db2_2) #40.96145
sd(D_new_db2_2) #2.343075
quantile(D_new_db2_2,c(0.025,0.975))#38.01071/47.65964 

#mean, sd, quantile D3
mean(D_new_db3_3) #23.51797
sd(D_new_db3_3) #6.679057
quantile(D_new_db3_3,c(0.025,0.975)) #18.45061/45.96069

#mean,sd, e quantile lambda
mean(lambda_new_1) #81.35
sd(lambda_new_1) # 3.800717
quantile(lambda_new_1,c(0.025,0.975)) #72.47202/88.43816 

#new plot with the distribution of D1,D2,D3 and lambda in which the first 
#5,000 procedures were eliminated.
plot(D_new_db1_1,type="l",xlab="Runs",ylab="D1")
plot(D_new_db3_3,type="l",xlab="Runs",ylab="D3")
plot(D_new_db2_2,type="l",xlab="Runs",ylab="D2")
plot(lambda_new_1,type="l",xlab="Runs",ylab=TeX("correction factor $\\zeta$"))

#########################Simulation################
simcap_db1<- array(NA, dim=c(10,5,1000)) #vector for simulated analysis first release
simcap_db2<- array(NA, dim=c(10,5,1000)) #vector for simulated analysis second release
simcap_db3<- array(NA, dim=c(10,5,1000)) #vector for simulated analysis third release

YY<- sample(1:length(D_new_db1_1),1000,replace = T) #

#Parameters for simulated analysis first release
mosquitoes_db1<-array(NA, dim=c(sim2,anelli,length(YY)))#matrix for number of mosquitoes recapture in 10 days in the simulation 
mosquitoesavg_db1<-array(NA, dim=c(day.sim,anelli,length(YY))) #matrix for average mosquitoes
mosquitoes24_db1<- array(NA, dim=c(anelli,5,length(YY)))  #matrix for the number of mosquitoes recapture in 5 days

##Parameters for simulated analysis second release
mosquitoes_db2<-array(NA, dim=c(sim2,anelli,length(YY)))
mosquitoesavg_db2<-array(NA, dim=c(day.sim,anelli,length(YY))) 
mosquitoes24_db2<- array(NA, dim=c(anelli,5,length(YY))) 

#Parameters for simulated analysis first release

mosquitoes_db3<-array(NA, dim=c(sim2,anelli,length(YY)))
mosquitoesavg_db3<-array(NA, dim=c(day.sim,anelli,length(YY))) 
mosquitoes24_db3<- array(NA, dim=c(anelli,5,length(YY))) 


#Simulation 
for (iter in 1:length(YY)){
  
  for(day in 1:day.sim){
    
    stand_dev_db1<-2* (D_new_db1_1[YY[iter]])*day*12
    stand_dev_db2<-2* (D_new_db2_2[YY[iter]])*day*12
    stand_dev_db3<-2* (D_new_db3_3[YY[iter]])*day*12
    
    cov.matrix_sim_db1<-matrix(c(stand_dev_db1,0,0,stand_dev_db1),ncol=2)
    cov.matrix_sim_db2<-matrix(c(stand_dev_db2,0,0,stand_dev_db2),ncol=2)
    cov.matrix_sim_db3<-matrix(c(stand_dev_db3,0,0,stand_dev_db3),ncol=2)
    
    for( g in 1:sim2){
      zanzare_db1[g,(day+1)]  <-  mort2(zanzare_db1[g,day],mu_1,1)
      zanzare_db2[g,(day+1)]  <-  mort2(zanzare_db2[g,day],mu_2,1)
      zanzare_db3[g,(day+1)]  <-  mort2(zanzare_db3[g,day],mu_3,1)
      
      pos_db1   <- rmvnorm(n=zanzare_db1[g,day],mean= c(0,0),sigma=cov.matrix_sim_db1, method = "eigen") 
      pos_db2   <- rmvnorm(n=zanzare_db2[g,day],mean= c(0,0),sigma=cov.matrix_sim_db2, method = "eigen") 
      pos_db3   <- rmvnorm(n=zanzare_db3[g,day],mean= c(0,0),sigma=cov.matrix_sim_db3, method = "eigen") 
      
      radius_db1<- apply(pos_db1,1,raggio)
      radius_db2<- apply(pos_db2,1,raggio)
      radius_db3<- apply(pos_db3,1,raggio)
      
      where_db1<- findInterval(radius_db1, seq(0,500,50))
      where_db2<- findInterval(radius_db2, seq(0,500,50))
      where_db3<- findInterval(radius_db3, seq(0,500,50))
      
      
      dummy_db1<- data.frame(anelli = factor(names(table(where_db1)),
                                             levels=c(1:10)),
                             valori = as.numeric(table(where_db1)),
                             sim    = g)
      
      dummy_db2<- data.frame(anelli = factor(names(table(where_db2)),
                                             levels=c(1:10)),
                             valori = as.numeric(table(where_db2)),
                             sim    = g)
      
      dummy_db3<- data.frame(anelli = factor(names(table(where_db3)),
                                             levels=c(1:10)),
                             valori = as.numeric(table(where_db3)),
                             sim    = g)
      
      
      mosquitoes_db1[g,,iter] <- tapply(dummy_db1$valori,dummy_db1$anelli,sum,na.rm=T)
      mosquitoes_db1[g,is.na( mosquitoes_db1[g,,iter]),iter]<-0  
      
      mosquitoes_db2[g,,iter] <- tapply(dummy_db2$valori,dummy_db2$anelli,sum,na.rm=T)
      mosquitoes_db2[g,is.na(mosquitoes_db2[g,,iter]),iter]<-0 
      
      mosquitoes_db3[g,,iter] <- tapply(dummy_db3$valori,dummy_db3$anelli,sum,na.rm=T)
      mosquitoes_db3[g,is.na(mosquitoes_db3[g,,iter]),iter]<-0 
    }
    mosquitoesavg_db1[day,,iter] <- apply(mosquitoes_db1[,,iter],2,mean)
    mosquitoesavg_db2[day,,iter] <- apply(mosquitoes_db2[,,iter],2,mean)
    mosquitoesavg_db3[day,,iter] <- apply(mosquitoes_db3[,,iter],2,mean)
    
  } 
  
  h24 <- seq(2,10,by=2)
  for( cc in 1:5){
    mosquitoes24_db1[,cc,] <- mosquitoesavg_db1[h24[cc],,]  +  mosquitoesavg_db1[(h24[cc]-1),,] 
  }
  
  for( cc in 1:5){
    mosquitoes24_db2[,cc,] <- mosquitoesavg_db2[h24[cc],,]  +  mosquitoesavg_db2[(h24[cc]-1),,] 
  }
  
  for( cc in 1:5){
    mosquitoes24_db3[,cc,] <- mosquitoesavg_db3[h24[cc],,]  +  mosquitoesavg_db3[(h24[cc]-1),,] 
  }
  for(day in 1:5){
    for( s in 1:anelli){
      lmb_db1= capture.anello_db1[s,day]*mosquitoes24_db1[s,day,iter]*lambda_new_1[YY[iter]]
      simcap_db1[s,day,iter] <- rpois(1,lambda = lmb_db1)
      likelihood_1<-dpois(x=osservate_db1[s,day],lambda=mean(lmb_db1))
    }
  } 
  for(day in 1:5){
    for( s in 1:anelli){
      lmb_db2= capture.anello_db2[s,day]*mosquitoes24_db2[s,day,iter]*lambda_new_1[YY[iter]]
      simcap_db2[s,day,iter] <- rpois(1,lambda = lmb_db2)
      likelihood_2<-dpois(x=osservate_db2[s,day],lambda=mean(lmb_db2),log=T)
     }
  }

  for(day in 1:5){
    for( s in 1:anelli){
      lmb_db3= capture.anello_db3[s,day]*mosquitoes24_db3[s,day,iter]*lambda_new_1[YY[iter]]
      simcap_db3[s,day,iter] <- rpois(1,lambda = lmb_db3)
      likelihood_3<-dpois(x=osservate_db3[s,day],lambda=mean(lmb_db3),log=T)
      
    }
  }
likelihood_D<-sum(likelihood_1,likelihood_2,likelihood_3)
print(iter)
  }

#Initiling of anullus  
anelli_in<-seq(0,450,by=50)
anelli_out<-seq(50,500,by=50)

#########################analitic solution################
analitic_solution_MRR1<-matrix(NA,ncol=anelli,nrow=day) #vector for analitic solution first release
analitic_solution_MRR2<-matrix(NA,ncol=anelli,nrow=day) #vector for analitic solution second release
analitic_solution_MRR3<-matrix(NA,ncol=anelli,nrow=day) #vector for analitic solution third release

#calculation of the exact solution 
for(i in 1 :day){
  K1<-M1*exp(-mu_1*i)
  K2<-M2*exp(-mu_2*i)
  K3<-M3*exp(-mu_3*i)
  
  s1<-2*mean(D_new_db1_1)*i*24
  s2<-2*mean(D_new_db2_2)*i*24
  s3<-2*mean(D_new_db3_3)*i*24
  
  mosquitoess1<- function(r) (r*K1/s1)*(exp(-(r^2)/(2*s1)))
  mosquitoess2<- function(r) (r*K2/s2)*(exp(-(r^2)/(2*s2)))
  mosquitoess3<- function(r) (r*K3/s3)*(exp(-(r^2)/(2*s3)))
  
for(j in 1:anelli){
  analitic_solution_MRR1[i,j]<-integral(mosquitoess1,xmin=anelli_in[j],
                                        xmax=anelli_out[j],method="Simpson")
                   } 

for(j in 1:anelli){
  analitic_solution_MRR2[i,j]<-integral(mosquitoess2,xmin=anelli_in[j],
                                        xmax=anelli_out[j],method="Simpson")
               }

for(j in 1:anelli){
  analitic_solution_MRR3[i,j]<-integral(mosquitoess3,xmin=anelli_in[j],
                                        xmax=anelli_out[j],method="Simpson")
                 }

}

#round of the exact solution
analitic_solution_MRR1<-round(analitic_solution_MRR1)
analitic_solution_MRR2<-round(analitic_solution_MRR2)
analitic_solution_MRR3<-round(analitic_solution_MRR3)
#condiering the traspost of matrix  day=colums, anulli=row 
analitic_solution_MRR1<-t(analitic_solution_MRR1)
analitic_solution_MRR2<-t(analitic_solution_MRR2)
analitic_solution_MRR3<-t(analitic_solution_MRR3)

#quantile calculation of the simulation of the first release(2.5%,25%,75%,95%)
XX_db1_mosquitoes<-apply(mosquitoesavg_db1[seq(2,10,by=2),,],c(1,2),quantile,c(0.975))   
YY_db1_mosquitoes<-apply(mosquitoesavg_db1[seq(2,10,by=2),,],c(1,2),quantile,c(0.025))
ZZ_db1_mosquitoes<-apply(mosquitoesavg_db1[seq(2,10,by=2),,],c(1,2),quantile,c(0.25))
TT_db1_mosquitoes<-apply(mosquitoesavg_db1[seq(2,10,by=2),,],c(1,2),quantile,c(0.75))

#turn every quantile into a matrix and do the transposed first release 
XX_db1_mosquitoes<-as.matrix(XX_db1_mosquitoes)
XX_db1_mosquitoes<-t(XX_db1_mosquitoes)
YY_db1_mosquitoes<-as.matrix(YY_db1_mosquitoes)
YY_db1_mosquitoes<-t(YY_db1_mosquitoes)
ZZ_db1_mosquitoes<-as.matrix(ZZ_db1_mosquitoes)
ZZ_db1_mosquitoes<-t(ZZ_db1_mosquitoes)
TT_db1_mosquitoes<-as.matrix(TT_db1_mosquitoes)
TT_db1_mosquitoes<-t(TT_db1_mosquitoes)


XX_db2_mosquitoes<-apply(mosquitoesavg_db2[seq(2,10,by=2),,],c(1,2),quantile,c(0.975))   
YY_db2_mosquitoes<-apply(mosquitoesavg_db2[seq(2,10,by=2),,],c(1,2),quantile,c(0.025))
ZZ_db2_mosquitoes<-apply(mosquitoesavg_db2[seq(2,10,by=2),,],c(1,2),quantile,c(0.25))
TT_db2_mosquitoes<-apply(mosquitoesavg_db2[seq(2,10,by=2),,],c(1,2),quantile,c(0.75))

XX_db2_mosquitoes<-as.matrix(XX_db2_mosquitoes)
XX_db2_mosquitoes<-t(XX_db2_mosquitoes)
YY_db2_mosquitoes<-as.matrix(YY_db2_mosquitoes)
YY_db2_mosquitoes<-t(YY_db2_mosquitoes)
ZZ_db2_mosquitoes<-as.matrix(ZZ_db2_mosquitoes)
ZZ_db2_mosquitoes<-t(ZZ_db2_mosquitoes)
TT_db2_mosquitoes<-as.matrix(TT_db2_mosquitoes)
TT_db2_mosquitoes<-t(TT_db2_mosquitoes)


XX_db3_mosquitoes<-apply(mosquitoesavg_db3[seq(2,10,by=2),,],c(1,2),quantile,c(0.975))   
YY_db3_mosquitoes<-apply(mosquitoesavg_db3[seq(2,10,by=2),,],c(1,2),quantile,c(0.025))
ZZ_db3_mosquitoes<-apply(mosquitoesavg_db3[seq(2,10,by=2),,],c(1,2),quantile,c(0.25))
TT_db3_mosquitoes<-apply(mosquitoesavg_db3[seq(2,10,by=2),,],c(1,2),quantile,c(0.75))

XX_db3_mosquitoes<-as.matrix(XX_db3_mosquitoes)
XX_db3_mosquitoes<-t(XX_db3_mosquitoes)
YY_db3_mosquitoes<-as.matrix(YY_db3_mosquitoes)
YY_db3_mosquitoes<-t(YY_db3_mosquitoes)
ZZ_db3_mosquitoes<-as.matrix(ZZ_db3_mosquitoes)
ZZ_db3_mosquitoes<-t(ZZ_db3_mosquitoes)
TT_db3_mosquitoes<-as.matrix(TT_db3_mosquitoes)
TT_db3_mosquitoes<-t(TT_db3_mosquitoes)

#calculate the average number of exact values within 95% of the simulated values
n1<-n2<-n3<-matrix(NA,nrow=10,ncol=5) #initiling a matrix of each esperiments
for(j in 1:10){
    for(i in 1:5){ 
      if( analitic_solution_MRR1[j,i]<=XX_db1_mosquitoes[j,i] & analitic_solution_MRR1[j,i]>=YY_db1_mosquitoes[j,i]){
        n1[j,i]<-1 
        }
        else {n1[j,i]<-0}
     
       if( analitic_solution_MRR2[j,i]<=XX_db2_mosquitoes[j,i] & analitic_solution_MRR2[j,i]>=YY_db2_mosquitoes[j,i]){
      n2[j,i]<-1 
    }
    else {n2[j,i]<-0}
     
       if( analitic_solution_MRR3[j,i]<=XX_db3_mosquitoes[j,i] & analitic_solution_MRR3[j,i]>=YY_db3_mosquitoes[j,i])
        {
        n3[j,i]<-1 
      }
      else {n3[j,i]<-0}
  }
      }
    
#calcule of mean
mean(n1==1)*100
mean(n2==1)*100
mean(n3==1)*100
#data frame plot analitic solution vs simulated solution
analitic_vs_simulated<-data.frame(anulli=rep(anelli_out,by=5),
                                  anulli_1=rep(c(1:10),by=5),
                                  day=rep(c(1:5),each=10),
                                  MRR=as.factor(rep(c(1:3),each=50)),
                                  primo_quantile=c(YY_db1_mosquitoes[1,1],YY_db1_mosquitoes[2,1],YY_db1_mosquitoes[3,1],YY_db1_mosquitoes[4,1],YY_db1_mosquitoes[5,1],
                                                       YY_db1_mosquitoes[6,1],YY_db1_mosquitoes[7,1],YY_db1_mosquitoes[8,1],YY_db1_mosquitoes[9,1],YY_db1_mosquitoes[10,1],
                                                       YY_db1_mosquitoes[1,2],YY_db1_mosquitoes[2,2],YY_db1_mosquitoes[3,2],YY_db1_mosquitoes[4,2],YY_db1_mosquitoes[5,2],
                                                       YY_db1_mosquitoes[6,2],YY_db1_mosquitoes[7,2],YY_db1_mosquitoes[8,2],YY_db1_mosquitoes[9,2],YY_db1_mosquitoes[10,2],
                                                       YY_db1_mosquitoes[1,3],YY_db1_mosquitoes[2,3],YY_db1_mosquitoes[3,3],YY_db1_mosquitoes[4,3],YY_db1_mosquitoes[5,3],
                                                       YY_db1_mosquitoes[6,3],YY_db1_mosquitoes[7,3],YY_db1_mosquitoes[8,3],YY_db1_mosquitoes[9,3],YY_db1_mosquitoes[10,3],
                                                       YY_db1_mosquitoes[1,4],YY_db1_mosquitoes[2,4],YY_db1_mosquitoes[3,4],YY_db1_mosquitoes[4,4],YY_db1_mosquitoes[5,4],
                                                       YY_db1_mosquitoes[6,4],YY_db1_mosquitoes[7,4],YY_db1_mosquitoes[8,4],YY_db1_mosquitoes[9,4],YY_db1_mosquitoes[10,4],
                                                       YY_db1_mosquitoes[1,5],YY_db1_mosquitoes[2,5],YY_db1_mosquitoes[3,5],YY_db1_mosquitoes[4,5],YY_db1_mosquitoes[5,5],
                                                       YY_db1_mosquitoes[6,5],YY_db1_mosquitoes[7,5],YY_db1_mosquitoes[8,5],YY_db1_mosquitoes[9,5],YY_db1_mosquitoes[10,5],
                                                       
                                                       YY_db2_mosquitoes[1,1],YY_db2_mosquitoes[2,1],YY_db2_mosquitoes[3,1],YY_db2_mosquitoes[4,1],YY_db2_mosquitoes[5,1],
                                                       YY_db2_mosquitoes[6,1],YY_db2_mosquitoes[7,1],YY_db2_mosquitoes[8,1],YY_db2_mosquitoes[9,1],YY_db2_mosquitoes[10,1],
                                                       YY_db2_mosquitoes[1,2],YY_db2_mosquitoes[2,2],YY_db2_mosquitoes[3,2],YY_db2_mosquitoes[4,2],YY_db2_mosquitoes[5,2],
                                                       YY_db2_mosquitoes[6,2],YY_db2_mosquitoes[7,2],YY_db2_mosquitoes[8,2],YY_db2_mosquitoes[9,2],YY_db2_mosquitoes[10,2],
                                                       YY_db2_mosquitoes[1,3],YY_db2_mosquitoes[2,3],YY_db2_mosquitoes[3,3],YY_db2_mosquitoes[4,3],YY_db2_mosquitoes[5,3],
                                                       YY_db2_mosquitoes[6,3],YY_db2_mosquitoes[7,3],YY_db2_mosquitoes[8,3],YY_db2_mosquitoes[9,3],YY_db2_mosquitoes[10,3],
                                                       YY_db2_mosquitoes[1,4],YY_db2_mosquitoes[2,4],YY_db2_mosquitoes[3,4],YY_db2_mosquitoes[4,4],YY_db2_mosquitoes[5,4],
                                                       YY_db2_mosquitoes[6,4],YY_db2_mosquitoes[7,4],YY_db2_mosquitoes[8,4],YY_db2_mosquitoes[9,4],YY_db2_mosquitoes[10,4],
                                                       YY_db2_mosquitoes[1,5],YY_db2_mosquitoes[2,5],YY_db2_mosquitoes[3,5],YY_db2_mosquitoes[4,5],YY_db2_mosquitoes[5,5],
                                                       YY_db2_mosquitoes[6,5],YY_db2_mosquitoes[7,5],YY_db2_mosquitoes[8,5],YY_db2_mosquitoes[9,5],YY_db2_mosquitoes[10,5],
                                                       
                                                       YY_db3_mosquitoes[1,1],YY_db3_mosquitoes[2,1],YY_db3_mosquitoes[3,1],YY_db3_mosquitoes[4,1],YY_db3_mosquitoes[5,1],
                                                       YY_db3_mosquitoes[6,1],YY_db3_mosquitoes[7,1],YY_db3_mosquitoes[8,1],YY_db3_mosquitoes[9,1],YY_db3_mosquitoes[10,1],
                                                       YY_db3_mosquitoes[1,2],YY_db3_mosquitoes[2,2],YY_db3_mosquitoes[3,2],YY_db3_mosquitoes[4,2],YY_db3_mosquitoes[5,2],
                                                       YY_db3_mosquitoes[6,2],YY_db3_mosquitoes[7,2],YY_db3_mosquitoes[8,2],YY_db3_mosquitoes[9,2],YY_db3_mosquitoes[10,2],
                                                       YY_db3_mosquitoes[1,3],YY_db3_mosquitoes[2,3],YY_db3_mosquitoes[3,3],YY_db3_mosquitoes[4,3],YY_db3_mosquitoes[5,3],
                                                       YY_db3_mosquitoes[6,3],YY_db3_mosquitoes[7,3],YY_db3_mosquitoes[8,3],YY_db3_mosquitoes[9,3],YY_db3_mosquitoes[10,3],
                                                       YY_db3_mosquitoes[1,4],YY_db3_mosquitoes[2,4],YY_db3_mosquitoes[3,4],YY_db3_mosquitoes[4,4],YY_db3_mosquitoes[5,4],
                                                       YY_db3_mosquitoes[6,4],YY_db3_mosquitoes[7,4],YY_db3_mosquitoes[8,4],YY_db3_mosquitoes[9,4],YY_db3_mosquitoes[10,4],
                                                       YY_db3_mosquitoes[1,5],YY_db3_mosquitoes[2,5],YY_db3_mosquitoes[3,5],YY_db3_mosquitoes[4,5],YY_db3_mosquitoes[5,5],
                                                       YY_db3_mosquitoes[6,5],YY_db3_mosquitoes[7,5],YY_db3_mosquitoes[8,5],YY_db3_mosquitoes[9,5],YY_db3_mosquitoes[10,5]),
                                  
                                  terzo_quantile=c(XX_db1_mosquitoes[1,1],XX_db1_mosquitoes[2,1],XX_db1_mosquitoes[3,1],XX_db1_mosquitoes[4,1],XX_db1_mosquitoes[5,1],
                                                       XX_db1_mosquitoes[6,1],XX_db1_mosquitoes[7,1],XX_db1_mosquitoes[8,1],XX_db1_mosquitoes[9,1],XX_db1_mosquitoes[10,1],
                                                       XX_db1_mosquitoes[1,2],XX_db1_mosquitoes[2,2],XX_db1_mosquitoes[3,2],XX_db1_mosquitoes[4,2],XX_db1_mosquitoes[5,2],
                                                       XX_db1_mosquitoes[6,2],XX_db1_mosquitoes[7,2],XX_db1_mosquitoes[8,2],XX_db1_mosquitoes[9,2],XX_db1_mosquitoes[10,2],
                                                       XX_db1_mosquitoes[1,3],XX_db1_mosquitoes[2,3],XX_db1_mosquitoes[3,3],XX_db1_mosquitoes[4,3],XX_db1_mosquitoes[5,3],
                                                       XX_db1_mosquitoes[6,3],XX_db1_mosquitoes[7,3],XX_db1_mosquitoes[8,3],XX_db1_mosquitoes[9,3],XX_db1_mosquitoes[10,3],
                                                       XX_db1_mosquitoes[1,4],XX_db1_mosquitoes[2,4],XX_db1_mosquitoes[3,4],XX_db1_mosquitoes[4,4],XX_db1_mosquitoes[5,4],
                                                       XX_db1_mosquitoes[6,4],XX_db1_mosquitoes[7,4],XX_db1_mosquitoes[8,4],XX_db1_mosquitoes[9,4],XX_db1_mosquitoes[10,4],
                                                       XX_db1_mosquitoes[1,5],XX_db1_mosquitoes[2,5],XX_db1_mosquitoes[3,5],XX_db1_mosquitoes[4,5],XX_db1_mosquitoes[5,5],
                                                       XX_db1_mosquitoes[6,5],XX_db1_mosquitoes[7,5],XX_db1_mosquitoes[8,5],XX_db1_mosquitoes[9,5],XX_db1_mosquitoes[10,5],
                                                       
                                                       XX_db2_mosquitoes[1,1],XX_db2_mosquitoes[2,1],XX_db2_mosquitoes[3,1],XX_db2_mosquitoes[4,1],XX_db2_mosquitoes[5,1],
                                                       XX_db2_mosquitoes[6,1],XX_db2_mosquitoes[7,1],XX_db2_mosquitoes[8,1],XX_db2_mosquitoes[9,1],XX_db2_mosquitoes[10,1],
                                                       XX_db2_mosquitoes[1,2],XX_db2_mosquitoes[2,2],XX_db2_mosquitoes[3,2],XX_db2_mosquitoes[4,2],XX_db2_mosquitoes[5,2],
                                                       XX_db2_mosquitoes[6,2],XX_db2_mosquitoes[7,2],XX_db2_mosquitoes[8,2],XX_db2_mosquitoes[9,2],XX_db2_mosquitoes[10,2],
                                                       XX_db2_mosquitoes[1,3],XX_db2_mosquitoes[2,3],XX_db2_mosquitoes[3,3],XX_db2_mosquitoes[4,3],XX_db2_mosquitoes[5,3],
                                                       XX_db2_mosquitoes[6,3],XX_db2_mosquitoes[7,3],XX_db2_mosquitoes[8,3],XX_db2_mosquitoes[9,3],XX_db2_mosquitoes[10,3],
                                                       XX_db2_mosquitoes[1,4],XX_db2_mosquitoes[2,4],XX_db2_mosquitoes[3,4],XX_db2_mosquitoes[4,4],XX_db2_mosquitoes[5,3],
                                                       XX_db2_mosquitoes[6,4],XX_db2_mosquitoes[7,4],XX_db2_mosquitoes[8,4],XX_db2_mosquitoes[9,4],XX_db2_mosquitoes[10,4],
                                                       XX_db2_mosquitoes[1,5],XX_db2_mosquitoes[2,5],XX_db2_mosquitoes[3,5],XX_db2_mosquitoes[4,5],XX_db2_mosquitoes[5,5],
                                                       XX_db2_mosquitoes[6,5],XX_db2_mosquitoes[7,5],XX_db2_mosquitoes[8,5],XX_db2_mosquitoes[9,5],XX_db2_mosquitoes[10,5],
                                                       
                                                       XX_db3_mosquitoes[1,1],XX_db3_mosquitoes[2,1],XX_db3_mosquitoes[3,1],XX_db3_mosquitoes[4,1],XX_db3_mosquitoes[5,1],
                                                       XX_db3_mosquitoes[6,1],XX_db3_mosquitoes[7,1],XX_db3_mosquitoes[8,1],XX_db3_mosquitoes[9,1],XX_db3_mosquitoes[10,1],
                                                       XX_db3_mosquitoes[1,2],XX_db3_mosquitoes[2,2],XX_db3_mosquitoes[3,2],XX_db3_mosquitoes[4,2],XX_db3_mosquitoes[5,2],
                                                       XX_db3_mosquitoes[6,2],XX_db3_mosquitoes[7,2],XX_db3_mosquitoes[8,2],XX_db3_mosquitoes[9,2],XX_db3_mosquitoes[10,2],
                                                       XX_db3_mosquitoes[1,3],XX_db3_mosquitoes[2,3],XX_db3_mosquitoes[3,3],XX_db3_mosquitoes[4,3],XX_db3_mosquitoes[5,3],
                                                       XX_db3_mosquitoes[6,3],XX_db3_mosquitoes[7,3],XX_db3_mosquitoes[8,3],XX_db3_mosquitoes[9,3],XX_db3_mosquitoes[10,3],
                                                       XX_db3_mosquitoes[1,4],XX_db3_mosquitoes[2,4],XX_db3_mosquitoes[3,4],XX_db3_mosquitoes[4,4],XX_db3_mosquitoes[5,4],
                                                       XX_db3_mosquitoes[6,4],XX_db3_mosquitoes[7,4],XX_db3_mosquitoes[8,4],XX_db3_mosquitoes[9,4],XX_db3_mosquitoes[10,4],
                                                       XX_db3_mosquitoes[1,5],XX_db3_mosquitoes[2,5],XX_db3_mosquitoes[3,5],XX_db3_mosquitoes[4,5],XX_db3_mosquitoes[5,5],
                                                       XX_db3_mosquitoes[6,5],XX_db3_mosquitoes[7,5],XX_db3_mosquitoes[8,5],XX_db3_mosquitoes[9,5],XX_db3_mosquitoes[10,5]),
                                  
                                  venticinque=c(ZZ_db1_mosquitoes[1,1],ZZ_db1_mosquitoes[2,1],ZZ_db1_mosquitoes[3,1],ZZ_db1_mosquitoes[4,1],ZZ_db1_mosquitoes[5,1],
                                                    ZZ_db1_mosquitoes[6,1],ZZ_db1_mosquitoes[7,1],ZZ_db1_mosquitoes[8,1],ZZ_db1_mosquitoes[9,1],ZZ_db1_mosquitoes[10,1],
                                                    ZZ_db1_mosquitoes[1,2],ZZ_db1_mosquitoes[2,2],ZZ_db1_mosquitoes[3,2],ZZ_db1_mosquitoes[4,2],ZZ_db1_mosquitoes[5,2],
                                                    ZZ_db1_mosquitoes[6,2],ZZ_db1_mosquitoes[7,2],ZZ_db1_mosquitoes[8,2],ZZ_db1_mosquitoes[9,2],ZZ_db1_mosquitoes[10,2],
                                                    ZZ_db1_mosquitoes[1,3],ZZ_db1_mosquitoes[2,3],ZZ_db1_mosquitoes[3,3],ZZ_db1_mosquitoes[4,3],ZZ_db1_mosquitoes[5,3],
                                                    ZZ_db1_mosquitoes[6,3],ZZ_db1_mosquitoes[7,3],ZZ_db1_mosquitoes[8,3],ZZ_db1_mosquitoes[9,3],ZZ_db1_mosquitoes[10,3],
                                                    ZZ_db1_mosquitoes[1,4],ZZ_db1_mosquitoes[2,4],ZZ_db1_mosquitoes[3,4],ZZ_db1_mosquitoes[4,4],ZZ_db1_mosquitoes[5,4],
                                                    ZZ_db1_mosquitoes[6,4],ZZ_db1_mosquitoes[7,4],ZZ_db1_mosquitoes[8,4],ZZ_db1_mosquitoes[9,4],ZZ_db1_mosquitoes[10,4],
                                                    ZZ_db1_mosquitoes[1,5],ZZ_db1_mosquitoes[2,5],ZZ_db1_mosquitoes[3,5],ZZ_db1_mosquitoes[4,5],ZZ_db1_mosquitoes[5,5],
                                                    ZZ_db1_mosquitoes[6,5],ZZ_db1_mosquitoes[7,5],ZZ_db1_mosquitoes[8,5],ZZ_db1_mosquitoes[9,5],ZZ_db1_mosquitoes[10,5],
                                                    
                                                    ZZ_db2_mosquitoes[1,1],ZZ_db2_mosquitoes[2,1],ZZ_db2_mosquitoes[3,1],ZZ_db2_mosquitoes[4,1],ZZ_db2_mosquitoes[5,1],
                                                    ZZ_db2_mosquitoes[6,1],ZZ_db2_mosquitoes[7,1],ZZ_db2_mosquitoes[8,1],ZZ_db2_mosquitoes[9,1],ZZ_db2_mosquitoes[10,1],
                                                    ZZ_db2_mosquitoes[1,2],ZZ_db2_mosquitoes[2,2],ZZ_db2_mosquitoes[3,2],ZZ_db2_mosquitoes[4,2],ZZ_db2_mosquitoes[5,2],
                                                    ZZ_db2_mosquitoes[6,2],ZZ_db2_mosquitoes[7,2],ZZ_db2_mosquitoes[8,2],ZZ_db2_mosquitoes[9,2],ZZ_db2_mosquitoes[10,2],
                                                    ZZ_db2_mosquitoes[1,3],ZZ_db2_mosquitoes[2,3],ZZ_db2_mosquitoes[3,3],ZZ_db2_mosquitoes[4,3],ZZ_db2_mosquitoes[5,3],
                                                    ZZ_db2_mosquitoes[6,3],ZZ_db2_mosquitoes[7,3],ZZ_db2_mosquitoes[8,3],ZZ_db2_mosquitoes[9,3],ZZ_db2_mosquitoes[10,3],
                                                    ZZ_db2_mosquitoes[1,4],ZZ_db2_mosquitoes[2,4],ZZ_db2_mosquitoes[3,4],ZZ_db2_mosquitoes[4,4],ZZ_db2_mosquitoes[5,3],
                                                    ZZ_db2_mosquitoes[6,4],ZZ_db2_mosquitoes[7,4],ZZ_db2_mosquitoes[8,4],ZZ_db2_mosquitoes[9,4],ZZ_db2_mosquitoes[10,4],
                                                    ZZ_db2_mosquitoes[1,5],ZZ_db2_mosquitoes[2,5],ZZ_db2_mosquitoes[3,5],ZZ_db2_mosquitoes[4,5],ZZ_db2_mosquitoes[5,5],
                                                    ZZ_db2_mosquitoes[6,5],ZZ_db2_mosquitoes[7,5],ZZ_db2_mosquitoes[8,5],ZZ_db2_mosquitoes[9,5],ZZ_db2_mosquitoes[10,5],
                                                    
                                                    ZZ_db3_mosquitoes[1,1],ZZ_db3_mosquitoes[2,1],ZZ_db3_mosquitoes[3,1],ZZ_db3_mosquitoes[4,1],ZZ_db3_mosquitoes[5,1],
                                                    ZZ_db3_mosquitoes[6,1],ZZ_db3_mosquitoes[7,1],ZZ_db3_mosquitoes[8,1],ZZ_db3_mosquitoes[9,1],ZZ_db3_mosquitoes[10,1],
                                                    ZZ_db3_mosquitoes[1,2],ZZ_db3_mosquitoes[2,2],ZZ_db3_mosquitoes[3,2],ZZ_db3_mosquitoes[4,2],ZZ_db3_mosquitoes[5,2],
                                                    ZZ_db3_mosquitoes[6,2],ZZ_db3_mosquitoes[7,2],ZZ_db3_mosquitoes[8,2],ZZ_db3_mosquitoes[9,2],ZZ_db3_mosquitoes[10,2],
                                                    ZZ_db3_mosquitoes[1,3],ZZ_db3_mosquitoes[2,3],ZZ_db3_mosquitoes[3,3],ZZ_db3_mosquitoes[4,3],ZZ_db3_mosquitoes[5,3],
                                                    ZZ_db3_mosquitoes[6,3],ZZ_db3_mosquitoes[7,3],ZZ_db3_mosquitoes[8,3],ZZ_db3_mosquitoes[9,3],ZZ_db3_mosquitoes[10,3],
                                                    ZZ_db3_mosquitoes[1,4],ZZ_db3_mosquitoes[2,4],ZZ_db3_mosquitoes[3,4],ZZ_db3_mosquitoes[4,4],ZZ_db3_mosquitoes[5,4],
                                                    ZZ_db3_mosquitoes[6,4],ZZ_db3_mosquitoes[7,4],ZZ_db3_mosquitoes[8,4],ZZ_db3_mosquitoes[9,4],ZZ_db3_mosquitoes[10,4],
                                                    ZZ_db3_mosquitoes[1,5],ZZ_db3_mosquitoes[2,5],ZZ_db3_mosquitoes[3,5],ZZ_db3_mosquitoes[4,5],ZZ_db3_mosquitoes[5,5],
                                                    ZZ_db3_mosquitoes[6,5],ZZ_db3_mosquitoes[7,5],ZZ_db3_mosquitoes[8,5],ZZ_db3_mosquitoes[9,5],ZZ_db3_mosquitoes[10,5]),
                                  
                                  settantacinque=c(TT_db1_mosquitoes[1,1],TT_db1_mosquitoes[2,1],TT_db1_mosquitoes[3,1],TT_db1_mosquitoes[4,1],TT_db1_mosquitoes[5,1],
                                                       TT_db1_mosquitoes[6,1],TT_db1_mosquitoes[7,1],TT_db1_mosquitoes[8,1],TT_db1_mosquitoes[9,1],TT_db1_mosquitoes[10,1],
                                                       TT_db1_mosquitoes[1,2],TT_db1_mosquitoes[2,2],TT_db1_mosquitoes[3,2],TT_db1_mosquitoes[4,2],TT_db1_mosquitoes[5,2],
                                                       TT_db1_mosquitoes[6,2],TT_db1_mosquitoes[7,2],TT_db1_mosquitoes[8,2],TT_db1_mosquitoes[9,2],TT_db1_mosquitoes[10,2],
                                                       TT_db1_mosquitoes[1,3],TT_db1_mosquitoes[2,3],TT_db1_mosquitoes[3,3],TT_db1_mosquitoes[4,3],TT_db1_mosquitoes[5,3],
                                                       TT_db1_mosquitoes[6,3],TT_db1_mosquitoes[7,3],TT_db1_mosquitoes[8,3],TT_db1_mosquitoes[9,3],TT_db1_mosquitoes[10,3],
                                                       TT_db1_mosquitoes[1,4],TT_db1_mosquitoes[2,4],TT_db1_mosquitoes[3,4],TT_db1_mosquitoes[4,4],TT_db1_mosquitoes[5,4],
                                                       TT_db1_mosquitoes[6,4],TT_db1_mosquitoes[7,4],TT_db1_mosquitoes[8,4],TT_db1_mosquitoes[9,4],TT_db1_mosquitoes[10,4],
                                                       TT_db1_mosquitoes[1,5],TT_db1_mosquitoes[2,5],TT_db1_mosquitoes[3,5],TT_db1_mosquitoes[4,5],TT_db1_mosquitoes[5,5],
                                                       TT_db1_mosquitoes[6,5],TT_db1_mosquitoes[7,5],TT_db1_mosquitoes[8,5],TT_db1_mosquitoes[9,5],TT_db1_mosquitoes[10,5],
                                                       
                                                       TT_db2_mosquitoes[1,1],TT_db2_mosquitoes[2,1],TT_db2_mosquitoes[3,1],TT_db2_mosquitoes[4,1],TT_db2_mosquitoes[5,1],
                                                       TT_db2_mosquitoes[6,1],TT_db2_mosquitoes[7,1],TT_db2_mosquitoes[8,1],TT_db2_mosquitoes[9,1],TT_db2_mosquitoes[10,1],
                                                       TT_db2_mosquitoes[1,2],TT_db2_mosquitoes[2,2],TT_db2_mosquitoes[3,2],TT_db2_mosquitoes[4,2],TT_db2_mosquitoes[5,2],
                                                       TT_db2_mosquitoes[6,2],TT_db2_mosquitoes[7,2],TT_db2_mosquitoes[8,2],TT_db2_mosquitoes[9,2],TT_db2_mosquitoes[10,2],
                                                       TT_db2_mosquitoes[1,3],TT_db2_mosquitoes[2,3],TT_db2_mosquitoes[3,3],TT_db2_mosquitoes[4,3],TT_db2_mosquitoes[5,3],
                                                       TT_db2_mosquitoes[6,3],TT_db2_mosquitoes[7,3],TT_db2_mosquitoes[8,3],TT_db2_mosquitoes[9,3],TT_db2_mosquitoes[10,3],
                                                       TT_db2_mosquitoes[1,4],TT_db2_mosquitoes[2,4],TT_db2_mosquitoes[3,4],TT_db2_mosquitoes[4,4],TT_db2_mosquitoes[5,4],
                                                       TT_db2_mosquitoes[6,4],TT_db2_mosquitoes[7,4],TT_db2_mosquitoes[8,4],TT_db2_mosquitoes[9,4],TT_db2_mosquitoes[10,4],
                                                       TT_db2_mosquitoes[1,5],TT_db2_mosquitoes[2,5],TT_db2_mosquitoes[3,5],TT_db2_mosquitoes[4,5],TT_db2_mosquitoes[5,5],
                                                       TT_db2_mosquitoes[6,5],TT_db2_mosquitoes[7,5],TT_db2_mosquitoes[8,5],TT_db2_mosquitoes[9,5],TT_db2_mosquitoes[10,5],
                                                       
                                                       TT_db3_mosquitoes[1,1],TT_db3_mosquitoes[2,1],TT_db3_mosquitoes[3,1],TT_db3_mosquitoes[4,1],TT_db3_mosquitoes[5,1],
                                                       TT_db3_mosquitoes[6,1],TT_db3_mosquitoes[7,1],TT_db3_mosquitoes[8,1],TT_db3_mosquitoes[9,1],TT_db3_mosquitoes[10,1],
                                                       TT_db3_mosquitoes[1,2],TT_db3_mosquitoes[2,2],TT_db3_mosquitoes[3,2],TT_db3_mosquitoes[4,2],TT_db3_mosquitoes[5,2],
                                                       TT_db3_mosquitoes[6,2],TT_db3_mosquitoes[7,2],TT_db3_mosquitoes[8,2],TT_db3_mosquitoes[9,2],TT_db3_mosquitoes[10,2],
                                                       TT_db3_mosquitoes[1,3],TT_db3_mosquitoes[2,3],TT_db3_mosquitoes[3,3],TT_db3_mosquitoes[4,3],TT_db3_mosquitoes[5,3],
                                                       TT_db3_mosquitoes[6,3],TT_db3_mosquitoes[7,3],TT_db3_mosquitoes[8,3],TT_db3_mosquitoes[9,3],TT_db3_mosquitoes[10,3],
                                                       TT_db3_mosquitoes[1,4],TT_db3_mosquitoes[2,4],TT_db3_mosquitoes[3,4],TT_db3_mosquitoes[4,4],TT_db3_mosquitoes[5,4],
                                                       TT_db3_mosquitoes[6,4],TT_db3_mosquitoes[7,4],TT_db3_mosquitoes[8,4],TT_db3_mosquitoes[9,4],TT_db3_mosquitoes[10,4],
                                                       TT_db3_mosquitoes[1,5],TT_db3_mosquitoes[2,5],TT_db3_mosquitoes[3,5],TT_db3_mosquitoes[4,5],TT_db3_mosquitoes[5,5],
                                                       TT_db3_mosquitoes[6,5],TT_db3_mosquitoes[7,5],TT_db3_mosquitoes[8,5],TT_db3_mosquitoes[9,5],TT_db3_mosquitoes[10,5]),
                                  
                                  osservate_db1=c(analitic_solution_MRR1[1,1],analitic_solution_MRR1[2,1],analitic_solution_MRR1[3,1],analitic_solution_MRR1[4,1],analitic_solution_MRR1[5,1],
                                                  analitic_solution_MRR1[6,1],analitic_solution_MRR1[7,1],analitic_solution_MRR1[8,1],analitic_solution_MRR1[9,1],analitic_solution_MRR1[10,1],
                                                  analitic_solution_MRR1[1,2],analitic_solution_MRR1[2,2],analitic_solution_MRR1[3,2],analitic_solution_MRR1[4,2],analitic_solution_MRR1[5,2],
                                                  analitic_solution_MRR1[6,2],analitic_solution_MRR1[7,2],analitic_solution_MRR1[8,2],analitic_solution_MRR1[9,2],analitic_solution_MRR1[10,2],
                                                  analitic_solution_MRR1[1,3],analitic_solution_MRR1[2,3],analitic_solution_MRR1[3,3],analitic_solution_MRR1[4,3],analitic_solution_MRR1[5,3],
                                                  analitic_solution_MRR1[6,3],analitic_solution_MRR1[7,3],analitic_solution_MRR1[8,3],analitic_solution_MRR1[9,3],analitic_solution_MRR1[10,3],
                                                  analitic_solution_MRR1[1,4],analitic_solution_MRR1[2,4],analitic_solution_MRR1[3,4],analitic_solution_MRR1[4,4],analitic_solution_MRR1[5,4],
                                                  analitic_solution_MRR1[6,4],analitic_solution_MRR1[7,4],analitic_solution_MRR1[8,4],analitic_solution_MRR1[9,4],analitic_solution_MRR1[10,4],
                                                  analitic_solution_MRR1[1,5],analitic_solution_MRR1[2,5],analitic_solution_MRR1[3,5],analitic_solution_MRR1[4,5],analitic_solution_MRR1[5,5],
                                                  analitic_solution_MRR1[6,5],analitic_solution_MRR1[7,5],analitic_solution_MRR1[8,5],analitic_solution_MRR1[9,5],analitic_solution_MRR1[10,5],
                                                  
                                                  analitic_solution_MRR2[1,1],analitic_solution_MRR2[2,1],analitic_solution_MRR2[3,1],analitic_solution_MRR2[4,1],analitic_solution_MRR2[5,1],
                                                  analitic_solution_MRR2[6,1],analitic_solution_MRR2[7,1],analitic_solution_MRR2[8,1],analitic_solution_MRR2[9,1],analitic_solution_MRR2[10,1],
                                                  analitic_solution_MRR2[1,2],analitic_solution_MRR2[2,2],analitic_solution_MRR2[3,2],analitic_solution_MRR2[4,2],analitic_solution_MRR2[5,2],
                                                  analitic_solution_MRR2[6,2],analitic_solution_MRR2[7,2],analitic_solution_MRR2[8,2],analitic_solution_MRR2[9,2],analitic_solution_MRR2[10,2],
                                                  analitic_solution_MRR2[1,3],analitic_solution_MRR2[2,3],analitic_solution_MRR2[3,3],analitic_solution_MRR2[4,3],analitic_solution_MRR2[5,3],
                                                  analitic_solution_MRR2[6,3],analitic_solution_MRR2[7,3],analitic_solution_MRR2[8,3],analitic_solution_MRR2[9,3],analitic_solution_MRR2[10,3],
                                                  analitic_solution_MRR2[1,4],analitic_solution_MRR2[2,4],analitic_solution_MRR2[3,4],analitic_solution_MRR2[4,4],analitic_solution_MRR2[5,3],
                                                  analitic_solution_MRR2[6,4],analitic_solution_MRR2[7,4],analitic_solution_MRR2[8,4],analitic_solution_MRR2[9,4],analitic_solution_MRR2[10,4],
                                                  analitic_solution_MRR2[1,5],analitic_solution_MRR2[2,5],analitic_solution_MRR2[3,5],analitic_solution_MRR2[4,5],analitic_solution_MRR2[5,5],
                                                  analitic_solution_MRR2[6,5],analitic_solution_MRR2[7,5],analitic_solution_MRR2[8,5],analitic_solution_MRR2[9,5],analitic_solution_MRR2[10,5],
                                                  
                                                  analitic_solution_MRR3[1,1],analitic_solution_MRR3[2,1],analitic_solution_MRR3[3,1],analitic_solution_MRR3[4,1],analitic_solution_MRR3[5,1],
                                                  analitic_solution_MRR3[6,1],analitic_solution_MRR3[7,1],analitic_solution_MRR3[8,1],analitic_solution_MRR3[9,1],analitic_solution_MRR3[10,1],
                                                  analitic_solution_MRR3[1,2],analitic_solution_MRR3[2,2],analitic_solution_MRR3[3,2],analitic_solution_MRR3[4,2],analitic_solution_MRR3[5,2],
                                                  analitic_solution_MRR3[6,2],analitic_solution_MRR3[7,2],analitic_solution_MRR3[8,2],analitic_solution_MRR3[9,2],analitic_solution_MRR3[10,2],
                                                  analitic_solution_MRR3[1,3],analitic_solution_MRR3[2,3],analitic_solution_MRR3[3,3],analitic_solution_MRR3[4,3],analitic_solution_MRR3[5,3],
                                                  analitic_solution_MRR3[6,3],analitic_solution_MRR3[7,3],analitic_solution_MRR3[8,3],analitic_solution_MRR3[9,3],analitic_solution_MRR3[10,3],
                                                  analitic_solution_MRR3[1,4],analitic_solution_MRR3[2,4],analitic_solution_MRR3[3,4],analitic_solution_MRR3[4,4],analitic_solution_MRR3[5,4],
                                                  analitic_solution_MRR3[6,4],analitic_solution_MRR3[7,4],analitic_solution_MRR3[8,4],analitic_solution_MRR3[9,4],analitic_solution_MRR3[10,4],
                                                  analitic_solution_MRR3[1,5],analitic_solution_MRR3[2,5],analitic_solution_MRR3[3,5],analitic_solution_MRR3[4,5],analitic_solution_MRR3[5,5],
                                                  analitic_solution_MRR3[6,5],analitic_solution_MRR3[7,5],analitic_solution_MRR3[8,5],analitic_solution_MRR3[9,5],analitic_solution_MRR3[10,5]))

# New facet label names for supp variable
day.labs <- c("day 1", "day 2", "day 3", "day 4 ", "day 5")
names(day.labs) <- c("1","2","3","4","5")

# New facet label names for supp variable
MRR.labs <- c("MRR 1", "MRR 2", "MRR 3" )
names(MRR.labs) <- c("1", "2","3")

#plot
pd <- position_dodge(width=1)
ggplot(analitic_vs_simulated,aes(x=anulli_1,y=osservate_db1,color=MRR),position=pd) +
  geom_point(aes(color=MRR),position=pd)+
  theme_minimal()+theme(text = element_text(size=15))+
  scale_x_continuous(breaks =seq(from=1,to=10,by=1))+
  geom_linerange(aes(ymin = primo_quantile,ymax = terzo_quantile,color=MRR),position=pd)+
  theme()+
  geom_rect(data=analitic_vs_simulated, mapping=aes(xmin=anulli_1-0.05,xmax=anulli_1+0.05,ymin=venticinque,
                                                    ymax=settantacinque,color=MRR),
            fill="white",position=pd) +
  facet_grid(MRR~day, labeller = labeller(day = day.labs, MRR = MRR.labs),scale="free")+
  labs(title ="", 
       y="Number of mosquitoes",
       x="Anulli")+theme(legend.position="none")

#########################Model fit################
#calculation of the quantile of the simulations for the first release 
XX_db1<-apply(simcap_db1,c(1,2),quantile,c(0.975))   
YY_db1<-apply(simcap_db1,c(1,2),quantile,c(0.025))
ZZ_db1<-apply(simcap_db1,c(1,2),quantile,c(0.25))
TT_db1<-apply(simcap_db1,c(1,2),quantile,c(0.75))

#calculation of the quantile of the simulations for the second release 
XX_db2<-apply(simcap_db2,c(1,2),quantile,c(0.975))  
YY_db2<-apply(simcap_db2,c(1,2),quantile,c(0.025))
ZZ_db2<-apply(simcap_db2,c(1,2),quantile,c(0.25))
TT_db2<-apply(simcap_db2,c(1,2),quantile,c(0.75))

#calculation of the quantile of the simulations for the third release 

XX_db3<-apply(simcap_db3,c(1,2),quantile,c(0.975))    
YY_db3<-apply(simcap_db3,c(1,2),quantile,c(0.025))
ZZ_db3<-apply(simcap_db3,c(1,2),quantile,c(0.25))
TT_db3<-apply(simcap_db3,c(1,2),quantile,c(0.75))

#calculate the average number of exact values within 95% of the simulated values
m1<-m2<-m3<-matrix(NA,nrow=10,ncol=5)

for(j in 1:10){
  for(i in 1:5){ 
    if(osservate_db1[j,i]<=XX_db1[j,i] & osservate_db1[j,i]>=YY_db1[j,i]){
      m1[j,i]<-1 
    }
    else {m1[j,i]<-0}
    
    if( osservate_db2[j,i]<=XX_db2[j,i] & osservate_db2[j,i]>=YY_db2[j,i]){
      m2[j,i]<-1 
    }
    else {m2[j,i]<-0}
    
    if( osservate_db3[j,i]<=XX_db3[j,i] & osservate_db3[j,i]>=YY_db3[j,i]){
      m3[j,i]<-1 
    }
    else {m3[j,i]<-0}
  }
}


mean(m1==1)*100
mean(m2==1)*100
mean(m3==1)*100
anelli_out_1<-seq(1,10,by=1)
#data frame for model fit
sim_db1<-data.frame(anulli=rep(anelli_out,by=5), 
                    anulli_1=rep(c(1:10),by=5),
                    day=rep(c(1:5),each=10),
                    MRR=as.factor(rep(c(1:3),each=50)),
                    primo_quantile_db1=c(YY_db1[1,1],YY_db1[2,1],YY_db1[3,1],YY_db1[4,1],YY_db1[5,1],
                                         YY_db1[6,1],YY_db1[7,1],YY_db1[8,1],YY_db1[9,1],YY_db1[10,1],
                                         YY_db1[1,2],YY_db1[2,2],YY_db1[3,2],YY_db1[4,2],YY_db1[5,2],
                                         YY_db1[6,2],YY_db1[7,2],YY_db1[8,2],YY_db1[9,2],YY_db1[10,2],
                                         YY_db1[1,3],YY_db1[2,3],YY_db1[3,3],YY_db1[4,3],YY_db1[5,3],
                                         YY_db1[6,3],YY_db1[7,3],YY_db1[8,3],YY_db1[9,3],YY_db1[10,3],
                                         YY_db1[1,4],YY_db1[2,4],YY_db1[3,4],YY_db1[4,4],YY_db1[5,4],
                                         YY_db1[6,4],YY_db1[7,4],YY_db1[8,4],YY_db1[9,4],YY_db1[10,4],
                                         YY_db1[1,5],YY_db1[2,5],YY_db1[3,5],YY_db1[4,5],YY_db1[5,5],
                                         YY_db1[6,5],YY_db1[7,5],YY_db1[8,5],YY_db1[9,5],YY_db1[10,5],
                                         
                                         YY_db2[1,1],YY_db2[2,1],YY_db2[3,1],YY_db2[4,1],YY_db2[5,1],
                                         YY_db2[6,1],YY_db2[7,1],YY_db2[8,1],YY_db2[9,1],YY_db2[10,1],
                                         YY_db2[1,2],YY_db2[2,2],YY_db2[3,2],YY_db2[4,2],YY_db2[5,2],
                                         YY_db2[6,2],YY_db2[7,2],YY_db2[8,2],YY_db2[9,2],YY_db2[10,2],
                                         YY_db2[1,3],YY_db2[2,3],YY_db2[3,3],YY_db2[4,3],YY_db2[5,3],
                                         YY_db2[6,3],YY_db2[7,3],YY_db2[8,3],YY_db2[9,3],YY_db2[10,3],
                                         YY_db2[1,4],YY_db2[2,4],YY_db2[3,4],YY_db2[4,4],YY_db2[5,4],
                                         YY_db2[6,4],YY_db2[7,4],YY_db2[8,4],YY_db2[9,4],YY_db2[10,4],
                                         YY_db2[1,5],YY_db2[2,5],YY_db2[3,5],YY_db2[4,5],YY_db2[5,5],
                                         YY_db2[6,5],YY_db2[7,5],YY_db2[8,5],YY_db2[9,5],YY_db2[10,5],
                                         
                                         YY_db3[1,1],YY_db3[2,1],YY_db3[3,1],YY_db3[4,1],YY_db3[5,1],
                                         YY_db3[6,1],YY_db3[7,1],YY_db3[8,1],YY_db3[9,1],YY_db3[10,1],
                                         YY_db3[1,2],YY_db3[2,2],YY_db3[3,2],YY_db3[4,2],YY_db3[5,2],
                                         YY_db3[6,2],YY_db3[7,2],YY_db3[8,2],YY_db3[9,2],YY_db3[10,2],
                                         YY_db3[1,3],YY_db3[2,3],YY_db3[3,3],YY_db3[4,3],YY_db3[5,3],
                                         YY_db3[6,3],YY_db3[7,3],YY_db3[8,3],YY_db3[9,3],YY_db3[10,3],
                                         YY_db3[1,4],YY_db3[2,4],YY_db3[3,4],YY_db3[4,4],YY_db3[5,4],
                                         YY_db3[6,4],YY_db3[7,4],YY_db3[8,4],YY_db3[9,4],YY_db3[10,4],
                                         YY_db3[1,5],YY_db3[2,5],YY_db3[3,5],YY_db3[4,5],YY_db3[5,5],
                                         YY_db3[6,5],YY_db3[7,5],YY_db3[8,5],YY_db3[9,5],YY_db3[10,5]),

                    terzo_quantile_db1=c(XX_db1[1,1],XX_db1[2,1],XX_db1[3,1],XX_db1[4,1],XX_db1[5,1],
                                         XX_db1[6,1],XX_db1[7,1],XX_db1[8,1],XX_db1[9,1],XX_db1[10,1],
                                         XX_db1[1,2],XX_db1[2,2],XX_db1[3,2],XX_db1[4,2],XX_db1[5,2],
                                         XX_db1[6,2],XX_db1[7,2],XX_db1[8,2],XX_db1[9,2],XX_db1[10,2],
                                         XX_db1[1,3],XX_db1[2,3],XX_db1[3,3],XX_db1[4,3],XX_db1[5,3],
                                         XX_db1[6,3],XX_db1[7,3],XX_db1[8,3],XX_db1[9,3],XX_db1[10,3],
                                         XX_db1[1,4],XX_db1[2,4],XX_db1[3,4],XX_db1[4,4],XX_db1[5,4],
                                         XX_db1[6,4],XX_db1[7,4],XX_db1[8,4],XX_db1[9,4],XX_db1[10,4],
                                         XX_db1[1,5],XX_db1[2,5],XX_db1[3,5],XX_db1[4,5],XX_db1[5,5],
                                         XX_db1[6,5],XX_db1[7,5],XX_db1[8,5],XX_db1[9,5],XX_db1[10,5],
                                         
                                         XX_db2[1,1],XX_db2[2,1],XX_db2[3,1],XX_db2[4,1],XX_db2[5,1],
                                         XX_db2[6,1],XX_db2[7,1],XX_db2[8,1],XX_db2[9,1],XX_db2[10,1],
                                         XX_db2[1,2],XX_db2[2,2],XX_db2[3,2],XX_db2[4,2],XX_db2[5,2],
                                         XX_db2[6,2],XX_db2[7,2],XX_db2[8,2],XX_db2[9,2],XX_db2[10,2],
                                         XX_db2[1,3],XX_db2[2,3],XX_db2[3,3],XX_db2[4,3],XX_db2[5,3],
                                         XX_db2[6,3],XX_db2[7,3],XX_db2[8,3],XX_db2[9,3],XX_db2[10,3],
                                         XX_db2[1,4],XX_db2[2,4],XX_db2[3,4],XX_db2[4,4],XX_db2[5,3],
                                         XX_db2[6,4],XX_db2[7,4],XX_db2[8,4],XX_db2[9,4],XX_db2[10,4],
                                         XX_db2[1,5],XX_db2[2,5],XX_db2[3,5],XX_db2[4,5],XX_db2[5,5],
                                         XX_db2[6,5],XX_db2[7,5],XX_db2[8,5],XX_db2[9,5],XX_db2[10,5],
                                         
                                         XX_db3[1,1],XX_db3[2,1],XX_db3[3,1],XX_db3[4,1],XX_db3[5,1],
                                         XX_db3[6,1],XX_db3[7,1],XX_db3[8,1],XX_db3[9,1],XX_db3[10,1],
                                         XX_db3[1,2],XX_db3[2,2],XX_db3[3,2],XX_db3[4,2],XX_db3[5,2],
                                         XX_db3[6,2],XX_db3[7,2],XX_db3[8,2],XX_db3[9,2],XX_db3[10,2],
                                         XX_db3[1,3],XX_db3[2,3],XX_db3[3,3],XX_db3[4,3],XX_db3[5,3],
                                         XX_db3[6,3],XX_db3[7,3],XX_db3[8,3],XX_db3[9,3],XX_db3[10,3],
                                         XX_db3[1,4],XX_db3[2,4],XX_db3[3,4],XX_db3[4,4],XX_db3[5,4],
                                         XX_db3[6,4],XX_db3[7,4],XX_db3[8,4],XX_db3[9,4],XX_db3[10,4],
                                         XX_db3[1,5],XX_db3[2,5],XX_db3[3,5],XX_db3[4,5],XX_db3[5,5],
                                         XX_db3[6,5],XX_db3[7,5],XX_db3[8,5],XX_db3[9,5],XX_db3[10,5]),
                    
                    venticinque_db1=c(ZZ_db1[1,1],ZZ_db1[2,1],ZZ_db1[3,1],ZZ_db1[4,1],ZZ_db1[5,1],
                                      ZZ_db1[6,1],ZZ_db1[7,1],ZZ_db1[8,1],ZZ_db1[9,1],ZZ_db1[10,1],
                                      ZZ_db1[1,2],ZZ_db1[2,2],ZZ_db1[3,2],ZZ_db1[4,2],ZZ_db1[5,2],
                                      ZZ_db1[6,2],ZZ_db1[7,2],ZZ_db1[8,2],ZZ_db1[9,2],ZZ_db1[10,2],
                                      ZZ_db1[1,3],ZZ_db1[2,3],ZZ_db1[3,3],ZZ_db1[4,3],ZZ_db1[5,3],
                                      ZZ_db1[6,3],ZZ_db1[7,3],ZZ_db1[8,3],ZZ_db1[9,3],ZZ_db1[10,3],
                                      ZZ_db1[1,4],ZZ_db1[2,4],ZZ_db1[3,4],ZZ_db1[4,4],ZZ_db1[5,4],
                                      ZZ_db1[6,4],ZZ_db1[7,4],ZZ_db1[8,4],ZZ_db1[9,4],ZZ_db1[10,4],
                                      ZZ_db1[1,5],ZZ_db1[2,5],ZZ_db1[3,5],ZZ_db1[4,5],ZZ_db1[5,5],
                                      ZZ_db1[6,5],ZZ_db1[7,5],ZZ_db1[8,5],ZZ_db1[9,5],ZZ_db1[10,5],
                                      
                                      ZZ_db2[1,1],ZZ_db2[2,1],ZZ_db2[3,1],ZZ_db2[4,1],ZZ_db2[5,1],
                                      ZZ_db2[6,1],ZZ_db2[7,1],ZZ_db2[8,1],ZZ_db2[9,1],ZZ_db2[10,1],
                                      ZZ_db2[1,2],ZZ_db2[2,2],ZZ_db2[3,2],ZZ_db2[4,2],ZZ_db2[5,2],
                                      ZZ_db2[6,2],ZZ_db2[7,2],ZZ_db2[8,2],ZZ_db2[9,2],ZZ_db2[10,2],
                                      ZZ_db2[1,3],ZZ_db2[2,3],ZZ_db2[3,3],ZZ_db2[4,3],ZZ_db2[5,3],
                                      ZZ_db2[6,3],ZZ_db2[7,3],ZZ_db2[8,3],ZZ_db2[9,3],ZZ_db2[10,3],
                                      ZZ_db2[1,4],ZZ_db2[2,4],ZZ_db2[3,4],ZZ_db2[4,4],ZZ_db2[5,3],
                                      ZZ_db2[6,4],ZZ_db2[7,4],ZZ_db2[8,4],ZZ_db2[9,4],ZZ_db2[10,4],
                                      ZZ_db2[1,5],ZZ_db2[2,5],ZZ_db2[3,5],ZZ_db2[4,5],ZZ_db2[5,5],
                                      ZZ_db2[6,5],ZZ_db2[7,5],ZZ_db2[8,5],ZZ_db2[9,5],ZZ_db2[10,5],
                                      
                                      ZZ_db3[1,1],ZZ_db3[2,1],ZZ_db3[3,1],ZZ_db3[4,1],ZZ_db3[5,1],
                                      ZZ_db3[6,1],ZZ_db3[7,1],ZZ_db3[8,1],ZZ_db3[9,1],ZZ_db3[10,1],
                                      ZZ_db3[1,2],ZZ_db3[2,2],ZZ_db3[3,2],ZZ_db3[4,2],ZZ_db3[5,2],
                                      ZZ_db3[6,2],ZZ_db3[7,2],ZZ_db3[8,2],ZZ_db3[9,2],ZZ_db3[10,2],
                                      ZZ_db3[1,3],ZZ_db3[2,3],ZZ_db3[3,3],ZZ_db3[4,3],ZZ_db3[5,3],
                                      ZZ_db3[6,3],ZZ_db3[7,3],ZZ_db3[8,3],ZZ_db3[9,3],ZZ_db3[10,3],
                                      ZZ_db3[1,4],ZZ_db3[2,4],ZZ_db3[3,4],ZZ_db3[4,4],ZZ_db3[5,4],
                                      ZZ_db3[6,4],ZZ_db3[7,4],ZZ_db3[8,4],ZZ_db3[9,4],ZZ_db3[10,4],
                                      ZZ_db3[1,5],ZZ_db3[2,5],ZZ_db3[3,5],ZZ_db3[4,5],ZZ_db3[5,5],
                                      ZZ_db3[6,5],ZZ_db3[7,5],ZZ_db3[8,5],ZZ_db3[9,5],ZZ_db3[10,5]),
                    
                    settantacinque_db1=c(TT_db1[1,1],TT_db1[2,1],TT_db1[3,1],TT_db1[4,1],TT_db1[5,1],
                                         TT_db1[6,1],TT_db1[7,1],TT_db1[8,1],TT_db1[9,1],TT_db1[10,1],
                                         TT_db1[1,2],TT_db1[2,2],TT_db1[3,2],TT_db1[4,2],TT_db1[5,2],
                                         TT_db1[6,2],TT_db1[7,2],TT_db1[8,2],TT_db1[9,2],TT_db1[10,2],
                                         TT_db1[1,3],TT_db1[2,3],TT_db1[3,3],TT_db1[4,3],TT_db1[5,3],
                                         TT_db1[6,3],TT_db1[7,3],TT_db1[8,3],TT_db1[9,3],TT_db1[10,3],
                                         TT_db1[1,4],TT_db1[2,4],TT_db1[3,4],TT_db1[4,4],TT_db1[5,4],
                                         TT_db1[6,4],TT_db1[7,4],TT_db1[8,4],TT_db1[9,4],TT_db1[10,4],
                                         TT_db1[1,5],TT_db1[2,5],TT_db1[3,5],TT_db1[4,5],TT_db1[5,5],
                                         TT_db1[6,5],TT_db1[7,5],TT_db1[8,5],TT_db1[9,5],TT_db1[10,5],
                                         
                                         TT_db2[1,1],TT_db2[2,1],TT_db2[3,1],TT_db2[4,1],TT_db2[5,1],
                                         TT_db2[6,1],TT_db2[7,1],TT_db2[8,1],TT_db2[9,1],TT_db2[10,1],
                                         TT_db2[1,2],TT_db2[2,2],TT_db2[3,2],TT_db2[4,2],TT_db2[5,2],
                                         TT_db2[6,2],TT_db2[7,2],TT_db2[8,2],TT_db2[9,2],TT_db2[10,2],
                                         TT_db2[1,3],TT_db2[2,3],TT_db2[3,3],TT_db2[4,3],TT_db2[5,3],
                                         TT_db2[6,3],TT_db2[7,3],TT_db2[8,3],TT_db2[9,3],TT_db2[10,3],
                                         TT_db2[1,4],TT_db2[2,4],TT_db2[3,4],TT_db2[4,4],TT_db2[5,4],
                                         TT_db2[6,4],TT_db2[7,4],TT_db2[8,4],TT_db2[9,4],TT_db2[10,4],
                                         TT_db2[1,5],TT_db2[2,5],TT_db2[3,5],TT_db2[4,5],TT_db2[5,5],
                                         TT_db2[6,5],TT_db2[7,5],TT_db2[8,5],TT_db2[9,5],TT_db2[10,5],
                                         
                                         TT_db3[1,1],TT_db3[2,1],TT_db3[3,1],TT_db3[4,1],TT_db3[5,1],
                                         TT_db3[6,1],TT_db3[7,1],TT_db3[8,1],TT_db3[9,1],TT_db3[10,1],
                                         TT_db3[1,2],TT_db3[2,2],TT_db3[3,2],TT_db3[4,2],TT_db3[5,2],
                                         TT_db3[6,2],TT_db3[7,2],TT_db3[8,2],TT_db3[9,2],TT_db3[10,2],
                                         TT_db3[1,3],TT_db3[2,3],TT_db3[3,3],TT_db3[4,3],TT_db3[5,3],
                                         TT_db3[6,3],TT_db3[7,3],TT_db3[8,3],TT_db3[9,3],TT_db3[10,3],
                                         TT_db3[1,4],TT_db3[2,4],TT_db3[3,4],TT_db3[4,4],TT_db3[5,4],
                                         TT_db3[6,4],TT_db3[7,4],TT_db3[8,4],TT_db3[9,4],TT_db3[10,4],
                                         TT_db3[1,5],TT_db3[2,5],TT_db3[3,5],TT_db3[4,5],TT_db3[5,5],
                                         TT_db3[6,5],TT_db3[7,5],TT_db3[8,5],TT_db3[9,5],TT_db3[10,5]),
                    
                    osservate_db1=c(osservate_db1[1,1],osservate_db1[2,1],osservate_db1[3,1],osservate_db1[4,1],osservate_db1[5,1],
                                    osservate_db1[6,1],osservate_db1[7,1],osservate_db1[8,1],osservate_db1[9,1],osservate_db1[10,1],
                                    osservate_db1[1,2],osservate_db1[2,2],osservate_db1[3,2],osservate_db1[4,2],osservate_db1[5,2],
                                    osservate_db1[6,2],osservate_db1[7,2],osservate_db1[8,2],osservate_db1[9,2],osservate_db1[10,2],
                                    osservate_db1[1,3],osservate_db1[2,3],osservate_db1[3,3],osservate_db1[4,3],osservate_db1[5,3],
                                    osservate_db1[6,3],osservate_db1[7,3],osservate_db1[8,3],osservate_db1[9,3],osservate_db1[10,3],
                                    osservate_db1[1,4],osservate_db1[2,4],osservate_db1[3,4],osservate_db1[4,4],osservate_db1[5,4],
                                    osservate_db1[6,4],osservate_db1[7,4],osservate_db1[8,4],osservate_db1[9,4],osservate_db1[10,4],
                                    osservate_db1[1,5],osservate_db1[2,5],osservate_db1[3,5],osservate_db1[4,5],osservate_db1[5,5],
                                    osservate_db1[6,5],osservate_db1[7,5],osservate_db1[8,5],osservate_db1[9,5],osservate_db1[10,5],
                                    
                                    osservate_db2[1,1],osservate_db2[2,1],osservate_db2[3,1],osservate_db2[4,1],osservate_db2[5,1],
                                    osservate_db2[6,1],osservate_db2[7,1],osservate_db2[8,1],osservate_db2[9,1],osservate_db2[10,1],
                                    osservate_db2[1,2],osservate_db2[2,2],osservate_db2[3,2],osservate_db2[4,2],osservate_db2[5,2],
                                    osservate_db2[6,2],osservate_db2[7,2],osservate_db2[8,2],osservate_db2[9,2],osservate_db2[10,2],
                                    osservate_db2[1,3],osservate_db2[2,3],osservate_db2[3,3],osservate_db2[4,3],osservate_db2[5,3],
                                    osservate_db2[6,3],osservate_db2[7,3],osservate_db2[8,3],osservate_db2[9,3],osservate_db2[10,3],
                                    osservate_db2[1,4],osservate_db2[2,4],osservate_db2[3,4],osservate_db2[4,4],osservate_db2[5,3],
                                    osservate_db2[6,4],osservate_db2[7,4],osservate_db2[8,4],osservate_db2[9,4],osservate_db2[10,4],
                                    osservate_db2[1,5],osservate_db2[2,5],osservate_db2[3,5],osservate_db2[4,5],osservate_db2[5,5],
                                    osservate_db2[6,5],osservate_db2[7,5],osservate_db2[8,5],osservate_db2[9,5],osservate_db2[10,5],
                                    
                                    osservate_db3[1,1],osservate_db3[2,1],osservate_db3[3,1],osservate_db3[4,1],osservate_db3[5,1],
                                    osservate_db3[6,1],osservate_db3[7,1],osservate_db3[8,1],osservate_db3[9,1],osservate_db3[10,1],
                                    osservate_db3[1,2],osservate_db3[2,2],osservate_db3[3,2],osservate_db3[4,2],osservate_db3[5,2],
                                    osservate_db3[6,2],osservate_db3[7,2],osservate_db3[8,2],osservate_db3[9,2],osservate_db3[10,2],
                                    osservate_db3[1,3],osservate_db3[2,3],osservate_db3[3,3],osservate_db3[4,3],osservate_db3[5,3],
                                    osservate_db3[6,3],osservate_db3[7,3],osservate_db3[8,3],osservate_db3[9,3],osservate_db3[10,3],
                                    osservate_db3[1,4],osservate_db3[2,4],osservate_db3[3,4],osservate_db3[4,4],osservate_db3[5,4],
                                    osservate_db3[6,4],osservate_db3[7,4],osservate_db3[8,4],osservate_db3[9,4],osservate_db3[10,4],
                                    osservate_db3[1,5],osservate_db3[2,5],osservate_db3[3,5],osservate_db3[4,5],osservate_db3[5,5],
                                    osservate_db3[6,5],osservate_db3[7,5],osservate_db3[8,5],osservate_db3[9,5],osservate_db3[10,5]))

# New facet label names for supp variable
day.labs <- c("day 1", "day 2", "day 3", "day 4 ", "day 5")
names(day.labs) <- c("1","2","3","4","5")

# New facet label names for supp variable
MRR.labs <- c("MRR 1", "MRR 2", "MRR 3" )
names(MRR.labs) <- c("1", "2","3")

#plot model fit
pd <- position_dodge(width=1)
 ggplot(sim_db1,aes(x=anulli_1,y=osservate_db1,col=MRR),position=pd) +
   geom_point(aes(color=MRR),position=pd)+
   ylim(0,35)+scale_x_continuous(breaks =seq(from=1,to=10,by=1))+
  geom_linerange(aes(ymin = primo_quantile_db1,ymax = terzo_quantile_db1,color=MRR),position=pd)+
  geom_rect(data=sim_db1, mapping=aes(xmin=anulli_1-0.05,xmax=anulli_1+0.05,ymin=venticinque_db1,
                                      ymax=settantacinque_db1,color=MRR),
            fill="white",position=pd)+
   facet_grid(MRR~day, labeller = labeller(day = day.labs, MRR = MRR.labs),scale="free")+
      labs(title ="", y="Observate data",x="Anulli")+
   theme_minimal()+theme(legend.position="none")+theme(text = element_text(size=15))
  
 #data frame  box plot D1,D2,D3
 box<-data.frame(D=c(D_new_db1_1,D_new_db2_2,D_new_db3_3),
                 MRR=as.factor(rep(c(1:3),each=10000)))

 #data frame box plot lambda  
lambda<-data.frame(L=lambda_new_1,MRR=rep(c("1-2-3"),each=10000))
 
#plot box plot diffusion coefficients
a<-ggplot(box,aes(x=MRR,y=D))+geom_boxplot()+theme_bw()+
  labs(y= "Diffusion coefficent")

#plot box plot lambda
b<-ggplot(lambda,aes(x=MRR,y=L))+geom_boxplot()+theme_bw()+
  labs(y= TeX("correction factor $\\zeta$"))

#arrange the two plot
grid.arrange(a,b,ncol=2,nrow=1)

#########################meters/day traveled by 50% and 95% of mosquitoes################
day=5    
zanzare_1<- rep(NA,day+1)
zanzare_2<- rep(NA,day+1)
zanzare_3<- rep(NA,day+1)

zanzare_1[1] <- M1
zanzare_2[1] <- M2
zanzare_3[1] <- M3

ci95_1<-matrix(NA,ncol=day,nrow=100)
ci95_2<-matrix(NA,ncol=day,nrow=100)
ci95_3<-matrix(NA,ncol=day,nrow=100)

ci50_1<-matrix(NA,ncol=day,nrow=100)
ci50_2<-matrix(NA,ncol=day,nrow=100)
ci50_3<-matrix(NA,ncol=day,nrow=100)


for( i in 1:100){
  for(j in 1:day){
    stand_dev_db1<-sqrt(2* mean(D_new_db1_1)*j*24)
    stand_dev_db2<-sqrt(2* mean(D_new_db2_2)*j*24)
    stand_dev_db3<-sqrt(2* mean(D_new_db3_3)*j*24)
    

    cov.matrix_sim_db1<-matrix(c(stand_dev_db1^2,0,0,stand_dev_db1^2),ncol=2)
    cov.matrix_sim_db2<-matrix(c(stand_dev_db2^2,0,0,stand_dev_db2^2),ncol=2)
    cov.matrix_sim_db3<-matrix(c(stand_dev_db3^2,0,0,stand_dev_db3^2),ncol=2)
    

      zanzare_1[j+1]  <-  mort2(zanzare_1[j],mu_1,1)
      zanzare_2[j+1]  <-  mort2(zanzare_2[j],mu_2,1)
      zanzare_3[j+1]  <-  mort2(zanzare_3[j],mu_3,1)
      

      pos_db1   <- rmvnorm(n=zanzare_1[j],mean= c(0,0),
                           sigma=cov.matrix_sim_db1, method = "eigen") 
      
      pos_db2   <- rmvnorm(n=zanzare_2[j],mean= c(0,0),
                           sigma=cov.matrix_sim_db2, method = "eigen") 
      
      pos_db3   <- rmvnorm(n=zanzare_3[j],mean= c(0,0),
                           sigma=cov.matrix_sim_db3, method = "eigen") 
      
      radius_db1<- apply(pos_db1,1,raggio)
      radius_db2<- apply(pos_db2,1,raggio)
      radius_db3<- apply(pos_db3,1,raggio)
      
      
      ci95_1[i,j]<-quantile(radius_db1,0.975)
      ci95_2[i,j]<-quantile(radius_db2,0.975)
      ci95_3[i,j]<-quantile(radius_db3,0.975)
      
      ci50_1[i,j]<-quantile(radius_db1,0.50)
      ci50_2[i,j]<-quantile(radius_db2,0.50)
      ci50_3[i,j]<-quantile(radius_db3,0.50)
  }
}

#mean of meters traveled by 95% of mosquitoes
mean_1MRR<-apply(ci95_1,c(2),mean)
mean_2MRR<-apply(ci95_2,c(2),mean)
mean_3MRR<-apply(ci95_3,c(2),mean)

#mean of meters traveled by 50% of mosquitoes
mean_1MRR_50<-apply(ci50_1,c(2),mean)
mean_2MRR_50<-apply(ci50_2,c(2),mean)
mean_3MRR_50<-apply(ci50_3,c(2),mean)

#quantile of meters traveled by 95% of mosquitoes
quantile_1MRR<-apply(ci95_1,c(2),quantile,c(0.025,0.975))
quantile_2MRR<-apply(ci95_2,c(2),quantile,c(0.025,0.975))
quantile_3MRR<-apply(ci95_3,c(2),quantile,c(0.025,0.975))

#quantile of meters traveled by 50% of mosquitoes
quantile_1MRR_50<-apply(ci50_1,c(2),quantile,c(0.025,0.975))
quantile_2MRR_50<-apply(ci50_2,c(2),quantile,c(0.025,0.975))
quantile_3MRR_50<-apply(ci50_3,c(2),quantile,c(0.025,0.975))


#data frame for plot
meters_mosquitoes<-data.frame(meters=c(mean_1MRR,mean_2MRR,mean_3MRR,
                                       mean_1MRR_50,mean_2MRR_50,mean_3MRR_50),
                              days=rep(1:5),
                              MRR=as.factor(rep(c(1:3),each=5)),
                              mosquitoes=c("%95","%95","%95","%95","%95",
                                           "%95","%95","%95","%95","%95",
                                           "%95","%95","%95","%95","%95",
                                           
                                           "%50","%50","%50","%50","%50",
                                           "%50","%50","%50","%50","%50",
                                           "%50","%50","%50","%50","%50"),
              
lw=c(quantile_1MRR[1,1],quantile_1MRR[1,2],quantile_1MRR[1,3],quantile_1MRR[1,4],quantile_1MRR[1,5],
     quantile_2MRR[1,1],quantile_2MRR[1,2],quantile_2MRR[1,3],quantile_2MRR[1,4],quantile_2MRR[1,5],
     quantile_3MRR[1,1],quantile_3MRR[1,2],quantile_3MRR[1,3],quantile_3MRR[1,4],quantile_3MRR[1,5],
     
     quantile_1MRR_50[1,1],quantile_1MRR_50[1,2],quantile_1MRR_50[1,3],quantile_1MRR_50[1,4],quantile_1MRR_50[1,5],
     quantile_2MRR_50[1,1],quantile_2MRR_50[1,2],quantile_2MRR_50[1,3],quantile_2MRR_50[1,4],quantile_2MRR_50[1,5],
     quantile_3MRR_50[1,1],quantile_3MRR_50[1,2],quantile_3MRR_50[1,3],quantile_3MRR_50[1,4],quantile_3MRR_50[1,5]),
              
hw=c(quantile_1MRR[2,1],quantile_1MRR[2,2],quantile_1MRR[2,3],quantile_1MRR[2,4],quantile_1MRR[2,5],
     quantile_2MRR[2,1],quantile_2MRR[2,2],quantile_2MRR[2,3],quantile_2MRR[2,4],quantile_2MRR[2,5],
     quantile_3MRR[2,1],quantile_3MRR[2,2],quantile_3MRR[2,3],quantile_3MRR[2,4],quantile_3MRR[2,5],
     
     quantile_1MRR_50[2,1],quantile_1MRR_50[2,2],quantile_1MRR_50[2,3],quantile_1MRR_50[2,4],quantile_1MRR_50[2,5],
     quantile_2MRR_50[2,1],quantile_2MRR_50[2,2],quantile_2MRR_50[2,3],quantile_2MRR_50[2,4],quantile_2MRR_50[2,5],
     quantile_3MRR_50[2,1],quantile_3MRR_50[2,2],quantile_3MRR_50[2,3],quantile_3MRR_50[2,4],quantile_3MRR_50[2,5]))

#plot of meters/day traveled by 50% and 95% of mosquitoes 
ggplot(meters_mosquitoes,aes(meters,days,col=MRR))+geom_point()+geom_line()+
  geom_errorbarh(aes(xmin=lw,xmax=hw),height = .2)+
labs(title="" )+theme_bw()+facet_wrap(~mosquitoes,scale="free")+theme_minimal()+
  theme(text = element_text(size=15))


save.image()


