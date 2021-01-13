rm(list=ls())
setwd("C:/Users/virgillitc/Desktop/Dottorato/pde/script e database/Mortality")

library(ggplot2)
library(minpack.lm)  

#Mortality Data####
db_mark_mortality=read.csv(file="Dati_mortlità.csv",sep = ";",header=TRUE,na.strings ="NA",dec=".")


########### Survaival Functions ##########

morta_exp<-function(B0, u, t)
{
  s=B0*exp(-u*t)
  return(s)
}

morta_gomp<-function(A, G, t){
  # browser()
  s = exp((A/G)*(1-exp(G*t)))
  return(s)
}

morta_Weib<-function(a, g, t)
{
  s=exp(-(a/g)*t^g)
  # m=a*t^(g-1)
  return(s)
}

#Error Function####
RMSE <- function(x){
  sqrt(sum(x^2)/length(x))
}

########################### Primo release ############################

db_mark_mortality_1 <- subset(db_mark_mortality, db_mark_mortality$Release==1)
db_mark_mortality_1$proportion<-db_mark_mortality_1$marked_alive/52


### Fit the models ###

exp_fit_1<-nlsLM(proportion~morta_exp(B0, u, day_after_release), start=list(B0=0.001,u=0.001),data=db_mark_mortality_1)

gomp_fit_1<-nlsLM(proportion~morta_gomp(A, G, day_after_release), start=list(A=0.001,G=0.01),data=db_mark_mortality_1)

weib_fit_1<-nlsLM(proportion~morta_Weib(a, g, day_after_release), start=list(a=0.0000001,g=2.5),data=db_mark_mortality_1, control = nls.lm.control(maxiter = 100))

summary(exp_fit_1)
summary(gomp_fit_1)
summary(weib_fit_1)

# Look at the AICs for comparison 
AIC(exp_fit_1)
AIC(weib_fit_1)
AIC(gomp_fit_1)

### Plot our fits 

time <- seq(min(db_mark_mortality_1$day_after_release), max(db_mark_mortality_1$day_after_release), length=12)

exp_points_1  <- morta_exp(coef(exp_fit_1)["B0"], coef(exp_fit_1)["u"], time)
exp_points_1[exp_points_1>1] <-1
gomp_points_1 <- morta_gomp(coef(gomp_fit_1)["A"], coef(gomp_fit_1)["G"], time)
weib_points_1 <- morta_Weib(coef(weib_fit_1)["a"], coef(weib_fit_1)["g"], time)

#Poletti####
T1<-db_mark_mortality_1$Tmedia
mu1<-muMRR1<-c(NA,length(T1))
for(i in 1:length(T1)){
  mu1[i]<-(0.031+95820*exp(T1[i]-50.4))
  muMRR1[i] <- exp(-mu1[i]*i)
  }

#Modello di regressione####
glm1<-glm(cbind(marked_alive,marked_dead.cum ) ~ day_after_release, 
          data=db_mark_mortality_1, family=binomial)
summary(glm1)
AIC(glm1)

# beta <- coef(glm1)
# X <- model.matrix(~day_after_release, data = data.frame(day_after_release=time))
# fitted.values <- exp(X%*%beta)/(1+exp(X%*%beta))
# # the same
# fitted.values <- predict(glm1, newdata= data.frame(day_after_release=time),type = "response")
# 

plot(db_mark_mortality_1$day_after_release, db_mark_mortality_1$proportion,
     xlab="Time (days)", ylab="Proportion Survival",ylim=c(0,1),main="MRR1")
lines(time, weib_points_1,col="blue")
lines(time, exp_points_1, col="red")
lines(time, gomp_points_1, col="green")
lines(time,muMRR1,col="pink")
lines(time,glm1$fitted.values)

 
E_exp_fit_1=resid(exp_fit_1)
E_gomp_fit_1=resid(gomp_fit_1)
E_weib_fit_1=resid(weib_fit_1)
E_glm = resid(glm1)

#calcolo dell'errore quadratico medio#### 
RMSE(E_exp_fit_1) # winner!
RMSE(E_gomp_fit_1)
RMSE(E_weib_fit_1)
RMSE(E_glm)
RMSE(muMRR1)

########################### Secondo release ############################

db_mark_mortality_2<-subset(db_mark_mortality,db_mark_mortality$Release==2)
db_mark_mortality_2$proportion<-db_mark_mortality_2$marked_alive/43

### Fit the models ###

exp_fit_2<-nlsLM(proportion~morta_exp(B0, u, day_after_release), start=list(B0=0.001,u=0.001),data=db_mark_mortality_2)

gomp_fit_2<-nlsLM(proportion~morta_gomp(A, G, day_after_release), start=list(A=0.001,G=0.01),data=db_mark_mortality_2)

weib_fit_2<-nlsLM(proportion~morta_Weib(a, g, day_after_release), start=list(a=0.0000001,g=2.5),data=db_mark_mortality_2, control = nls.lm.control(maxiter = 100))

summary(exp_fit_2)
summary(gomp_fit_2)
summary(weib_fit_2)

# Look at the AICs for comparison 
AIC(exp_fit_2)
AIC(weib_fit_2)
AIC(gomp_fit_2)

### Plot our fits 

time2 <- seq(min(db_mark_mortality_2$day_after_release), max(db_mark_mortality_2$day_after_release), length=17)
exp_points_2 <- morta_exp(coef(exp_fit_2)["B0"], coef(exp_fit_2)["u"], time2)
exp_points_2[exp_points_2>1] <-1
gomp_points_2 <- morta_gomp(coef(gomp_fit_2)["A"], coef(gomp_fit_2)["G"], time2)
weib_points_2 <- morta_Weib(coef(weib_fit_2)["a"], coef(weib_fit_2)["g"], time2)

#Poletti
T2<-db_mark_mortality_2$Tmedia
mu2<-muMRR2<-c(NA,length(T2))
for(i in 1:length(T2)){
  mu2[i]<-(0.031+95820*exp(T2[i]-50.4))
  muMRR2[i] <- exp(-mu2[i]*i)
    }

#Modello di regressione
glm2<-glm(cbind(marked_alive,marked_dead.cum) ~ day_after_release, data=db_mark_mortality_2, family=binomial)
summary(glm2)


plot(db_mark_mortality_2$day_after_release, db_mark_mortality_2$proportion,
     xlab="Time (days)", ylab="Proportion Survival",ylim=c(0,1))
lines(time2, weib_points_2,col="blue")
lines(time2, exp_points_2, col="red")
lines(time2, gomp_points_2, col="green")
lines(time2,muMRR2,col="pink")
lines(time2,glm2$fitted.values)

E_exp_fit2=resid(exp_fit_2)
E_gomp_fit_2=resid(gomp_fit_2)
E_weib_fit_2=resid(weib_fit_2)
E_glm2=resid(glm2)

RMSE(E_exp_fit2) # winner!
RMSE(E_gomp_fit_2)
RMSE(E_weib_fit_2)
RMSE(E_glm2)
RMSE(muMRR2)

########################### Terzo release ############################

db_mark_mortality_3<-subset(db_mark_mortality,db_mark_mortality$Release==3)
db_mark_mortality_3$proportion<-db_mark_mortality_3$marked_alive/50

### Fit the models ###

exp_fit_3<-nlsLM(proportion~morta_exp(B0, u, day_after_release), start=list(B0=0.001,u=0.001),data=db_mark_mortality_3)

gomp_fit_3<-nlsLM(proportion~morta_gomp(A, G, day_after_release), start=list(A=0.001,G=0.01),data=db_mark_mortality_3)

weib_fit_3<-nlsLM(proportion~(morta_Weib(a, g, day_after_release)), start=list(a=0.0000001,g=2.5),data=db_mark_mortality_3, control = nls.lm.control(maxiter = 100))

summary(exp_fit_3)
summary(gomp_fit_3)
summary(weib_fit_3)

# Look at the AICs for comparison 
AIC(exp_fit_3)
AIC(weib_fit_3)
AIC(gomp_fit_3)

### Plot our fits 

time3 <- seq(min(db_mark_mortality_3$day_after_release), max(db_mark_mortality_3$day_after_release), length=17)
exp_points_3 <- morta_exp(coef(exp_fit_3)["B0"], coef(exp_fit_3)["u"], time3)
exp_points_3[exp_points_3>1] <-1
gomp_points_3 <- morta_gomp(coef(gomp_fit_3)["A"], coef(gomp_fit_3)["G"], time3)
weib_points_3 <- morta_Weib(coef(weib_fit_3)["a"], coef(weib_fit_3)["g"], time3)

#Poletti
T3<-db_mark_mortality_3$Tmedia
mu3<-muMRR3<-c(NA,length(T3))
for(i in 1:length(T3)){
  mu3[i]<-(0.031+95820*exp(T3[i]-50.4))
  muMRR3[i] <- exp(-mu3[i]*i)}


#Modello di regressione

glm3<-glm(cbind(marked_alive,marked_dead.cum) ~ day_after_release, data=db_mark_mortality_3, family=binomial)
summary(glm3)


# plot all 3 on the same graph 
plot(db_mark_mortality_3$day_after_release, db_mark_mortality_3$proportion,
     xlab="Time (days)", ylab="Proportion Survival",ylim=c(0,1))
lines(time3, weib_points_3,col="blue")
lines(time3, exp_points_3, col="red")
lines(time3, gomp_points_3, col="green")
lines(time3, muMRR3,col="pink")
lines(time3, glm3$fitted.values)

E_exp_fit_3=resid(exp_fit_3)
E_gomp_fit_3=resid(gomp_fit_3)
E_weib_fit_3=resid(weib_fit_3)
E_glm3=resid(glm3)

RMSE(E_exp_fit_3) # winner!
RMSE(E_gomp_fit_3)
RMSE(E_weib_fit_3)
RMSE(E_glm3)
RMSE(muMRR3)

#tutte e tra assieme
# legend("bottomright", legend = c("Exponential","Gompertz","Weibull","Glm","Temperature-dependens\\functions"),
#        col = c("red", "green","blue","black","pink"),box.lty=0,lty=1,text.width = 7,
#        lwd=3)
# par(mfcol=c(1,3))
# plot(db_mark_mortality_1$day_after_release, db_mark_mortality_1$proportion,
#      xlab="Time (days)", ylab="Proportion Survival",ylim=c(0,1),main="MRR1",axes = F,cex=2)
# lines(time, weib_points_1,col="blue")
# lines(time, exp_points_1, col="red")
# lines(time, gomp_points_1, col="green")
# lines(time,muMRR1,col="pink")
# lines(time,glm1$fitted.values)
# axis(1)
# axis(2)
# 
# plot(db_mark_mortality_2$day_after_release, db_mark_mortality_2$proportion,
#      xlab="Time (days)", ylab="",ylim=c(0,1),main="MRR2",axes = F,cex=2)
# lines(time2, weib_points_2,col="blue")
# lines(time2, exp_points_2, col="red")
# lines(time2, gomp_points_2, col="green")
# lines(time2,muMRR2,col="pink")
# lines(time2,glm2$fitted.values)
# axis(1)
# 
# plot(db_mark_mortality_3$day_after_release, db_mark_mortality_3$proportion,
#      xlab="Time (days)", ylab="",ylim=c(0,1),main="MRR3",
#      axes = F,cex=2,las=2)
# lines(time3, weib_points_3,col="blue")
# lines(time3, exp_points_3, col="red")
# lines(time3, gomp_points_3, col="green")
# lines(time3, muMRR3,col="pink")
# lines(time3, glm3$fitted.values)
# axis(1)
# axis(2)


db<-data.frame(point=c(weib_points_1,weib_points_2,weib_points_3,
                       exp_points_1, exp_points_2,exp_points_3,
                       gomp_points_1,gomp_points_2,gomp_points_3,
                       glm1$fitted.values,glm2$fitted.values,glm3$fitted.values),
               
           proportion=c(db_mark_mortality_1$proportion,db_mark_mortality_2$proportion,db_mark_mortality_3$proportion),
           time=c(time,time2,time3),
          MRR=c("1","1","1","1",
                "1","1","1","1",
                "1","1","1","1",
                
                "2","2","2","2",
                "2","2","2","2",
                "2","2","2","2",
                "2","2","2","2","2",
                
                "3","3","3","3",
                "3","3","3","3",
                "3","3","3","3",
                "3","3","3","3","3",
                
                "1","1","1","1",
                "1","1","1","1",
                "1","1","1","1",
                
                "2","2","2","2",
                "2","2","2","2",
                "2","2","2","2",
                "2","2","2","2","2",
                
                "3","3","3","3",
                "3","3","3","3",
                "3","3","3","3",
                "3","3","3","3","3",
                
                "1","1","1","1",
                "1","1","1","1",
                "1","1","1","1",
                
                "2","2","2","2",
                "2","2","2","2",
                "2","2","2","2",
                "2","2","2","2","2",
                
                "3","3","3","3",
                "3","3","3","3",
                "3","3","3","3",
                "3","3","3","3","3",
                
                "1","1","1","1",
                "1","1","1","1",
                "1","1","1","1",
                
                "2","2","2","2",
                "2","2","2","2",
                "2","2","2","2",
                "2","2","2","2","2",
                
                "3","3","3","3",
                "3","3","3","3",
                "3","3","3","3",
                "3","3","3","3","3"),
          functions=c("Weibull","Weibull","Weibull","Weibull",
                      "Weibull","Weibull","Weibull","Weibull",
                      "Weibull","Weibull","Weibull","Weibull",
                      
                      "Weibull","Weibull","Weibull","Weibull",
                      "Weibull","Weibull","Weibull","Weibull",
                      "Weibull","Weibull","Weibull","Weibull",
                      "Weibull","Weibull","Weibull","Weibull","Weibull",
                      
                      "Weibull","Weibull","Weibull","Weibull",
                      "Weibull","Weibull","Weibull","Weibull",
                      "Weibull","Weibull","Weibull","Weibull",
                      "Weibull","Weibull","Weibull","Weibull","Weibull",
                      
                      "Exponential","Exponential","Exponential","Exponential",
                      "Exponential","Exponential","Exponential","Exponential",
                      "Exponential","Exponential","Exponential","Exponential",
                      
                      "Exponential","Exponential","Exponential","Exponential",
                      "Exponential","Exponential","Exponential","Exponential",
                      "Exponential","Exponential","Exponential","Exponential",
                      "Exponential","Exponential","Exponential","Exponential","Exponential",
                      
                      "Exponential","Exponential","Exponential","Exponential",
                      "Exponential","Exponential","Exponential","Exponential",
                      "Exponential","Exponential","Exponential","Exponential",
                      "Exponential","Exponential","Exponential","Exponential","Exponential",
                      
                      "Gompertz","Gompertz","Gompertz","Gompertz",
                      "Gompertz","Gompertz","Gompertz","Gompertz",
                      "Gompertz","Gompertz","Gompertz","Gompertz",
                      
                      "Gompertz","Gompertz","Gompertz","Gompertz",
                      "Gompertz","Gompertz","Gompertz","Gompertz",
                      "Gompertz","Gompertz","Gompertz","Gompertz",
                      "Gompertz","Gompertz","Gompertz","Gompertz","Gompertz",
                      
                      
                      "Gompertz","Gompertz","Gompertz","Gompertz",
                      "Gompertz","Gompertz","Gompertz","Gompertz",
                      "Gompertz","Gompertz","Gompertz","Gompertz",
                      "Gompertz","Gompertz","Gompertz","Gompertz","Gompertz",
          
                      "GLM","GLM","GLM","GLM",
                      "GLM","GLM","GLM","GLM",
                      "GLM","GLM","GLM","GLM",
                      
                      "GLM","GLM","GLM","GLM",
                      "GLM","GLM","GLM","GLM",
                      "GLM","GLM","GLM","GLM",
                      "GLM","GLM","GLM","GLM","GLM",
                      
                      "GLM","GLM","GLM","GLM",
                      "GLM","GLM","GLM","GLM",
                      "GLM","GLM","GLM","GLM",
                      "GLM","GLM","GLM","GLM","GLM"))
db$functions<-factor(db$functions,levels=c("Exponential","Gompertz","Weibull",
                                             "GLM"))

MRR.labs <- c("MRR 1", "MRR 2", "MRR 3" )
names(MRR.labs) <- c("1", "2","3")

ggplot(db,aes(x=time,y=proportion))+geom_point()+
  geom_line(stat="identity",aes(x=time,y=point,col=functions),size=1)+
  facet_wrap(~MRR,scale="free",
             labeller = labeller( MRR = MRR.labs))+ylim(0.7,1)+theme_minimal()+
  labs(x="Time of experiment",
       y="Proportion of live mosquitoes")
 



#log_likelihood####
#exponential function
p_e<-exp_points_1
N<-52
likelihood_exp1<-sum(dbinom(x = db_mark_mortality_1$marked_alive, prob = p_e, size = N,log=T))

p2_e<-exp_points_2
N2<-43
likelihood_exp2<-sum(dbinom(x = db_mark_mortality_2$marked_alive, prob = p2_e, size = N2,log=T))

p3_e<-exp_points_3
N3<-50
likelihood_exp3<-sum(dbinom(x = db_mark_mortality_3$marked_alive, prob = p3_e, size = N3,log=T))

#gomptez
p_g<-gomp_points_1
N<-52
likelihood_gomp1<-sum(dbinom(x = db_mark_mortality_1$marked_alive, prob = p_g, size = N,log=T))

p2_g<-gomp_points_2
N2<-43
likelihood_gomp2<-sum(dbinom(x = db_mark_mortality_2$marked_alive, prob = p2_g, size = N2,log=T))

p3_g<-gomp_points_3
N3<-50
likelihood_gomp3<-sum(dbinom(x = db_mark_mortality_3$marked_alive, prob = p3_g, size = N3,log=T))

#weibul
p_w<-weib_points_1
N<-52
likelihood_weib1<-sum(dbinom(x = db_mark_mortality_1$marked_alive, prob = p_w, size = N,log=T))

p2_w<-weib_points_2
N2<-43
likelihood_weib2<-sum(dbinom(x = db_mark_mortality_2$marked_alive, prob = p2_w, size = N2,log=T))

p3_w<-weib_points_3
N3<-50
likelihood_weib3<-sum(dbinom(x = db_mark_mortality_3$marked_alive, prob = p3_w, size = N3,log=T))

#glm
p_glm<-glm1$fitted.values
N<-52
likelihood_glm1<-sum(dbinom(x = db_mark_mortality_1$marked_alive, prob = p_glm, size = N,log=T))

p2_glm<-glm2$fitted.values
N2<-43
likelihood_glm2<-sum(dbinom(x = db_mark_mortality_2$marked_alive, prob = p2_glm, size = N2,log=T))

p3_glm<-glm3$fitted.values
N3<-50
likelihood_glm3<-sum(dbinom(x = db_mark_mortality_3$marked_alive, prob = p3_glm, size = N3,log=T))

#Poletti
p_m<-muMRR1
N<-52
likelihood_muMRR1<-sum(dbinom(x = db_mark_mortality_1$marked_alive, prob = p_m, size = N,log=T))

p2_m<-muMRR2
N2<-43
likelihood_muMRR2<-sum(dbinom(x = db_mark_mortality_2$marked_alive, prob = p2_m, size = N2,log=T))

p3_m<-muMRR3
N3<-50
likelihood_muMRR3<-sum(dbinom(x = db_mark_mortality_3$marked_alive, prob = p3_m, size = N3,log=T))

#Confronto likelihood
# somma tutto, quella maggiore è la migliore
likelihood_exp  <- sum(likelihood_exp1,likelihood_exp2,likelihood_exp3)
likelihood_gomp   <- sum(likelihood_gomp1,likelihood_gomp2,likelihood_gomp3)
likelihood_weib <- sum(likelihood_weib1,likelihood_weib2,likelihood_weib3)
likelihood_glm  <- sum(likelihood_glm1,likelihood_glm2,likelihood_glm3)
likelihood_muMRR<- sum(likelihood_muMRR1,likelihood_muMRR2,likelihood_muMRR3)


#confronto tra le due likelihood
anova(likelihood_exp,likelihood_weib )
LRT_2.1<-(2*(likelihood_weib-likelihood_exp))
pchisq(LRT_2.1,df=1,lower.tail=FALSE)

#p_value è 0.21 quandi le due likelihood non sono diverse statisticamente, possiamo
#considerare quindi l'esponenziale poichè in tal modo non è necessario 
#cambiare la soluzione dell'equaizone differenziale.
# par(las = 1, cex.lab = 1.2)
# 
# 
# 
# par(las = 1, cex.lab = 1.2)
# xbars <- barplot(db_mark_mortality_1$proportion, xlab = "No. of successes",
#                  ylab = "Proportion",main="1MRR")
# points(xbars, dbinom(db_mark_mortality_1$marked_alive, prob = exp_points_1, size = 52), type = "b", col = "blue",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_1$marked_alive, prob = gomp_points_1, size = 52), type = "b", col = "red",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_1$marked_alive, prob = weib_points_1, size = 52), type = "b", col = "yellow",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_1$marked_alive, prob = glm1$fitted.values, size = 52), type = "b", col = "orange",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_1$marked_alive, prob = muMRR1, size = 52), type = "b", col = "magenta",
#        pch = 19, lty = 2)
# 
# legend("topright", legend = c("Data","exp","gomp","weib","glm","poletti"),
#        col = c("gray", "blue","red","yellow","orange","magenta"), pch = c(15, 19, 19,19,19,19))
# #2 rilascio
# xbars <- barplot(db_mark_mortality_2$proportion, xlab = "No. of successes",
#                  ylab = "Proportion",main="2MRR")
# points(xbars, dbinom(db_mark_mortality_2$marked_alive, prob = exp_points_2, size = 43), type = "b", col = "blue",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_2$marked_alive, prob = gomp_points_2, size = 43), type = "b", col = "red",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_2$marked_alive, prob = weib_points_2, size = 43), type = "b", col = "yellow",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_2$marked_alive, prob = glm2$fitted.values, size = 43), type = "b", col = "orange",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_2$marked_alive, prob = muMRR2, size = 43), type = "b", col = "magenta",
#        pch = 19, lty = 2)
# 
# legend("topright", legend = c("Data","exp","gomp","weib","glm","poletti"),
#        col = c("gray", "blue","red","yellow","orange","magenta"), pch = c(15, 19, 19,19,19,19))
# # 3 rilascio
# xbars <- barplot(db_mark_mortality_3$proportion, db_mark_mortality_1$day_after_release, xlab = "No. of successes",
#                  ylab = "Proportion",main="3MRR")
# points(xbars, dbinom(db_mark_mortality_3$marked_alive, prob = exp_points_3, size = 50), type = "b", col = "blue",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_3$marked_alive, prob = gomp_points_3, size = 50), type = "b", col = "red",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_3$marked_alive, prob = weib_points_3, size = 50), type = "b", col = "yellow",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_3$marked_alive, prob = glm3$fitted.values, size = 50), type = "b", col = "orange",
#        pch = 19, lty = 2)
# points(xbars, dbinom(db_mark_mortality_3$marked_alive, prob = muMRR3, size = 50), type = "b", col = "magenta",
#        pch = 19, lty = 2)
# 
# legend("topright", legend = c("Data","exp","gomp","weib","glm","poletti"),
#        col = c("gray", "blue","red","yellow","orange","magenta"), pch = c(15, 19, 19,19,19,19))


save.image()
