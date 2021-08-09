 setwd("~/Dropbox/sjsu/FALL 2017/MATH 261A Regression/project/student")
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)
attach(d2)
library(MASS)

par(mfrow=c(1,3), oma=c(1,1,1,2.5))
plot(age,G3,main = "Age")
plot(absences,G3,main = "Absences")
plot(failures,G3,main = "Failures")
mtext("Scatter Plot of Grade with all Quantative Variables ",outer=T,cex=0.8,font=2)
dev.off()
?par
colnames(d2)
grade<-cbind(G1,G2,G3)
table(G1)

#fit full model:
fit.full<-lm(G3~school+sex+age+address+famsize+Pstatus+Medu+
               Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
               schoolsup+famsup+paid+activities+nursery+higher+internet+
               romantic+famrel+freetime+goout+Dalc+Walc+health+absences)
summary(fit.full)
G3new<-G3+0.1
bc<-boxcox(fit.fullnew)
bc$x[which.max(bc$y)]
bc$x[bc$y > max(bc$y) - 1/2 * qchisq(.95,1)]
fit.fullnew<-lm(G3new~school+sex+age+address+famsize+Pstatus+Medu+
                  Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
                  schoolsup+famsup+paid+activities+nursery+higher+internet+
                  romantic+famrel+freetime+goout+Dalc+Walc+health+absences)
boxcox(fit.fullnew) #No need to transform for y

plot(residuals(fit.fullnew))
qqnorm(residuals(fit.fullnew))
qqline(residuals(fit.fullnew))

#outliers
plot(rstandard(fit.full))
abline(-3,0,col="blue")
abline(3,0,col="blue")
rstandard(fit.final)[which(abs(rstandard(fit.full))>3)]  #only 8 outliers

cooks.distance(fit.full)[c(441,520,587,598,604,627,638,640)] #not influencial



#factor all the categories variables
school<-factor(school,levels = c("GP","MS"),labels = c(0,1))
sex<-factor(sex,levels=c("F","M"),labels=c(0,1))
address<-factor(address,levels=c("U","R"),labels=c(0,1))
famsize<-factor(famsize,levels = c("LE3","GT3"),labels = c(0,1))
Pstatus<-factor(Pstatus,levels = c("T","A"),labels = c(0,1))
Medu<-factor(Medu,levels = c(0,1,2,3,4))
Fedu<-factor(Fedu,levels = c(0,1,2,3,4))
Mjob<-factor(Mjob,levels = c("teacher","health","services","at_home","other"),
             labels = c(0,1,2,3,4))
Fjob<-factor(Fjob,levels = c("teacher","health","services","at_home","other"),
             labels = c(0,1,2,3,4))
reason<-factor(reason,levels = c("home","reputation","course","other"),
               labels = c(0,1,2,3))
guardian<-factor(guardian,levels = c("mother","father","other"),labels=c(0,1,2))
traveltime<-factor(traveltime,levels = c(1,2,3,4))
studytime<-factor(studytime,levels = c(1,2,3,4))
schoolsup<-factor(schoolsup,levels = c("yes","no"),labels = c(0,1))
famsup<-factor(famsup,levels = c("yes","no"),labels = c(0,1))
paid<-factor(paid,levels = c("yes","no"),labels = c(0,1))
activities<-factor(activities,levels = c("yes","no"),labels = c(0,1))
nursery<-factor(nursery,levels = c("yes","no"),labels = c(0,1))
higher<-factor(higher,levels = c("yes","no"),labels = c(0,1))
internet<-factor(internet,levels = c("yes","no"),labels = c(0,1))
romantic<-factor(romantic,levels = c("yes","no"),labels = c(0,1))
famrel<-factor(famrel,levels = c(1,2,3,4,5))
freetime<-factor(freetime,levels = c(1,2,3,4,5))
goout<-factor(goout,levels = c(1,2,3,4,5))
Dalc<-factor(Dalc,levels = c(1,2,3,4,5))
Walc<-factor(Walc,levels = c(1,2,3,4,5))
health<-factor(health,levels = c(1,2,3,4,5))


#Model selection
#ForwardSelection:alpha=0.1
fit.0<-lm(G3~1)
add1(fit.0,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")
fit.1<-lm(G3~failures)
add1(fit.1,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")
fit.2<-lm(G3~failures+higher)
add1(fit.2,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")
fit.3<-lm(G3~failures+higher+school)
add1(fit.3,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")
fit.4<-lm(G3~failures+higher+school+Dalc)
add1(fit.4,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")
fit.5<-lm(G3~failures+higher+school+Dalc+schoolsup)
add1(fit.5,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")
fit.6<-lm(G3~failures+higher+school+Dalc+schoolsup+studytime)
add1(fit.6,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health,test = "F")

fit.7<-lm(G3~failures+higher+school+Dalc+schoolsup+studytime+health)
add1(fit.7,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")
fit.8<-lm(G3~failures+higher+school+Dalc+schoolsup+studytime+health+goout)
add1(fit.8,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")
fit.9<-lm(G3~failures+higher+school+Dalc+schoolsup+studytime+health+goout
          +Medu)
add1(fit.9,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")

fit.10<-lm(G3~failures+higher+school+Dalc+schoolsup+studytime+health+goout
           +Medu+famrel) 
add1(fit.10,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")

fit.11<-lm(G3~failures+higher+school+Dalc+schoolsup+studytime+health+goout
           +Medu+famrel+sex) #p-value for sex=0.0168
add1(fit.11,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")

fit.12<-lm(G3~failures+higher+school+Dalc+schoolsup+studytime+health+goout
           +Medu+famrel+sex+age) #p-value for age=0.0891
#if set alpha=0.05 this is final model
add1(fit.12,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")

fit.13<-lm(G3~failures+higher+school+Dalc+schoolsup+studytime+health+goout
           +Medu+famrel+sex+age+romantic)
add1(fit.13,G3~school+sex+age+address+famsize+Pstatus+Medu+
       Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+
       schoolsup+famsup+paid+activities+nursery+higher+internet+
       romantic+famrel+freetime+goout+Dalc+Walc+health+absences,test = "F")
summary(fit.13)
##Total number of variables:13

#Backward Selection:15variables
G3.t1<-log(G3)
fitb.16.t1<-lm(I(G3**0.5)~school+sex+age+Mjob+Fjob+guardian+studytime+failures+
              schoolsup+higher+romantic+famrel+goout+Dalc+health)
plot(fitted(fitb.16.t1),rstandard(fitb.16.t1))

summary(G3)

fit.123<-lm(G3~age+failures+absences)
plot(fitted(fit.123),rstandard(fit.123))
summary(fitted(fit.123))

fitb.16<-lm(G3~school+sex+age+Mjob+Fjob+guardian+studytime+failures+
              schoolsup+higher+romantic+famrel+goout+Dalc+health)
anova(fitb.16)
summary(fitb.16)$adj.r.squared
summary(fitb.16)$sigma

fitb.16new<-lm(G3new~school+sex+age+Mjob+Fjob+guardian+studytime+failures+
     schoolsup+higher+romantic+famrel+goout+Dalc+health)
summary(fitted(fitb.16new))
summary(G3)
bc<-boxcox(fitb.16new)
bc$x[which.max(bc$y)]
bc$x[bc$y > max(bc$y) - 1/2 * qchisq(.95,1)]
#Exhaustive Selection
library(leaps)
x_variable=cbind(school,sex,age,address,famsize,Pstatus,Medu,
                 Fedu,Mjob,Fjob,reason,guardian,traveltime,studytime,failures,
                 schoolsup,famsup,paid,activities,nursery,higher,internet,
                 romantic,famrel,freetime,goout,Dalc,Walc,health,absences)
all<-regsubsets(x=x_variable,y=G3,method="exhaustive",all.best=F,nbest = 1,nvmax = 15)
summary(all)
summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
p <- apply(Matrix,1, sum)
MSE <- SSRes/(649-p)
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSE, Cp)
colnames(output)[3:32] <- c("school","sex","age","address","famsize","Pstatus","Medu",
                            "Fedu", "Mjob","Fjob","reason","guardian","traveltime","studytime","failures",
                            "schoolsup","famsup","paid","activities","nursery","higher","internet",
                            "romantic","famrel","freetime","goout","Dalc","Walc","health","absences")

write.csv(output,"output.csv")
#Here, there are three relative good models, they are row 12,13,14
f<-regsubsets(x=x_variable,y=G3,method="forward",all.best=F,nbest = 1,nvmax=30)
summary(f)
b<-regsubsets(x=x_variable,y=G3,method="backward",all.best=F,nbest = 1,nvmax=30)
backwardresult<-summary(b)
backwardresult

#Perform comparation between 5 different models
summary(fitb.16)$adj.r.squared #0.3458 backwrd
summary(fitb.16)$sigma**2  #MSRes

summary(fit.13)$adj.r.squared #0.3407 fw
summary(fit.13)$sigma**2  #MSres

n<-nrow(d2)
anova(fitb.16)
anova(fit.full)


anova(fitb.16)
p.b16<-16
ssres.b16<-sum(fitb.16$residuals^2)
ssres.b16
sigmahat<-summary(fit.full)$sigma
sigmahat
cp.b16<-ssres.b16/(sigmahat**2)+2*p.b16-n
cp.b16
anova(fitb.16) #MSE=6.83, ssres=4178.7

p.13<-14
ssres.13<-sum(fit.13$residuals^2)
ssres.13
sigmahat<-summary(fit.full)$sigma
cp.13<-ssres.13/(sigmahat**2)+2*p.13-n
cp.13
anova(fit.13)  #MSE=6.88, ssres=4252.3


#Decide to choose backward selection method model fitb.16
fitb.16<-lm(G3~school+sex+age+Mjob+Fjob+guardian+studytime+failures+
              schoolsup+higher+romantic+famrel+goout+Dalc+health)


# calculate VIF
x_variable=cbind(school,sex,age,address,famsize,Pstatus,Medu,
                 Fedu,Mjob,Fjob,reason,guardian,traveltime,studytime,failures,
                 schoolsup,famsup,paid,activities,nursery,higher,internet,
                 romantic,famrel,freetime,goout,Dalc,Walc,health,absences)
VIF<-diag(solve(cor(x_variable)))
VIF
hist(G3)
hist(residuals(fitb.16))

#Perform chi-square test 
x<-cbind(school,sex,age,Mjob,Fjob,guardian,studytime,failures,
         schoolsup,higher,romantic,famrel,goout,Dalc,health)
for(i in 1:15){
  for(j in 1:15){
    if(chisq.test(x[,i],x[,j])$p.value<0.05){
      cat(colnames(x)[i],"&",colnames(x)[j],":",chisq.test(x[,i],x[,j])$p.value,"\n")
    }
  }
}

#there are some issues with it, a lot of small p values

#suppose we stick with model fitb.16
#residual diagnosis
#residuals check:Nomal
qqnorm(residuals(fitb.16))
qqline(residuals(fitb.16))
#independent,constant variance
plot(fitted(fitb.16),residuals(fitb.16))

summary(fitted(G3))

shapiro.test(residuals(fitb.16))
fitt
#identify residuals
c=c(rep(1,519),2,rep(1,29),2,rep(1,36),2,rep(1,10),
    2,rep(1,5),2,1,2,rep(1,20),2,rep(1,10),2,1,2,rep(1,8))
plot(rstandard(fitb.16),col=c)
abline(-3,0,col="blue")
abline(3,0,col="blue")
rstandard(fitb.16)[which(abs(rstandard(fitb.16))>3)] #outliers

#number of outliers:9:1.4% of total obs, could be ignored
9/nrow(d2)

#influencial pts
c<-cooks.distance(fitb.16)
c[which(c>1)] #none
cooks.distance(fitb.16)[c(520,550,587,598,604,606,627,638,640)]
#all the Di<1,therefore are not considered influencial

#Model validation
set.seed(1234)
sample<-sample(c(1:649),65)
d2.sample<-d2[-sample,]
fit.sample<-lm(G3~school+sex+age+Mjob+Fjob+guardian+studytime+failures+
                 schoolsup+higher+romantic+famrel+goout+Dalc+health,data=d2.sample)
y<-d2[sample,]$G3
y
y_hat<-predict(fit.sample,newdata=d2[sample,])
y_hat
pre_err<-y-y_hat
pre_err #348,186,164,510
4/65
summary(abs(pre_err))
summary(fit.full) 
2.621*2 #5.24


loop.y<-matrix(0,nrow=100,ncol = 65)
loop.y_hat<-matrix(0,nrow=100,ncol = 65)
loop.pre_err<-matrix(0,nrow=100,ncol = 65)
ssres_pred<-rep(0,100)
for(i in 1:100){
  
  sample<-sample(c(1:649),65)
  d2.sample<-d2[-sample,]
  fit.sample<-lm(G3~school+sex+age+guardian+studytime+failures+
                   +higher+romantic+famrel+goout+Dalc+health,data=d2.sample)
  loop.y[i,]<-d2[sample,]$G3
  loop.y_hat[i,]<-predict(fit.sample,newdata=d2[sample,])
  loop.pre_err[i,]<-loop.y[i,]-loop.y_hat[i,]
  ssres_pred[i]<-sum((loop.pre_err[i,])**2)
  
}
loop.pre_err[,1]
head(loop.pre_err)
summary(fitb.16)
abs(loop.pre_err[,1])>5.24

c=matrix(0,100,65)
for(j in 1:100){
  for(i in 1:65){
    if(abs(loop.pre_err[j,i])>5.25){c[j,i]=1}
  }}
c

pre_err.count<-apply(c,1,sum)
pre_err.percent<-round(pre_err.count/65,3)

pre_err.percent[which(pre_err.percent>0.05)]

length(pre_err.percent[which(pre_err.percent>0.05)])

#Mean square prediction error compared with standard err
ssres_pred
summary(ssres_pred)/(65-16) #mean=9.797

anova(fitb.16) #MSres=6.83


