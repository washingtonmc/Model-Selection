library(leaps)
library(car)
heart=read.csv("C:/Users/lucky/OneDrive/Documents/School stuff/COFC/FALL_2018/MATH449/project/Heart_adj.csv",header=T)
attach(heart)
head(heart)
dim(heart)
pairs(heart)
names(heart)
Sex=Sex+.01
cp_typical=cp_typical+.01
cp_asymptomatic=cp_asymptomatic+.01
cp_nonanginal=cp_nonanginal+.01
cp_nontypical=cp_nontypical+.01
Fbs=Fbs+.01
RestECG=RestECG+.01
ExAng=ExAng+.01
Oldpeak=Oldpeak+.01
Ca=Ca+.01
Thal_fixed=Thal_fixed+.01
Thal_normal=Thal_normal+.01
Thal_reversable=Thal_reversable+.01
AHD=AHD+.01
d=cbind(Age,Sex,cp_typical,cp_asymptomatic,cp_nonanginal,cp_nontypical,RestBP,Chol,Fbs,RestECG,MaxHR,ExAng,
        Oldpeak,Slope,Ca,Thal_fixed,Thal_normal,Thal_reversable,AHD)
d1=data.frame(d)
d.fit=lm(AHD~Age+Sex+cp_typical+cp_asymptomatic+cp_nonanginal+cp_nontypical+RestBP+Chol+Fbs+RestECG+MaxHR+
           ExAng+Oldpeak+Slope+Ca+Thal_fixed+Thal_normal+Thal_reversable)
summary(d.fit)
powerTransform(d1)
cor(d1)
tAge=Age^1.190235775
tSex=Sex^0.494834792
tcp_typical=cp_typical^-2.816744364
tcp_asymptomatic=log(cp_asymptomatic) #log because close to zero
tcp_nonanginal=cp_nonanginal^-0.650041950
tcp_nontypical=cp_nontypical^-1.331952843
tRestBP=RestBP^-0.517623372
tChol=log(Chol) #decided to use log because it was close to zero
tFbs=Fbs^-1.460692221
tRestECG=log(RestECG) #log because close to zero
tMaxHR=MaxHR^2.454337568
tExAng=ExAng^-0.483809071
tOldpeak=Oldpeak^0.224381709
tSlope=Slope^-0.358223860
tCa=Ca^-0.218157590
tThal_fixed=Thal_fixed^-3.599306509
tThal_normal=Thal_normal^0.126808431
tThal_reversable=Thal_reversable^-0.310100663
tAHD=AHD^-0.100361945
d2=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol+tFbs+tRestECG+
        tMaxHR+tExAng+tOldpeak+tSlope+tCa+tThal_fixed+tThal_normal+tThal_reversable)
summary(d2)
om1=lm(tAHD~tAge)
om2=lm(tAHD~tAge+tSex)
om3=lm(tAHD~tAge+tSex+tcp_typical)
om4=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic)
om5=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal)
om6=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical)
om7=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP)
om8=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol)
om9=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol+tFbs)
om10=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol+tFbs+tRestECG)
om11=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol+tFbs+tRestECG+
          tMaxHR)
om12=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol+tFbs+tRestECG+
          tMaxHR+ExAng)
om13=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol+tFbs+tRestECG+
          tMaxHR+tExAng+tOldpeak)
om14=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol+tFbs+tRestECG+
          tMaxHR+tExAng+tOldpeak+tSlope)
om15=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol+tFbs+tRestECG+
          tMaxHR+tExAng+tOldpeak+tSlope+tCa)
om16=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol+tFbs+tRestECG+
          tMaxHR+tExAng+tOldpeak+tSlope+tCa+tThal_fixed)
om17=lm(tAHD~tAge+tSex+tcp_typical+tcp_asymptomatic+tcp_nonanginal+tcp_nontypical+tRestBP+tChol+tFbs+tRestECG+
          tMaxHR+tExAng+tOldpeak+tSlope+tCa+tThal_fixed+tThal_normal)
om18=d2

#Subset size=1
n <- length(om1$residuals)
npar <- length(om1$coefficients) +1
#Calculate AIC
extractAIC(om1,k=2)
#Calculate AICc
extractAIC(om1,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om1,k=log(n))

#Subset size=2
npar <- length(om2$coefficients) +1
#Calculate AIC
extractAIC(om2,k=2)
#Calculate AICc
extractAIC(om2,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om2,k=log(n))

#Subset size=3
npar <- length(om3$coefficients) +1
#Calculate AIC
extractAIC(om3,k=2)
#Calculate AICc
extractAIC(om3,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om3,k=log(n))

#Subset size=4
npar <- length(om4$coefficients) +1
#Calculate AIC
extractAIC(om4,k=2)
#Calculate AICc
extractAIC(om4,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om4,k=log(n))

#Subset size=5
npar <- length(om5$coefficients) +1
#Calculate AIC
extractAIC(om5,k=2)
#Calculate AICc
extractAIC(om5,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om5,k=log(n))

#Subset size=6
npar <- length(om5$coefficients) +1
#Calculate AIC
extractAIC(om6,k=2)
#Calculate AICc
extractAIC(om6,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om6,k=log(n))

#Subset size=7
npar <- length(om5$coefficients) +1
#Calculate AIC
extractAIC(om7,k=2)
#Calculate AICc
extractAIC(om7,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om7,k=log(n))

backAIC <- step(d2,direction="backward", data=heart)
backBIC <- step(d2,direction="backward", data=heart, k=log(n))

heartAIC = lm(tAHD~tSex+tcp_asymptomatic+tRestBP+tMaxHR+tExAng+tSlope+tCa+tThal_fixed+tThal_normal)
heartBIC = lm(tAHD~tSex+tcp_asymptomatic+tExAng+tSlope+tCa+tThal_normal)
par(mfrow=c(2,2))
plot(heartAIC, main="Akaike's Information Criterion")
summary(heartAIC)
plot(heartBIC, main="Bayesian Information Criterion")
summary(heartBIC)
avPlots(d2,terms=~.,ask=F,pch=19)
