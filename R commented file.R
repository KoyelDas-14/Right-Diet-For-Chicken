setwd("C:\\Users\\Koyel Das\\Desktop\\Data Mining")
rm(list=ls())
ChickWeight$Diet=as.factor(ChickWeight$Diet)
library(ggplot2)
ggplot(ChickWeight,aes(x=Time,y=weight,col=Diet))+geom_smooth(method="lm",se=FALSE)+ggtitle("Fitted line for Each Diet")
m1=lm(weight~Time+Diet+Time:Diet,data=ChickWeight);summary(m1) # Diet-1 as the base level
# Growth is something which should be evaluated over time, only diet may not be relevant. So we need to consider reg coefs of interaction term of time and diet, which is also clear from the summary.
I=matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),4,3)
colnames(I)=c("X1","X2","X3")
rownames(I)=c("Diet-1","Diet-2","Diet-3","Diet-4")
I #matrix of indicator variables for diet
# w=b0+b11*X1+b12*X2+b13*X3+b21*X1*t+b22*X2*t+b23*X3*t+b3*t
# Diet-1 : w=b0+b3*t
# Diet-2 : w=(b0+b11)+(b3+b21)*t ; b21=diff between the slopes of Diet-2 and Diet-1
# Diet-3 : w=(b0+b12)+(b3+b22)*t ; b22=diff between the slopes of Diet-3 and Diet-1
# Diet-4 : w=(b0+b13)+(b3+b23)*t ; b23=diff between the slopes of Diet-4 and Diet-1
b21=summary(m1)$coef[6];b21 # reg coeff corr to diet2:time
b22=summary(m1)$coef[7];b22 # reg coeff corr to diet3:time
b23=summary(m1)$coef[8];b23 # reg coeff corr to diet4:time
library(ggplot2)
ggplot(ChickWeight,aes(x=Time,y=weight,col=Diet))+geom_smooth(method="lm",se=FALSE)+ggtitle("Fitted line for Each Diet")
# b21,b22,b23 >0 i.e change in weight by unit value change in time for Diet-2,3,4 is more as compared to Diet-1.Also Diet-3 seems to have more impact on weight gain than that of others.
# Let us check this by i) Testing of hypothesis, ii)Confidence Interval and iii) boxplot of reg coeffs 
# We will do Non-parametric Bootstrap (as we don't have any distributional assumption for them) to get samples corresponding to each reg coeffs.
n=nrow(ChickWeight)
b=1000
b21_samp=b22_samp=b23_samp=c() # Bootstrap Sample
for(i in 1:b){
ts=sample(1:n,size=n,replace=TRUE)
D=ChickWeight[ts,]
m=lm(weight~Time+Diet+Time:Diet,data=D)
b21_samp[i]=summary(m)$coef[6]
b22_samp[i]=summary(m)$coef[7]
b23_samp[i]=summary(m)$coef[8]
}
v_b21=var(b21_samp);v_b21
v_b22=var(b22_samp);v_b22
v_b23=var(b23_samp);v_b23
par(mfrow=c(1,3))
qqnorm(b21_samp)
qqline(b21_samp,col="red") # 
qqnorm(b22_samp)
qqline(b22_samp,col="red")
qqnorm(b23_samp)
qqline(b23_samp,col="red")
shapiro.test(b21_samp) # p-value >0.05 H0: normally distributed is accepted
shapiro.test(b22_samp) # p-value >0.05 H0: normally distributed is accepted
shapiro.test(b23_samp) # p-value >0.05 H0: normally distributed is accepted
# hence we can go for t.test(Welch's t-test as it does not assume equality of variances) to compare the means of two independent samples
#H0_21: b21=b22 ; H1: b21<b22
t.test(b21_samp,b22_samp,alternative="less") # pval<0.05 hence reject H0
#H0_23: b23=b22 ; H1: b23<b22
t.test(b23_samp,b22_samp,alternative="less")
# 95% Confidence Interval
l_b21=b21-1.96*sqrt(v_b21);l_b21
u_b21=b21+1.96*sqrt(v_b21);u_b21
l_b22=b22-1.96*sqrt(v_b22);l_b22
u_b22=b22+1.96*sqrt(v_b22);u_b22
l_b23=b23-1.96*sqrt(v_b23);l_b23
u_b23=b23+1.96*sqrt(v_b23);u_b23
reg_min=c(l_b21,l_b22,l_b23)
reg_max=c(u_b21,u_b22,u_b23)
df=data.frame(
  values=c(b21_samp,b22_samp,b23_samp),lower=(rep(reg_min,each=1000)),upper=rep(reg_max,each=1000),Time=rep(c("b21_samp","b22_samp","b23_samp"),each=1000)
)
df$Time=as.factor(df$Time)
pd=position_dodge(0.1)
ggplot(data=df,aes(x=Time,y=values,color=Time))+geom_point()+geom_line()+geom_errorbar(aes(ymin=lower,ymax=upper))+stat_summary(fun = "mean",col="black")+ggtitle("Confidence Intervals of Regression Coefficients")+xlab("reg coeffs")
#Boxplot
ggplot(data=df,aes(x=Time,y=values,color=Time))+geom_boxplot()+ggtitle("Boxplots of Regression Coefficients")+xlab("reg Coeff")
