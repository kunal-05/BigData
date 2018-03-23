#Dataset-Region-wise monthly rainfall in mm(INDIA)
#Data-mining And Linear Regression Analysis using Various Pictorial Representations
main=read.csv("Data.csv")

#I-->PIE CHART 1
dataset1 <-read.csv("sau_pie.csv")
a1=dataset1[["Annual"]]
x1=dataset1[["Year"]]
pie(a1,x1,100,main="Rainfall in Saurashtra from 1990 to 2015.", col=rainbow(length(x1)))

#I-->PIE CHART 2
dataset2 <-read.csv("andaman_pie.csv")
a2=dataset2[["Rain"]]
x2=dataset2[["Month"]]
pie(a2,x2,100,main="Rainfall in year 1991(Andaman & Nicobar)",col=rainbow(length(a2)))

#I-->PIE CHART 3
dataset3 <-read.csv("uttarakhand_pie.csv")
a3=dataset3[["Data"]]
x3=dataset3[["Month"]]
pie(a3,x3,500,main="Rainfall in year 2015(Uttarakhand)",col=rainbow(length(a3)))

#II-->HISTOGRAM 1
library(ggplot2)
dataset4<-read.csv("Data.csv")
ggplot(dataset4,aes(x=Jun.Sept))+geom_histogram(binwidth = 300,col="red",fill="green",alpha=.2)+labs(title="Histogram for season June-September")+labs(x="Rainfall in mm",y="Frequency")+facet_wrap(~SubDiv,scale="free_y")

#II-->HISTOGRAM 2
library(ggplot2)
dataset5<-read.csv("Data.csv")
dataset6=subset(dataset5,SubDiv="ASSAM & MEGHALAYA")
ggplot(dataset6,aes(x=Oct.Dec))+geom_histogram(binwidth = 300,col="red",fill="green",alpha=.2)+labs(title="Histogram for season Oct-Dec in ASSAM & MEGHALAYA")+labs(x="Rainfall in mm",y="Frequency")+facet_wrap(~Year,scale="free_y")

#III-->BARPLOT 1
dataset7 <-read.csv("jammu_bar.csv")
barplot(as.matrix(dataset7),main="SEASONAL BEHAVIOUR FROM 2000-2014(JAMMU & KASHMIR)",xlab ="YEAR",beside=TRUE , col=rainbow(4))
legend("topleft",c("Jan to Feb","Mar to May","June to Sept","Oct to Dec"),cex=0.7,bty="n",fill=rainbow(4))

#III-->BARPLOT 2
dataset8 <-read.csv("orrisa_bar.csv")
barplot(as.matrix(dataset8),main="RAINFALL IN ORISSA IN 2015",xlab ="MONTH",beside=TRUE)

#IV-->LINE CHART 1
dataset9 <-read.csv("Data.csv")
library(ggplot2)
ggplot(dataset9,aes(x=Year,y=Annual,color=SubDiv))+geom_smooth(se=FALSE)

#V-->BOXPLOT 1
dataset10 <-read.csv("Data.csv")
library(ggplot2)
dataset11=subset(dataset10,SubDiv=="KERALA")
ggplot(dataset10,aes(x=SubDiv,y=Annual))+geom_boxplot()+scale_y_continuous(limits=c(0,4000),breaks=seq(0,4000,200))

#VI-->NORMAL DISTRIBUTION 1
dataset12 <-read.csv("Data.csv")
dataset13=subset(dataset12,SubDiv=="HIMACHAL")
r11=dataset13[["Annual"]]
a11=mean(r11)
d11=sd(r11)
x11=seq(100,1500,by=5)
y11=dnorm(x11,a11,d11)
plot(x11,y11)

#VI-->NORMAL DISTRIBUTION 2
dataset14 <-read.csv("Data.csv")
dataset15=subset(dataset14,SubDiv=="TAMIL NADU")
r12=dataset15[["Annual"]]
a12=mean(r12)
d12=sd(r12)
x12=seq(100,1500,by=5)
y12=dnorm(x12,a12,d12)
plot(x12,y12)

#VII-->LINEAR REGRESSEION 1
dataset16<-read.csv("sau_reg.csv")
attach(dataset16)
relation=lm(Annual~Year,dataset16)
summary(relation)
library(ggplot2)
ggplot(dataset16,aes(x=Year,y=Annual))+geom_point()+geom_smooth(method="lm")
newYear=data.frame(Year=2020)
predict(relation,newYear)

#VII-->LINEAR REGRESSEION 1
dataset17<-read.csv("mad_reg.csv")
attach(dataset17)
relation=lm(Jun.Sept~Year,dataset17)
summary(relation)
library(ggplot2)
ggplot(dataset17,aes(x=Year,y=Jun.Sept))+geom_point()+geom_smooth(method="lm")
newYear=data.frame(Year=2020)
predict(relation,newYear)