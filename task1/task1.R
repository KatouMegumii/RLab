library(ggplot2)
library(car)
#导入数据和处理数据
load("task1/WorldBankData.RData")
data=WorldBank2017[,c(1,7,9,19)]
data_NA.rm=na.omit(data)

#自变量因变量和统计学数据
summary(data_NA.rm$Lifeexpectancy)
summary(data_NA.rm$NursesMidwives)
summary(data_NA.rm$basicdrinkingwater)

#画图
par(mfrow=c(1,3))
hist(data_NA.rm$Lifeexpectancy,breaks=15,freq=F)
lines(density(data_NA.rm$Lifeexpectancy))
hist(data_NA.rm$NursesMidwives,breaks=15,freq=F)
lines(density(data_NA.rm$NursesMidwives))
hist(data_NA.rm$basicdrinkingwater,breaks=15,freq=F)
lines(density(data_NA.rm$basicdrinkingwater))

#分别做线性回归
cor.test(data_NA.rm$Lifeexpectancy,data_NA.rm$NursesMidwives)
ggplot(data = data_NA.rm,aes(x=Lifeexpectancy,y=NursesMidwives))+geom_point()+stat_smooth(method = "lm",se=TRUE)
cor.test(data_NA.rm$Lifeexpectancy,data_NA.rm$basicdrinkingwater)
ggplot(data = data_NA.rm,aes(x=Lifeexpectancy,y=basicdrinkingwater))+geom_point()+stat_smooth(method = "lm",se=TRUE)

#多元线性回归
model=lm(Lifeexpectancy ~ NursesMidwives + basicdrinkingwater,data = data_NA.rm)
summary(model)

#多元线性回归的一些检验
crPlots(model)#线性关系检验
shapiro.test(model$residuals)#正态性检验
qqPlot(model,id.method = "identity",simulate=TRUE)#正态性检验
ncvTest(model)#同方差性
durbinWatsonTest(model)#独立性

vif(model)#多重共线性
outlierTest(model)#离群值
par(mfrow=c(1,1))
influencePlot(model,id.method="identity",main="Influence Plot")#｜y｜>2离群值，x>0.2高杠杆，圆圈大强影响




