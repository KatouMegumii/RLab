library(ggplot2)
#导入数据和处理数据
load("task1/WorldBankData.RData")
data=WorldBank2017[,c(2,18,19)]
data_NA.rm=na.omit(data)

#自变量因变量和统计学数据
summary(data_NA.rm$maternalmortality)
summary(data_NA.rm$Doctors)
summary(data_NA.rm$NursesMidwives)

#画图
par(mfrow=c(1,3))
hist(data_NA.rm$maternalmortality,breaks=15,freq=F)
lines(density(data_NA.rm$maternalmortality))
hist(data_NA.rm$Doctors,breaks=15,freq=F)
lines(density(data_NA.rm$Doctors))
hist(data_NA.rm$NursesMidwives,breaks=15,freq=F)
lines(density(data_NA.rm$NursesMidwives))

#分别做线性回归
cor.test(data_NA.rm$maternalmortality,data_NA.rm$NursesMidwives)
ggplot(data = data_NA.rm,aes(x=maternalmortality,y=NursesMidwives))+geom_point()+stat_smooth(method = "lm",se=TRUE)


