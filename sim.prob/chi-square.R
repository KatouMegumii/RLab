library(epiDisplay)
data=matrix(c(2319,25497,339,8119),nrow=2,ncol=2)
chisq.test(data)
cci(cctable = data)