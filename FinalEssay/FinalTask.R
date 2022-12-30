data=readRDS("FinalEssay/ESS9.rds")
#筛选离散变量
datanum=data.frame(1:49519)
col=vector()
i=1
while (i <= 7900){
  data_temp=data[,i]
  if (is.numeric(data_temp) == TRUE){
    col=append(col,i)
  }
}

cor.test()


