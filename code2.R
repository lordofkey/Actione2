library("xlsx", lib.loc="~/R/win-library/3.2")
workdir = 'data'


orgdata1 = read.xlsx(paste0(workdir,'/2014-2015 总数据-20150509.xlsx',collapse = NULL),colClasses = c('numeric',NA,NA,'numeric','numeric','numeric','numeric','numeric',NA,'numeric','numeric','numeric','numeric','numeric','numeric'),sheetIndex = 1)
orgdata2 = read.xlsx(paste0(workdir,'/2014-2015 总数据-20150509.xlsx',collapse = NULL),colClasses = c('numeric',NA,NA,'numeric','numeric','numeric','numeric','numeric',NA,'numeric','numeric','numeric','numeric','numeric','numeric'),sheetIndex = 2)

alldata = merge(orgdata1,orgdata2,all=TRUE)
alldata = alldata[!is.na(alldata$Type),]#消除无type数据
num = dim(alldata)[1]
alldata = alldata[!is.na(alldata$Acetone),]#消除无Acetone数据
ex = num - dim(alldata)[1]
print(ex)
num = dim(alldata)[1]
alldata = alldata[!is.na(alldata$BMI),]#消除无BMI数据
ex = num - dim(alldata)[1]
print(ex)
num = dim(alldata)[1]
alldata = alldata[alldata$Time == 1,]#提取空腹数据
ex = num - dim(alldata)[1]
print(ex)
num = dim(alldata)[1]


setwd("data/")
tiff('ex_type0.tif')
boxplot(alldata[alldata$Type == 0,]$Acetone)
dev.off()
tiff('ex_type1.tif')
boxplot(alldata[alldata$Type == 1,]$Acetone)
dev.off()
tiff('ex_type2.tif')
boxplot(alldata[alldata$Type == 2,]$Acetone)
dev.off()



alldata = alldata[alldata$Acetone<15,]#消除异常数据
ex = num - dim(alldata)[1]
print(ex)
num = dim(alldata)[1]




#正态性检验
Actone = c()
for (i in c(1:dim(alldata)[1]))
{
  if(!is.na(alldata[i,10]))
  {
    Actone = c(Actone,alldata[i,10])
  }
}
tiff('normaltest1.tif')
hist(Actone,freq = FALSE,ylim = c(0,0.8))
lines(density(Actone),col='blue')
dev.off()
shapiro.test(Actone)
tiff('normaltest2.tif')
qqnorm(Actone, main="Q-Q plot: Price")
qqline(Actone)
dev.off()



rocresult = data.frame(阈值 = 0,检出率 = 0,误诊率 = 0)

Rtype1 = alldata$Type != 2
Rtype2 = alldata$Type != 1
library(Daim)
#直接绘制ROC曲线
tiff('roctype1.tif')
roc = roc(alldata[Rtype1,]$Acetone,alldata[Rtype1,]$Type,'1')
plot(roc)
dev.off()
TPR95 = table(roc$TPR>=0.95)['TRUE']
rocresult[1,] = c(roc$cutoff[TPR95],roc$TPR[TPR95],roc$FPR[TPR95])
FPR5 = length(roc$FPR)-table(roc$FPR<0.05)['TRUE']
rocresult[2,] = c(roc$cutoff[FPR5],roc$TPR[FPR5],roc$FPR[FPR5])
rocresult[3,] = c(summary(roc)$best.cut,summary(roc)$TPR,summary(roc)$FPR)
row.names(rocresult) = c('筛查','确诊','最佳阈值')
write.xlsx2(rocresult,'result.xlsx',sheetName = 'roct1',append = TRUE)

tiff('roctype2.tif')
roc = roc(alldata[Rtype2,]$Acetone,alldata[Rtype2,]$Type,'2')
plot(roc)
dev.off()
TPR95 = table(roc$TPR>=0.95)['TRUE']
rocresult[1,] = c(roc$cutoff[TPR95],roc$TPR[TPR95],roc$FPR[TPR95])
FPR5 = length(roc$FPR)-table(roc$FPR<0.05)['TRUE']
rocresult[2,] = c(roc$cutoff[FPR5],roc$TPR[FPR5],roc$FPR[FPR5])
rocresult[3,] = c(summary(roc)$best.cut,summary(roc)$TPR,summary(roc)$FPR)
row.names(rocresult) = c('筛查','确诊','最佳阈值')
write.xlsx2(rocresult,'result.xlsx',sheetName = 'roct2',append = TRUE)


bmig = list()
#分组绘制ROC曲线bmig bmi分组
bmig = c(bmig,list(alldata$BMI<18.5))
bmig = c(bmig,list((alldata$BMI>=18.5)&(alldata$BMI<24.99)))
bmig = c(bmig,list((alldata$BMI>=25)&(alldata$BMI<28)))
bmig = c(bmig,list((alldata$BMI>=28)&(alldata$BMI<32)))
bmig = c(bmig,list(alldata$BMI>=32))
#type分组
type = list()
type = c(type,list(alldata$Type==0))
type = c(type,list(alldata$Type==1))
type = c(type,list(alldata$Type==2))

numframe = data.frame(健康人 = 0,一型糖尿病 = 0,二型糖尿病 = 0)
bestcuts1 = c()
bestcuts2 = c()
for(i in c(1:5))
{
  t0 = as.numeric(table(bmig[[i]]&type[[1]])['TRUE'])
  t1 = as.numeric(table(bmig[[i]]&type[[2]])['TRUE'])
  t2 = as.numeric(table(bmig[[i]]&type[[3]])['TRUE'])
  numframe[i,] = c(t0,t1,t2)
  
  if(!is.na(t0))
  {
    if(!is.na(t1))
    {
      tiff(paste0('roctype1g',i,'.tif'))
      roc = roc(alldata[bmig[[i]]&Rtype1,'Acetone'],alldata[bmig[[i]]&Rtype1,'Type'],'1')
      plot(roc)
      dev.off()
      TPR95 = table(roc$TPR>=0.95)['TRUE']
      rocresult[1,] = c(roc$cutoff[TPR95],roc$TPR[TPR95],roc$FPR[TPR95])
      FPR5 = length(roc$FPR)-table(roc$FPR<0.05)['TRUE']
      rocresult[2,] = c(roc$cutoff[FPR5],roc$TPR[FPR5],roc$FPR[FPR5])
      rocresult[3,] = c(summary(roc)$best.cut,summary(roc)$TPR,summary(roc)$FPR)
      row.names(rocresult) = c('筛查','确诊','最佳阈值')
      write.xlsx2(rocresult,'result.xlsx',sheetName = paste0('roct1g',i),append = TRUE)
      bestcuts1[i] = summary(roc)$best.cut
    }
    if(!is.na(t2))
    {
      tiff(paste0('roctype2g',i,'.tif'))
      roc = roc(alldata[bmig[[i]]&Rtype2,'Acetone'],alldata[bmig[[i]]&Rtype2,'Type'],'2')
      plot(roc)
      dev.off()
      TPR95 = table(roc$TPR>=0.95)['TRUE']
      rocresult[1,] = c(roc$cutoff[TPR95],roc$TPR[TPR95],roc$FPR[TPR95])
      FPR5 = length(roc$FPR)-table(roc$FPR<0.05)['TRUE']
      rocresult[2,] = c(roc$cutoff[FPR5],roc$TPR[FPR5],roc$FPR[FPR5])
      rocresult[3,] = c(summary(roc)$best.cut,summary(roc)$TPR,summary(roc)$FPR)
      row.names(rocresult) = c('筛查','确诊','最佳阈值')
      write.xlsx2(rocresult,'result.xlsx',sheetName = paste0('roct2g',i),append = TRUE)
      bestcuts2[i] = summary(roc)$best.cut
    }
  }
}
t0 = as.numeric(table(type[[1]])['TRUE'])
t1 = as.numeric(table(type[[2]])['TRUE'])
t2 = as.numeric(table(type[[3]])['TRUE'])
numframe[6,] = c(t0,t1,t2)
row.names(numframe) = c('~18.5','18.5-24.99','25-28','28-32','32~','total')
write.xlsx2(numframe,'result.xlsx',sheetName = 'num',append = TRUE)

tiff('bestcut1.tif')
plot(1:3,bestcuts1)
lines(1:3,bestcuts1)
dev.off()
tiff('bestcut2.tif')
plot(1:5,bestcuts2)
lines(c(1,2,3,5),bestcuts2[c(1,2,3,5)])
dev.off()

#svm
library('e1071')
inputData = data.frame(alldata[, c(9,10)], response = as.factor(alldata$Type))
svmfit = svm(response ~., data = inputData, kernel = "polynomial",degree = 5,cost = 10, scale = FALSE) # linear svm, scaling turned OFFpolynomial:radial sigmoid
tiff('svm.tif')
plot(svmfit, inputData)
dev.off()
