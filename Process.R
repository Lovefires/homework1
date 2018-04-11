install.package("DMwR")
install.package("car")
library("DMwR")

#定位到项目所在的路径
setwd("C://Users//Meggie//Desktop//Building_Permits")

#读取数据
data <- read.csv("Building_Permits_Similarity.csv", header = T, sep = ",")

###################################
#分析数据摘要
###################################
sink("Summary.txt")
summary(data)
sink()

data_summary <- summary(data)
print(data_summary)



###################################
#画直方图，QQ图和盒图
###################################
# Number Attributes
# "Street.Number, Number.of.Existing.Stories,Number.of.Proposed.Stories,
# Estimated.Cost, Revised.Cost, Existing.Units,Proposed.Units"

library(car)

# Hist, QQ and Box of 'Street.Number'<1>
jpeg(file=paste('Hist of','Street.Number', '.jpg' ))
hist(data$Street.Number, prob = T)
lines(density(data$Street.Number,na.rm=T))
rug(jitter(data$Street.Number))
dev.off( )
jpeg(file=paste('QQ of ','Street.Number','.jpg') )
qqPlot(data$Street.Number,main=paste('Normal QQ plot of ','Street.Number'),ylab= as.character('Street.Number'))
dev.off( )
jpeg(file=paste('box of ','Street.Number','.jpg') )
boxplot(data$Street.Number,ylab= as.character('Street.Number'))
rug(jitter(data$Street.Number),side=2)
abline(h=mean(data$Street.Number,na.rm=T),lty=2)
dev.off( )

# Hist, QQ and Box of 'Number.of.Existing.Stories'<2>
jpeg(file=paste('Hist of','Number.of.Existing.Stories', '.jpg' ))
hist(data$Number.of.Existing.Stories, prob = T)
lines(density(data$Number.of.Existing.Stories,na.rm=T))
rug(jitter(data$Number.of.Existing.Stories))
dev.off( )
jpeg(file=paste('QQ of ','Number.of.Existing.Stories','.jpg') )
qqPlot(data$Number.of.Existing.Stories,main=paste('Normal QQ plot of ','PNumber.of.Existing.Stories'),ylab= as.character('Number.of.Existing.Stories'))
dev.off( )
jpeg(file=paste('box of ','Number.of.Existing.Stories','.jpg') )
boxplot(data$Number.of.Existing.Stories,ylab= as.character('Number.of.Existing.Stories'))
rug(jitter(data$Number.of.Existing.Stories),side=2)
abline(h=mean(data$Number.of.Existing.Stories,na.rm=T),lty=2)
dev.off( )

# Hist, QQ and Box of 'Number.of.Proposed.Stories'<3>
jpeg(file=paste('Hist of','Number.of.Proposed.Stories', '.jpg' ))
hist(data$Number.of.Proposed.Stories, prob = T)
lines(density(data$Number.of.Proposed.Stories,na.rm=T))
rug(jitter(data$Number.of.Proposed.Stories))
dev.off( )
jpeg(file=paste('QQ of ','Number.of.Proposed.Stories','.jpg') )
qqPlot(data$Number.of.Proposed.Stories,main=paste('Normal QQ plot of ','Number.of.Proposed.Stories'),ylab= as.character('Number.of.Proposed.Stories'))
dev.off( )
jpeg(file=paste('box of ','Number.of.Proposed.Stories','.jpg') )
boxplot(data$Number.of.Proposed.Stories,ylab= as.character('Number.of.Proposed.Stories'))
rug(jitter(data$Number.of.Proposed.Stories),side=2)
abline(h=mean(data$Number.of.Proposed.Stories,na.rm=T),lty=2)
dev.off( )

# Hist, QQ and Box of 'Estimated.Cost'<4>
jpeg(file=paste('Hist of','Estimated.Cost', '.jpg' ))
hist(data$Estimated.Cost, prob = T)
lines(density(data$Estimated.Cost,na.rm=T))
rug(jitter(data$Estimated.Cost))
dev.off( )
jpeg(file=paste('QQ of ','Estimated.Cost','.jpg') )
qqPlot(data$Estimated.Cost,main=paste('Normal QQ plot of ','Estimated.Cost'),ylab= as.character('Estimated.Cost'))
dev.off( )
jpeg(file=paste('box of ','Estimated.Cost','.jpg') )
boxplot(data$Estimated.Cost,ylab= as.character('Estimated.Cost'))
rug(jitter(data$Estimated.Cost),side=2)
abline(h=mean(data$Estimated.Cost,na.rm=T),lty=2)
dev.off( )

# Hist, QQ and Box of 'Revised.Cost'<5>
jpeg(file=paste('Hist of','Revised.Cost', '.jpg' ))
hist(data$Revised.Cost, prob = T)
lines(density(data$Revised.Cost,na.rm=T))
rug(jitter(data$Revised.Cost))
dev.off( )
jpeg(file=paste('QQ of ','Revised.Cost','.jpg') )
qqPlot(data$Revised.Cost,main=paste('Normal QQ plot of ','Revised.Cost'),ylab= as.character('Revised.Cost'))
dev.off( )
jpeg(file=paste('box of ','Revised.Cost','.jpg') )
boxplot(data$Revised.Cost,ylab= as.character('Revised.Cost'))
rug(jitter(data$Revised.Cost),side=2)
abline(h=mean(data$Revised.Cost,na.rm=T),lty=2)
dev.off( )

# Hist, QQ and Box of 'Existing.Units'<6>
jpeg(file=paste('Hist of','Existing.Units', '.jpg' ))
hist(data$Existing.Units, prob = T)
lines(density(data$Existing.Units,na.rm=T))
rug(jitter(data$Existing.Units))
dev.off( )
jpeg(file=paste('QQ of ','Existing.Units','.jpg') )
qqPlot(data$Existing.Units,main=paste('Normal QQ plot of ','Existing.Units'),ylab= as.character('Existing.Units'))
dev.off( )
jpeg(file=paste('box of ','Existing.Units','.jpg') )
boxplot(data$Existing.Units,ylab= as.character('Existing.Units'))
rug(jitter(data$Existing.Units),side=2)
abline(h=mean(data$Existing.Units,na.rm=T),lty=2)
dev.off( )

# Hist, QQ and Box of 'Proposed.Units'<7>
jpeg(file=paste('Hist of','Proposed.Units', '.jpg' ))
hist(data$Proposed.Units, prob = T)
lines(density(data$Proposed.Units,na.rm=T))
rug(jitter(data$Proposed.Units))
dev.off( )
jpeg(file=paste('QQ of ','Proposed.Units','.jpg') )
qqPlot(data$Proposed.Units,main=paste('Normal QQ plot of ','Proposed.Units'),ylab= as.character('Proposed.Units'))
dev.off( )
jpeg(file=paste('box of ','Proposed.Units','.jpg') )
boxplot(data$Proposed.Units,ylab= as.character('Proposed.Units'))
rug(jitter(data$Proposed.Units),side=2)
abline(h=mean(data$Proposed.Units,na.rm=T),lty=2)
dev.off( )


###################################
#用四张方法处理确实数据
###################################
# 0. Save Original Data
write.csv(data, file = "Building_Permits_Original.csv", na = "XXXXXXX")

# 1. 删除
data_delete <- na.omit(data)
View(data_delete)
dim(data_delete)
write.csv(data_delete, file = "Building_Permits_Delete.csv", na = "XXXXXXX")

# 2. 最大频数
data_most <- centralImputation(data)
View(data_most)
dim(data_most)
write.csv(data_most, file = "Building_Permits_Frequency.csv", na = "XXXXXXX")

# 3. 属性相关性
data_cor <- data
symnum(cor(data[,21:22], use="complete.obs"))
lm(Number.of.Existing.Stories ~ Number.of.Proposed.Stories , data = data)
# 经验证，“Number of Existing Stories”和“Number of Proposed Stories”的相关关系为：
# Number.of.Existing.Stories = -0.0171 + 0.9968 *  Number.of.Proposed.Stories
fillNumber.of.Existing.Stories <- function(Number.of.Proposed.Stories){
  if(is.na(Number.of.Proposed.Stories))
    return(NA)
  else return (-0.0171 + 0.9968 *  Number.of.Proposed.Stories)
}
data_cor[is.na(data_cor$Number.of.Existing.Stories),'Number.of.Existing.Stories']<- 
  sapply(data_cor[is.na(data_cor$Number.of.Existing.Stories),'Number.of.Proposed.Stories'],fillNumber.of.Existing.Stories)
write.csv(data_cor,file = "Building_Permits_Correalation.csv", na = "XXXXXXX")

# 4. 数据对象相似性
data_similar <- data[-manyNAs(data),]
data_similar = knnImputation(data,k=10)
write.csv(data_similar,file = "Building_Permits_Similarity.csv", na = "XXXXXXX")