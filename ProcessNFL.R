# install.package("DMwR")
library("DMwR")

#定位到项目所在的路径
setwd("C://Users//Meggie//Desktop//NFL Play by Play")

#读取数据
data <- read.csv("NFL Play by Play 2009-2017 (v4)_Correalation.csv", header = T, sep = ",")
# 列的属性名
colnames(data) <- c('Date','GameID','Drive','qtr','down','time','TimeUnder','TimeSecs','PlayTimeDiff','SideofField','yrdln','yrdline100','ydstogo','ydsnet','GoalToGo','FirstDown','posteam','DefensiveTeam','desc','PlayAttempted','Yards.Gained','sp','Touchdown','ExPointResult','TwoPointConv','DefTwoPoint','Safety','Onsidekick','PuntResult','PlayType','Passer','Passer_ID','PassAttempt','PassOutcome','PassLength','AirYards','YardsAfterCatch','QBHit','PassLocation','InterceptionThrown','Interceptor','Rusher','Rusher_ID','RushAttempt','RunLocation','RunGap','Receiver','Receiver_ID','Reception','ReturnResult','Returner','BlockingPlayer','Tackler1','Tackler2','FieldGoalResult','FieldGoalDistance','Fumble', 'RecFumbTeam','RecFumbPlayer','Sack','Challenge.Replay','ChalReplayResult','Accepted.Penalty','PenalizedTeam','PenaltyType','PenalizedPlayer','Penalty.Yards','PosTeamScore','DefTeamScore','ScoreDiff','AbsScoreDiff','HomeTeam','AwayTeam','Timeout_Indicator','Timeout_Team','posteam_timeouts_pre','HomeTimeouts_Remaining_Pre','AwayTimeouts_Remaining_Pre','HomeTimeouts_Remaining_Post','AwayTimeouts_Remaining_Post','No_Score_Prob','Opp_Field_Goal_Prob','Opp_Safety_Prob','Opp_Touchdown_Prob','Field_Goal_Prob','Safety_Prob','Touchdown_Prob','ExPoint_Prob','TwoPoint_Prob','ExpPts','EPA','airEPA',' yacEPA','Home_WP_pre','Away_WP_pre','Home_WP_post','Away_WP_post','Win_Prob','WPA','airWPA','yacWPA','Season')

###################################
#分析数据摘要
###################################
sink("Summary.txt")
summary(data)
sink()

###################################
#画直方图，QQ图和盒图
###################################
value_attr <- c('TimeUnder','TimeSecs','PlayTimeDiff','yrdln','yrdline100','ydsnet','Yards.Gained','AirYards','YardsAfterCatch','Penalty.Yards','PosTeamScore','DefTeamScore','ScoreDiff','posteam_timeouts_pre','HomeTimeouts_Remaining_Pre','AwayTimeouts_Remaining_Pre','HomeTimeouts_Remaining_Post','AwayTimeouts_Remaining_Post','No_Score_Prob','Opp_Field_Goal_Prob','Opp_Safety_Prob','Opp_Touchdown_Prob','Field_Goal_Prob','Safety_Prob','Touchdown_Prob','ExPoint_Prob','TwoPoint_Prob','ExpPts','EPA','airEPA',' yacEPA','Home_WP_pre','Away_WP_pre','Home_WP_post','Away_WP_post','Win_Prob','WPA','airWPA','yacWPA')

# Number Attributes
library(car)
# Hist, QQ and Box of 'Date '
for (i in 1:102)
{
	a <- data[,i]
	attr <- colnames(data[i]) 
	if (attr %in% value_attr)
	{
	jpeg(file=paste('Histogram of ',attr,'.jpg') )
      hist(a, prob=T, xlab='',main=paste('Histogram of ',attr),ylim=0:1)
      lines(density(a,na.rm=T))
      rug(jitter(a))
      dev.off( )
      jpeg(file=paste('QQ of ',attr,'.jpg') )
      qqPlot(a,main=paste('Normal QQ plot of ',attr),ylab= as.character(attr))
      dev.off( )
	jpeg(file=paste('box of ',attr,'.jpg') )
	boxplot(a,ylab= as.character('GameID'))
	rug(jitter(a),side=2)
	abline(h=mean(a,na.rm=T),lty=2)
	dev.off( )

	}
}


###################################
#用四张方法处理确实数据
###################################
# 0. Save Original Data
write.csv(data, file = "NFL Play by Play 2009-2017 (v4)_Original.csv", na = "XXXXXXX")

# 1. 删除
data_delete <- na.omit(data)
write.csv(data_delete, file = "NFL Play by Play 2009-2017 (v4)_Delete.csv", na = "XXXXXXX")

# 2. 最大频数
data_most <- centralImputation(data)
write.csv(data_most, file = "NFL Play by Play 2009-2017 (v4)_Frequency.csv", na = "XXXXXXX")

# 3. 属性相关性
data_cor <- data
symnum(cor(data[,76:102], use="complete.obs"))
#'posteam_timeouts_pre','HomeTimeouts_Remaining_Pre','AwayTimeouts_Remaining_Pre',
#'HomeTimeouts_Remaining_Post','AwayTimeouts_Remaining_Post','No_Score_Prob','Opp_Field_Goal_Prob',
#'Opp_Safety_Prob','Opp_Touchdown_Prob','Field_Goal_Prob','Safety_Prob','Touchdown_Prob',
#'ExPoint_Prob','TwoPoint_Prob','ExpPts','EPA','airEPA',' yacEPA','Home_WP_pre','Away_WP_pre',
#'Home_WP_post','Away_WP_post','Win_Prob','WPA','airWPA','yacWPA')

lm(Home_WP_post ~ Away_WP_post , data = data)
# 经验证，“Home_WP_post”和“Away_WP_post”的相关关系为：
# Home_WP_post = 0.9992 + -0.9973*  Away_WP_post
fillHome_WP_post <- function(Away_WP_post){
  if(is.na(Away_WP_post))
    return(NA)
  else return (-0.0171 + 0.9968 *  Away_WP_post)
}
data_cor[is.na(data_cor$Home_WP_post),'Home_WP_post']<- 
  sapply(data_cor[is.na(data_cor$Home_WP_post),'Away_WP_post'],fillHome_WP_post)
write.csv(data_cor,file = "NFL Play by Play 2009-2017 (v4)_Correalation.csv", na = "0")

# 4. 数据对象相似性
data_similar <- data[-manyNAs(data),]
data_similar = knnImputation(data,k=10)
write.csv(data_similar,file = "NFL Play by Play 2009-2017 (v4)_Similarity.csv", na = "0")
