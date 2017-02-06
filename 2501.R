#http://www.25-01.com/
# - 瀏覽熱門日
# - 瀏覽熱門時段
# - 回購：首購，平均回購天數
# - 舊客：新客，平均回戰週期
# - 受眾興趣分群
# - 使用者裝置轉換漏斗表現
# - 流量來源轉換漏斗表現
# - 商品
# - 優先汰換商品
# - 客戶狀態分群(new)
# - 品牌力指數(new)
# - 看有轉換的人top轉換路徑

########################################################################
############################Read Data & Manipulate######################
########################################################################

eval(parse("sop_function.R", encoding="UTF-8"))
eval(parse("cluster_function.R", encoding="UTF-8"))
myjson11<-jsonlite::fromJSON('../_data/event1.json', flatten = TRUE)

myjson11<-jsonlite::fromJSON('../_data/event1.json', flatten = TRUE)
myjson12<-jsonlite::fromJSON('../_data/event2.json', flatten = TRUE)
myjson13<-jsonlite::fromJSON('../_data/event3.json', flatten = TRUE)
myjson14<-jsonlite::fromJSON('../_data/event4.json', flatten = TRUE)
myjson15<-jsonlite::fromJSON('../_data/event5.json', flatten = TRUE)
#myjson<-remove_id(myjson, c('1705117656.1467707925','1957040293.1471416113'))

myjson<-rbind.fill(myjson11,myjson12,myjson13,myjson14,myjson15)
colnames(myjson)[which(names(myjson) == "ts")] <- "Time"
colnames(myjson)[which(names(myjson) == "ip")] <- "IP"
colnames(myjson)[which(names(myjson) == "event")] <- "Event"

mydata<-my_data_manipulate(myjson)
mydata$clientId<-mydata$gid
mydata$clientId <- ifelse(!is.na(mydata$clientId), mydata$clientId, mydata$fid)
mydata$clientId <- ifelse(!is.na(mydata$clientId), mydata$clientId, mydata$cid)
mydata<-my_session6(mydata)

rm(myjson11,myjson12,myjson13,myjson14,myjson15)

mydata_ViewContent<- mydata %>% filter(Event=='ViewContent')
mydata_cart<- mydata %>% filter(Event=='AddToCart')
mydata_purchase<- mydata %>% filter(Event=='Purchase')

mydata<-device_generator(mydata)
mydata<-medium_modifier(mydata)
mydata<- referrer_generator(mydata)



########################################################################
############################瀏覽熱門日######################
########################################################################
my_user_time_plot(mydata,time_dimension = 'actual_date',fill_dimension = ,type = 'line',facet_or_not = 'No')
my_user_time_plot(mydata,time_dimension = 'weekday',fill_dimension = ,type = 'line',facet_or_not = 'No')
my_user_time_plot(mydata,time_dimension = 'hour',fill_dimension = ,type = 'line',facet_or_not = 'No')
multiplot(p1, p2, p3, cols=2)

#session
week_visit<- mydata %>% filter(!is.na(clientId)) %>% group_by(actual_date, weekday, clientId) %>% summarise(session_count=n_distinct(session)) 
week_visit<- week_visit %>% group_by(actual_date, weekday) %>% summarise(session_sum=sum(session_count)) 
week_visit<- week_visit %>% group_by(weekday) %>% summarise(session_avg=mean(session_sum)) 

week_visit_hot<-c(0,week_visit$session_avg[2],0,0,week_visit$session_avg[5],0,0)

dev.off()
dev.new(width=8, height=7)
opar<- par(mfrow=c(1,1), cex.axis=2, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
barplot(week_visit$session_avg[1:7], col=rgb(20, 61, 85, max=255), ylim=c(0, 500), ylab="", axes=FALSE, main="", border=1, space=0.5, names.arg=week_visit$weekday[1:7])
barplot(week_visit_hot, col=rgb(40, 122, 169, max=255), ylim=c(0, 5000), ylab="", axes=FALSE, main="", border=NA, space=0.5, add=TRUE)
axis(2, las=3,line=-1, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 500, 100),cex=2)
mtext(seq(0, 500, 100), las=2, side=2,cex=1.3, line=-0.8, col="grey10", at=seq(0, 500, 100))
mtext("平均工作階段", side=2, line=1.5, col="grey10",cex=2)


########################################################################
############################瀏覽熱門時段######################
########################################################################
#先算每天的每個小時總共有多少個row
hour_visit<- mydata %>% filter(!is.na(clientId)) %>% group_by(actual_date, hour) %>% summarise(view_count=n()) 
hour_visit<- hour_visit %>% group_by(hour) %>% summarise(view_avg=mean(view_count)) 

hour_visit_hot<-c(hour_visit$view_avg[1],rep(0,18),hour_visit$view_avg[20:24])

dev.off()
dev.new(width=8, height=7)
opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
barplot(hour_visit$view_avg[1:24], col=rgb(20, 61, 85, max=255), ylim=c(0, 110), ylab="", axes=FALSE, main="", border=1, space=0.5, names.arg=hour_visit$hour[1:24])
barplot(hour_visit_hot, col=rgb(40, 122, 169, max=255), ylim=c(0, 110), ylab="", axes=FALSE, main="", border=NA, space=0.5, add=TRUE)
axis(2, las=3,line=-1, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 110, 10),cex=2)
mtext(seq(0, 110, 10), las=2, side=2,cex=1.3, line=-0.8, col="grey10", at=seq(0, 110, 10))
mtext("平均瀏覽頁數", side=2, line=1.5, col="grey10",cex=2)
#mtext("回購天數", side=1, line=2, col="grey10")
# text(1.7, interval[1,2]+15, "68%", col="grey10", adj=1, cex=0.8)
# text(3.2, interval[2,2]+15, "13%", col="grey10", adj=1, cex=0.8)
# text(4.6, interval[3,2]+15, "6%", col="grey10", adj=1, cex=0.8)
# text(6.1, interval[4,2]+15, "2%", col="grey10", adj=1, cex=0.8)


########################################################################
############################回購：首購，平均回購天數 ######################
########################################################################
mydata_return<- mydata_purchase %>% group_by(clientId,actual_date) %>% summarise(count=n()) %>%
  arrange(clientId, actual_date) %>%select(clientId, actual_date) 

mydata_return$duplicated<- duplicated(mydata_return$clientId)
mydata_return$Date.2<- c(as.Date('2017-01-02'),mydata_return$actual_date[1:(dim(mydata_return)[1]-1)])
mydata_return$lag<- as.numeric(mydata_return$actual_date- mydata_return$Date.2)
mydata_return[mydata_return$duplicated==FALSE,"lag"]<- -1

x<- mydata_return[mydata_return$duplicated==TRUE,]
summary(x$lag)
length(unique(mydata_return$clientId))/length(unique(x$clientId))

interval<- as.data.frame(table(mydata_return$lag))
names(interval)<-c("days","counts")
interval$days<- as.numeric(as.character(interval$days))
interval<- interval[interval$day>=0,]
interval$percentage<- round(interval$counts/sum(interval$counts)*100,2)

dev.off()
dev.new(width=8, height=7)
opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
barplot(interval$counts[1:15], col=rgb(20, 61, 85, max=255), ylim=c(0, 30), ylab="", axes=FALSE, main="", border=NA, space=0.5, names.arg=interval$days[1:15])
axis(2, las=1, line=-1, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 30, 10))
mtext(seq(0, 30, 10), las=2, side=2, line=-0.8, col="grey10", at=seq(0, 30, 10))
mtext("回購筆數", side=2, line=1.5, col="grey10",cex=2)
mtext("回購天數", side=1, line=2, col="grey10",cex=2)
text(1.4, interval[1,2]+2, "30%", col="grey10", adj=1, cex=1.8)
text(2.9, interval[2,2]+2, "16%", col="grey10", adj=1, cex=1.8)
text(4.3, interval[3,2]+2, "10%", col="grey10", adj=1, cex=1.8)
text(5.8, interval[4,2]+2, "4%", col="grey10", adj=1, cex=1.8)

dev.off()
dev.new(width=8, height=7)
opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
barplot(interval$counts[1:7], col=rgb(20, 61, 85, max=255), ylim=c(0, 30), ylab="", axes=FALSE, main="", border=NA, space=0.5, names.arg=interval$days[1:7])
axis(2, las=1, line=0, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 30, 10))
mtext(seq(0, 30, 10), cex=1.4,las=2, side=2, line=0.1, col="grey10", at=seq(0, 30, 10))
mtext("回購筆數", side=2, line=1.5, col="grey10",cex=2)
mtext("回購天數", side=1, line=2, col="grey10",cex=2)
text(1.5, interval[1,2]+2, "30%", col="grey10", adj=1, cex=1.8)
text(3.1, interval[2,2]+2, "16%", col="grey10", adj=1, cex=1.8)
text(4.6, interval[3,2]+2, "10%", col="grey10", adj=1, cex=1.8)
text(6.0, interval[4,2]+2, "4%", col="grey10", adj=1, cex=1.8)




########################################################################
############################舊客：新客，平均回戰 ######################
########################################################################
###################################  回站  ########################################
mydata_retention<- mydata %>% group_by(clientId,actual_date) %>% summarise(count=n()) %>%
  arrange(clientId, actual_date) %>%select(clientId, actual_date) 
  
mydata_retention$duplicated<- duplicated(mydata_retention$clientId)
mydata_retention$Date.2<- c(as.Date('2017-01-02'),mydata_retention$actual_date[1:(dim(mydata_retention)[1]-1)])
mydata_retention$lag<- as.numeric(mydata_retention$actual_date- mydata_retention$Date.2)
mydata_retention[mydata_retention$duplicated==FALSE,"lag"]<- -1

y<- mydata_retention[mydata_retention$duplicated==TRUE,]
summary(y$lag)
length(unique(mydata_retention$clientId))/length(unique(y$clientId))

interval<- as.data.frame(table(mydata_retention$lag))
names(interval)<-c("days","counts")
interval$days<- as.numeric(as.character(interval$days))
interval<- interval[interval$day>=0,]
interval$percentage<- round(interval$counts/sum(interval$counts)*100,2)

dev.off()
dev.new(width=8, height=7)
opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
barplot(interval$counts[1:15], col=rgb(20, 61, 85, max=255), ylim=c(0, 2500), ylab="", axes=FALSE, main="", border=NA, space=0.5, names.arg=interval$days[1:15])
axis(2, las=1, line=-1, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 2500, 500))
mtext(seq(0, 2500, 500), las=1, side=2, line=-0.8, col="grey10", at=seq(0, 2500, 500))
mtext("回站次數", side=2, line=1.5, col="grey10",cex=2)
mtext("回站天數", side=1, line=2, col="grey10",cex=2)
text(1.4, interval[1,2]+100, "30%", col="grey10", adj=1, cex=1.8)
text(3.0, interval[2,2]+100, "11%", col="grey10", adj=1, cex=1.8)
text(4.5, interval[3,2]+100, "7%", col="grey10", adj=1, cex=1.8)
text(5.9, interval[4,2]+100, "5%", col="grey10", adj=1, cex=1.8)
text(7.4, interval[5,2]+100, "4%", col="grey10", adj=1, cex=1.8)

dev.off()
dev.new(width=8, height=7)
opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
barplot(interval$counts[1:7], col=rgb(20, 61, 85, max=255), ylim=c(0, 2500), ylab="", axes=FALSE, main="", border=NA, space=0.5, names.arg=interval$days[1:7])
axis(2, las=1, line=0, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 2500, 500))
mtext(seq(0, 2500, 500), cex=1.4, las=1, side=2, line=0.1, col="grey10", at=seq(0, 2500, 500))
mtext("回站次數", side=2, line=1.5, col="grey10",cex=2)
mtext("回站天數", side=1, line=2, col="grey10",cex=2)
text(1.5, interval[1,2]+100, "30%", col="grey10", adj=1, cex=1.8)
text(3.1, interval[2,2]+100, "11%", col="grey10", adj=1, cex=1.8)
text(4.6, interval[3,2]+100, "7%", col="grey10", adj=1, cex=1.8)
text(6.0, interval[4,2]+100, "5%", col="grey10", adj=1, cex=1.8)
text(7.5, interval[5,2]+100, "4%", col="grey10", adj=1, cex=1.8)




########################################################################
############################受眾興趣分群 ######################
########################################################################

########################################################################
############################使用者裝置轉換漏斗表現 ######################
########################################################################
###################################裝置漏斗######################################
funnel_device<-mydata %>% select(clientId, session, Event, dv)
funnel_device$duplicated<-duplicated(funnel_device$clientId)
funnel_device_visit<- funnel_device %>% filter(dv %in% c("MOB","PC")) %>% group_by(dv) %>% 
  summarise(counts=n_distinct(clientId)) %>% mutate(class='visitors')
funnel_device_active<- funnel_device %>% filter(duplicated==TRUE) %>% filter(dv %in% c("MOB","PC")) %>% 
  group_by(dv) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='active visitors')
funnel_device_cart<- funnel_device %>% filter(Event=='AddToCart') %>% filter(dv %in% c("MOB","PC")) %>% 
  group_by(dv) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='cart')
funnel_device_purchase<- funnel_device %>% filter(Event=='Purchase') %>% filter(dv %in% c("MOB","PC")) %>% 
  group_by(dv) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='purchased')

funnel_device_final<-rbind(funnel_device_visit, funnel_device_active, funnel_device_cart, funnel_device_purchase)
funnel_device_final_pc<- funnel_device_final %>% filter(dv=='PC')
funnel_device_final_pc$percentage<- round(funnel_device_final_pc$counts/funnel_device_final_pc$counts[1]*100,2)
funnel_device_final_mob<- funnel_device_final %>% filter(dv=='MOB')
funnel_device_final_mob$percentage<- round(funnel_device_final_mob$counts/funnel_device_final_mob$counts[1]*100,2)
funnel_device_final<- rbind(funnel_device_final_pc,funnel_device_final_mob)
funnel_device_final[funnel_device_final$percentage==100,"percentage"]<- 80

dev.off()
dev.new(width=8, height=7)
opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=1.5, family="STKaiti", oma=c(1.5,1.5,0,0)) 
barplot(matrix(funnel_device_final$percentage, nr=2, byrow=TRUE), col=c(rgb(20, 61, 85, max=255), rgb(40, 122, 169, max=255)), ylim=c(0, 85), ylab="轉換率 (%)", axes=FALSE, beside=T, names.arg=c("瀏覽", "有效瀏覽", "購物車", "完成購物"))
axis(2, las=1,line=0, at=seq(0,80,10), labels=c(seq(0,70,10), 100), tck=0.01, cex=1)
#axis(2, las=1,line=-1, at=seq(0,100,20), labels=seq(0,100,20) ,tck=0.01, cex=1.2,lwd=2)
#text(1.2, 17538, expression(paste(bold("100%"))), col=rgb(30, 144, 255, max=255), cex=1, adj=0)
text(4.1, funnel_device_final$percentage[2]+5, funnel_device_final$percentage[2], col=rgb(20, 61, 85, max=255), cex=2, adj=0)
text(7.2, funnel_device_final$percentage[3]+5, funnel_device_final$percentage[3], col=rgb(20, 61, 85, max=255), cex=2, adj=0)
text(10.2, funnel_device_final$percentage[4]+5, funnel_device_final$percentage[4], col=rgb(20, 61, 85, max=255), cex=2, adj=0)
#text(2.2, 19762, expression(paste(bold("100%"))), col=rgb(150, 150, 150, max=255), cex=1, adj=0)
text(5.2, funnel_device_final$percentage[6]+5, funnel_device_final$percentage[6], col=rgb(40, 122, 169, max=255), cex=2, adj=0)
text(8.2, funnel_device_final$percentage[7]+5, funnel_device_final$percentage[7], col=rgb(40, 122, 169, max=255), cex=2, adj=0)
text(11.2, funnel_device_final$percentage[8]+5, funnel_device_final$percentage[8], col=rgb(40, 122, 169, max=255), cex=2, adj=0)
axis.break(72,73, style="gap")
box()
legend(x= 9.5,y= 73, c("Desktop","Mobile"), pch=15, 
       col=c(rgb(20, 61, 85, max=255), rgb(40, 122, 169, max=255)), 
       bty="n", cex=1.8)      
par(opar)



########################################################################
############################流量來源轉換漏斗表現 ######################
########################################################################
###################################流量類型漏斗######################################
funnel_source<-mydata %>% select(clientId, session, Event, referrer)

funnel_source$referrer[funnel_source$referrer=='Bing'] <- "others"
funnel_source$referrer[funnel_source$referrer=='Yahoo'] <- "others"


funnel_source$duplicated<-duplicated(funnel_source$clientId)
funnel_source_visit<- funnel_source %>% group_by(referrer) %>% 
  summarise(counts=n_distinct(clientId)) %>% mutate(class='visitors')
funnel_source_active<- funnel_source %>% filter(duplicated==TRUE) %>% 
  group_by(referrer) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='active visitors')
funnel_source_cart<- funnel_source %>% filter(Event=='AddToCart') %>% 
  group_by(referrer) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='cart')
funnel_source_purchase<- funnel_source %>% filter(Event=='Purchase')  %>% 
  group_by(referrer) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='purchased')

funnel_source_final<-rbind(funnel_source_visit, funnel_source_active, funnel_source_cart, funnel_source_purchase)
funnel_source_final_Facebook<- funnel_source_final %>% filter(referrer=='Facebook')
funnel_source_final_Facebook$percentage<- round(funnel_source_final_Facebook$counts/funnel_source_final_Facebook$counts[1]*100,2)
funnel_source_final_Direct<- funnel_source_final %>% filter(referrer=='Direct')
funnel_source_final_Direct$percentage<- round(funnel_source_final_Direct$counts/funnel_source_final_Direct$counts[1]*100,2)
funnel_source_final_Google<- funnel_source_final %>% filter(referrer=='Google')
funnel_source_final_Google$percentage<- round(funnel_source_final_Google$counts/funnel_source_final_Google$counts[1]*100,2)
funnel_source_final_others<- funnel_source_final %>% filter(referrer=='others')
funnel_source_final_others$percentage<- round(funnel_source_final_others$counts/funnel_source_final_others$counts[1]*100,2)

funnel_source_final<- rbind(funnel_source_final_Direct,
                            funnel_source_final_Facebook,
                            funnel_source_final_Google,
                            funnel_source_final_others)
funnel_source_final[funnel_source_final$percentage==100,"percentage"]<- 80

dev.off()
dev.new(width=8, height=7)
opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=1.5, family="STKaiti", oma=c(1.5,1.5,0,0)) 
barplot(matrix(funnel_source_final$percentage, nr=4, byrow=TRUE), col=c(rgb(42,78,108, max=255), rgb(63,95,122, max=255), rgb(105,131,152, max=255), rgb(169, 184, 196, max=255)), ylim=c(0, 85), ylab="轉換率 (%)", axes=FALSE, beside=T, names.arg=c("瀏覽", "有效瀏覽", "購物車", "完成購物"))
axis(2, las=1,line=0, at=seq(0,80,10), labels=c(seq(0,70,10), 100), tck=0.01, cex=1)


#text(5.9, funnel_source_final$percentage[2]+5, funnel_source_final$percentage[2], col=rgb(42,78,108, max=255), cex=2, adj=0)
text(11.1, funnel_source_final$percentage[3]+5, funnel_source_final$percentage[3], col=rgb(42,78,108, max=255), cex=2, adj=0)
text(16.0, funnel_source_final$percentage[4]+5, funnel_source_final$percentage[4], col=rgb(42,78,108, max=255), cex=2, adj=0)

text(7.0, funnel_source_final$percentage[6]+5, funnel_source_final$percentage[6], col=rgb(63,95,122, max=255), cex=1.8, adj=0)
text(12.1, funnel_source_final$percentage[7]+5, funnel_source_final$percentage[7], col=rgb(63,95,122, max=255), cex=2, adj=0)
text(17.1, funnel_source_final$percentage[8]+5, funnel_source_final$percentage[8], col=rgb(63,95,122, max=255), cex=2, adj=0)

text(8.0, funnel_source_final$percentage[10]+5, funnel_source_final$percentage[10], col=rgb(105,131,152, max=255), cex=2, adj=0)
text(13.1, funnel_source_final$percentage[11]+5, funnel_source_final$percentage[11], col=rgb(105,131,152, max=255), cex=2, adj=0)
text(18.1, funnel_source_final$percentage[12]+5, funnel_source_final$percentage[12], col=rgb(105,131,152, max=255), cex=2, adj=0)

text(9.0, funnel_source_final$percentage[14]+5, funnel_source_final$percentage[10], col=rgb(169, 184, 196, max=255), cex=2, adj=0)
text(14.2, funnel_source_final$percentage[15]+5, funnel_source_final$percentage[11], col=rgb(169, 184, 196, max=255), cex=2, adj=0)
text(19.2, funnel_source_final$percentage[16]+5, funnel_source_final$percentage[12], col=rgb(169, 184, 196, max=255), cex=2, adj=0)

axis.break(72,73, style="gap")
box()
legend(x= 11.5,y= 73, c("Direct","Facebook","Google","others"), pch=15, 
       col=c(rgb(42,78,108, max=255), rgb(63,95,122, max=255), rgb(105,131,152, max=255), rgb(169, 184, 196, max=255)), 
       bty="n", cex=2)      
par(opar)







########################################################################
############################商品關聯性 ######################
########################################################################
########################## viewcontent產品主分類 product_category1 #####
product_num<-as.data.frame(sort(table(mydata_ViewContent$product_name)))
####Start to using clientID and 主分類 to run arules
###Client Amount= 18833
###Big category amount=380
mydf_product_main<- mydata_purchase %>% select(clientId, product_name) %>% 
  filter(product_name!="") %>% filter(product_name!=" ")

mydf_product_main$product_name <-as.factor(mydf_product_main$product_name)
basket_main<-split(mydf_product_main$product_name,mydf_product_main$clientId)
basket_main<-basket_main[lapply(basket_main, length) > 0]
for(i in 1:length(basket_main)){
  basket_main[[i]]<-unique(basket_main[[i]])
}
basket_tran_main<-as(basket_main,"transactions")
#itemFrequencyPlot(basket_tran_big,topN=11,type="absolute")

rules_support_main<-apriori(basket_tran_main,parameter=list(supp=0.01,conf = 0.2,maxlen=3))
rules_support_main<-sort(rules_support_main, by="confidence", decreasing=TRUE)
inspect(rules_support_main)
subrules_support_main<-rules_support_main[c(1:10,12:14)]
inspect(subrules_support_main)
plot_subrules_support_main<-subrules_support_main[c(1:12)]
plot(plot_subrules_support_main,method="graph",vertex.label.family = "STKaiti",
     main= "",
     shading=NA,measure='confidence',control=list(cex=1.5,nodeCol='#104E8B'))

subrules_support_main_df<-as(subrules_support_main, "data.frame");
subrules_support_main_df<-subrules_support_main_df %>% mutate()
subrules_support_main_df$Support_Number=as.integer(7032*subrules_support_main_df$support)
subrules_support_main_df$Confidence_Number=as.integer(subrules_support_main_df$Support_Number*subrules_support_main_df$confidence)
subrules_support_main_df$support<-percent(subrules_support_main_df$support)
subrules_support_main_df$confidence<-percent(subrules_support_main_df$confidence)
subrules_support_main_df<-subrules_support_main_df[c(1,5,6,2,3)]





########################################################################
############################優先汰換商品 ######################
########################################################################

product_view<- mydata %>% filter(Event=='ViewContent') %>% group_by(product_name) %>% 
  dplyr::summarise(view_count=n())
product_addtocart<- mydata %>% filter(Event=='AddToCart') %>% group_by(product_name) %>%
  dplyr::summarise(cart_count=n())
product_final<- merge(product_view, product_addtocart, by='product_name', all=TRUE)
product_final[is.na(product_final)] <- 0
product_final$cart_rate<- round((product_final$cart_count/product_final$view_count)*100,2)
product_final<- product_final %>% arrange(desc(cart_rate))
product_final<- product_final %>% filter(view_count>=100)

dev.off()
dev.new(width=3, height=3)
par(cex.axis=2, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), mar=c(5, 5, 5, 5), col.lab="grey10", col.axis="grey10", col.main="grey10") 
plot(product_final$view_count, product_final$cart_count, xlim=c(0,6000), ylim=c(0, 500), pch=20, cex=0.3, col=rgb(20, 61, 85, max=255), axes=FALSE, xlab="商品瀏覽次數 (1,000)", ylab="加入購物車")
axis(1, las=1, tck=0.01, col="grey10", at=seq(0, 6000, 1000), labels=seq(0,6, 1), cex.axis=1.5)
axis(2, las=1, tck=0.01, col="grey10", at=seq(0, 500, 100), labels=seq(0, 500, 100), cex.axis=1.5)
abline(a=0,b=1/12, lty="dashed", col=rgb(126, 146, 52, max=255), lwd=1)

legend(4000,350, "商品項目", pch=15, col=rgb(20, 61, 85, max=255), box.col="transparent",cex=1)
#polygon(c(0,0, 50000,50000), c(0,0.05,0.05,0), border=rgb(255, 153, 51, max=255))


product_final_bad<- product_final %>% arrange(cart_rate,desc(view_count))
product_final_bad_5<- head(product_final_bad,5) %>% arrange(desc(view_count))

dev.off()
dev.new(width=3.5, height=3)
opar<-par(mfrow=c(1,1), cex.axis=1, cex.lab=1, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(5, 5, 5 ,5)) 
barplot(product_final_bad_5$view_count[1:5], col=rgb(20,61,85, max=255), xlim=c(0, 1000), ylab="", axes=FALSE, main="", horiz=TRUE, space=1.5, width=1.5, border=NA, ylim=c(16,0))
text(0, 1.6,"愛", cex=1.05, adj=0)
text(0, 5.4,"動物本能", cex=0.9, adj=0)
text(0, 9.1,"Fall in", cex=0.8, adj=0)
text(0, 12.8,"不要忘記 不想忘記 不會忘記", cex=0.9, adj=0)
#text(0, 16,"Panasonic國際牌 視訊盒", cex=0.9, adj=0)
mtext("Lune (白/黑)", side=1, line=-0.5, at=0, cex=0.9, adj=0)
mtext(expression(bold(1)), side=2, line=1, at=3, adj=0, cex=1, las=1, family="sans", col=rgb(40, 122, 169, max=255))
mtext(expression(bold(2)), side=2, line=1, at=6.8, adj=0, cex=1, las=1, family="sans", col=rgb(40, 122, 169, max=255))
mtext(expression(bold(3)), side=2, line=1, at=10.6, adj=0, cex=1, las=1, family="sans", col=rgb(40, 122, 169, max=255))
mtext(expression(bold(4)), side=2, line=1, at=14.4, adj=0, cex=1, las=1, family="sans", col=rgb(40, 122, 169, max=255))
mtext(expression(bold(5)), side=2, line=1, at=18, adj=0, cex=1, las=1, family="sans", col=rgb(40, 122, 169, max=255))
text(30, 3,format(product_final_bad_5$view_count[1], big.mark=","), adj=0, cex=0.8, las=1, col="white",family="sans")
text(100, 3,"瀏覽次數", adj=0, cex=0.8, las=1, col="white",family="STKaiti")
text(30, 6.8,format(product_final_bad_5$view_count[2], big.mark=","), adj=0, cex=0.8, las=1, col="white",family="sans")
text(30, 10.6,format(product_final_bad_5$view_count[3], big.mark=","), adj=0, cex=0.8, las=1, col="white",family="sans")
text(30, 14.4,format(product_final_bad_5$view_count[4], big.mark=","), adj=0, cex=0.8, las=1, col="white",family="sans")
mtext(format(product_final_bad_5$view_count[5], big.mark=","), at=30, side=1, line=0.5, adj=0, cex=0.8, col="white",family="sans")
par(opar)




########################################################################
############################客戶狀態分群(new) ######################
########################################################################
# - 未開發 Unactivated: bounced(one session with 1 page view)
# - 新使用者 New users: never purchased
# - 購買過一次 One-time buyers: only one purchase ever
# - 零星購買者 Sporadic Customers, Sporadic buyers: infrequent purchasers (buy more than once)
# - 忠實客戶 Loyal Customers, Power users: very frequent purchasers(buy more than once and has come directly to the website at least once)
length(unique(mydata$clientId))

customer_status<- data.frame(clientId=unique(mydata$clientId),status=NA)
customer_status$clientId <- as.character(customer_status$clientId)

Unactivated_id<- mydata %>% group_by(clientId) %>% summarise(count=n()) %>% filter(count==1)
Unactivated_id<- Unactivated_id$clientId
customer_status$status[customer_status$clientId %in% Unactivated_id] <- "未開發"

purchased_id<- unique(mydata_purchase$clientId)
newuser_id<- mydata %>% filter(!(clientId %in% purchased_id)) %>% filter(!(clientId %in% Unactivated_id))
newuser_id<- unique(newuser_id$clientId)
customer_status$status[customer_status$clientId %in% newuser_id] <- "新使用者"

onetime_buyers_id<- mydata_purchase %>% group_by(clientId, purchase_number) %>% summarise(count=n()) %>% 
  group_by(clientId) %>% summarise(purchase_count=n()) %>% filter(purchase_count==1)
onetime_buyers_id<- unique(onetime_buyers_id$clientId)
customer_status$status[customer_status$clientId %in% onetime_buyers_id] <- "購買過一次"

power_users_id<- mydata_purchase %>% filter(!(clientId %in% onetime_buyers_id)) %>% 
  group_by(clientId, referrer) %>% summarise(count=n()) %>% filter(referrer=='Direct')
power_users_id<- unique(power_users_id$clientId)
customer_status$status[customer_status$clientId %in% power_users_id] <- "忠實客戶"

sporadic_id<- mydata_purchase %>% filter(!(clientId %in% onetime_buyers_id)) %>%
  filter(!(clientId %in% power_users_id))
sporadic_id<- unique(sporadic_id$clientId)
customer_status$status[customer_status$clientId %in% sporadic_id] <- "零星購買者"


Unactivated<- customer_status %>% filter(status=='未開發')
customer_status_table<- data.frame(sort(table(customer_status$status),decreasing = TRUE))
target<- c('未開發','新使用者','購買過一次','零','忠實客戶')
customer_status_table<-customer_status_table[match(target, customer_status_table$Var1),]

dev.off()
dev.new(width=8, height=7)
opar<- par(mfrow=c(1,1), cex.axis=1, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3.5, 1 ,0)) 
barplot(customer_status_table$Freq[1:5], col=rgb(20, 61, 85, max=255), ylim=c(0, 20000), ylab="", axes=FALSE, main="", border=NA, space=0.5)#, names.arg=cluster2_top$Var1[1:10]
text(x=seq(1,15,1.5),y=c(1,1), labels = customer_status_table$Var1[1:5], srt = 0, pos = 1, xpd = TRUE)

axis(2, las=1, line=-0.5, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 20000, 4000))
mtext(seq(0, 20000, 4000), las=2, side=2, line=-0.3, col="grey10", at=seq(0, 20000, 4000))
mtext("族群人數", side=2, line=1.7, col="grey10",cex=2)
mtext("狀態", side=1, line=2, col="grey10",cex=2)
text(1.5, customer_status_table[1,2]+800, customer_status_table$Freq[1], col="grey10", adj=1, cex=1.8)
text(3.1, customer_status_table[2,2]+800, customer_status_table$Freq[2], col="grey10", adj=1, cex=1.8)
text(4.3, customer_status_table[3,2]+800, customer_status_table$Freq[3], col="grey10", adj=1, cex=1.8)
text(5.6, customer_status_table[4,2]+800, customer_status_table$Freq[4], col="grey10", adj=1, cex=1.8)
text(7.2, customer_status_table[5,2]+800, customer_status_table$Freq[5], col="grey10", adj=1, cex=1.8)




########################################################################
############################品牌力指數(new) ######################
########################################################################
referrer <- mydata %>% group_by(clientId, session) %>% slice(which.min(actual_time))   

referrer_table<- referrer %>% group_by(referrer) %>% summarise(count=n()) 
referrer_table$referrer[referrer_table$referrer=='Bing']<-'others'
referrer_table$referrer[referrer_table$referrer=='Yahoo']<-'others'
referrer_table<- referrer_table %>% group_by(referrer) %>% summarise(count=sum(count)) 
referrer_table$percentage<- round(100*(referrer_table$count/sum(referrer_table$count)),0)


dev.off()
dev.new(width=8, height=7)
#opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
opar<- par(mfrow=c(1,1),cex=1.3, cex.axis=2, cex.lab=1, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(0, 3, 0 ,2)) 
doughnut(referrer_table$count, inner.radius=0.5, col=c(rgb(171, 130, 255, max=255), rgb(20, 61, 85, max=255), rgb(40, 122, 169, max=255), rgb(126, 146, 52 , max=255), rgb(204, 204, 204, max=255),rgb(204, 204, 204, max=255)), labels=c('23% Direct', '73% Facebook', '3% Google', '1% others'), clockwise=TRUE, init.angle=20)
text(0,0,"流量來源", cex=1.2)

#rgb(240, 128, 128, max=255), 


########################################################################
####################### 看有轉換的人top轉換路徑 ######################
########################################################################
mydata_purchase_alllog<-mydata %>% filter(clientId %in% unique(mydata_purchase$clientId))


purchase_referrer_process<-mydata_purchase_alllog %>% group_by(clientId,session,referrer) %>% 
  dplyr::summarise(process=paste(referrer,collapse=' -> '))

purchase_referrer_process<-purchase_referrer_process %>% group_by(clientId) %>% 
  dplyr::summarise(process=paste(referrer,collapse=' -> '))

process_df<-as.data.frame(sort(table(purchase_referrer_process$process), decreasing = TRUE))


purchase_inweb_process<-mydata_purchase_alllog %>% group_by(clientId,session,Event) %>% 
  dplyr::summarise(process=paste(Event,collapse=' -> '))

purchase_inweb_process<-purchase_inweb_process %>% group_by(clientId) %>% 
  dplyr::summarise(process=paste(Event,collapse=' -> '))

process_inweb_df<-as.data.frame(sort(table(purchase_inweb_process$process), decreasing = TRUE))



#####LTV Calculation
ltv_event<-c('ViewContent', 'AddToCart', 'Checkout', 'Purchase')
ltv_table1_1<-mydata %>% filter(!is.na(product_name)) %>% filter(Event %in% ltv_event) %>%
  group_by(clientId,Event,product_name,actual_date) %>% summarise(count=n())

ltv_table1_2<- mydata %>% filter(!is.na(product_name)) %>% filter(Event %in% ltv_event) %>%
  group_by(product_name, Event) %>% summarise(count=n())


ltv_table1_3<- mydata %>% filter(!is.na(product_name)) %>% 
  group_by(product_name, product_price) %>% summarise(count=n())
ltv_table1_3$product_price<- sub("T\\$","", ltv_table1_3$product_price)
ltv_table1_3$product_price<- sub("\\,","", ltv_table1_3$product_price)
ltv_table1_3<- ltv_table1_3 %>% group_by(product_name, product_price) %>% summarise(count=sum(count))
#ltv_table1_3<- ltv_table1_3 %>% group_by(product_name) %>% slice(which.max(count)) 


#  df2 <- df %>% group_by(clientId, session) %>% slice(which.min(actual_time)) 
















# #https://www.etungo.com.tw/
# # - 產品關聯
# # - 漏斗
# # - 瀏覽、銷售熱門時段
# # - 回購狀況、留存
# # - 優先汰換商品
# # - 分族群輪廓etungo主分頁）
# # - 其他
# #   - 找出有規律性被購買/瀏覽的商品
# #   - 潛力商品？
# library(arules)
# library(arulesViz)
# library(plotrix)
# 
# eval(parse("sop_function.R", encoding="UTF-8"))
# eval(parse("cluster_function.R", encoding="UTF-8"))
# 
# #mydata<-read.csv('final_nissan_old.csv',colClasses=c(rep("character",25)))
# #mydata$X<-NULL
# #mydata<-set_column_type(mydata)
# 
# myjson11<-jsonlite::fromJSON('_clientId_json/type1_event1.json', flatten = TRUE)
# myjson12<-jsonlite::fromJSON('_clientId_json/type1_event2.json', flatten = TRUE)
# myjson13<-jsonlite::fromJSON('_clientId_json/type1_event3.json', flatten = TRUE)
# myjson14<-jsonlite::fromJSON('_clientId_json/type1_event4.json', flatten = TRUE)
# myjson15<-jsonlite::fromJSON('_clientId_json/type1_event5.json', flatten = TRUE)
# 
# myjson22<-jsonlite::fromJSON('_clientId_json/type2_event2.json', flatten = TRUE)
# myjson23<-jsonlite::fromJSON('_clientId_json/type2_event3.json', flatten = TRUE)
# myjson24<-jsonlite::fromJSON('_clientId_json/type2_event4.json', flatten = TRUE)
# myjson25<-jsonlite::fromJSON('_clientId_json/type2_event5.json', flatten = TRUE)
# 
# myjson31<-jsonlite::fromJSON('_new_data/id_event1.json', flatten = TRUE)
# myjson32<-jsonlite::fromJSON('_new_data/id_event2.json', flatten = TRUE)
# myjson33<-jsonlite::fromJSON('_new_data/id_event3.json', flatten = TRUE)
# myjson34<-jsonlite::fromJSON('_new_data/id_event4.json', flatten = TRUE)
# myjson35<-jsonlite::fromJSON('_new_data/id_event5.json', flatten = TRUE)
# 
# #myjson<-remove_id(myjson, c('1705117656.1467707925','1957040293.1471416113'))
# myjson<-rbind.fill(myjson11,myjson12,myjson13,myjson14,myjson15,
#                    myjson22,myjson23,myjson24,myjson25,
#                    myjson31,myjson32,myjson33,myjson34,myjson35)
# colnames(myjson)[which(names(myjson) == "ts")] <- "Time"
# colnames(myjson)[which(names(myjson) == "ip")] <- "IP"
# 
# 
# #mydata_old<-mydata
# 
# mydata<-my_data_manipulate(myjson)
# mydata<-my_session6(mydata)
# 
# #mydata3<- mydata2 %>% select(1,4,6,54,41) %>% arrange(clientId, actual_time)
# 
# mydata<-mydata[,c(2,44,45,3,46:53,16:43,4:15,1)]
# mydata$clientId <- ifelse(!is.na(mydata$clientId), mydata$clientId, mydata$gid)
# 
# mydata <- mydata[!grepl("qas.etungo.com.tw", mydata$urlCurrent),]
# mydata <- mydata[!grepl("qas.etungo.com.tw", mydata$urlReferrer),]
# mydata <- mydata[!grepl("test.etungo.com.tw", mydata$urlCurrent),]
# mydata <- mydata[!grepl("test.etungo.com.tw", mydata$urlReferrer),]
# 
# colnames(mydata)[which(names(mydata) == "event")] <- "Event"
# colnames(mydata)[which(names(mydata) == "product_category0")] <- "Product_Category0"
# colnames(mydata)[which(names(mydata) == "product_category1")] <- "Product_Category1"
# colnames(mydata)[which(names(mydata) == "product_category2")] <- "Product_Category2"
# colnames(mydata)[which(names(mydata) == "product_category3")] <- "Product_Category3"
# colnames(mydata)[which(names(mydata) == "product_category4")] <- "Product_Category4"
#  
# mydata_purchased<-mydata %>% filter(Event=='Purchased')
# mydata_ViewContent<- mydata %>% filter(Event=='ViewContent')
# 
# 
# rm(myjson11,myjson12,myjson13,myjson14,myjson15,myjson22,myjson23,myjson24,myjson25,myjson31,myjson32,myjson33,myjson34,myjson35)
# ################################################################################### 
# ##################################### arules ###################################### 
# ###################################################################################
# 
# #################################### viewcontent產品主分類 product_category1 ######
# 
# ####Start to using clientID and 主分類 to run arules
# ###Client Amount= 168326
# ###Big category amount=1538
# mydf_product_main<- mydata_ViewContent %>% select(clientId, Product_Category1) %>% 
#   filter(Product_Category1!="") %>% filter(Product_Category1!=" ") %>%
#   filter(!is.na(clientId)) %>% filter(clientId!="") %>% filter(clientId!=" ")
# 
# mydf_product_main$Product_Category1 <-as.factor(mydf_product_main$Product_Category1)
# basket_main<-split(mydf_product_main$Product_Category1,mydf_product_main$clientId)
# basket_main<-basket_main[lapply(basket_main, length) > 0]
# for(i in 1:length(basket_main)){
#   basket_main[[i]]<-unique(basket_main[[i]])
# }
# basket_tran_main<-as(basket_main,"transactions")
# #itemFrequencyPlot(basket_tran_big,topN=11,type="absolute")
# 
# rules_support_main<-apriori(basket_tran_main,parameter=list(supp=0.005,conf = 0.005,maxlen=3))
# rules_support_main<-sort(rules_support_main, by="confidence", decreasing=TRUE)
# inspect(rules_support_main)
# subrules_support_main<-rules_support_main[c(1:4,6:12)]
# inspect(subrules_support_main)
# plot_subrules_support_main<-subrules_support_main[c(1:7)]
# plot(plot_subrules_support_big,method="graph",measure='confidence', shading='support'
#      ,vertex.label.family = "STKaiti",main= "etungo Assosiation Analysis by Big Category")
# 
# subrules_support_main_df<-as(subrules_support_main, "data.frame");
# subrules_support_main_df<-subrules_support_main_df %>% mutate()
# subrules_support_main_df$Support_Number=as.integer(7032*subrules_support_main_df$support)
# subrules_support_main_df$Confidence_Number=as.integer(subrules_support_main_df$Support_Number*subrules_support_main_df$confidence)
# subrules_support_main_df$support<-percent(subrules_support_main_df$support)
# subrules_support_main_df$confidence<-percent(subrules_support_main_df$confidence)
# subrules_support_main_df<-subrules_support_main_df[c(1,5,6,2,3)]
# 
# #################################### viewcontent產品大分類 product_category2 ######
# 
# ####Start to using clientID and 大分類 to run arules
# ###Client Amount= 168326
# ###Big category amount=1538
# mydf_product_big<- mydata_ViewContent %>% select(clientId, Product_Category2) %>% 
#   filter(Product_Category2!="") %>% filter(Product_Category2!=" ") %>%
#   filter(!is.na(clientId)) %>% filter(clientId!="") %>% filter(clientId!=" ")
# 
# mydf_product_big$Product_Category2 <-as.factor(mydf_product_big$Product_Category2)
# basket_big<-split(mydf_product_big$Product_Category2,mydf_product_big$clientId)
# basket_big<-basket_big[lapply(basket_big, length) > 0]
# for(i in 1:length(basket_big)){
#   basket_big[[i]]<-unique(basket_big[[i]])
# }
# basket_tran_big<-as(basket_big,"transactions")
# #itemFrequencyPlot(basket_tran_big,topN=11,type="absolute")
# 
# rules_support_big<-apriori(basket_tran_big,parameter=list(supp=0.005,conf = 0.20,maxlen=3))
# rules_support_big<-sort(rules_support_big, by="confidence", decreasing=TRUE)
# inspect(rules_support_big)
# subrules_support_big<-rules_support_big[c(1:4,6:7)]
# inspect(subrules_support_big)
# plot_subrules_support_big<-subrules_support_big[c(1:6)]
# plot(plot_subrules_support_big,method="graph",measure='confidence', shading=NA
#      ,vertex.label.family = "STKaiti",main= "", control=list(cex=2,nodeCol='#104E8B'))
# 
# subrules_support_big_df<-as(subrules_support_big, "data.frame");
# subrules_support_big_df<-subrules_support_big_df %>% mutate()
# subrules_support_big_df$Support_Number=as.integer(7032*subrules_support_big_df$support)
# subrules_support_big_df$Confidence_Number=as.integer(subrules_support_big_df$Support_Number*subrules_support_big_df$confidence)
# subrules_support_big_df$support<-percent(subrules_support_big_df$support)
# subrules_support_big_df$confidence<-percent(subrules_support_big_df$confidence)
# subrules_support_big_df<-subrules_support_big_df[c(1,5,6,2,3)]
# 
# #################################### viewcontent產品中分類 product_category3 ######
# ####Start to using clientID and product_category3 to run arules
# ###Client Amount= 155060
# ###category amount=927
# data_arules_medium<- mydata_ViewContent %>% select(clientId, Product_Category3) %>% 
#   filter(Product_Category3!="") %>% filter(Product_Category3!=" ") %>%
#   filter(!is.na(clientId)) %>% filter(clientId!="") %>% filter(clientId!=" ")
# 
# basket_medium<-split(data_arules_medium$Product_Category3,data_arules_medium$clientId)
# basket_medium<-basket_medium[lapply(basket_medium, length) > 0]
# for(i in 1:length(basket_medium)){
#   basket_medium[[i]]<-unique(basket_medium[[i]])
# }
# basket_tran_medium<-as(basket_medium,"transactions")
# #itemFrequencyPlot(basket_tran_temp,topN=11,type="absolute")
# 
# rules_medium<-apriori(basket_tran_medium,parameter=list(supp=0.005,conf = 0.15,maxlen=2))
# rules_medium<-sort(rules_medium, by="confidence", decreasing=TRUE)
# inspect(rules_medium)
# rules_medium<-rules_medium[c(1:6)]
# inspect(rules_medium)
# plot_subrules_medium<-rules_medium[c(1:6)]
# plot(plot_subrules_medium,method="graph",vertex.label.family = "STKaiti",
#      main= "",
#      shading=NA,measure='confidence', control=list(cex=2,nodeCol='#104E8B'))
# 
# rules_medium_df<-as(rules_medium, "data.frame");
# rules_medium_df$Support_Number=as.integer(product_medium*rules_medium_df$support)
# rules_medium_df$Confidence_Number=as.integer(rules_medium_df$Support_Number*rules_medium_df$confidence)
# rules_medium_df$support<-percent(rules_medium_df$support)
# rules_medium_df$confidence<-percent(rules_medium_df$confidence)
# 
# #################################### viewcontent產品名 product_name ##########
# ####Start to using clientID and product_name to run arules
# ###Client Amount= 168362
# ###category amount= 6566
# data_arules_title<- mydata_ViewContent %>% select(clientId, product_name) %>% 
#   filter(product_name!="") %>% filter(product_name!=" ") %>%
#   filter(!is.na(clientId)) %>% filter(clientId!="") %>% filter(clientId!=" ")
# 
# product_title<-length(unique(data_arules_title$clientId))
# length(unique(data_arules_title$product_name))
# 
# basket_title<-split(data_arules_title$product_name,data_arules_title$clientId)
# basket_title<-basket_title[lapply(basket_title, length) > 0]
# for(i in 1:length(basket_title)){
#   basket_title[[i]]<-unique(basket_title[[i]])
# }
# basket_tran_title<-as(basket_title,"transactions")
# #itemFrequencyPlot(basket_tran_temp,topN=11,type="absolute")
# 
# rules_title<-apriori(basket_tran_title,parameter=list(supp=0.002,conf = 0.15,maxlen=2))
# rules_title<-sort(rules_title, by="confidence", decreasing=TRUE)
# inspect(rules_title)
# rules_title<-rules_title[c(1:4)]
# inspect(rules_title)
# plot_subrules_title<-rules_title[c(1:4)]
# plot(plot_subrules_title,method="graph",vertex.label.family = "STKaiti",
#      main= "",
#      shading=NA,measure='confidence',control=list(cex=1.5,nodeCol='#104E8B'))
# 
# rules_title_df<-as(rules_title, "data.frame");
# rules_title_df$Support_Number=as.integer(product_title*rules_title_df$support)
# rules_title_df$Confidence_Number=as.integer(rules_title_df$Support_Number*rules_title_df$confidence)
# rules_title_df$support<-percent(rules_title_df$support)
# rules_title_df$confidence<-percent(rules_title_df$confidence)
# 
# 
# 
# 
# 
# 
# 
# #################################### mapping產品id和產品名稱######
# # arules_purchased <- mydata_purchased %>% select(clientId, product_productId) %>% 
# #   filter(product_productId!="") %>% filter(product_productId!=" ") %>%
# #   filter(!is.na(clientId)) %>% filter(clientId!="") %>% filter(clientId!=" ")
# # colnames(arules_purchased)[which(names(arules_purchased) == "product_productId")] <- "product_id"
# # arules_viewproduct_list <- mydata_ViewContent %>% select(product_id, product_name)
# # arules_viewproduct_list<- arules_viewproduct_list[!duplicated(arules_viewproduct_list), ]
# # arules_purchased2<- merge(arules_purchased,arules_viewproduct_list, by='product_id',all=FALSE)
# 
# 
# #################################### purchase產品 product_category2 ######
# #################################### purchase產品 product_category3 ######
# #################################### purchase產品 product_name ######
# ####Start to using clientID and product_category3 to run arules
# ###Client Amount= 1567
# ###category amount= 638
# arules_purchased <- mydata_purchased %>% select(clientId, product_productId) %>% 
#   filter(product_productId!="") %>% filter(product_productId!=" ") %>%
#   filter(!is.na(clientId)) %>% filter(clientId!="") %>% filter(clientId!=" ")
# 
# product_title<-length(unique(arules_purchased$clientId))
# length(unique(arules_purchased$product_productId))
# 
# basket_purchased_title<-split(arules_purchased$product_productId,arules_purchased$clientId)
# basket_purchased_title<-basket_purchased_title[lapply(basket_purchased_title, length) > 0]
# for(i in 1:length(basket_purchased_title)){
#   basket_purchased_title[[i]]<-unique(basket_purchased_title[[i]])
# }
# basket_purchased_tran_title<-as(basket_purchased_title,"transactions")
# #itemFrequencyPlot(basket_tran_temp,topN=11,type="absolute")
# 
# rules_purchased_title<-apriori(basket_purchased_tran_title,parameter=list(supp=0.02,conf = 0.001,maxlen=2))
# rules_purchased_title<-sort(rules_purchased_title, by="confidence", decreasing=TRUE)
# inspect(rules_purchased_title)
# rules_purchased_title<-rules_purchased_title[c(4:12)]
# inspect(rules_purchased_title)
# plot_subrules_purchased_title<-rules_purchased_title[c(1:9)]
# plot(plot_subrules_title,method="graph",vertex.label.family = "STKaiti",
#      main= "PurpleCane Assosiation Analysis by Product Name",
#      shading='support',measure='confidence')
# 
# rules_purchased_title_df<-as(rules_purchased_title, "data.frame");
# rules_purchased_title_df$Support_Number=as.integer(product_title*rules_purchased_title_df$support)
# rules_purchased_title_df$Confidence_Number=as.integer(rules_purchased_title_df$Support_Number*rules_purchased_title_df$confidence)
# rules_purchased_title_df$support<-percent(rules_purchased_title_df$support)
# rules_purchased_title_df$confidence<-percent(rules_purchased_title_df$confidence)
# 
# 
# #################################### addtocart產品 product_name ######
# #client: 6400
# #product: 1376
# mydata_addtocart<-mydata %>% filter(Event=='AddToCart')
# 
# arules_addtocart <- mydata_addtocart %>% select(clientId, Product_Category2) %>% 
#   filter(Product_Category2!="") %>% filter(Product_Category2!=" ") %>%
#   filter(!is.na(clientId)) %>% filter(clientId!="") %>% filter(clientId!=" ")
# 
# product_title<-length(unique(arules_addtocart$clientId))
# length(unique(arules_addtocart$Product_Category2))
# 
# basket_addtocart_title<-split(arules_addtocart$Product_Category2,arules_addtocart$clientId)
# basket_addtocart_title<-basket_addtocart_title[lapply(basket_addtocart_title, length) > 0]
# for(i in 1:length(basket_addtocart_title)){
#   basket_addtocart_title[[i]]<-unique(basket_addtocart_title[[i]])
# }
# basket_addtocart_tran_title<-as(basket_addtocart_title,"transactions")
# #itemFrequencyPlot(basket_tran_temp,topN=11,type="absolute")
# 
# rules_addtocart_title<-apriori(basket_addtocart_tran_title,parameter=list(supp=0.01,conf = 0.001,maxlen=2))
# rules_addtocart_title<-sort(rules_addtocart_title, by="confidence", decreasing=TRUE)
# inspect(rules_addtocart_title)
# rules_addtocart_title<-rules_addtocart_title[c(4:12)]
# inspect(rules_addtocart_title)
# plot_subrules_purchased_title<-rules_addtocart_title[c(1:9)]
# plot(plot_subrules_title,method="graph",vertex.label.family = "STKaiti",
#      main= "",
#      shading=NA,measure='confidence',control=list(cex=2))
# 
# 
# 
# 
# 
# ###################################################################################
# #################################### 漏斗 #########################################
# ###################################################################################
# my_funnel(mydata,c('ViewContent','AddToCart','Purchased'))
# 
# my_funnel(mydata,c('ViewContent','AddToCart','Purchased'),'dv')
# 
# my_funnel(mydata,c('ViewContent','AddToCart','Purchased'),'country_name')
# 
# #temp_df2 taken from 族群分析大分類
# temp_df2_nature<- temp_df2 %>% filter(source=='nature')
# my_funnel(temp_df2_nature,c('ViewContent','AddToCart','Purchased'))
# my_funnel(temp_df2_nature,c('ViewContent','AddToCart','Purchased'),'cluster')
# 
# temp_df2_paid<- temp_df2 %>% filter(source %in% c('paid','urad'))
# my_funnel(temp_df2_paid,c('ViewContent','AddToCart','Purchased'))
# my_funnel(temp_df2_paid,c('ViewContent','AddToCart','Purchased'),'cluster')
# 
# temp_df2_purchased<- temp_df2 %>% filter(Event=='Purchased')
# 
# 
# 
# country<-as.data.frame(sort(table(mydata$country_name),decreasing = TRUE))
# mydata_country<-mydata %>% filter(country_name %in% country$Var1[1:5])
# my_funnel(mydata_country,c('ViewContent','AddToCart','Purchased'),'country_name')
# 
# city<-as.data.frame(sort(table(mydata[mydata$country_name=='Taiwan',]$city_name),decreasing = TRUE))
# mydata_city<-mydata %>% filter(city_name %in% city$Var1[1:6])
# my_funnel(mydata_city,c('ViewContent','AddToCart','Purchased'),'city_name')
# 
# temp_df2_urad<-temp_df2 %>% filter(source=='urad')
# sort(table(temp_df2_urad$country_name))
# sort(table(temp_df2_urad$city_name))
# 
# ###################################裝置漏斗######################################
# funnel_device<-mydata %>% select(clientId, session, Event, dv)
# funnel_device$duplicated<-duplicated(funnel_device$clientId)
# funnel_device_visit<- funnel_device %>% filter(dv %in% c("MOB","PC")) %>% group_by(dv) %>% 
#   summarise(counts=n_distinct(clientId)) %>% mutate(class='visitors')
# funnel_device_active<- funnel_device %>% filter(duplicated==TRUE) %>% filter(dv %in% c("MOB","PC")) %>% 
#   group_by(dv) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='active visitors')
# funnel_device_cart<- funnel_device %>% filter(Event=='AddToCart') %>% filter(dv %in% c("MOB","PC")) %>% 
#   group_by(dv) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='cart')
# funnel_device_purchase<- funnel_device %>% filter(Event=='Purchased') %>% filter(dv %in% c("MOB","PC")) %>% 
#   group_by(dv) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='purchased')
# 
# funnel_device_final<-rbind(funnel_device_visit, funnel_device_active, funnel_device_cart, funnel_device_purchase)
# funnel_device_final_pc<- funnel_device_final %>% filter(dv=='PC')
# funnel_device_final_pc$percentage<- round(funnel_device_final_pc$counts/funnel_device_final_pc$counts[1]*100,2)
# funnel_device_final_mob<- funnel_device_final %>% filter(dv=='MOB')
# funnel_device_final_mob$percentage<- round(funnel_device_final_mob$counts/funnel_device_final_mob$counts[1]*100,2)
# funnel_device_final<- rbind(funnel_device_final_pc,funnel_device_final_mob)
# funnel_device_final[funnel_device_final$percentage==100,"percentage"]<- 80
# 
# dev.off()
# dev.new(width=8, height=7)
# opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=1.5, family="STKaiti", oma=c(1.5,1.5,0,0)) 
# barplot(matrix(funnel_device_final$percentage, nr=2, byrow=TRUE), col=c(rgb(20, 61, 85, max=255), rgb(40, 122, 169, max=255)), ylim=c(0, 85), ylab="轉換率 (%)", axes=FALSE, beside=T, names.arg=c("瀏覽", "有", "購物車", "完成購物"))
# axis(2, las=1,line=0, at=seq(0,80,10), labels=c(seq(0,70,10), 100), tck=0.01, cex=1)
# #axis(2, las=1,line=-1, at=seq(0,100,20), labels=seq(0,100,20) ,tck=0.01, cex=1.2,lwd=2)
# #text(1.2, 17538, expression(paste(bold("100%"))), col=rgb(30, 144, 255, max=255), cex=1, adj=0)
# text(4.1, funnel_device_final$percentage[2]+5, funnel_device_final$percentage[2], col=rgb(20, 61, 85, max=255), cex=2, adj=0)
# text(7.2, funnel_device_final$percentage[3]+5, funnel_device_final$percentage[3], col=rgb(20, 61, 85, max=255), cex=2, adj=0)
# text(10.2, funnel_device_final$percentage[4]+5, funnel_device_final$percentage[4], col=rgb(20, 61, 85, max=255), cex=2, adj=0)
# #text(2.2, 19762, expression(paste(bold("100%"))), col=rgb(150, 150, 150, max=255), cex=1, adj=0)
# text(5.2, funnel_device_final$percentage[6]+5, funnel_device_final$percentage[6], col=rgb(40, 122, 169, max=255), cex=2, adj=0)
# text(8.2, funnel_device_final$percentage[7]+5, funnel_device_final$percentage[7], col=rgb(40, 122, 169, max=255), cex=2, adj=0)
# text(11.2, funnel_device_final$percentage[8]+5, funnel_device_final$percentage[8], col=rgb(40, 122, 169, max=255), cex=2, adj=0)
# axis.break(72,73, style="gap")
# box()
# legend(x= 9.5,y= 73, c("Desktop","Mobile"), pch=15, 
#        col=c(rgb(20, 61, 85, max=255), rgb(40, 122, 169, max=255)), 
#        bty="n", cex=1.2)      
# par(opar)
# 
# 
# ###################################流量類型漏斗######################################
# funnel_source<-mydata %>% select(clientId, session, Event, utmSource)
# 
# funnel_source$source<- ifelse(funnel_source$utmSource %in% c('FB_NewsFeedAd','FB_MultiProductAd','FB_news feed ad','FB_news%20feed%20ad'),'urad',
#                               ifelse(!is.na(funnel_source$utmSource),'paid','nature'))
# 
# 
# funnel_source_match <- funnel_source %>%
#   group_by(clientId,session) %>%
#   arrange(session) %>%
#   slice(1) %>%
#   ungroup %>% select(clientId,session,source)
# 
# funnel_source<-merge(funnel_source, funnel_source_match, by=c('clientId','session'))
# funnel_source$source.x<-NULL
# colnames(funnel_source)[which(names(funnel_source) == "source.y")] <- "source"
# 
# 
# funnel_source$duplicated<-duplicated(funnel_source$clientId)
# funnel_source_visit<- funnel_source %>% group_by(source) %>% 
#   summarise(counts=n_distinct(clientId)) %>% mutate(class='visitors')
# funnel_source_active<- funnel_source %>% filter(duplicated==TRUE) %>% 
#   group_by(source) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='active visitors')
# funnel_source_cart<- funnel_source %>% filter(Event=='AddToCart') %>% 
#   group_by(source) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='cart')
# funnel_source_purchase<- funnel_source %>% filter(Event=='Purchased')  %>% 
#   group_by(source) %>% summarise(counts=n_distinct(clientId)) %>% mutate(class='purchased')
# 
# funnel_source_final<-rbind(funnel_source_visit, funnel_source_active, funnel_source_cart, funnel_source_purchase)
# funnel_source_final_urad<- funnel_source_final %>% filter(source=='urad')
# funnel_source_final_urad$percentage<- round(funnel_source_final_urad$counts/funnel_source_final_urad$counts[1]*100,2)
# funnel_source_final_paid<- funnel_source_final %>% filter(source=='paid')
# funnel_source_final_paid$percentage<- round(funnel_source_final_paid$counts/funnel_source_final_paid$counts[1]*100,2)
# funnel_source_final_nature<- funnel_source_final %>% filter(source=='nature')
# funnel_source_final_nature$percentage<- round(funnel_source_final_nature$counts/funnel_source_final_nature$counts[1]*100,2)
# 
# funnel_source_final<- rbind(funnel_source_final_urad,funnel_source_final_paid,funnel_source_final_nature)
# funnel_source_final[funnel_source_final$percentage==100,"percentage"]<- 80
# 
# dev.off()
# dev.new(width=8, height=7)
# opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=1.5, family="STKaiti", oma=c(1.5,1.5,0,0)) 
# barplot(matrix(funnel_source_final$percentage, nr=3, byrow=TRUE), col=c(rgb(30,144,255, max=255), rgb(40, 122, 169, max=255), rgb(20, 61, 85, max=255)), ylim=c(0, 85), ylab="轉換率 (%)", axes=FALSE, beside=T, names.arg=c("瀏覽", "有效瀏覽", "購物車", "完成購物"))
# axis(2, las=1,line=0, at=seq(0,80,10), labels=c(seq(0,70,10), 100), tck=0.01, cex=1)
# #axis(2, las=1,line=-1, at=seq(0,100,20), labels=seq(0,100,20) ,tck=0.01, cex=1.2,lwd=2)
# 
# text(5.1, funnel_source_final$percentage[2]+5, funnel_source_final$percentage[2], col=rgb(30,144,255, max=255), cex=2, adj=0)
# text(9.1, funnel_source_final$percentage[3]+5, funnel_source_final$percentage[3], col=rgb(30,144,255, max=255), cex=2, adj=0)
# text(13.0, funnel_source_final$percentage[4]+5, funnel_source_final$percentage[4], col=rgb(30,144,255, max=255), cex=2, adj=0)
# 
# text(6.0, funnel_source_final$percentage[6]+5, funnel_source_final$percentage[6], col=rgb(40, 122, 169, max=255), cex=2, adj=0)
# text(10.5, funnel_source_final$percentage[7]+5, funnel_source_final$percentage[7], col=rgb(40, 122, 169, max=255), cex=2, adj=0)
# text(14.0, funnel_source_final$percentage[8]+5, funnel_source_final$percentage[8], col=rgb(40, 122, 169, max=255), cex=2, adj=0)
# 
# text(7.0, funnel_source_final$percentage[10]+5, funnel_source_final$percentage[10], col=rgb(20, 61, 85, max=255), cex=2, adj=0)
# text(11.2, funnel_source_final$percentage[11]+5, funnel_source_final$percentage[11], col=rgb(20, 61, 85, max=255), cex=2, adj=0)
# text(15.2, funnel_source_final$percentage[12]+5, funnel_source_final$percentage[12], col=rgb(20, 61, 85, max=255), cex=2, adj=0)
# axis.break(72,73, style="gap")
# box()
# legend(x= 9.5,y= 73, c("urAD廣告投放","其他廣告流量","自然流量"), pch=15, 
#        col=c(rgb(30,144,255, max=255), rgb(40, 122, 169, max=255), rgb(20, 61, 85, max=255)), 
#        bty="n", cex=1.2)      
# par(opar)
# 
# 
# 
# dftest <- data.frame(id=c(1,1,1,2,2,2,3,3,3), 
#                  stopId=c("a","b","c","a","b","c","a","b","c"), 
#                  stopSequence=c(1,2,3,3,1,4,3,1,2))
# 
# firstStop <- dftest %>%
#   group_by(id) %>%
#   arrange(stopSequence) %>%
#   slice(1) %>%
#   ungroup
# 
# dftest2<-dftest %>%
#   group_by(id) %>%
#   arrange(stopSequence) %>%
#   filter(row_number()==1 | row_number()==n())
# 
# 
# mydata_source<-mydata %>% select(clientId, session, Event, utmSource,product_name:Product_Category3)
# 
# mydata_source$source<- ifelse(mydata_source$utmSource %in% c('FB_NewsFeedAd','FB_MultiProductAd','FB_news feed ad','FB_news%20feed%20ad'),'urad',
#                               ifelse(!is.na(mydata_source$utmSource),'paid','nature'))
# 
# mydata_source_urad<-mydata_source %>% filter(source=='urad')
# source_urad_big<-as.data.frame(sort(table(mydata_source_urad$Product_Category1),decreasing=TRUE))
# source_urad_product<-as.data.frame(sort(table(mydata_source_urad$product_name),decreasing=TRUE))
# ###################################################################################
# #################################### 瀏覽、銷售熱門時段 ###########################
# ###################################################################################
# 
# ##################################### 瀏覽熱門日 ################################
# my_user_time_plot(mydata,time_dimension = 'actual_date',fill_dimension = ,type = 'line',facet_or_not = 'No')
# my_user_time_plot(mydata[!is.na(mydata$dv),],time_dimension = 'weekday',fill_dimension ='dv' ,type = 'line',facet_or_not = 'Yes')
# my_user_time_plot(mydata[!is.na(mydata$dv),],time_dimension = 'hour',fill_dimension ='dv' ,type = 'line',facet_or_not = 'Yes')
# my_user_time_plot(mydata,time_dimension = 'hour',fill_dimension = ,type = 'line',facet_or_not = 'No')
# 
# multiplot(p1, p2, p3, cols=2)
# 
# #先算每天總共有多少個session
# week_visit<- mydata %>% filter(!is.na(clientId)) %>% group_by(actual_date, weekday, clientId) %>% summarise(session_count=n_distinct(session)) 
# week_visit<- week_visit %>% group_by(actual_date, weekday) %>% summarise(session_sum=sum(session_count)) 
# week_visit<- week_visit %>% group_by(weekday) %>% summarise(session_avg=mean(session_sum)) 
#   
# week_visit_hot<-c(0,0,0,week_visit$session_avg[4:5],0,0)
# 
# dev.off()
# dev.new(width=8, height=7)
# opar<- par(mfrow=c(1,1), cex.axis=2, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
# barplot(week_visit$session_avg[1:7], col=rgb(20, 61, 85, max=255), ylim=c(0, 5000), ylab="", axes=FALSE, main="", border=1, space=0.5, names.arg=week_visit$weekday[1:7])
# barplot(week_visit_hot, col=rgb(40, 122, 169, max=255), ylim=c(0, 5000), ylab="", axes=FALSE, main="", border=NA, space=0.5, add=TRUE)
# axis(2, las=3,line=-1, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 5000, 1000),cex=2)
# mtext(seq(0, 5000, 1000), las=2, side=2,cex=1.3, line=-0.8, col="grey10", at=seq(0, 5000, 1000))
# mtext("平均工作階段", side=2, line=1.5, col="grey10",cex=2)
# #mtext("回購天數", side=1, line=2, col="grey10")
# # text(1.7, interval[1,2]+15, "68%", col="grey10", adj=1, cex=0.8)
# # text(3.2, interval[2,2]+15, "13%", col="grey10", adj=1, cex=0.8)
# # text(4.6, interval[3,2]+15, "6%", col="grey10", adj=1, cex=0.8)
# # text(6.1, interval[4,2]+15, "2%", col="grey10", adj=1, cex=0.8)
# 
# 
# ##################################### 瀏覽熱門時段 ################################
# #先算每天的每個小時總共有多少個row
# hour_visit<- mydata %>% filter(!is.na(clientId)) %>% group_by(actual_date, hour) %>% summarise(view_count=n()) 
# hour_visit<- hour_visit %>% group_by(hour) %>% summarise(view_avg=mean(view_count)) 
# 
# hour_visit_hot<-c(rep(0,11),hour_visit$view_avg[12:18],rep(0,7))
# 
# dev.off()
# dev.new(width=8, height=7)
# opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
# barplot(hour_visit$view_avg[1:24], col=rgb(20, 61, 85, max=255), ylim=c(0, 1000), ylab="", axes=FALSE, main="", border=1, space=0.5, names.arg=hour_visit$hour[1:24])
# barplot(hour_visit_hot, col=rgb(40, 122, 169, max=255), ylim=c(0, 1000), ylab="", axes=FALSE, main="", border=NA, space=0.5, add=TRUE)
# axis(2, las=3,line=-1, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 1000, 200),cex=2)
# mtext(seq(0, 1000, 200), las=2, side=2,cex=1.3, line=-0.8, col="grey10", at=seq(0, 1000, 200))
# mtext("平均瀏覽頁數", side=2, line=1.5, col="grey10",cex=2)
# #mtext("回購天數", side=1, line=2, col="grey10")
# # text(1.7, interval[1,2]+15, "68%", col="grey10", adj=1, cex=0.8)
# # text(3.2, interval[2,2]+15, "13%", col="grey10", adj=1, cex=0.8)
# # text(4.6, interval[3,2]+15, "6%", col="grey10", adj=1, cex=0.8)
# # text(6.1, interval[4,2]+15, "2%", col="grey10", adj=1, cex=0.8)
# 
# 
# 
# 
# ##################################### 銷售熱門時段 ################################
# my_time_revenue_plot(data_sop,"actual_date","newold")
# my_time_revenue_plot(data_sop,"weekday","newold")
# my_time_revenue_plot(data_sop,"hour","newold")
# 
# ###################################################################################
# #################################### 找出有規律性被購買/瀏覽的商品 ################
# ###################################################################################
# 
# 
# ###################################################################################
# #################################### 回購狀況、留存 ###############################
# ###################################################################################
# mydata_return<- mydata_purchased %>% select(clientId, purchased_orderNumber, actual_time, Time) %>% 
#   arrange(clientId, Time)
# mydata_return<- mydata_return[!duplicated(mydata_return[,c(1,2)]), ]
# 
# 
# mydata_return$duplicated<- duplicated(mydata_return$clientId)
# mydata_return$Time.2<- c(0,mydata_return$Time[1:(dim(mydata_return)[1]-1)])
# mydata_return$lag<- mydata_return$Time- mydata_return$Time.2
# mydata_return[mydata_return$duplicated==FALSE,"lag"]<- -60*60*24
# mydata_return$duration<- round(mydata_return$lag/60/60/24)
# x<- mydata_return[mydata_return$duplicated==TRUE,]
# x2<- mydata_return[mydata_return$duplicated==FALSE,]
# interval<- as.data.frame(table(mydata_return$duration))
# names(interval)<-c("days","counts")
# interval$days<- as.numeric(as.character(interval$days))
# interval<- interval[interval$day>=0,]
# interval$percentage<- round(interval$counts/sum(interval$counts)*100,2)
# 
# dev.off()
# dev.new(width=8, height=7)
# opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
# barplot(interval$counts[1:15], col=rgb(20, 61, 85, max=255), ylim=c(0, 300), ylab="", axes=FALSE, main="", border=NA, space=0.5, names.arg=interval$days[1:15])
# axis(2, las=1, line=-1, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 400, 100))
# mtext(seq(0, 300, 100), las=2, side=2, line=-0.8, col="grey10", at=seq(0, 400, 100))
# mtext("回購筆數", side=2, line=1.5, col="grey10",cex=2)
# mtext("回購天數", side=1, line=2, col="grey10",cex=2)
# text(1.4, interval[1,2]+15, "69%", col="grey10", adj=1, cex=1.8)
# text(2.9, interval[2,2]+15, "13%", col="grey10", adj=1, cex=1.8)
# text(4.3, interval[3,2]+15, "6%", col="grey10", adj=1, cex=1.8)
# text(5.8, interval[4,2]+15, "3%", col="grey10", adj=1, cex=1.8)
# 
# ###################################  回站  ########################################
# mydata_retention<- mydata %>% select(clientId, actual_date, actual_time, Time) %>% 
#   arrange(clientId, actual_time, Time)
# 
# mydata_retention<- mydata_retention[!duplicated(mydata_retention[,c(1,2)]), ]
# 
# mydata_retention$duplicated<- duplicated(mydata_retention$clientId)
# mydata_retention$Time.2<- c(0,mydata_retention$Time[1:(dim(mydata_retention)[1]-1)])
# mydata_retention$lag<- mydata_retention$Time- mydata_retention$Time.2
# mydata_retention[mydata_retention$duplicated==FALSE,"lag"]<- -60*60*24
# mydata_retention$duration<- round(mydata_retention$lag/60/60/24)
# y<- mydata_retention[mydata_retention$duplicated==TRUE,]
# y2<- mydata_retention[mydata_retention$duplicated==FALSE,]
# interval<- as.data.frame(table(mydata_retention$duration))
# names(interval)<-c("days","counts")
# interval$days<- as.numeric(as.character(interval$days))
# interval<- interval[interval$day>=0,]
# interval$percentage<- round(interval$counts/sum(interval$counts)*100,2)
# 
# dev.off()
# dev.new(width=8, height=7)
# opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
# barplot(interval$counts[1:15], col=rgb(20, 61, 85, max=255), ylim=c(0, 20000), ylab="", axes=FALSE, main="", border=NA, space=0.5, names.arg=interval$days[1:15])
# axis(2, las=1, line=-1, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 18000, 3000))
# mtext(seq(0, 18000, 3000), las=1, side=2, line=-0.8, col="grey10", at=seq(0, 18000, 3000))
# mtext("回站次數", side=2, line=1.5, col="grey10",cex=2)
# mtext("回站天數", side=1, line=2, col="grey10",cex=2)
# text(1.4, interval[1,2]+500, "3%", col="grey10", adj=1, cex=1.8)
# text(3.0, interval[2,2]+500, "25%", col="grey10", adj=1, cex=1.8)
# text(4.5, interval[3,2]+500, "11%", col="grey10", adj=1, cex=1.8)
# text(5.9, interval[4,2]+500, "7%", col="grey10", adj=1, cex=1.8)
# text(7.4, interval[5,2]+500, "6%", col="grey10", adj=1, cex=1.8)
# 
# 
# dev.off()
# dev.new(width=8, height=7)
# opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
# barplot(interval$counts[1:15], col=rgb(20, 61, 85, max=255), ylim=c(0, 300), ylab="", axes=FALSE, main="", border=NA, space=0.5, names.arg=interval$days[1:15])
# axis(2, las=1, line=-1, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 400, 100))
# mtext(seq(0, 300, 100), las=2, side=2, line=-0.8, col="grey10", at=seq(0, 400, 100))
# mtext("回購筆數", side=2, line=1.5, col="grey10",cex=2)
# mtext("回購天數", side=1, line=2, col="grey10",cex=2)
# text(1.4, interval[1,2]+15, "69%", col="grey10", adj=1, cex=1.8)
# text(2.9, interval[2,2]+15, "13%", col="grey10", adj=1, cex=1.8)
# text(4.3, interval[3,2]+15, "6%", col="grey10", adj=1, cex=1.8)
# text(5.8, interval[4,2]+15, "2%", col="grey10", adj=1, cex=1.8)
# 
# 
# 
# temp_cohort<- my_cohort_df(data_sop,group_dimension =,time_period = "days", event_array =, count_unique = 1)
# temp_cohort_percent<-my_cohort_percent_df(temp_cohort)
# my_cohort_plot(temp_cohort_percent,'','days',7)
# 
# 
# 
# data_sop$Member_Status2<-factor(data_sop$Member_Status,levels=c('Yes','No'))
# temp_cohort<- my_cohort_df(data_sop,group_dimension ='Member_Status2',time_period = "days", event_array =, count_unique = 1)
# temp_cohort_percent<-my_cohort_percent_df(temp_cohort)
# my_cohort_plot(temp_cohort_percent,'Member_Status','days',7)
# 
# temp_cohort<- my_cohort_df(data_sop,group_dimension ='User_Device',time_period = "days", event_array =, count_unique = 1)
# temp_cohort_percent<-my_cohort_percent_df(temp_cohort)
# my_cohort_plot(temp_cohort_percent,'User_Device','days',7)
# 
# temp_cohort<- my_cohort_df(data_sop,group_dimension ='User_Member',time_period = "days", event_array =, count_unique = 1)
# temp_cohort_percent<-my_cohort_percent_df(temp_cohort)
# my_cohort_plot(temp_cohort_percent,'User_Member','days',7)
# 
# 
# 
# ###################################################################################
# #################################### 優先汰換商品 #################################
# ###################################################################################
# 
# product_view<- mydata %>% filter(Event=='ViewContent') %>% group_by(product_name) %>% 
#   dplyr::summarise(view_count=n())
# product_addtocart<- mydata %>% filter(Event=='AddToCart') %>% group_by(product_name) %>%
#   dplyr::summarise(cart_count=n())
# product_final<- merge(product_view, product_addtocart, by='product_name', all=TRUE)
# product_final[is.na(product_final)] <- 0
# product_final$cart_rate<- round((product_final$cart_count/product_final$view_count)*100,2)
# product_final<- product_final %>% arrange(desc(cart_rate))
# product_final<- product_final %>% filter(view_count>=100)
# 
# dev.off()
# dev.new(width=3, height=3)
# par(cex.axis=2, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), mar=c(5, 5, 5, 5), col.lab="grey10", col.axis="grey10", col.main="grey10") 
# plot(product_final$view_count, product_final$cart_count, xlim=c(0,9000), ylim=c(0, 500), pch=20, cex=0.3, col=rgb(20, 61, 85, max=255), axes=FALSE, xlab="商品瀏覽次數 (1,000)", ylab="加入購物車")
# axis(1, las=1, tck=0.01, col="grey10", at=seq(0, 10000, 2000), labels=seq(0,5, 1), cex.axis=1.5)
# axis(2, las=1, tck=0.01, col="grey10", at=seq(0, 500, 100), labels=seq(0, 500, 100), cex.axis=1.5)
# abline(a=0,b=0.05, lty="dashed", col=rgb(126, 146, 52, max=255), lwd=1)
# 
# legend(6600,350, "商品項目", pch=15, col=rgb(20, 61, 85, max=255), box.col="transparent",cex=1)
# #polygon(c(0,0, 50000,50000), c(0,0.05,0.05,0), border=rgb(255, 153, 51, max=255))
# 
# 
# product_final_bad<- product_final %>% arrange(cart_rate,desc(view_count))
# 
# 
# dev.off()
# dev.new(width=3.5, height=3)
# opar<-par(mfrow=c(1,1), cex.axis=1, cex.lab=1, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(5, 5, 5 ,5)) 
# barplot(product_final_bad$view_count[1:5], col=rgb(20,61,85, max=255), xlim=c(0, 6000), ylab="", axes=FALSE, main="", horiz=TRUE, space=1.5, width=1.5, border=NA, ylim=c(16,0))
# text(0, 1.6,"【大同3C X 心路基金會】大同寶寶中秋禮盒", cex=1.05, adj=0)
# text(0, 5.4,"【寶島之光】電子式螺旋23W省電燈泡(白/黃光)", cex=0.9, adj=0)
# text(0, 9.1,"【韓國Neoflam】Aeni系列20cm湯鍋(象牙白)", cex=0.8, adj=0)
# text(0, 12.8,"【TATUNG大同】自由配冷藏冰箱380L", cex=0.9, adj=0)
# #text(0, 16,"Panasonic國際牌 視訊盒", cex=0.9, adj=0)
# mtext("Panasonic國際牌 視訊盒", side=1, line=-0.5, at=0, cex=0.9, adj=0)
# mtext(expression(bold(1)), side=2, line=1, at=3, adj=0, cex=1, las=1, family="sans", col=rgb(40, 122, 169, max=255))
# mtext(expression(bold(2)), side=2, line=1, at=6.8, adj=0, cex=1, las=1, family="sans", col=rgb(40, 122, 169, max=255))
# mtext(expression(bold(3)), side=2, line=1, at=10.6, adj=0, cex=1, las=1, family="sans", col=rgb(40, 122, 169, max=255))
# mtext(expression(bold(4)), side=2, line=1, at=14.4, adj=0, cex=1, las=1, family="sans", col=rgb(40, 122, 169, max=255))
# mtext(expression(bold(5)), side=2, line=1, at=18, adj=0, cex=1, las=1, family="sans", col=rgb(40, 122, 169, max=255))
# text(100, 3,format(product_final_bad$view_count[1], big.mark=","), adj=0, cex=0.8, las=1, col="white",family="sans")
# text(1000, 3,"瀏覽次數", adj=0, cex=0.8, las=1, col="white",family="STKaiti")
# text(100, 6.8,format(product_final_bad$view_count[2], big.mark=","), adj=0, cex=0.8, las=1, col="white",family="sans")
# text(100, 10.6,format(product_final_bad$view_count[3], big.mark=","), adj=0, cex=0.8, las=1, col="white",family="sans")
# text(100, 14.4,format(product_final_bad$view_count[4], big.mark=","), adj=0, cex=0.8, las=1, col="white",family="sans")
# mtext(format(product_final_bad$view_count[5], big.mark=","), at=100, side=1, line=0.5, adj=0, cex=0.8, col="white",family="sans")
# par(opar)
# 
# 
# 
# 
# 
# ###################################################################################
# #################################### 分族群輪廓分析（依照etungo主分頁） ###########
# ###################################################################################
# ##### filter and reshape the data
# #L_class_df<-my_reshape_lclass(mydata,cart_factor=5)  
# 
# 
# # 映鮮嚴選：映鮮嚴選
# # 家電專區：廚電專區、家電專區
# # 居家生活：居家生活
# # 3C數位：3C數位、3C家電
# # 運動休閒：運動休閒
# # 食品：食品/飲品/休 
# 
# 
# mydata_cluster<- mydata 
# mydata_cluster$Product_Category1[mydata_cluster$Product_Category1 == "3C家電"] <- "3C數位"
# mydata_cluster$Product_Category1[mydata_cluster$Product_Category1 == "食品/飲品/休閒"] <- "食品"
# mydata_cluster$Product_Category1[mydata_cluster$Product_Category1 == "食品/休閒"] <- "食品"
#  
# mydata_cluster$Product_Category1[mydata_cluster$Product_Category1 == "廚電專"] <- "家電專區"
# 
# mydata_cluster<- mydata_cluster %>% filter(!is.na(Product_Category0)) %>% filter(!is.na(clientId)) %>%
#   filter(Product_Category1 %in% c('映鮮嚴選','家電專區','居家生活','3C數位','運動休閒','食品'))
# 
# # temp<- mydata_cluster %>% select(clientId, actual_time, actual_date, urlCurrent,Product_Category0:Product_Category4) %>% 
# #   arrange(actual_time) %>% filter(!is.na(Product_Category0))
# 
# cluster_df_1<-my_reshape2(mydata_cluster, filter_argument = "Event=='ViewContent'", reshape_column = 'Product_Category1', multiple_factor=1)
# cluster_df_2<-my_reshape2(mydata_cluster, filter_argument = "Event=='AddToCart'", reshape_column = 'Product_Category1', multiple_factor=5)
# cluster_df<-plyr::rbind.fill(cluster_df_1,cluster_df_2)
# cluster_df[is.na(cluster_df)] <- 0
# cluster_df<-cluster_df %>% dplyr::group_by(clientId) %>% dplyr::summarise_each(funs(sum))
# 
# 
# 
# ##### normalize the data
# L_class_df.scaled<-my_normalize(cluster_df)
# 
# 
# kmeans.km_3<- clara(L_class_df.scaled, 6,samples=100,sampsize=500, pamLike=TRUE)  
# km.cluster_df.scaled.cluster<-my_cluster_df(cluster_df, L_class_df.scaled, kmeans.km_3)
# 
# ############### filtering the columns, which the max of it are larger than 0.5
# km.cluster_df.scaled.cluster.filter<-my_cluster_filter(km.cluster_df.scaled.cluster, 0.5)
# 
# ############### merge back the cluster and filter out the clientid that does show up for more than 90 days.
# ############### For the user with only one event, we only keep them for 7 days
# #km.final_cluster_2<-my_cluster_merge_back(original_df=mydata, my_reshape=type_cluster_df, km.object=kmeans.km_2, 1000, 1000)
# km.final_cluster_3<-my_cluster_merge_back2(my_reshape=cluster_df, km.object=kmeans.km_3)
# cluster_name<-cluster_name_table(km.cluster_df.scaled.cluster.filter,0.5)
# km.final_cluster_3<-my_cluster_name(km.final_cluster_3,cluster_name)
# 
# temp_df<-L_class_df.scaled
# temp_df$clientId<-cluster_df$clientId
# temp_df$cluster<-kmeans.km_3$cluster
# temp_df<- temp_df %>% select(clientId, cluster)
# temp_df<-my_cluster_name(temp_df,cluster_name)
# temp_df2<- merge(mydata, temp_df, by.x='clientId')
# temp_df2$source<- ifelse(temp_df2$utmSource %in% c('FB_NewsFeedAd','FB_MultiProductAd','FB_news feed ad','FB_news%20feed%20ad'),'urad',
#                               ifelse(!is.na(temp_df2$utmSource),'paid','nature'))
# 
# 
# funnel_source_match <- funnel_source %>%
#   group_by(clientId,session) %>%
#   arrange(session) %>%
#   slice(1) %>%
#   ungroup %>% select(clientId,session,source)
# 
# temp_df2<-merge(temp_df2, funnel_source_match, by=c('clientId','session'))
# temp_df2$source.x<-NULL
# colnames(temp_df2)[which(names(temp_df2) == "source.y")] <- "source"
# 
# # t<-temp_df2 %>% group_by(cluster,source) %>% summarise(count=n_distinct(clientId))
# # t<-t %>% group_by(cluster) %>% mutate(percentage=round(count/sum(count)*100,2))
# 
# dat %>%
#   group_by(x) %>%
#   mutate(z = y/sum(y))
# 
# ############
# cluster_main <- as.data.frame(table(temp_df2$cluster)) 
# cluster_main <- cluster_main %>% arrange(desc(Freq))
# 
# 
# dev.off()
# dev.new(width=8, height=7)
# #opar<- par(mfrow=c(1,1), cex.axis=1.5, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3, 1 ,0)) 
# opar<- par(mfrow=c(1,1), cex.axis=1, cex.lab=1, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(0, 3, 0 ,2)) 
# doughnut(cluster_main[,2], inner.radius=0.5, col=c(rgb(240, 128, 128, max=255), rgb(171, 130, 255, max=255), rgb(20, 61, 85, max=255), rgb(40, 122, 169, max=255), rgb(126, 146, 52 , max=255), rgb(204, 204, 204, max=255),rgb(204, 204, 204, max=255)), labels=c("家電專區","3C數位","居家生活","家電專區 & 居家生活","食品","映鮮嚴選"), clockwise=TRUE, init.angle=20)
# text(0,0,"主分類", cex=1.2)
# 
# ###################################################################################
# #################################### 分族群輪廓分析（依照etungo中分類） ###########
# ###################################################################################
# ##### filter and reshape the data
# #L_class_df<-my_reshape_lclass(mydata,cart_factor=5)  
# 
# 
# mydata_cluster2<- mydata 
# mydata_cluster2<- mydata_cluster2 %>% filter(!is.na(Product_Category0)) %>% filter(!is.na(clientId)) 
# mysubcat<-as.data.frame(sort(table(mydata_cluster2$Product_Category2),decreasing = TRUE))
# mysubcat<-mysubcat[c(1:25,27:29,31:32,34:35,37:38,42:43,45:46,48,53:61,63:65,67:69,72,74,78:79,81:82,84,92,95:96,102:104,108,118,122,
#                      133:134,139:140,150),]
# mydata_cluster2<- mydata_cluster2 %>% 
#   filter(Product_Category2 %in% mysubcat$Var1)
# 
# mydata_cluster2$Product_Category2[mydata_cluster2$Product_Category2 == "手機/週邊"] <- "手機"
# mydata_cluster2$Product_Category2[mydata_cluster2$Product_Category2 == "福利品"] <- "福利品/展示機"
# 
# 
# # temp<- mydata_cluster %>% select(clientId, actual_time, actual_date, urlCurrent,Product_Category0:Product_Category4) %>% 
# #   arrange(actual_time) %>% filter(!is.na(Product_Category0))
# 
# cluster2_df_1<-my_reshape2(mydata_cluster2, filter_argument = "Event=='ViewContent'", reshape_column = 'Product_Category2', multiple_factor=1)
# cluster2_df_2<-my_reshape2(mydata_cluster2, filter_argument = "Event=='AddToCart'", reshape_column = 'Product_Category2', multiple_factor=5)
# cluster2_df<-plyr::rbind.fill(cluster2_df_1,cluster2_df_2)
# cluster2_df[is.na(cluster2_df)] <- 0
# cluster2_df<-cluster2_df %>% dplyr::group_by(clientId) %>% dplyr::summarise_each(funs(sum))
# 
# 
# 
# ##### normalize the data
# L_class_df2.scaled<-my_normalize(cluster2_df)
# 
# 
# kmeans.km_4<- clara(L_class_df2.scaled, 74,samples=100,sampsize=500, pamLike=TRUE)  
# km.cluster2_df.scaled.cluster<-my_cluster_df(cluster2_df, L_class_df2.scaled, kmeans.km_4)
# 
# ############### filtering the columns, which the max of it are larger than 0.5
# km.cluster2_df.scaled.cluster.filter<-my_cluster_filter(km.cluster2_df.scaled.cluster, 0.5)
# 
# ############### merge back the cluster and filter out the clientid that does show up for more than 90 days.
# ############### For the user with only one event, we only keep them for 7 days
# #km.final_cluster_2<-my_cluster_merge_back(original_df=mydata, my_reshape=type_cluster_df, km.object=kmeans.km_2, 1000, 1000)
# km.final_cluster_4<-my_cluster_merge_back2(my_reshape=cluster2_df, km.object=kmeans.km_4)
# cluster2_name<-cluster_name_table(km.cluster2_df.scaled.cluster.filter,0.5)
# km.final_cluster_4<-my_cluster_name2(km.final_cluster_4,cluster2_name)
# 
# # temp2_df<-L_class_df2.scaled
# # temp2_df$clientId<-cluster2_df$clientId
# # temp2_df$cluster<-kmeans.km_4$cluster
# # temp2_df<- temp_df %>% select(clientId, cluster)
# # temp2_df<-my_cluster_name(temp_df,cluster_name)
# # temp2_df2<- merge(mydata, temp2_df, by.x='clientId')
# 
# cluster2<-as.data.frame(sort(table(km.final_cluster_4$cluster),decreasing = TRUE))
# cluster2_top<-cluster2[c(1:15),]
# 
# dev.off()
# dev.new(width=8, height=7)
# opar<- par(mfrow=c(1,1), cex.axis=1, cex.lab=2, cex.main=1.2, family="STKaiti", oma=c(0,0,0,0), col.lab="grey10", col.axis="grey10", col.main="grey10", mar=c(3.5, 3.5, 1 ,0)) 
# barplot(cluster2_top$Freq[1:10], col=rgb(20, 61, 85, max=255), ylim=c(0, 35000), ylab="", axes=FALSE, main="", border=NA, space=0.5)#, names.arg=cluster2_top$Var1[1:10]
# barplot(cluster2_top$Freq[1:3], col=rgb(40, 122, 169, max=255), ylim=c(0, 35000), ylab="", axes=FALSE, main="", border=NA, space=0.5, add=TRUE)
# text(x=seq(1,15,1.5),y=c(1,1), labels = cluster2_top$Var1[1:10], srt = 20, pos = 1, xpd = TRUE)
# 
# axis(2, las=1, line=-0.5, labels=FALSE, tck=0.01, col="grey10", lwd=2, at=seq(0, 35000, 7000))
# mtext(seq(0, 35000, 7000), las=2, side=2, line=-0.3, col="grey10", at=seq(0, 35000, 7000))
# mtext("族群人數", side=2, line=1.7, col="grey10",cex=2)
# mtext("", side=1, line=2, col="grey10",cex=2)
# # text(1.4, interval[1,2]+15, "69%", col="grey10", adj=1, cex=1.8)
# # text(2.9, interval[2,2]+15, "13%", col="grey10", adj=1, cex=1.8)
# # text(4.3, interval[3,2]+15, "6%", col="grey10", adj=1, cex=1.8)
# # text(5.8, interval[4,2]+15, "2%", col="grey10", adj=1, cex=1.8)
# 
# 
# 
# ###################################################################################
# ###################################################################################
# ###################################################################################





