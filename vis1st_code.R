library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(sf)
library(maptools)
setwd("d:/data1")
#----------(경기도 데이터 전처리)---------
gy=read_csv("경기도_기온.csv")
temp=read_csv("경기도_강수.csv")
names(gy)=c("date","hour","temperature")
gy$Rain_Qauntity=temp$`value location:60_120 Start : 20190101`

gy=gy[-length(gy$date),]
alpha=gy$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
gy$month=alpha
gy=gy[!is.na(gy$hour),]
gy$date=as.numeric(gy$date)
gy$month=as.numeric(gy$month)
write.csv(gy,"경기도.csv")

#----------(강원도 데이터 전처리)-----
ga=read_csv("강원도_기온.csv")
temp=read_csv("강원도_강수.csv")
names(ga)=c("date","hour","temperature")
ga$Rain_Qauntity=temp$`value location:92_130 Start : 20190101`
ga=ga[-length(ga$date),]
alpha=ga$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
ga$month=alpha
ga=ga[!is.na(ga$hour),]
write.csv(ga,"강원도.csv")

#----------(충청북도 데이터 전처리)-----
chn=read_csv("충북_기온.csv")
temp=read_csv("충북_강수.csv")
names(chn)=c("date","hour","temperature")
chn$Rain_Qauntity=temp$`value location:67_107 Start : 20190101`

chn=chn[-length(chn$date),]
alpha=chn$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
chn$month=alpha
chn=chn[!is.na(chn$hour),]
gy$date=as.numeric(gy$date)
gy$month=as.numeric(gy$month)

write.csv(chn,"충북.csv")


#----------(충청남도 데이터 전처리)--------------
chs=read_csv("충남_기온.csv")
temp=read_csv("충남_강수.csv")
names(chs)=c("date","hour","temperature")
chs$Rain_Qauntity=temp$`value location:61_100 Start : 20190101`
chs=chs[-length(chs$date),]
alpha=chs$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
chs$month=alpha
chs=chs[!is.na(chs$hour),]
gy$date=as.numeric(gy$date)
gy$month=as.numeric(gy$month)

write.csv(chs,"충남.csv")
#----------(경상북도 데이터 전처리)--------------
gyn=read_csv("경북_기온.csv")
temp=read_csv("경북_강수.csv")
names(gyn)=c("date","hour","temperature")
gyn$Rain_Qauntity=temp$`value location:98_90 Start : 20190101`
gyn=gyn[-length(gyn$date),]
alpha=gyn$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
gyn$month=alpha
gyn=gyn[!is.na(gyn$hour),]
gy$date=as.numeric(gy$date)
gy$month=as.numeric(gy$month)

write.csv(gyn,"경북.csv")

#----------(경상남도 데이터 전처리)-----
gys=read_csv("경남_기온.csv")
temp=read_csv("경남_강수.csv")
names(gys)=c("date","hour","temperature")
gys$Rain_Qauntity=temp$`value location:87_74 Start : 20190101`

gys=gys[-length(gys$date),]
alpha=gys$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
gys$month=alpha
gys=gys[!is.na(gys$hour),]
gy$date=as.numeric(gy$date)
gy$month=as.numeric(gy$month)

write.csv(gys,"경남.csv")

#----------(전라북도 데이터 전처리)-----
jrn=read_csv("전북_기온.csv")
temp=read_csv("전북_강수.csv")
names(jrn)=c("date","hour","temperature")
jrn$Rain_Qauntity=temp$`value location:56_91 Start : 20190101`

jrn=jrn[-length(jrn$date),]
alpha=jrn$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
jrn$month=alpha
jrn=jrn[!is.na(jrn$hour),]
jrn$date=as.numeric(jrn$date)
jrn$month=as.numeric(jrn$month)

write.csv(jrn,"전북.csv")

#----------(전라남도 데이터 전처리)-----
jrs=read_csv("전남_기온.csv")
temp=read_csv("전남_강수.csv")
names(jrs)=c("date","hour","temperature")
jrs$Rain_Qauntity=temp$`value location:70_71 Start : 20190101`

jrs=jrs[-length(jrs$date),]
alpha=jrs$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
jrs$month=alpha
jrs=jrs[!is.na(jrs$hour),]
jrs$date=as.numeric(jrs$date)
jrs$month=as.numeric(jrs$month)

write.csv(jrs,"전남.csv")



#----------(제주도 데이터 전처리)---------
jj=read_csv("제주_기온.csv")
temp=read_csv("제주_강수.csv")
names(jj)=c("date","hour","temperature")
jj$Rain_Qauntity=temp$`value location:59_38 Start : 20190101`

jj=jj[-length(jj$date),]
alpha=jj$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
jj$month=alpha
jj=jj[!is.na(jj$hour),]
gy$date=as.numeric(gy$date)
gy$month=as.numeric(gy$month)

write.csv(jj,"제주도.csv")


#----------(서울 데이터 전처리)--------
se=read_csv("서울_기온.csv")
temp=read_csv("서울_강수.csv")
names(se)=c("date","hour","temperature")
se$Rain_Qauntity=temp$`value location:60_126 Start : 20190101`

se=se[-length(se$date),]
alpha=se$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
se$month=alpha
se=se[!is.na(se$hour),]
se$date=as.numeric(se$date)
se$month=as.numeric(se$month)
write.csv(se,"서울.csv")
#----------(부산 데이터 전처리)----------------
bs=read_csv("부산_기온.csv")
temp=read_csv("부산_강수.csv")
names(bs)=c("date","hour","temperature")
bs$Rain_Qauntity=temp$`value location:97_74 Start : 20190101`

bs=bs[-length(bs$date),]
alpha=bs$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
bs$month=alpha
bs=bs[!is.na(bs$hour),]
bs$date=as.numeric(bs$date)
bs$month=as.numeric(bs$month)
write.csv(bs,"부산.csv")
#----------(인천 데이터 전처리)--------
ic=read_csv("인천_기온.csv")
temp=read_csv("인천_강수.csv")
names(ic)=c("date","hour","temperature")
ic$Rain_Qauntity=temp$`value location:54_124 Start : 20190101`

ic=ic[-length(ic$date),]
alpha=ic$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
ic$month=alpha
ic=ic[!is.na(ic$hour),]
ic$date=as.numeric(ic$date)
ic$month=as.numeric(ic$month)
write.csv(ic,"인천.csv")
#----------(세종 데이터 전처리)--------------
sj=read_csv("세종_기온.csv")
temp=read_csv("세종_강수.csv")
names(sj)=c("date","hour","temperature")
sj$Rain_Qauntity=temp$`value location:66_105 Start : 20190101`

sj=sj[-length(sj$date),]
alpha=sj$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
sj$month=alpha
sj=sj[!is.na(sj$hour),]
sj$date=as.numeric(sj$date)
sj$month=as.numeric(sj$month)
write.csv(sj,"세종.csv")
#----------(대전 데이터 전처리)-------------
dj=read_csv("대전_기온.csv")
temp=read_csv("대전_강수.csv")
names(dj)=c("date","hour","temperature")
dj$Rain_Qauntity=temp$`value location:67_100 Start : 20190101`

dj=dj[-length(dj$date),]
alpha=dj$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
dj$month=alpha
dj=dj[!is.na(dj$hour),]
dj$date=as.numeric(dj$date)
dj$month=as.numeric(dj$month)
write.csv(dj,"대전.csv")
#----------(대구 데이터 전처리)----------
dg=read_csv("대구_기온.csv")
temp=read_csv("대구_강수.csv")
names(dg)=c("date","hour","temperature")
dg$Rain_Qauntity=temp$`value location:88_90 Start : 20190101`

dg=dg[-length(dg$date),]
alpha=dg$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
dg$month=alpha
dg=dg[!is.na(dg$hour),]
dg$date=as.numeric(dg$date)
dg$month=as.numeric(dg$month)
write.csv(dg,"대구.csv")
#----------(광주 데이터 전처리)--------------
gj=read_csv("광주_기온.csv")
temp=read_csv("광주_강수.csv")
names(gj)=c("date","hour","temperature")
gj$Rain_Qauntity=temp$`value location:59_74 Start : 20190101`

gj=gj[-length(gj$date),]
alpha=gj$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
gj$month=alpha
gj=gj[!is.na(gj$hour),]
gj$date=as.numeric(gj$date)
gj$month=as.numeric(gj$month)
write.csv(gj,"광주.csv")

#----------(울산 데이터 전처리)---------------
ws=read_csv("울산_기온.csv")
temp=read_csv("울산_강수.csv")
names(ws)=c("date","hour","temperature")
ws$Rain_Qauntity=temp$`value location:104_83 Start : 20190101`

ws=ws[-length(ws$date),]
alpha=ws$date

tmp="01"
#월 데이터 추가하기
for(i in 1:length(alpha)) {
  if (substr(alpha[i],1,1)=="S") {
    tmp=substr(alpha[i],13,14)
  }
  alpha[i]=tmp
}
ws$month=alpha
ws=ws[!is.na(ws$hour),]
ws$date=as.numeric(ws$date)
ws$month=as.numeric(ws$month)
write.csv(ws,",울산.csv")
#----------(데이터를 하나로 합치기)--------
kor=rbind(se %>% mutate(c_code="SE"),ic %>% mutate(c_code="IC"),sj %>% mutate(c_code="SJ"),dg %>% mutate(c_code="DG"),dj %>% mutate(c_code="DJ"),bs %>% mutate(c_code="BS"),ws %>% mutate(c_code="WS"),gj %>% mutate(c_code="GJ"),gy %>% mutate(c_code="GY"),ga %>% mutate(c_code="GA"),chn %>% mutate(c_code="CHN"),chs %>% mutate(c_code="CHS"),gyn %>% mutate(c_code="GYN"),gys %>% mutate(c_code="GYS"),jrn %>% mutate(c_code="JRN"),jrs %>% mutate(c_code="JRS"),jj %>% mutate(c_code="JJ"))
kor
write.csv(kor,"kor_data.csv")
kor=read_csv("kor_data.csv")
kor=kor[-1]
#----------(데이터 분석)  -----------------------
#연간 기온 변화 추이시각화()
alpha=gy %>% group_by(month,date) %>% summarise(m_temp=mean(temperature))

dev.new()
ggplot(alpha,aes(x=paste0(month,"월",date,"일"),y=m_temp)) +geom_point()+theme(axis.text.x = element_text(angle =90,size=5))
m_temp=alpha %>% group_by(month) %>% summarise(m_m_temp=mean(m_temp))
m_temp
mean=mean(m_temp$m_m_temp)
temp_p=ggplot(m_temp,aes(x=month,y=m_m_temp))+geom_line(size=2,color="skyblue")+geom_point(col="blue",size=4)+geom_hline(yintercept=mean,col="red",lty="dashed")
temp_p=temp_p+scale_x_continuous(breaks = seq(1,12,by=1))+scale_y_continuous(breaks = seq(-10,30,by=5))+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+labs(title="월간 평균 기온 변화")+xlab("달")+ylab("기온(C)")
temp_p
#연간 강수량 변화 추이 시각화
alpha=gy %>% group_by(month,date) %>% summarise(m_RQ=sum(Rain_Qauntity))
alpha
m_RQ=alpha %>% group_by(month) %>% summarise(m_m_RQ=mean(m_RQ))
m_RQ
sum(m_RQ$m_m_RQ)
#
RQ_p=ggplot(m_RQ,aes(month,m_m_RQ))+geom_point(col="lightgreen",size=4)+geom_line(col="green",size=2)+geom_hline(yintercept=mean(m_RQ$m_m_RQ),col="blue",lty="dashed")
RQ_p+scale_x_continuous(breaks = seq(1,12,by=1))+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+labs(title="월간 평균 기온 변화")+xlab("달")+ylab("강수량(mm)")

#데이터 통합본 연간 기온 & 강수량 추이
#기온
kor_D_m_temp=kor %>% group_by(c_code,month,date) %>% summarise(m_temp=mean(temperature))#도,월,일 별 평균 기온
kor_M_m_temp=kor_D_m_temp %>% group_by(c_code,month) %>% summarise(m_m_temp=mean(m_temp))#도,월 별 평균 기온
kor_M_m_temp
#시각화
dev.new()
temp_p=ggplot(kor_M_m_temp,aes(x=month,y=m_m_temp))+geom_line(size=2,color="skyblue")+geom_point(col="blue",size=4)+facet_grid(~c_code)
temp_p=temp_p+geom_hline(yintercept=10,col="red",lty="dashed")+geom_hline(yintercept=-3,col="purple",lty="dashed")
temp_p=temp_p+scale_x_continuous(breaks = seq(1,12,by=2))+scale_y_continuous(breaks = seq(-10,30,by=5))+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+labs(title="월간 평균 기온 변화")+xlab("달")+ylab("기온(C)")
temp_p

#강수량
kor_D_s_RQ=kor %>% group_by(c_code,month,date) %>% summarise(s_RQ=sum(Rain_Qauntity))
kor_M_s_RQ=kor_D_s_RQ %>% group_by(c_code,month) %>% summarise(s_m_RQ=sum(s_RQ))
kor_M_s_RQ
kor_Y_s_RQ=kor_M_s_RQ %>% group_by(c_code) %>% summarise(s_Y_RQ=sum(s_m_RQ))

#시각화
RQ_p=ggplot(kor_M_s_RQ,aes(month,s_m_RQ))+geom_point(col="#008000",size=4)+geom_line(col="#006400",size=2)+facet_grid(~c_code)
RQ_p=RQ_p+scale_x_continuous(breaks = seq(1,12,by=2))+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+labs(title="월간 강수량 변화")+xlab("달")+ylab("강수량(mm)")
RQ_p+geom_hline(yintercept=60,col="red",size=1.4,lty="dashed")


#----------(1차 구분을 위한 전처리와 1차구분)-------------
#전처리
df=data.frame(c_code=kor_Y_s_RQ$c_code)
alpha=kor_M_m_temp %>% group_by(c_code) %>% summarise(max=max(m_m_temp),min=min(m_m_temp))#분류를 위한 최한월/최난월 추출
alpha
#1차 구분
df$first=ifelse(alpha$min>=18,"A",ifelse(alpha$min>=0,"C",ifelse(alpha$max>=10,"D","E")))
df
#----------(2차 구분을 위한 전처리와 2차구분)-------------------
#전처리
tmp=c("CHN","GY","GA","GYN","GYS","JRN","JRS","JJ","BS","DG","DJ","GJ","IC","SJ","SE","WS")
alpha=kor_M_s_RQ %>% filter(c_code=="CHS") %>% arrange(s_m_RQ)
alpha=alpha[1:6,]
for (i in tmp) {
  beta=kor_M_s_RQ %>% filter(c_code==i) %>% arrange(s_m_RQ)
  alpha=rbind(alpha,beta[1:6,])
}
alpha$result=ifelse(alpha$s_m_RQ<60,"M",FALSE)
alpha$result
kor_w_RQ=rbind(kor_M_s_RQ %>% filter(month==1),kor_M_s_RQ %>% filter(month==2),kor_M_s_RQ %>% filter(month==12))
kor_s_RQ=rbind(kor_M_s_RQ %>% filter(month==6),kor_M_s_RQ %>% filter(month==7),kor_M_s_RQ %>% filter(month==8))
w_m=kor_w_RQ %>% group_by(c_code) %>% summarise(min=min(s_m_RQ),max=max(s_m_RQ))
s_m=kor_s_RQ %>% group_by(c_code) %>% summarise(min=min(s_m_RQ),max=max(s_m_RQ))
#2차 구분
tmp=data.frame(c_code=w_m$c_code,w_m=w_m$min,`s_m*0.1`=s_m$max*0.1,s_m=s_m$min,`w_m*0.33`=w_m$max*0.33)#겨울 최건월 강수량과 여름 최건월*0.1/여름 최건월과 겨울 최건월*0.33 비교
df$second=ifelse(tmp$w_m<tmp$s_m.0.1,"w",ifelse(tmp$s_m<tmp$w_m.0.33,"s","f"))


#----------(3차 구분)---------------------
dev.new()
kor_Y_m_temp=kor_M_m_temp %>% group_by(c_code) %>% summarise(mean=mean(m_m_temp))
ggplot(kor_Y_m_temp,aes(c_code,mean))+geom_point(size=4,color="blue")+geom_hline(yintercept=18,col="red",lty="dashed")+theme_bw()+theme(panel.grid.major.y = element_blank(),panel.grid.minor = element_blank())+labs(title="월간 평균 기온 변화")+xlab("달")+ylab("기온(C)")
alpha=kor_M_m_temp %>% group_by(c_code) %>% summarise(max=max(m_m_temp),min=min(m_m_temp))#분류를 위한 최한월/최난월 추출
alpha
df$third=ifelse(alpha$max>22,"a","b")
df
df$result=paste0(df$first,df$second,df$third) #구분기준 합치기
df
result=df[c(-2,-3,-4)]

#----------(한국 지도상의 시각화)------------
dev.new()
result$id=c(15,6,5,14,11,7,12,8,2,1,13,0,4,3,16,9,10) #지도데이터와 결합시키기 위한 전처리
result=arrange(result,id)
result
kor_map_shp=readShapePoly("2013_si_do.shp")
kor_map=fortify(kor_map_shp)
kor_map=merge(kor_map,result,by="id")
alpha=ggplot(data=kor_map,aes(long,lat,group=group,fill=result))+geom_polygon(colour="black")
alpha=alpha+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
alpha+scale_fill_manual(values=c("#94FE97","#A5AFFF"))+labs(title = "쾨펜의 기후구분으로 보는 우리나라의 기후분포")+xlab(" ")+ylab(" ")
kor_map
