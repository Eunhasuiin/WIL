library(foreign)
library(reshape)
library(dplyr)
library(ggplot2)
library(readxl)
library(treemap)
windows()
setwd("D:/beta")
alpha=read.spss(file="Koweps_hpc10_2015_beta1.sav",to.data.frame=TRUE)
ffjob_data=read_excel("Koweps_Codebook.xlsx",sheet = "직종 코드")
job_data

alpha
#직업에 따른 산재 가입 비율
alphaJS=data.frame(code_job=alpha$h10_eco9,san=alpha$h10_soc8)
alphaJS <- alphaJS %>% filter(!is.na(code_job))
alphaJS <- alphaJS %>% filter(san!=3)
alphaJS
all=alphaJS %>% group_by(code_job) %>% summarise(total=n()) %>% arrange(code_job)
yes=alphaJS %>% group_by(code_job,san) %>% summarise(count=n()) %>% arrange(code_job) %>% filter(san==1)
alphaJS
temp=merge(x=all,y=yes,by="code_job",all=TRUE)
temp$san=ifelse(is.na(temp$san),"미가입 or 해당사항 없음","가입")
temp$count=ifelse(is.na(temp$count),0,temp$count)
temp$qrt=temp$count/temp$total
alphaJSQ=left_join(temp,job_data,id="code_job")
temp=alphaJSQ %>% arrange(desc(qrt))%>% select("job","qrt")
head(temp,20) %>% ggplot(aes(job,qrt))+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45,hjust = 1))
tail(temp,20) %>% ggplot(aes(job,qrt))+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45,hjust = 1))
#나이대에 따른 이용 병원의 비율
alphaAH=data.frame(birth=alpha$h10_g4,hos=alpha$h10_med7)
alphaAH
is.na(alphaAH$hos)
alphaAH$age=(2015-alphaAH$birth+1)
alphaAH$agegroup=ifelse(alphaAH$age<10,"10대 미만",ifelse(alphaAH$age<20,"10대",ifelse(alphaAH$age<30,"20대",ifelse(alphaAH$age<40,"30대",ifelse(alphaAH$age<50,"40대",ifelse(alphaAH$age<50,"60대","60대 이상"))))))
alphaAH=alphaAH %>% filter(hos!=0)
alphaAH$hos=ifelse(alphaAH$hos==1,"종합,대학병원",ifelse(alphaAH$hos==2,"지역 내외 병의원",ifelse(alphaAH$hos==3,"한방 병의원",ifelse(alphaAH$hos==4,"보건소","기타"))))
count=alphaAH %>% group_by(agegroup,hos) %>% summarise(count=n())
all=alphaAH %>% group_by(agegroup) %>% summarise(total=n())
alphaAHc=left_join(count,all,id="agegroup")
alphaAHc$qtr=alphaAHc$count/alphaAHc$total
alphaAHc %>% ggplot(aes(hos,qtr)) + geom_bar(stat="identity") +facet_grid(~agegroup) + theme(axis.text.x = element_text(angle=45,hjust = 1))
#성별에 따른 정규직과 비정규직의 비율
alphaGJ=data.frame(gender=alpha$h10_g3,Job_style=alpha$h10_eco4)
summary(alphaGJ)
alphaGJ$Job_style=ifelse(alphaGJ$Job_style==1,"정규직",ifelse(alphaGJ$Job_style==2 | alphaGJ$Job_style==3,"비정규직",NA))
alphaGJ=alphaGJ %>% filter(!is.na(alphaGJ$Job_style))
alphaGJ$gender=ifelse(alphaGJ$gender==1,"남","여")
count=alphaGJ %>% group_by(gender,Job_style) %>% summarise(count=n())
all=alphaGJ %>% group_by(gender) %>% summarise(total=n())
alphaGJC=left_join(count,all,id="gender")
alphaGJC
alphaGJC$qtr=alphaGJC$count/alphaGJC$total
alphaGJC %>% ggplot(aes(Job_style,qtr,fill=Job_style))+geom_bar(stat="identity") + facet_grid(~gender) + theme(axis.text.x = element_text(angle=45,hjust = 1)) 
#성별에 따른 만성질환
alphaGC=data.frame(gender=alpha$h10_g3,chronic_disease=alpha$h10_g9_1)
alphaGC
summary(alphaGC)
alphaGC$gender=ifelse(alphaGC$gender==1,"남","여")
alphaGC$chronic_disease=ifelse(alphaGC$chronic_disease==0,"?빐?떦 ?뾾?쓬",ifelse(alphaGC$chronic_disease==1,"3媛쒖썡 誘몃쭔 ?닾蹂?,?닾?빟",ifelse(alphaGC$chronic_disease==2,"3~6媛쒖썡 ?닾蹂? ?닾?빟","6媛쒖썡 ?씠?긽 ?닾?빟")))
count=alphaGC %>% group_by(gender,chronic_disease) %>% summarise(count=n())
all=alphaGC %>% group_by(gender) %>% summarise(total=n())
alphaGCC=left_join(count,all,id="gender")
alphaGCC$qtr=alphaGCC$count/alphaGCC$total
alphaGCC %>% ggplot(aes(chronic_disease,qtr,fill=chronic_disease)) + geom_bar(stat="identity") + facet_grid(~gender) + theme(axis.text.x = element_text(angle=45,hjust = 1))
#만성 질환에 따른 병명으로의 변화
disease=read.csv("alpha.csv")
disease
alphaGCD=alphaGC %>% mutate(disease=alpha$h10_med9)
alphaGCD=left_join(alphaGCD,disease,"disease")ff
count=alphaGCD %>% group_by(chronic_disease,disease_name) %>% summarise(count=n())
all=alphaGCD %>% group_by(chronic_disease) %>% summarise(total=n())
alphaGCD=left_join(count,all,id="chronic_disease")
alphaGCD$qrt=alphaGCD$count/alphaGCD$total
alphaGCD %>% ggplot(aes(disease_name,qrt,fill=disease_name)) + geom_bar(stat="identity") + facet_grid(~chronic_disease) + theme(axis.text.x = element_blank())
