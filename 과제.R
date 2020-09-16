library(foreign)
library(reshape)
library(dplyr)
library(ggplot2)
library(readxl)
library(treemap)
windows()
setwd("D:/beta")
alpha=read.spss(file="Koweps_hpc10_2015_beta1.sav",to.data.frame=TRUE)
job_data=read_excel("Koweps_Codebook.xlsx",sheet = "직종 코드")
View(alphaA)
class(alphaA)
#전처리
#사용할 변수 이름 변경
alpha=dplyr::rename(alpha,gender="h10_g3",birth="h10_g4",merriage="h10_g10",religion="h10_g11",income="p1002_8aq1",job_code="h10_eco9",region_code="h10_reg7")

#사용할 변수만 분리
alphaA=data.frame(gender=alpha$gender,birth=alpha$birth,merriage=alpha$merriage,religion=alpha$religion,income=alpha$income,jobcode=alpha$job_code,regioncode=alpha$region_code)

#나이 계산 밑 나이 그룹 계산
alphaA$age=2015-alphaA$birth
alphaA$agegroup=ifelse(alphaA$age>=60,"60~",ifelse(alphaA$age>=30,"30~59","~29"))

#성별 데이터 가시화
alphaA$gender=ifelse(alphaA$gender==1,"M","F")
alphaA <- alphaA %>% filter(!is.na(jobcode))

#직업코드 데이터 가시화 -.이거 좀더 좋은 방법 없을까
c=c()
for (i in alphaA$jobcode) {
  c=c(c,(job_data %>% filter(code_job==i))$job)
}
alphaA$job=c
#성별 별 평균 월급이 다를까?
alphaAGI=data.frame(gender=alphaA$gender,income=alphaA$income)
alphaAGI <- alphaAGI %>% filter(!is.na(income))
alphaAGI %>% group_by(gender) %>% summarise(mean_income=mean(income))
#몇살때 평균 월급을 가장 많이 받을까?
alphaAai=data.frame(age=alphaA$age,income=alphaA$income)
alphaAai <- alphaAai %>% filter(!is.na(income))
meanai=alphaAai %>% group_by(age) %>% summarise(mean=mean(income))
top_n(meanai %>% arrange(desc(mean)),5)
ggplot(meanai,aes(age,mean))+geom_bar(stat="identity")
#어떤 연령대가 월급을 가장 많이 받을까?
#연령대 초년 30under 중년 30~59 노년 60upper
alphaAgI=data.frame(agegroup=alphaA$agegroup,income=alphaA$income)
alphaAgI <- alphaAgI %>% filter(!is.na(income))
meangi=alphaAgI %>% group_by(agegroup) %>% summarise(mean_income=mean(income))
meangi
ggplot(meangi,aes(agegroup,mean_income))+geom_bar(stat="identity")
#성별에 따른 월급차이는 연령대별로 다를까?
alphaAGIG=data.frame(gender=alphaA$gender,agegroup=alphaA$agegroup,income=alphaA$income)
alphaAGIG <- alphaAGIG %>% filter(!is.na(income))
meanGIG=alphaAGIG %>% group_by(gender,agegroup) %>% summarise(mean_income=mean(income))
meanGIG
ggplot(meanGIG,aes(agegroup,mean_income))+geom_bar(stat="identity")+facet_grid(~gender)
#어떤 직업이 월급을 제일 많이 받을까?
alphaAJI=data.frame(job=alphaA$job,income=alphaA$income)
alphaAJI <- alphaAJI %>% filter(!is.na(income))
meanJI=alphaAJI %>% group_by(job)%>% summarise(mean_income=mean(income)) %>% arrange(desc(mean_income))
meanJI
ggplot(top_n(meanJI,10),aes(job,mean_income))+geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45,hjust=1))
#성별에 따라 어떤 직업이 제일 많을까?
alphaAGJ=data.frame(gender=alphaA$gender,job=alphaA$job)
countGI=alphaAGJ %>% group_by(gender,job) %>% summarise(count=n()) %>% arrange(desc(count))
countGI
countGIM=countGI %>% filter(gender=="M")
countGIF=countGI %>% filter(gender=="F")
#countGI_10=countGI %>% group_by(gender) %>% top_n(10)
#countGI_10
treemap(countGIM,index="job",vSize="count")
treemap(countGIF,index="job",vSize="count")
ggplot(countGI_10,aes(job,count,fill=job))+geom_bar(stat="identity")+facet_grid(~gender)+theme(axis.text.x = element_blank())
#종교가 있는사람은 이혼을 덜할까?
alphaAMR=data.frame(merriage=alphaA$merriage,religion=alphaA$religion)
alphaAMR$merriage=ifelse(alphaAMR$merriage==1 | alphaAMR$merriage==4,"unbreak",ifelse(alphaAMR$merriage==3,"break",NA))
alphaAMR <- alphaAMR %>% filter(!is.na(merriage))
alphaAMR$religion=ifelse(alphaAMR$religion==1,T,F)
countMR=alphaAMR %>% group_by(merriage,religion) %>% summarise(count=n()) %>% arrange(religion)
countMRT=countMR[3:4,];countMRF=countMR[1:2,]
countMRT$per=countMRT$count/sum(countMRT$count)
countMRF$per=countMRF$count/sum(countMRF$count)
countMRT %>%  ggplot(aes("",per,fill=merriage))+geom_bar(stat="identity")+coord_polar("y",start = 0)+geom_text(aes(label = paste0(round(per*100,1),"%")),position = position_stack(vjust = 0.5))+ggtitle("종교 있음")
countMRF %>%  ggplot(aes("",per,fill=merriage))+geom_bar(stat="identity")+coord_polar("y",start = 0)+geom_text(aes(label = paste0(round(per*100,1),"%")),position = position_stack(vjust = 0.5))+ggtitle("종교 없음")
#노년층이 많은 지역은?
count60R=alphaA %>% filter(agegroup=="60~") %>% group_by(regioncode) %>% summarise(count=n())
count60R$regioncode=c("서울","수도권(인천/경기)","부산/경남/울산","대구/경북","대전/충남","강원/충북","광주/전남/전북/제주도")
count60R
count60R %>% ggplot(aes(regioncode,count))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=45,hjust=1))
