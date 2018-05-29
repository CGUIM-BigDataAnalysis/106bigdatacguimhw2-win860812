##Q1-1
library(ggplot2)
library(readr)
library(dplyr)
fore103<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=25f64d5125016dcd6aed42e50c972ed0")
fore104<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=4d3e9b37b7b0fd3aa18a388cdbc77996")
fore105<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=19bedf88cf46999da12513de755c33c6")
fore106<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=50e3370f9f8794f2054c0c82a2ed8c91")
colnames(fore103)<-c("洲別","國別","學位生_正式修讀學位外國生","學位生_僑生(含港澳)","學位生_正式修讀學位陸生","非學位生_外國交換生","非學位生_外國短期研習及個人選讀","非學位生_大專附設華語文中心學生","非學位生_大陸研修生","非學位生_海青班","境外專班")
colnames(fore104)<-c("洲別","國別","學位生_正式修讀學位外國生","學位生_僑生(含港澳)","學位生_正式修讀學位陸生","非學位生_外國交換生","非學位生_外國短期研習及個人選讀","非學位生_大專附設華語文中心學生","非學位生_大陸研修生","非學位生_海青班","境外專班")
fore103to106<-rbind(fore103,fore104,fore105,fore106)
for(i in 1:nrow(fore103to106)){
  fore103to106$sum[i]<-sum(fore103to106[i,3:11])
}

fore103to106<-fore103to106[order(fore103to106$sum,decreasing=T),]

country<-unique(fore103to106$國別)
people<-NULL
for(i in 1:length(unique(fore103to106$國別))){
  people[i]<-sum(subset(fore103to106,國別==country[i],select="sum"))
}
head(country,10)
head(people,10)

##Q1-2
library(ggplot2)
library(readr)
library(dplyr)
f103<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=a6d1469f39fe41fb81dbfc373aef3331")
f104<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=8baeae81cba74f35cf0bb1333d3d99f5")
f105<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=1a485383cf9995da679c3798ab4fd681")
f106<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=883e2ab4d5357f70bea9ac44a47d05cc")
colnames(f103)<-c("學校類型","學校代碼","學校名稱","學位生_正式修讀學位外國生","學位生_僑生(含港澳)","學位生_正式修讀學位陸生","非學位生_外國交換生","非學位生_外國短期研習及個人選讀","非學位生_大專附設華語文中心學生","非學位生_大陸研修生","非學位生_海青班","境外專班")
colnames(f104)<-c("學校類型","學校代碼","學校名稱","學位生_正式修讀學位外國生","學位生_僑生(含港澳)","學位生_正式修讀學位陸生","非學位生_外國交換生","非學位生_外國短期研習及個人選讀","非學位生_大專附設華語文中心學生","非學位生_大陸研修生","非學位生_海青班","境外專班")
f103to106<-rbind(f103,f104,f105,f106)
f103to106$非學位生_大陸研修生<-gsub("…","0",f103to106$非學位生_大陸研修生)
f103to106$非學位生_大陸研修生<-as.numeric(f103to106$非學位生_大陸研修生)
for(i in 1:nrow(f103to106)){
  f103to106$sum[i]<-sum(f103to106[i,4:12])
}

f103to106<-f103to106[order(f103to106$sum,decreasing=T),]
f103to106<-f103to106[-1:-3,]
school<-unique(f103to106$學校名稱)
people2<-NULL
for(i in 1:length(unique(f103to106$學校名稱))){
  people2[i]<-sum(subset(f103to106,學校名稱==school[i],select="sum"))
}
head(school,10)
head(people,10)

##Q2
library(ggplot2)
countryDF<-data.frame(國別=country,
                      人數=people)
ggplot()+geom_bar(data=countryDF,aes(國別,人數),stat = "identity")

##Q3
library(choroplethr)
library(choroplethrMaps)

##Q4-1
library(readr)
library(dplyr)
taiwanout<-read_csv("C:/Users/win/Desktop/Student_RPT_07.csv",skip=2)
colnames(taiwanout)<- c("年度","學期","設立別","學校類別","學校代碼","學校名稱","系所代碼","系所名稱","學制","國別(地區)","對方學校(機構)名稱","英文名稱","小計","男","女")
group_by(taiwanout,`國別(地區)`)%>%
  summarise(人數=n())%>%
  arrange(desc(人數))%>%
  head(10)


##Q4-2
group_by(taiwanout,學校名稱)%>%
  summarise(人數=n())%>%
  arrange(desc(人數))%>%
  head(10)

##Q5
library(ggplot2)
library(dplyr)
collegedf<-group_by(taiwanout,`國別(地區)`)%>%
  summarise(人數=n())%>%
  arrange(desc(人數))%>%
  head(10)

ggplot()+geom_bar(data=collegedf,aes(`國別(地區)`,人數),stat = "identity")

##Q7
library(readr)
library(dplyr)
taiwan<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")
taiwan$X4<-NULL
taiwan$X5<-NULL
taiwan$X6<-NULL
arrange(taiwan,desc(總人數))%>%
  select("國別")%>%
  head(10)
  
