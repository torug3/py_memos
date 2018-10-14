#tidyverseの読み込み
library(tidyverse)

setwd("*:/*****/***/***/****")
lung <- read.csv("****.csv")

#生年月日を日付型に変換
birth <- as.Date(lung$生年月日)

#現在日付を設定
current <- rep(as.Date("2018-10-01"),length(birth))

#年齢を計算して列追加
#格納場所を予約
birthy <- numeric(length=length(birth))
#繰り返し処理で計算
for(i in 1:length(birth)) {
birthy[i] <- c(length(seq(birth[i],current[i],by="year")))-1
}

#ageの列を追加
lung <- lung %>% mutate(age=birthy)

#ggplotでちょっとグラフ化
gg <- ggplot(data=lung) 
gg  +aes(x=age,fill=性別) +geom_histogram()

gg <- ggplot(data=lung) 
gg  +aes(x=age,fill=受診種別) +geom_histogram()+
　　labs(x="年齢", y="人数")

