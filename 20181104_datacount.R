library(dplyr)

setwd("*:/U****/***/****/forR")

#データ読み込み
data1 <- read.csv("胃がん検診（全件）181018時点.csv", stringsAsFactors=FALSE)

#地区だけを取り出してtibbleにして数をカウント
district <- data1 %>% dplyr::group_by(地区) %>% summarise(n=n())

#tibbleをdataframeに
district2 <- as.data.frame(district)

#arrangeで多いもの順（降順）に
district3 <- district2 %>% arrange(desc(n))

#同じ要領で医療機関順に
#医療機関だけを取り出してtibbleにして数をカウント
hospital <- data1 %>% dplyr::group_by(検診機関) %>% summarise(n=n())

#tibbleをdataframeに
hospital2 <- as.data.frame(hospital)

#arrangeで多いもの順（降順）に
hospital3 <- hospital2 %>% arrange(desc(n))

hospital3
