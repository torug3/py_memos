#要ディレクトリ変更
#ライブラリ
library(ggplot2)
library(dplyr)
library(tidyr) #for spread
library(ggrepel)


#所属名
bureau_order <- c("副首都推進局","市政改革室","ICT戦略室","人事室","都市交通局","政策企画室",
"危機管理室","経済戦略局","中央卸売市場","IR推進局","総務局","市民局",
"財政局","契約管財局","都市計画局","福祉局","健康局","こども青少年局",
"環境局","都市整備局","建設局","港湾局","会計室","消防局","水道局",
"教育委員会事務局","行政委員会事務局","市会事務局",
"北区役所","都島区役所","福島区役所","此花区役所","中央区役所","西区役所","港区役所",
"大正区役所","天王寺区役所","浪速区役所","西淀川区役所","淀川区役所","東淀川区役所",
"東成区役所","生野区役所","旭区役所","城東区役所","鶴見区役所",
"阿倍野区役所","住之江区役所","住吉区役所","東住吉区役所","平野区役所","西成区役所")

#所属の書き出し
write.csv(bureau_order, file="bureau_list.csv",	fileEncoding="CP932", row.names=T)


#ダウンロードしたファイルの読み込み
data <-read.csv("all-files.csv")

#ファイル形式の大文字を小文字に統一
data$データ形式 <- tolower(data$データ形式)

#データ形式と公表組織に絞って件数をカウント（少数ファイルは無視）
data2 <- data %>% group_by(データ形式,公表組織) %>% count() %>% filter(n>=5)

#messyだが、たてもちデータをよこもちデータにspread
data3 <- data2 %>% spread(key = データ形式, value = n) %>% arrange(match(公表組織,c(bureau_order))) 

#NAを０に変換
data3[is.na(data3)] <- 0


#機械判読可能なファイル数のカラム追加
data4 <- data3 %>% mutate(readable = sum(csv,doc,docx,xls,xlsx))

#change to DF
data5 <- data.frame(data4)

#sum of all files
data7 <- data5 %>% mutate(all = rowSums(data5[,2:(ncol(data5)-1)]))

#calculate the rate
data8 <- data7 %>% mutate(rate = round(readable/all, digits=2))


#ファイルごと所属の書き出し
write.csv(data8, file="file_list.csv", fileEncoding="CP932", row.names=T)

#my colour palette
mycolour <- c("#ff6d00", "#2168ba", "#06d0ea", "#888888",
				 "#fe6a73", "#915eab", "#c1b0ef", "#006699",
				 "#c4c1a4", "#bc885c", "#06964d", "#89bc48")



#公表組織ごと
data9 <- data %>% group_by(データ形式,公表組織) %>% count() %>% filter(n>=20)
count_by_bureau <-
ggplot(data = data9, aes(x=公表組織,y=n　,fill=データ形式))+
geom_bar(stat ="identity")+
coord_flip()+
scale_x_discrete(limits = rev(bureau_order))+
scale_fill_manual(values = mycolour)

plot(count_by_bureau)
#グラフファイル出力
ggsave(file = "01_count_by_bureau.png", plot = count_by_bureau)

#ファイル形式ごとpie chart
data10 <- data %>% group_by(データ形式) %>% count() %>% filter(n>=30)
pie_chart <-
ggplot(data = data10, aes(x="",y=n, fill=データ形式))+
geom_bar(stat ="identity")+
coord_polar("y", start = 0)+
theme_light()+
geom_label_repel(aes(y=n,label = n), size=4, show.legend = F) +
scale_fill_manual(values = mycolour)

plot(count_by_bureau)

#グラフファイル出力
ggsave(file = "02_pie_chart.png", plot = pie_chart)


#元データ添付率
data8
readable <-
ggplot(data = data8, aes(x=reorder(公表組織,rate),y=rate, fill=公表組織))+
geom_bar(stat ="identity")+
coord_flip()+
theme(legend.position = "none")+
geom_text(aes(label=rate),size=2.7,hjust=2)+
labs(title="所属別公開割合（2019年4月時点）",x="所属名",y="割合（％）")


plot(readable)

#グラフファイル出力
ggsave(file = "03_readable.png", plot = readable)


