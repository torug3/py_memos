#ディレクトリ変更
#ライブラリ
library(ggplot2)
library(dplyr)
library(reshape2)#for melt

#前処理として各ファイル（CSV）を「名前を付けてCSV」で保存し直す必要あり
#data読み込み
hirano_1809 <- read.csv("population_201809_hirano.csv")
hirano_1803 <- read.csv("population_201803_hirano.csv")
hirano_1709 <- read.csv("population_201709_hirano.csv")
hirano_1703 <- read.csv("population_201703_hirano.csv")
hirano_1609 <- read.csv("population_201609_hirano.csv")

#男女別でなく合計の行rowだけでフィルタ
hirano_1809　<- hirano_1809 %>% filter(男女別=="計")
hirano_1803　<- hirano_1803 %>% filter(男女別=="計")
hirano_1709　<- hirano_1709 %>% filter(男女別=="計")
hirano_1703　<- hirano_1703 %>% filter(男女別=="計")
hirano_1609　<- hirano_1609 %>% filter(男女別=="計")

#各年度のデータから必要箇所だけを取り出して新たにDF作成
#たとえば５歳（１５行目が５歳のカラム）
age_five <- data.frame(hirano_1809[,2],hirano_1809[,15],hirano_1803[,15],hirano_1709[,15],hirano_1703[,15],hirano_1609[,15])
colnames(age_five) <- (c("area","20180901","20180301","20170901","20170301","20160901"))
#月までだとなぜか日付型に変換するときにNAになる
#colnames(age_five) <- (c("area","201809","201803","201709","201703","201609"))

#総数の行は削除
age_five <- age_five %>% filter(area!="総数")

#縦長に結合
data <- melt(age_five, id.vars = "area")

#column名rename
data <- rename(data,time=variable)

#時系列データ（折れ線グラフ）はx軸を日付型にしておく必要
#時点を日付型に変換
data$time <- as.character(data$time) #数字ではas.Dateできないため文字に変換
data$time <- as.Date(data$time, "%Y%m%d") #文字に変換後、日付に変換



#描画
gg1 <-
ggplot(data=data)+
	geom_line(aes(x=time,y=value,colour=area),show.legend=FALSE)+
	geom_text(data=subset(data, time=="2018-09-01"),aes(x=time,y=value,label=area),size=2.8,nudge_x=50,family="Japan1GothicBBB")+
	labs(title="平野区町丁目別人口（５歳）",x="時点",y="人口")+
	theme_bw(base_family="Japan1GothicBBB")+


#グラフファイル出力(PDF,png)
ggsave(file = "23_population_hirano.png", plot = gg1)
ggsave(file = "23_population_hirano.pdf", plot = gg1)


#x軸を調整する（データの時点に合わせる）方法がわからない

	scale_x_continuous(breaks="time")

	scale_x_continuous(breaks=c(2016-09,2017-03,2017-09,2018-03,2018-09))

	theme(family="Japan1GothicBBB")

	theme(plot.title=element_text(family="Japan1GothicBBB"))	



theme(legend.position='none',plot.title=element_text(family="Japan1GothicBBB"))+　#legendでつけると表示できないので



ggplot(data)+
geom_jitter(aes(x=time,y=value,colour=area))+
theme(legend.position='none')


shape <- st_read("h27ka27126.shp")
plot(st_geometry(shape[1:24,4]))

#dataframeをcsvファイル出力、Shift-JISで
write.csv(data, file="population_hirano.csv", fileEncoding="CP932",row.names=F)
