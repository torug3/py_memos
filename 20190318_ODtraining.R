library(ggplot2)
library(dplyr)

#１．受講履歴
#data読み込み
#前処理しなくてもskipで不要な行は読み込まない
history <- read.csv("******.csv", skip=4, fileEncoding="CP932")

#いつ受講したか、受講完了日で見る
history$コース受講完了日 <- as.POSIXct(history$コース受講完了日, format="%Y/%m/%d %H:%M:%S", tz = "Japan")

#受講状況の把握
table(history$コース受講状況)


#２．テスト
#data読み込み
#前処理しなくてもskipで不要な行は読み込まない
history <- read.csv("********.csv", skip=4, fileEncoding="CP932")


#３．アンケート
#data読み込み
#前処理しなくてもskipで不要な行は読み込まない
questionnaire <- read.csv("******.csv", skip=6, fileEncoding="CP932")

#回答状況の把握
table(questionnaire$設問番号)

#時間のものはPOSIXct型に
questionnaire$実施日時 <- as.POSIXct(questionnaire$実施日時, format="%Y/%m/%d %H:%M:%S", tz = "Japan")

#テストの値は除去
questionnaire <- questionnaire %>% filter(実施日時 > as.POSIXct("2019-02-04 00:00:00"))

#アンケートの設問ごとで分割
q1 <- questionnaire[questionnaire$設問番号==1,]
q2 <- questionnaire[questionnaire$設問番号==2,]
q3 <- questionnaire[questionnaire$設問番号==3,]
q4 <- questionnaire[questionnaire$設問番号==4,]
q5 <- questionnaire[questionnaire$設問番号==5,]


#設問ごとに回答状況を可視化
#Q1
qq1 <- ggplot(data=q1)+
geom_bar(aes(x=回答番号),fill="hotpink1")+
xlab("Q1：あなたは、この研修を受けるまで、オープンデータについてどの程度ご存知でしたか")+
ylim(0,6000)+
scale_x_discrete(labels=c("よく知っていた","知っていた","あまり知らなかった","全く知らなかった"))

ggsave(plot = qq1, file = "Q1.png")

#Q2
qq2 <- ggplot(data=q2)+
geom_bar(aes(x=回答番号),fill="plum")+
xlab("Q2：あなたは、大阪市オープンデータポータルサイトについてどの程度知っていましたか")+
ylim(0,6000)+
scale_x_discrete(labels=c("データを見たことがある","データは見たことがない","サイトは知っていた","サイトを知らなかった"))

ggsave(plot = qq2, file = "Q2.png")


#Q3
qq3 <- ggplot(data=q3)+
geom_bar(aes(x=回答番号),fill="orange")+
xlab("Q3：この研修を受けて、「人が見やすいデータ」と「機械判読性の高いデータ」の違いについて理解できましたか")+
scale_x_discrete(labels=c("よく理解できた","理解できた","あまり理解できなかった","全く理解できなかった"))

ggsave(plot = qq3, file = "Q3.png")

#Q4
qq4 <- ggplot(data=q4)+
geom_bar(aes(x=回答番号),fill="yellowgreen")+
ylim(0,8400)+
xlab("Q4：あなたは、これまで業務でWordファイルもしくはExcelファイルを作成したことがありますか")+
scale_x_discrete(labels=c("はい","いいえ","わからない"))

ggsave(plot = qq4, file = "Q4.png")

#Q5
qq5 <- ggplot(data=q5)+
geom_bar(aes(x=回答番号),fill="skyblue")+
ylim(0,8400)+
xlab("Q5：あなたは、これまで業務でCSVファイルを作成したことがありますか")+
scale_x_discrete(labels=c("はい","いいえ","わからない"))

ggsave(plot = qq5, file = "Q5.png")



#アンケートの自由記述についてワードクラウドとネットワークグラフ

#data読み込み
data <- read.csv("***********.csv", skip=6, fileEncoding="CP932")

#質問に対する回答の部分で、かつ設問６だけを取り出す
datax <- data[data$設問番号==6,7]

#特になし、などを削除
datay <- datax[-which(datax %in% c("何もない。","何もありません","とくになし","別になし","特になし","特にないです。","nasi","ない","特記事項なし","特に、ございません。","特になし。","特にない。","特にない","特に無し","特に無し。","特に無い。","なし","なし。","特段なし","無し","特にございません。","特にありません","特にありません。","　特にありません。","特に思い当たりません。","特段ありません。"))]

#自由回答部分のみをテキストで書き出し
write.table(datay, "C:/Users/*****/Desktop/output.txt", quote=F, col.names=F, row.names=F)

#package
library(RMeCab)

#単語の出現頻度を調べる

#termのfreqencyを数える
freq1 <- RMeCabFreq("output.txt")#集計までしてくれる

#名詞と動詞のみを抽出
freq2 <- subset(freq1, Info1 %in% c("名詞", "動詞", "形容詞"))

#細分類（Info2）から不要なものを除外
freq3 <- subset(freq2, !Info2 %in% c("数", "非自立", "接尾"))

#decreasingで30個見てみる
head(freq3[order(freq3$Freq,decreasing=T),],30)

#出現頻度の高い単語でワードクラウド作成
#ライブラリ
library(wordcloud)

#不要な単語を削除
freq <- freq3[(!freq3$Term=="する"&
		　  !freq3$Term=="ある"&
		　  !freq3$Term=="ない"&
		　  !freq3$Term=="思う"
		   ),]

#ワードクラウド
wordcloud(  words=freq$Term, 
		freq=freq$Freq,
		min.freq=4,
		color=brewer.pal(8, "Dark2"),
		scale=c(7,1.5),#一番大きな形態素でフォントサイズ最大７、最小1.5で描写
		family="IPAMincho",
		random.order=TRUE
	    )

#Ngramによる解析
#形態素の組み合わせの総数をカウント
ngram <- NgramDF2("output.txt",type=1,pos=c("名詞","形容詞","動詞","副詞"),N=3,minFreq=3)

#ngramをネットワーク可視化
#ライブラリ
library(igraph)

#Ngramをグラフデータフレームに変換
graph <- graph.data.frame(ngram)

#描写
plot(graph, 
	vertex.label=V(graph)$name,
	vertex.size=15, 
	vertex.color="lightgreen",
	vertex.label.cex=.9,
	edge.color="orange",
	edge.lty=1,
	)

