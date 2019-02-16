#出力したCSVの前処理
#１～７行目を削除

#data読み込み
data <- read.csv("*******.csv")

#質問に対する回答の部分で、かつ設問６だけを取り出す
data2 <- data[data$設問番号==6,]

#自由記述の設問６だけを取り出す
data3 <- data2[,7]

#???????????
#一緒にはできないのかな？
datax <- data[data$設問番号==6,7]


#自由回答部分のみをテキストで書き出し
write.table(data3, "C:/Users/i*******/Desktop/output.txt", quote=F,
		col.names=F, row.names=F)

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
head(freq1[order(freq1$Freq,decreasing=T),],30)

#出現頻度の高い単語でワードクラウド作成
#ライブラリ
library(wordcloud)
library(dplyr)

freq1 %>% head(40)

#不要な単語を削除
freq <- freq3[(!freq3$Term=="する"&
		　  !freq3$Term=="ある"
		   ),]

#ワードクラウド
wordcloud(  words=freq$Term, 
		freq=freq$Freq,
		min.freq=4,
		color=brewer.pal(8, "Dark2"),
		scale=c(7,1.5),#一番大きな形態素でフォントサイズ最大７、最小２で描写
		family="IPAMincho",
		random.order=TRUE
	    )

#Ngramによる解析
#形態素の組み合わせの総数をカウント
ngram <- NgramDF2("output.txt",type=1,pos=c("名詞","形容詞","動詞","副詞"),N=4,minFreq=3)
ngram <- NgramDF2("output.txt",type=1,pos=c("名詞","形容詞","動詞"),N=4,minFreq=2)
ngram <- NgramDF2("output.txt",type=1,pos=c("名詞"),N=3,minFreq=2)
ngram[order(ngram$output.txt,decreasing=T),]

ngram %>% head(decreasing=T)
ngram %>% ngram[order(decreasing=T),]
ngram[order(ngram$output.txt,decreasing=T),]





#ngramをネットワーク可視化
#ライブラリ
library(igraph)

#Ngramをグラフデータフレームに変換
graph <- graph.data.frame(ngram)

#描写
plot(graph, 
	vertex.label=V(graph)$name,
	vertex.size=20, 
	vertex.color="#e24565",
	vertex.label.cex=.9,
	edge.color="orange",
	edge.lty=1,
	)

#otehr options
layout=layout.grid
vertex.shape="none"
edge.arrow.size=0

#plot guide
#http://www.nemotos.net/igraph-tutorial/NetSciX_2016_Workshop_ja.html





#テキストマイニング

#2-6
#前処理をしてから形態素解析
#2-7形態素解析の基礎
#形態素＝品詞もしくは単語＝意味を持つ最小の単位
#2-8必要なソフトウェア＝形態素解析器
#MeCab
#Chasen、Igo、
#RMeCabのパッケージを利用
#RとのパッケージがあるのはMeCabのみ
#2-9留意点
#口語表現や新語（形態素解析器の辞書にない）、社内用語は苦手
#MeCabをインストールするとデスクトップにアイコン、開くとDOSプロンプト
#辞書は2007年現在？？この講義の時点
#でも辞書のカスタマイズは容易（文法のカスタマイズは難しい）
#ウィキの見出し語の変換ツールもある
#2-10

#http://rmecab.jpのサイトに行ってインストールした（2019.2.6）
#mecab-0.996
#Rでパッケージのインストール（プルダウンには出てこない）
install.packages("RMeCab" , repos="http://rmecab.jp/R")
library(RMeCab)
#関数の一覧が閲覧できる？？
??RMeCab
#2-11
RMeCabC("私は今日のお昼ご飯をとても楽しみにしています")
#MeCabの出力結果の多くはlist形式で帰ってくる
RMeCabText("example.txt")

#
result <- RMeCabText("example01.txt")
result[[62]]
result[[62]][2]
#
data <- read.csv("example02.csv", header=T)
RMeCabDF(data,"comment",mypref=1)#列名を指定して形態素解析ができる
RMeCabDF(data,3,mypref=1)#これでもOK

#2-12
#DFはデータフレーム
#myprefを「１」と指定すると形態素を原型に変換して出力
#原型とは活用されたものを原型戻す、EX:赤く→赤い
#普通は形態素解析は原型でするのでmyprefは１になるのが普通	
result <- RMeCabFreq("example03.txt")#集計までしてくれる
#
head(result)
result[order(result$Freq,decreasing=T),]
head(result[order(result$Freq,decreasing=T),])

#2-13 単語の共起
#前後に隣接する形態素同士の関係を共起という＝ある形態素とよく一緒に出てくる形態素ってなんだ？を調べる
#共起を集計するためにはcollocate関数を使う
result <- collocate("example04.txt", node="オリンピック", span=3)
#node="知りたい共起関係の片方を指定"
#span=nodeで指定した単語の前後何個までをカウント対象にするか
result[order(result$Total),]
tail(result[order(result$Total),])
tail(result[order(result$Total),],10)
#助詞ばっかり出てくるのは日本語なので当たり前→これでは分析にならない→collScoresを使う
#CollScoresのT→よく出てくる共起を重みづけした指標
#CollScoresのMI→例えばオリンピックとしか共起を持たない特殊な共起に重きを置く指標
result2 <- collScores(result,node="オリンピック",span=3)
result2[(result2$T>=2&result$MI>=2),])

#2-14 Ngram
#Ngramというテキストマイニングの考え方
#ある形態素の前後に出現する形態素の組み合わせをNgramという
#R-言語-基本、とか、説明-わかる-やすい、とか
#NgramのNは任意の数
#一般的にはNは２から４くらい
Ngram("example05-1.txt", type=1, pos=c("名詞","動詞","形容詞"), N=3)
#type=1は原型にする、の意味
#まとめたい形態素の種類をposで指定
#NgramDFならデータフレームで出力してくれる
NgramDF("example05-1.txt", type=1, pos=c("名詞","動詞","形容詞"), N=3)
result <- NgramDF("example05-1.txt", type=1, pos=c("名詞","動詞","形容詞"), N=3)
head(result[order(result$Freq,decreasing=T),],10)

#2-15　テキスト間の関係を分析（docDF）
#docDFの引数はディレクトリ
#テキストが似ているか似ていないか
result <- docDF("example06", type=1, pos=c("名詞","動詞","形容詞"), N=3)
head(result)
#複数のファイルに共通性があれば、共通して出てくるNgramも多い＝ファイル名の数字が多くなるはず
#比較するファイルの件数が違うと、正確な比較ができないので、その場合は指標化する
#重みづけのオプションは"tf*idf*norm"←言語学の世界でよく使われる重みづけの指標
result <- docDF("example06", type=1, pos=c("名詞","動詞","形容詞"), N=3, weight="tf*idf*norm")
#どうやって表示？？
result[(result[4]>0.1|result[5]>0.1),0]
#2-16まとめ

#2-17　分析結果の視覚的把握・ネットワーク分析
#グラフ理論
library(igraph)
#
#DF2では最小頻度をminFreq=として指定できる
#２回以上出てくるものだけを抽出
ng <- NgramDF2("example05-1.txt",type=1,pos=c("名詞","形容詞","動詞","副詞"),N=3,minFreq=2)
ng.net <- graph.data.frame(ng)#igraphの関数
#ネットワーク図を描写
plot(ng.net,vertex.label=V(ng.net)$name)
plot(ng.net)
#頂点（矢印の先端）を描写しない
plot(ng.net,vertex.label=V(ng.net)$name,vertex.shape="none",edge.arrow.size=0)

#どのように並べれば数学的に意味があるか？は研究対象
#エッジ（要素）の並び和えアルゴリズムを指定
plot(ng.net,vertex.label=V(ng.net)$name,layout=layout.kamada.kawai)

plot(ng.net,vertex.label=V(ng.net)$name,layout=layout.circle)
#layout.でタブキーを押すと、ほかにどんなレイアウトがあるか確認できる
#以下はvariation
plot(ng.net,vertex.label=V(ng.net)$name,layout=layout.fruchterman.reingold)
#ラベル（品詞）のサイズを指定
V(ng.net)$label.cex=1.8
#頂点と矢印を描画しない
plot(ng.net,vertex.label=V(ng.net)$name,layout=layout.fruchterman.reingold,vertex.shape="none",edge.arrow.size=0)

#2-18 ワードクラウド
install.packages("wordcloud")
library(wordcloud)
#
wc <- RMeCabFreq("example08.txt")
#不要な形態素をremoveする
part_wc <- wc[(wc$Info1=="名詞"|wc$Info1=="感動詞"|wc$Info1=="形容詞"|wc$Info1=="動詞")&
	(!wc$Info2=="数"&!wc$Info2=="接尾"&!wc$Info2=="非自立"&!wc$Info2=="アルファベット"&!wc$Info2=="ナイ形容詞語幹")&
	(!wc$Term=="RT"&!wc$Term==".co"&!wc$Term=="http"&!wc$Term=="www"&!wc$Term=="httptco"&
	 !wc$Term=="bot"&!wc$Term=="する"&!wc$Term=="フォロー"&!wc$Term=="フォロ"&!wc$Term=="人"&!wc$Term=="一"&
	 !wc$Term=="一一"&!wc$Term=="一一一"&!wc$Term=="笙"&!wc$Term=="縺"&!wc$Term=="なる"&!wc$Term=="ある"&
	 !wc$Term=="いる"&!wc$Term=="やる"&!wc$Term=="あと"&!wc$Term=="無料"&!wc$Term=="相互"&!wc$Term=="これ"&
	 !wc$Term=="それ"&!wc$Term=="こちら"&!wc$Term=="なん"&!wc$Term=="ここ"&!wc$Term=="なに"&!wc$Term=="やつ"&
	 !wc$Term=="そこ"&!wc$Term=="どこ"&!wc$Term=="いつ"&!wc$Term=="だれ"&!wc$Term=="気"),]

part_wc<-part_wc[(!part_wc$Term=="a"&!part_wc$Term=="b"&!part_wc$Term=="c"&
!part_wc$Term=="d"&!part_wc$Term=="e"&!part_wc$Term=="f"&!part_wc$Term=="g"&
!part_wc$Term=="h"&!part_wc$Term=="i"&!part_wc$Term=="j"&!part_wc$Term=="k"&
!part_wc$Term=="l"&!part_wc$Term=="m"&!part_wc$Term=="n"&!part_wc$Term=="o"&
!part_wc$Term=="p"&!part_wc$Term=="q"&!part_wc$Term=="r"&!part_wc$Term=="s"&
!part_wc$Term=="t"&!part_wc$Term=="u"&!part_wc$Term=="v"&!part_wc$Term=="w"&
!part_wc$Term=="x"&!part_wc$Term=="y"&!part_wc$Term=="z"&!part_wc$Term=="A"&
!part_wc$Term=="B"&!part_wc$Term=="C"&!part_wc$Term=="D"&!part_wc$Term=="E"&
!part_wc$Term=="F"&!part_wc$Term=="G"&!part_wc$Term=="H"&!part_wc$Term=="I"&
!part_wc$Term=="J"&!part_wc$Term=="K"&!part_wc$Term=="L"&!part_wc$Term=="M"&
!part_wc$Term=="N"&!part_wc$Term=="O"&!part_wc$Term=="P"&!part_wc$Term=="Q"&
!part_wc$Term=="R"&!part_wc$Term=="S"&!part_wc$Term=="T"&!part_wc$Term=="U"&
!part_wc$Term=="V"&!part_wc$Term=="W"&!part_wc$Term=="X"&!part_wc$Term=="Y"&!part_wc$Term=="Z"),]

part_wc<-part_wc[(!part_wc$Term=="あ"&!part_wc$Term=="い"&!part_wc$Term=="う"&
!part_wc$Term=="え"&!part_wc$Term=="お"&!part_wc$Term=="か"&!part_wc$Term=="き"&
!part_wc$Term=="く"&!part_wc$Term=="け"&!part_wc$Term=="こ"&!part_wc$Term=="さ"&
!part_wc$Term=="し"&!part_wc$Term=="す"&!part_wc$Term=="せ"&!part_wc$Term=="そ"&
!part_wc$Term=="た"&!part_wc$Term=="ち"&!part_wc$Term=="つ"&!part_wc$Term=="て"&
!part_wc$Term=="と"&!part_wc$Term=="な"&!part_wc$Term=="に"&!part_wc$Term=="ぬ"&
!part_wc$Term=="ね"&!part_wc$Term=="の"&!part_wc$Term=="は"&!part_wc$Term=="ひ"&
!part_wc$Term=="ふ"&!part_wc$Term=="へ"&!part_wc$Term=="ほ"&!part_wc$Term=="ま"&
!part_wc$Term=="み"&!part_wc$Term=="む"&!part_wc$Term=="め"&!part_wc$Term=="も"&
!part_wc$Term=="や"&!part_wc$Term=="ゆ"&!part_wc$Term=="よ"&!part_wc$Term=="わ"&
!part_wc$Term=="を"&!part_wc$Term=="ん"&!part_wc$Term=="ぁ"&!part_wc$Term=="ぃ"&
!part_wc$Term=="ぅ"&!part_wc$Term=="ぇ"&!part_wc$Term=="ぉ"&!part_wc$Term=="ア"&
!part_wc$Term=="イ"&!part_wc$Term=="ウ"&!part_wc$Term=="エ"&!part_wc$Term=="オ"&
!part_wc$Term=="カ"&!part_wc$Term=="キ"&!part_wc$Term=="ク"&!part_wc$Term=="ケ"&
!part_wc$Term=="コ"&!part_wc$Term=="サ"&!part_wc$Term=="シ"&!part_wc$Term=="ス"&
!part_wc$Term=="セ"&!part_wc$Term=="ソ"&!part_wc$Term=="タ"&!part_wc$Term=="チ"&
!part_wc$Term=="ツ"&!part_wc$Term=="テ"&!part_wc$Term=="ト"&!part_wc$Term=="ナ"&
!part_wc$Term=="ニ"&!part_wc$Term=="ヌ"&!part_wc$Term=="ネ"&!part_wc$Term=="ノ"&
!part_wc$Term=="ハ"&!part_wc$Term=="ヒ"&!part_wc$Term=="フ"&!part_wc$Term=="ヘ"&
!part_wc$Term=="ホ"&!part_wc$Term=="マ"&!part_wc$Term=="ミ"&!part_wc$Term=="ム"&
!part_wc$Term=="メ"&!part_wc$Term=="モ"&!part_wc$Term=="ヤ"&!part_wc$Term=="ユ"&
!part_wc$Term=="ヨ"&!part_wc$Term=="ワ"&!part_wc$Term=="ヲ"&!part_wc$Term=="ン"&
!part_wc$Term=="ァ"&!part_wc$Term=="ィ"&!part_wc$Term=="ゥ"&!part_wc$Term=="ェ"&
!part_wc$Term=="ォ"&!part_wc$Term=="ー"),]
part_wc<-head(part_wc[order(part_wc$Freq,decreasing=T),],n=100)
#ここまでやる必要あるの？？↑

head(part_wc)
#paletteの設定(RColorBrewerに依存)
#RColorBrewerではbrewer.palでパレットを作る
palette <- brewer.pal(8,"Dark2")
palette
#描画
#引数が最低２つ必要
#Freqは大きさになる
#scale=c(7,2)→一番大きな形態素でフォントサイズ７、一番小さなもので２で描写してください
wordcloud(part_wc$Term,part_wc$Freq,scale=c(7,1.5),max.words=Inf,random.order=T,random.color=F,colors=palette)

#Windows環境に限って、フォントを変えられる
#WindowsFont関数とWindowsFonts関数という２つがある
#WindowsFonts関数で使いたいフォントの名前を指定する
#そのフォントに対してRの中で呼び出すためのラベル（ここではJP1）をつける
windowsFonts(JP1=windowsFont("MS Mincho"))
palette <- brewer.pal(9,"Set1")
#オプションでfamily="JP1"を指定
wordcloud(part_wc$Term,part_wc$Freq,scale=c(7,1.5),max.words=Inf,random.order=T,random.color=F,colors=palette,family="JP1")

#RColorBrewerパッケージで使用可能なパレットを一覧表示
display.brewer.all()

#2-19 ネガポジ分析
#単語極性辞書がポイント、形態素一つ一つにポジティブどの点数がついているイメージ
#辞書をいかに作る、取り込むか、がキーになる
#じっさいにどうやってやるのかわからないやん


#2-20 自動分類
#クラスタ分析と多次元尺度法
#termと文書行列を作成する関数としてdocMatrix
#引数はディレクトリ（フォルダ）
#各ファイルである形態素がどれくらいの割合で出現したか
#数字の出方が似ているファイルは似ている
#大量のテキストファイルをいくつかのグループに分類できる
#ここから実際に
#クラスタ分析と多次元尺度法
#距離の算出（似ている度合い）を数字で表せる
dist
#引数にt
#t、行列の転置（行列の入れ替え）を行う関数
#canberraは距離を計算する手法の一つ
#デンドログラム（樹形図）、dendrogram
#多次元尺度法＝2次元で無理やり表記
library(MASS)
#距離を算出するところまでは上と共通

result<-docMatrix("good_point",pos=c("名詞","動詞","形容詞"),weight="tf*idf*norm")
result<-result[row.names(result) != "[[LESS-THAN-1]]",]
result<-result[row.names(result) != "[[TOTAL-TOKENS]]",]
result.dist<-dist(t(result),"canberra")
result.clust<-hclust(result.dist,"ward.D2")
plot(result.clust)
lines(c(1,200),c(750,750),lty=2,lwd=2,col=2)

c<-rect.hclust(result.clust,k=6)

result<-docMatrix("bad_point",pos=c("名詞","動詞","形容詞"),weight="tf*idf*norm")
result<-result[row.names(result) != "[[LESS-THAN-1]]",]
result<-result[row.names(result) != "[[TOTAL-TOKENS]]",]
result.dist<-dist(t(result),"canberra")
result.clust<-hclust(result.dist,"ward.D2")
plot(result.clust)
lines(c(1,150),c(880,880),lty=2,lwd=2,col=2)

c<-rect.hclust(result.clust,k=4)

require(MASS)
#result.samm<-sammon(result.dist)
result.isoMDS<-isoMDS(result.dist)
result.name<-unlist(strsplit(colnames(result),".txt"))
#plot(result.samm$points,type="n")
#text(result.samm$points,lab=result.name)
plot(result.isoMDS$points,type="n")
text(result.isoMDS$points,lab=result.name)

