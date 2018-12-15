#要ディレクトリ変更
#ライブラリ
library(ggplot2)
library(dplyr)

#読み込み
opendata <- read.csv("opendata.csv")

#列名変更
opendata <- rename(opendata,files=二次利用に適したファイル形式.CSV.Word.Excelファイル等.)
opendata <- rename(opendata,PDF=PDFファイル)
opendata <- rename(opendata,total=合計)
opendata <- rename(opendata,rate=二次利用に適したファイル形式の割合)

#いったんrateの列を削除
opendata <- select(opendata,-rate)

#改めて計算して追加
opendata <- mutate(opendata,rate= round(files/total*100,1))


#ggplot2
#xy軸設定（表示順を割合順に）
graph <- ggplot(opendata)+  aes(x=reorder(bureau,rate),y=rate,fill=bureau)
#棒グラフで表示＋xyの入れ替え
graph <- graph+geom_bar(stat="identity")+ coord_flip()
#凡例を消す
graph <- graph + theme(legend.position = "none")
#割合の数字を表示
graph <- graph + geom_text(aes(label=rate),size=2.7,hjust=2)
#表のタイトルとxy軸の名前を設定
graph <- graph + labs(title="所属別公開割合（2018年9月時点）",x="所属名",y="割合（％）")
#描画
plot(graph)

#グラフファイル出力
ggsave(file = "rate.png", plot = graph)

#列名を元に戻してdataframeをcsvファイル出力、Shift-JISで
opendata <- rename(opendata,二次利用に適したファイル形式.CSV.Word.Excelファイル等.=files)
opendata <- rename(opendata,PDFファイル=PDF)
opendata <- rename(opendata,合計=total)
opendata <- rename(opendata,二次利用に適したファイル形式の割合=rate)
write.csv(opendata, file="opendatarate.csv", fileEncoding="CP932",row.names=F)
