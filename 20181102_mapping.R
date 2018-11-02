library(dplyr)
library(sf)

setwd("C:/***")

#シェープファイル読み込みと表示（北区）
shape <- st_read("h27ka27127.shp")
#北区の地図表示
plot(st_geometry(shape))

#町丁目名表示（シェープファイルデータ）
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]+0.0005, labels=shape$MOJI, cex=0.5)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=shape$JINKO, cex=0.5)

#クリニック一覧読み込み
data1 <- read.csv("cliniclist20170703.csv")

#胃がん検診、北区のみ抽出
data3 <- data1 %>% filter(胃がん検診==1)
data4 <- data3 %>% filter(区コード==1)

#クリニックの所在をポイント
#（位置情報はあらかじめ付与）
cliniclongtitude <- data4[,43]
cliniclatitude <- data4[,42]
points(cliniclongtitude, cliniclatitude, col="red", pch=17)

#マップナビの位置情報を読み込もうとするがうまくいかない、タブ区切り？でもしたのでもダメ
#どうもわざわざ一度タブ区切りのテキストで保存しなおす必要があるみたい
#ほんとはダウンロードしたままでやりたいけど
school <- read.table("gakkou3.txt", sep="\t",header=T, encoding='SHIFT-Jis')

#小学校のみを抽出（ほんとうは私学は省きたいが）
elementary <- school %>% filter(施設名=="小学校$")
#さらに北区のみを抽出
elementary <- filter(elementary,  grepl("北区",所在地))

#なんでダメ？↓
elementary <- filter(school, str_detect(施設名, "^大阪市立"&"小学校$"))

#小学校の所在をポイント
schoollongtitude <- elementary[,1]
schoollatitude <- elementary[,2]
points(schoollongtitude, schoollatitude, col="blue", pch=16)


#人口データ読み込み、市民局の住基データから
#これもわざわざ一度タブ区切りのテキストで保存しなおしている
#本当はダウンロードしたままでやりたいけど
population <- read.table("juki.txt", sep="\t",header=T, encoding='SHIFT-Jis')
#このデータは合計行だけに絞る必要あり
population <- population %>% filter(population$男女別=="計")
#シェープファイルとくっつけて、ジオメトリ情報付加
data11 <- inner_join(shape, population, by=c("MOJI"="町丁目名"))
#塗分けの色定義
number <- data11$総数
class <- classIntervals(number, n=9, style="fixed", fixedBreaks= c(min(number),500,1000,1500,2000,2500,3000,3500,4000,max(number)))
palette <- brewer.pal(9, "YlOrRd")
mycolor <- findColours(class, palette)
#塗分け実行
plot(st_geometry(shape), col=mycolor)

#legend追加？
legendtext=c("500未満","500-1000","1000-1500","1500-2000","2000-2500","2500-3000","3000-3500","3500-4000","4000以上")

#住基データの数字をテキスト表示
text(st_coordinates(data11 %>% st_centroid)[,1], st_coordinates(data11 %>% st_centroid)[,2]+0.0005, labels=data11$S_NAME, cex=0.5,  bg="white")
text(st_coordinates(data11 %>% st_centroid)[,1], st_coordinates(data11 %>% st_centroid)[,2], labels=data11$総数, cex=0.5,  bg="white")

#ファイル出力
dev.print(png, file="myplot.png", width = 1024, height = 768)
png(file = "myplot.png", bg = "transparent")
plot(st_geometry(shape), col=mycolor)
points(schoollongtitude, schoollatitude, col="blue", pch=16)
text(st_coordinates(data11 %>% st_centroid)[,1], st_coordinates(data11 %>% st_centroid)[,2]+0.0005, labels=data11$S_NAME, cex=0.5,  bg="white")
text(st_coordinates(data11 %>% st_centroid)[,1], st_coordinates(data11 %>% st_centroid)[,2], labels=data11$総数, cex=0.5,  bg="white")
dev.off()

#プロット消すとき
plot.new()
