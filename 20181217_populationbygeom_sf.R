#ライブラリ
library(ggplot2)
library(sf)
library(viridis)
library(dplyr)

#必要なデータのファイル読み込み
#普通に住基ファイルをHPからダウンロードして読むと「列名よりも列数のほうが多いです」エラー
#セルの結合や余計なデータが入っているから？秘匿区分が＠ではじまっているから？
#結局ずたずたにデータを編集して読み込んだので結局どうすればいいかがわからない
#下のファイルpopulation_kitaのファイルは編集したもの
frame1 <- read.csv("population_kita_201809.csv")

#シェープファイルデータ読み込み
frame2 <- st_read("h27ka27127.shp")

#なんとなくcolumnをrenameしておく
frame1 <- rename(frame1,region=町丁目名)
frame1 <- rename(frame1,population=総数)
frame2 <- rename(frame2,region=S_NAME)

#データとシェープファイルをくっつける
frame3　<-　left_join(frame1,frame2,by="region")

#まずggplot()にどのデータを読むのか伝える
gg1 <- ggplot(frame3)
#geom_sfで地図描写できる
#境界線をwhiteにする
gg2 <- gg1+ geom_sf(aes(fill=population),colour="white")
# 背景色を白にする
gg3 <- gg2+theme_void()
#塗分けの色を変更（工夫の余地あり）
gg4 <- gg3 +scale_fill_viridis()
#描画
plot(gg4)

#保存
ggsave(file = "population_kita2.png", plot = gg4)

#データをくっつけて描写するとシェープファイルだけで表示するよりもつぶれたような感じになる

#シェープファイルデータ読み込み
shape <- st_read("h27ka27127.shp")

#まずggplot()にどのデータを読むのか伝える
gg1 <- ggplot(shape)
#geom_sfで地図描写できる
#境界線をwhiteにする
gg2 <- gg1+ geom_sf(aes(fill=JINKO),colour="white")
# 背景色を白にする
gg3 <- gg2+theme_void()
#塗分けの色を変更（工夫の余地あり）
gg4 <- gg3 +scale_fill_viridis()
#描画
plot(gg4)


ggplot()+geom_sf(data=shape,aes(fill=JINKO),colour="white")+theme_void()+scale_fill_viridis()
