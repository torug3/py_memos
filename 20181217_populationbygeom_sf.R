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

#データファイル読み込み
frame1 <- read.csv("population_kita_201809.csv")

#男女計のうち計の行のみを取り出し
frame1 <- frame1 %>% filter(frame1$男女別=="計")

#なんとなくcolumnをrenameしておく
frame1 <- rename(frame1,region=町丁目名)
frame1 <- rename(frame1,population=総数)

#シェープファイルデータ読み込み
frame2 <- st_read("h27ka27127.shp")

#なんとなくcolumnをrenameしておく
frame2 <- rename(frame2,region=S_NAME)

#データとシェープファイルをくっつける
frame3　<-　left_join(frame2,frame1,by="region")

#まずggplot()にどのデータを読むのか伝える
gg1 <- ggplot(frame3)

#geom_sfで地図描写できる
#境界線をwhiteにする
gg2 <- gg1+ geom_sf(aes(fill=population),colour="white")

# 背景色を白にする
gg3 <- gg2+theme_void()

#塗分けの色を変更（工夫の余地あり）
gg4 <- gg3 +scale_fill_viridis()


#次のステップは町名の表示のためのもの
#choose a point on the surface of each geometry
#それぞれのジオメトリの表面にポイントを選ぶ？
frame_points <- sf::st_point_on_surface(frame3)

# retrieve the coordinates
#座標軸をとってくる（ｘとｙの座標軸だけにする）
frame_coords <- as.data.frame(sf::st_coordinates(frame_points))

#shape_coordsにregionのカラムを追加
frame_coords <- mutate(frame_coords,region=frame3$region)
frame_coords <- mutate(frame_coords,population=frame3$population)

#合体版
gg5 <- gg4+geom_text(data=frame_coords,aes(X, Y+0.0005, label=region),colour="white",cex=2)
gg6 <- gg5+geom_text(data=frame_coords,aes(X, Y-0.0005, label=population),colour="red",cex=2)

#描画
plot(gg6)

#保存
ggsave(file = "population_region.png", plot = gg6)

#PDFが文字化けする
ggsave(file = "population_region.pdf", plot = gg6)

#データをくっつけて描写するとシェープファイルだけで表示するよりもつぶれたような感じになる









#シェープファイルだけで（シェープファイルのデータで描写する場合


#シェープファイルデータ読み込み
shape <- st_read("h27ka27127.shp")

g1 <- ggplot(shape)
#geom_sfで地図描写できる
#境界線をwhiteにする
g2 <- g1+ geom_sf(aes(fill=JINKO),colour="white")
# 背景色を白にする
g3 <- g2+theme_void()
#塗分けの色を変更（工夫の余地あり）
g4 <- g3 +scale_fill_viridis()

#次のステップは町名の表示のためのもの
#choose a point on the surface of each geometry
shape_points <- sf::st_point_on_surface(shape)
shape_coords <- as.data.frame(sf::st_coordinates(shape_points))
#いちおう（前もって）renameしておいて
shape <- rename(shape,region=S_NAME)
shape <- rename(shape,population=JINKO)

#shape_coordsにregionのカラムを追加
shape_coords$region <- shape$region
shape_coords$population <- shape$population

g5 <- g4+geom_text(data=shape_coords,aes(X, Y, label=region),colour="white",cex=2)
g6 <- g5+geom_text(data=shape_coords,aes(X, Y, label=population),colour="white",cex=2)


#描画
plot(g6)

#保存
ggsave(file = "population_shape.png", plot = g6)
