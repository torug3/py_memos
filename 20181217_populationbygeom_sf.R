#ライブラリ
library(ggplot2)
library(dplyr)
library(sf)
library(viridis)

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

#保存
ggsave(file = "population_kita.png", plot = gg4)


ggplot()+geom_sf(data=shape,aes(fill=JINKO),colour="white")+theme_void()+scale_fill_viridis()
