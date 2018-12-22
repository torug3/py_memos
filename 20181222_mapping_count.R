#受診医療機関の場所を見てみる＝同一区の数、他区の数


#ディレクトリ変更
#ライブラリ
library(ggplot2)
library(sf)
library(dplyr)#rename
library(viridis)#for plenty of colours
library(stringr)#for extract

#元データの日本語タイトルH**年度_肺がんをlung_20**に変更
#そのままread.csvできないのでいったんエクセルで開いて「名前を変更して保存」でCSV形式で上書き保存
#さらに日付（生年月日・受診日・前回）については日付型に変更、その他は標準に変更して保存

#データ読み込み
lung17 <- read.csv("lung_2017.csv")

#地区を区名までに編集
lung17$ward <- str_extract(lung17$地区,"\\w+区")

#区ごとにカウントして新たなdata.frame作成
count <- data.frame(table(lung17$ward))
colnames(count) <- c("ward_name","count")

#シェープファイルデータ読み込み
#eStatから大阪府を取得
shape <- st_read("h27_did_27.shp") 

#なんとなくcolumnをrenameしておく
shape <- rename(shape,ward_name=CITYNAME)

#データとシェープファイルをくっつける
data　<-　left_join(shape,count,by="ward_name")

#ggplot()にどのデータを読むのか伝える
#描写するのは大阪市かつcountの行のみなのでdata[1:24,7]とする
gg1 <- ggplot(data[1:24,7])

#geom_sfで地図描写、#境界線をwhiteにする
gg2 <- gg1+ geom_sf(aes(fill=count),colour="white")

#背景をvoidにする
#タイトルをつける、ゴシックのフォントで
gg3 <- gg2+theme_void(base_family="Japan1GothicBBB")+
  labs(title="区別受診者数（肺がん・平成29年度・男女計）")

#塗分けの色を変更（viridisで）
gg4 <- gg3 +scale_fill_viridis()

#区名と数を表示のための準備（座標軸取得）
data_points <- sf::st_point_on_surface(data[1:24,7])
#座標軸をとってくる（ｘとｙの座標軸だけにする）
data_coords <- as.data.frame(sf::st_coordinates(data_points))
#data_coordsにward_nameとcountのカラムを表示させるために追加
data_coords <- mutate(data_coords,count=data$count[1:24])
data_coords <- mutate(data_coords,ward=data$ward_name[1:24])

#区名表示
gg5 <- gg4+geom_text(data=data_coords,aes(X, Y+0.0005, label=ward),colour="white",cex=2.6,family="Japan1GothicBBB")
#数表示
gg6 <- gg5+geom_text(data=data_coords,aes(X, Y-0.005, label=count),colour="red",cex=2.6,family="Japan1GothicBBB")

#描画
plot(gg6)

#グラフファイル出力(PDF)
ggsave(file = "01_lung_2017_all.pdf", plot = gg6)



####男だけ抽出ヴァージョン
lung17 <- read.csv("lung_2017.csv")
lung17$ward <- str_extract(lung17$地区,"\\w+区")
lung17 <- lung17 %>% filter(lung17$性別=="男")
count <- data.frame(table(lung17$ward))
colnames(count) <- c("ward_name","count")
data　<-　left_join(shape,count,by="ward_name")
gg1 <- ggplot(data[1:24,7])
gg2 <- gg1+ geom_sf(aes(fill=count),colour="white")
gg3 <- gg2+theme_void(base_family="Japan1GothicBBB")+labs(title="区別受診者数（肺がん・平成29年度・男）")
gg4 <- gg3 +scale_fill_viridis()
data_points <- sf::st_point_on_surface(data[1:24,7])
data_coords <- as.data.frame(sf::st_coordinates(data_points))
data_coords <- mutate(data_coords,count=data$count[1:24])
data_coords <- mutate(data_coords,ward=data$ward_name[1:24])
gg5 <- gg4+geom_text(data=data_coords,aes(X, Y+0.0005, label=ward),colour="white",cex=2.6,family="Japan1GothicBBB")
gg6 <- gg5+geom_text(data=data_coords,aes(X, Y-0.005, label=count),colour="red",cex=2.6,family="Japan1GothicBBB")
plot(gg6)

#グラフファイル出力(PDF)
ggsave(file = "01_lung_2017_male.pdf", plot = gg6)

####女だけ抽出ヴァージョン
lung17 <- read.csv("lung_2017.csv")
lung17$ward <- str_extract(lung17$地区,"\\w+区")
lung17 <- lung17 %>% filter(lung17$性別=="女")
count <- data.frame(table(lung17$ward))
colnames(count) <- c("ward_name","count")
data　<-　left_join(shape,count,by="ward_name")
gg1 <- ggplot(data[1:24,7])
gg2 <- gg1+ geom_sf(aes(fill=count),colour="white")
gg3 <- gg2+theme_void(base_family="Japan1GothicBBB")+labs(title="区別受診者数（肺がん・平成29年度・女）")
gg4 <- gg3 +scale_fill_viridis()
data_points <- sf::st_point_on_surface(data[1:24,7])
data_coords <- as.data.frame(sf::st_coordinates(data_points))
data_coords <- mutate(data_coords,count=data$count[1:24])
data_coords <- mutate(data_coords,ward=data$ward_name[1:24])
gg5 <- gg4+geom_text(data=data_coords,aes(X, Y+0.0005, label=ward),colour="white",cex=2.6,family="Japan1GothicBBB")
gg6 <- gg5+geom_text(data=data_coords,aes(X, Y-0.005, label=count),colour="red",cex=2.6,family="Japan1GothicBBB")
plot(gg6)

#グラフファイル出力(PDF)
ggsave(file = "01_lung_2017_female.pdf", plot = gg6)




#結局フォントの問題はわからないまま
#フォントエラーが出てPDF出力時に文字が出ない
#「Windowsのフォントデータベースにフォントファミリが見つかりません」エラー

#以下では文字化けする

#ggplot()にどのデータを読むのか伝える
#描写するのは大阪市かつcountの行のみなのでdata[1:24,7]とする
gg1 <- ggplot(data[1:24,7])

#geom_sfで地図描写、#境界線をwhiteにする
gg2 <- gg1+ geom_sf(aes(fill=count),colour="white")

#背景をvoidにする
#タイトルをつける、ゴシックのフォントで
windowsFonts("MEI"=windowsFont("Meiryo"))
gg3 <- gg2+theme_void(base_family="MEI")+   #ここ
  labs(title="区別受診者数（肺がん・平成29年度）")

#塗分けの色を変更（viridisで）
gg4 <- gg3 +scale_fill_viridis()

#区名と数を表示のための準備（座標軸取得）
data_points <- sf::st_point_on_surface(data[1:24,7])
#座標軸をとってくる（ｘとｙの座標軸だけにする）
data_coords <- as.data.frame(sf::st_coordinates(data_points))
#data_coordsにward_nameとcountのカラムを表示させるために追加
data_coords <- mutate(data_coords,count=data$count[1:24])
data_coords <- mutate(data_coords,ward=data$ward_name[1:24])

#区名表示
gg5 <- gg4+geom_text(data=data_coords,aes(X, Y+0.0005, label=ward),colour="white",cex=2.6,family="Japan1GothicBBB")#ここ
#数表示
gg6 <- gg5+geom_text(data=data_coords,aes(X, Y-0.005, label=count),colour="red",cex=2.6,family="Japan1GothicBBB")#ここ

#描画
plot(gg6)

#グラフファイル出力(PDF)
#これだと文字化けする箇所あり
ggsave(file = "01_lung_2017_all.pdf", plot = gg6)



pdf("01_lung_2017_all.pdf")
par(family="MEI")　　　　　　　　　　#ここ
plot(gg6)
dev.off()

names(windowsFonts())
names(pdfFonts())


#タイトル追加はこのやり方も？フォントの指定は？
gg7 <- gg6 + ggtitle("区別受診者数（肺がん・平成29年度）") +
	 theme(plot.title=element_text(hjust = 0.5)) 
