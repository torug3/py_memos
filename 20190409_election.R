#ディレクトリ変更
#ライブラリ
library(ggplot2)
library(sf)
library(dplyr)#rename
library(viridis)#for plenty of colours

#
rate <- read.csv("ward_rate.csv")



#シェープファイルデータ読み込み
#eStatから大阪府を取得
shape <- st_read("h27_did_27.shp") 

#なんとなくcolumnをrenameしておく
shape <- rename(shape,ward_name=CITYNAME)


#データとシェープファイルをくっつける
data　<-　left_join(shape,rate,by="ward_name")


#座標軸取得
data_points <- sf::st_point_on_surface(data[1:24,7])
data_coords <- as.data.frame(sf::st_coordinates(data_points))
data_coords <- mutate(data_coords,rate=data$rate_mayor[1:24])
data_coords <- mutate(data_coords,ward=data$ward_name[1:24])

#ggplot
gg1 <- ggplot(data[1:24,7]) +  #描写するのは大阪市かつcountの行のみなのでdata[1:24,7]とする
	 geom_sf(aes(fill=rate_mayor),colour="black") +
	 theme_void(base_family="Japan1GothicBBB")+
	 labs(title="区別投票率（大阪市長選）")+
	 scale_fill_distiller(palette="Oranges",direction=1, limits=c(35,60))+
	　geom_text(data=data_coords,aes(X, Y+0.0005, label=ward),colour="black",cex=2.6,family="Japan1GothicBBB")+
	 geom_text(data=data_coords,aes(X, Y-0.005, label=rate),colour="black",cex=2.6,family="Japan1GothicBBB")
plot(gg1)

data$rate_governor
gg_gov <- ggplot(data[1:24,7]) +  #描写するのは大阪市かつcountの行のみなのでdata[1:24,7]とする
	 geom_sf(aes(fill=rate_governor),colour="black") +
	 theme_void(base_family="Japan1GothicBBB")+
	 labs(title="区別投票率（大阪府知事選）")+
	 scale_fill_distiller(palette="Oranges",direction=1, limits=c(35,60))+
	　geom_text(data=data_coords,aes(X, Y+0.0005, label=ward),colour="black",cex=2.6,family="Japan1GothicBBB")+
	 geom_text(data=data_coords,aes(X, Y-0.005, label=rate),colour="black",cex=2.6,family="Japan1GothicBBB")
plot(gg_gov)

data$rate_mayor-data$rate_governor

#グラフファイル出力(PDF)
ggsave(file = "01_mayor.png", plot = gg1)

ward_name	rate_mayor	rate_mayor_prev	difference	rate_governor
北区	50.97	49.14	1.83	51.01
都島区	56.08	52.84	3.24	56.14
福島区	50.97	50.01	0.96	51
此花区	54.29	49.27	5.02	54.29
中央区	46.94	46.57	0.37	47.01
西区	47.39	46.03	1.36	47.44
港区	54.07	50.28	3.79	54.08
大正区	55.85	52.21	3.64	55.87
天王寺区	55.22	54.12	1.1	55.25
浪速区	37.32	37.5	-0.18	37.35
西淀川区	54.81	50.33	4.48	54.82
淀川区	49.53	46.76	2.77	49.58
東淀川区	48.05	46.24	1.81	48.11
東成区	53.36	51.76	1.6	53.37
生野区	52.04	49.93	2.11	52.07
旭区	56.49	53.81	2.68	56.52
城東区	55.89	43.89	12	55.92
鶴見区	53.28	50.84	2.44	53.3
阿倍野区	59.13	58.31	0.82	59.15
住之江区	56.34	53.58	2.76	56.37
住吉区	54.21	52.88	1.33	54.24
東住吉区	54.39	52.87	1.52	54.43
平野区	54.56	50.46	4.1	54.61
西成区	50.01	49.3	0.71	50.02
