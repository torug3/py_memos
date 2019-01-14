#ディレクトリ変更
#ライブラリ
library(stringr)#for extract
library(dplyr)#rename
library(ggplot2)

#１．2017年
#データ読み込み
lung17 <- read.csv("lung_2017.csv")
stomach17 <- read.csv("stomach_2017.csv")
breast17 <- read.csv("breast_2017.csv")
uterine17 <- read.csv("uterine_2017.csv")
intestine17 <- read.csv("intestine_2017.csv")


#地区を区名までにしてwardの列追加
lung17$ward <- str_extract(lung17$地区,"\\w+区")
stomach17$ward <- str_extract(stomach17$地区,"\\w+区")
breast17$ward <- str_extract(breast17$地区,"\\w+区")
uterine17$ward <- str_extract(uterine17$地区,"\\w+区")
intestine17$ward <- str_extract(intestine17$地区,"\\w+区")

#区ごとにカウントして新たなdata.frame作成
lung <- data.frame(table(lung17$ward))
colnames(lung) <- c("ward_name","lung")

stomach <- data.frame(table(stomach17$ward))
colnames(stomach) <- c("ward_name","stomach")

breast <- data.frame(table(breast17$ward))
colnames(breast) <- c("ward_name","breast")

uterine <- data.frame(table(uterine17$ward))
colnames(uterine) <- c("ward_name","uterine")

intestine <- data.frame(table(intestine17$ward))
colnames(intestine) <- c("ward_name","intestine")


#各がんデータを結合
lungandstomach <- left_join(lung,stomach,by="ward_name")
twoplusbreast <- left_join(lungandstomach,breast,by="ward_name")
threeplusuterine <- left_join(twoplusbreast,uterine,by="ward_name")
cancer <- left_join(threeplusuterine,intestine,by="ward_name")

#sum列を追加
cancer <- cancer %>% mutate(sum=(apply(cancer[2:6],1,sum)))


#KDBデータ読み込み
kdb17 <- read.csv("kdbdata_2017.csv")

#わかりやすくrenameしておく
kdb17 <- rename(kdb17,ward_name=ward)

#割合を計算して列追加
kdb17 <- mutate(kdb17,percentage=round(kdb17[,2]/kdb17[,3]*100, digits=2))

#KDBデータと結合
data <- left_join(kdb17,cancer,by="ward_name")

#大阪市全体の行を削除
data <- data[-1,]

#受診率（受診者数/人口）の列を追加
data <- mutate(data,exampercent=round(data$sum/data$population*100, digits=2))


#１－２．全がん（受診率）

#どのデータをggplotするか、x軸とy軸を何にするか
gg1 <- ggplot(data, aes(x=data$percentage,y=data$exampercent))
#どのタイプの描写をするか（散布図はgeom_point）
gg2 <- gg1 + geom_point(size=2)
#回帰線を引く
gg3 <- gg2 + geom_smooth(method="lm",se=FALSE)
#テーマを設定、フォントも
gg4 <- gg3 + theme_bw(base_family="Japan1GothicBBB")
#軸の編集
gg5 <- gg4 + labs(x="国保率（国保加入者数／人口）",y="５がん受診率（受診者数／人口）")


#expressionは数式の描画に使う
#2乗はR^2とか、×は%*%とか
#formatは数字の桁数をそろえるため
#四捨五入とかとはちょっと違う？

#決定係数を書き込む
linerm_per <- lm(data$percentage~data$exampercent)
rs <- format(summary(linerm_per)$r.squared, digits = 3)
rsquared <- paste(expression("R^2=="),rs)
gg6 <- gg5 + annotate("text",x=29,y=6,label=rsquared,parse=TRUE)

#相関係数を書き込む
#相関係数
#-0.6165335
core <- format(cor(data$percentage,data$exampercent), digits=3)
coefficient <- paste(expression("r=="),core)
gg7 <- gg6 + annotate("text",x=30,y=14,label=coefficient,parse=TRUE)

#ピアソンの無相関検定
cor.test(data$percentage,data$exampercent, method="pearson")


#回帰式を書き込む
#ここからまだ
a <- format(coef(linerm)[1], digits=2)
b <- format(coef(linerm)[2], digits=2)

a_and_b <- paste(expression("y=="),a,expression(ifelse(b<0,"-",ifelse(b=0,"","+"))),b,expression("%*%"),"x")
gg6 <- gg5 + annotate("text",x=27,y=6,label=a_and_b,parse=TRUE)
gg6

plot(gg7)

a_and_b

#なぜこれではいけるのに
a_and_b <- paste(expression("y=="),a,expression("+"),b,expression("%*%"),"x")
gg6 <- gg5 + annotate("text",x=30,y=15000,label=a_and_b,parse=TRUE)
gg6

#これではだめ？
a_and_b <- paste(expression("y=="),a,expression("+"),b,"x")
gg6 <- gg5 + annotate("text",x=30,y=15000,label=a_and_b,parse=TRUE)
gg6

plot(gg7)

#決定係数は
#Rsquared:0.3801
#adjusted:0.3519
summary(linerm_per)


#グラフファイル出力(PDF)
ggsave(file = "74_2017_correlation_exampercent_sum_nationalinsurance.pdf", plot = gg7)


#１－３．全がん（受診者数）

#どのデータをggplotするか、x軸とy軸を何にするか
gg1 <- ggplot(data, aes(x=data$percentage,y=data$sum))
#どのタイプの描写をするか（散布図はgeom_point）
gg2 <- gg1 + geom_point(size=2)
#回帰線を引く
gg3 <- gg2 + geom_smooth(method="lm",se=FALSE)
#テーマを設定、フォントも
gg4 <- gg3 + theme_bw(base_family="Japan1GothicBBB")
#軸の編集
gg5 <- gg4 + labs(x="国保率（国保加入者数／人口）",y="５がん受診者数")


#expressionは数式の描画に使う
#2乗はR^2とか、×は%*%とか
#formatは数字の桁数をそろえるため
#四捨五入とかとはちょっと違う？

#決定係数を書き込む
linerm <- lm(data$percentage~data$sum)
rs <- format(summary(linerm)$r.squared, digits = 3)
rsquared <- paste(expression("R^2=="),rs)
gg6 <- gg5 + annotate("text",x=30,y=15000,label=rsquared,parse=TRUE)

#回帰式を書き込む
#ここからまだ
a <- format(coef(linerm)[1], digits=2)
b <- format(coef(linerm)[2], digits=2)

a_and_b <- paste(expression("y=="),a,expression(ifelse(b<0,"-",ifelse(b=0,"","+"))),b,expression("%*%"),"x")
gg6 <- gg5 + annotate("text",x=30,y=15000,label=a_and_b,parse=TRUE)
gg6

plot(gg6)

a_and_b

#なぜこれではいけるのに
a_and_b <- paste(expression("y=="),a,expression("+"),b,expression("%*%"),"x")
gg6 <- gg5 + annotate("text",x=30,y=15000,label=a_and_b,parse=TRUE)
gg6

#これではだめ？
a_and_b <- paste(expression("y=="),a,expression("+"),b,"x")
gg6 <- gg5 + annotate("text",x=30,y=15000,label=a_and_b,parse=TRUE)
gg6

#相関係数
#-0.4080177
cor(data$percentage,data$sum)

#決定係数は
#Rsquared:0.1665
#adjusted:0.1286
linerm <- lm(data$percentage~data$sum)
summary(linerm)


#グラフファイル出力(PDF)
ggsave(file = "75_correlation_examnumbers_sum_nationalinsurance.pdf", plot = gg6)









#２－１．2016年
#データ読み込み
lung16 <- read.csv("lung_2016.csv")
stomach16 <- read.csv("stomach_2016.csv")
breast16 <- read.csv("breast_2016.csv")
uterine16 <- read.csv("uterine_2016.csv")
intestine16 <- read.csv("intestine_2016.csv")


#地区を区名までにしてwardの列追加
lung16$ward <- str_extract(lung16$地区,"\\w+区")
stomach16$ward <- str_extract(stomach16$地区,"\\w+区")
breast16$ward <- str_extract(breast16$地区,"\\w+区")
uterine16$ward <- str_extract(uterine16$地区,"\\w+区")
intestine16$ward <- str_extract(intestine16$地区,"\\w+区")

#区ごとにカウントして新たなdata.frame作成
lung <- data.frame(table(lung16$ward))
colnames(lung) <- c("ward_name","lung")

stomach <- data.frame(table(stomach16$ward))
colnames(stomach) <- c("ward_name","stomach")

breast <- data.frame(table(breast16$ward))
colnames(breast) <- c("ward_name","breast")

uterine <- data.frame(table(uterine16$ward))
colnames(uterine) <- c("ward_name","uterine")

intestine <- data.frame(table(intestine16$ward))
colnames(intestine) <- c("ward_name","intestine")


#各がんデータを結合
lungandstomach <- left_join(lung,stomach,by="ward_name")
twoplusbreast <- left_join(lungandstomach,breast,by="ward_name")
threeplusuterine <- left_join(twoplusbreast,uterine,by="ward_name")
cancer <- left_join(threeplusuterine,intestine,by="ward_name")

#sum列を追加
cancer <- cancer %>% mutate(sum=(apply(cancer[2:6],1,sum)))


#KDBデータ読み込み
kdb16 <- read.csv("kdbdata_2016.csv")

#わかりやすくrenameしておく
kdb16 <- rename(kdb16,ward_name=ward)

#割合を計算して列追加
kdb16 <- mutate(kdb16,percentage=round(kdb16[,2]/kdb16[,3]*100, digits=2))

#KDBデータと結合
data <- left_join(kdb16,cancer,by="ward_name")

#大阪市全体の行を削除
data <- data[-1,]

#受診率（受診者数/人口）の列を追加
data <- mutate(data,exampercent=round(data$sum/data$population*100, digits=2))


#２－１．全がん（受診率）

#どのデータをggplotするか、x軸とy軸を何にするか
gg1 <- ggplot(data, aes(x=data$percentage,y=data$exampercent))
#どのタイプの描写をするか（散布図はgeom_point）
gg2 <- gg1 + geom_point(size=2)
#回帰線を引く
gg3 <- gg2 + geom_smooth(method="lm",se=FALSE)
#テーマを設定、フォントも
gg4 <- gg3 + theme_bw(base_family="Japan1GothicBBB")
#軸の編集
gg5 <- gg4 + labs(x="国保率（国保加入者数／人口）",y="５がん受診率（受診者数／人口）")


#expressionは数式の描画に使う
#2乗はR^2とか、×は%*%とか
#formatは数字の桁数をそろえるため
#四捨五入とかとはちょっと違う？

#決定係数を書き込む
linerm_per <- lm(data$percentage~data$exampercent)
rs <- format(summary(linerm_per)$r.squared, digits = 3)
rsquared <- paste(expression("R^2=="),rs)
gg6 <- gg5 + annotate("text",x=38,y=7.2,label=rsquared,parse=TRUE)

#相関係数を書き込む
#相関係数
#-0.6165335
core <- format(cor(data$percentage,data$exampercent), digits=3)
coefficient <- paste(expression("r=="),core)
gg7 <- gg6 + annotate("text",x=40,y=15,label=coefficient,parse=TRUE)

#回帰式を書き込む
#ここからまだ


#決定係数は
#Rsquared:0.3801
#adjusted:0.3519
summary(linerm_per)

plot(gg7)

#グラフファイル出力(PDF)
ggsave(file = "73_2016_correlation_exampercent_sum_nationalinsurance.pdf", plot = gg7)


#1.covarianceを手計算
per<-data$percentage
sum<-data$sum

#mean
permean<-mean(per)
summean<-mean(sum)
#numerator for cov
cornumerator<-sum((per-permean)*(sum-summean))
#calculation(numarator for R)
cornumerator<-cornumerator/length(per)
cornumerator

#comparison
cov(data$percentage,data$sum)


#2.R(cor)を手計算

#SD
persd<-sd(per)
sumsd<-sd(sum)
#calculation(denominator for R)
cordenominator<-persd*sumsd
numberR <- cornumerator/cordenominator
numberR
#comparison
cor(data$percentage,data$sum)


#回帰線を引く場合のalternative（正攻法）
slopea <- cov(data$percentage,data$sum)/var(data$percentage)
interceptb <- mean(data$sum)-slopea*mean(data$percentage)
slopea
interceptb

gg3 <- gg2 + geom_abline(slope=slopea,intercept=interceptb,colour="purple")
plot(gg3)






