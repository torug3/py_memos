library(dplyr)
library(ggplot2)
library(readxl)

#get file names
files <- list.files()

#filesを読み込んでbind
data <- do.call(rbind, lapply(files, read_excel))

#alternative
for (i in 1:length(files)){
  assign(paste("data", substr(as.character(files[i]),1,3), substr(as.character(files[i]),5,13),sep="_"), read_excel(files[i]))
}

#data/Timeの文字列を分解
year  <- as.numeric(substr(data$"data/Time", 1, 2)) + 2000
month <- as.numeric(substr(data$"data/Time", 3, 4))
day   <- as.numeric(substr(data$"data/Time", 5, 6))
hour  <- as.numeric(substr(data$"data/Time", 7, 8))
min   <- as.numeric(substr(data$"data/Time", 9,10))
hourmin <- hour * 100 + min
time  <- ISOdate(year, month, day, hour, min)
data <- data %>% mutate(Year=year,Month=month,Day=day,Hour=hour,Min=min,Time=time, Hourmin=hourmin)
data$Time <- as.POSIXct(data$Time, format="%Y/%m/%d", tz = "Japan")


#水道使用量を文字列から整数値に変換
data$"data/Dearee" <- as.integer(data$"data/Dearee")
#使用量を算出
data <- data %>% mutate(Amount = data$"data/Dearee" - lag(data$"data/Dearee"))

#ggplot
ggplot(data=data)+
geom_line(aes(x=Time, y=Amount))



#オブジェクトを確認
ls()
str(alldata)
View(alldata)


#try関数を間にかませるとエラーが出ても続けて読み込みできる
for(i in 1:30){
try(
assign(
	paste("data_01_",i,sep=""),
	read.csv(sprintf("201801%02dty_2701_oosaka.csv",i)))
,silent=TRUE)#silent=FALSEにするとエラーメッセージが表示される
}


#weather data
#品質情報は8なら正常
#天気記号で数値化可能？
data <- read.csv("20190514_weatherdata.csv", skip=5, fileEncoding="CP932")
names(data)
data <- data %>% rename(time=X,temperature_ave=X.1,temperature_max=X.4,temperature_min=X.5)
data <- data %>% select(time,temperature_ave,temperature_max,temperature_min)
data$time <- as.POSIXct(data$time, format="%Y/%m/%d", tz = "Japan")

ggplot(data=data)+
geom_line(aes(x=time, y=temperature_ave))

