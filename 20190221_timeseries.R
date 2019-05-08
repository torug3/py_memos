#time series data
#時系列データは「一定の間隔で」測定したもの。あらかじめ間隔を設定しておく必要がある。
#RFinanceYJというパッケージ
#quoteStockTsDataという関数を使うと、ヤフージャパンが提供している株価情報にアクセスしてHTMLからとってくる
fjstock <- quoteStockTsData("6702.t", "2012-01-01")

#時系列データはどこで生まれているか？
#サーバのアクセスログ、センサーデータ、コールセンターの問い合わせ

#時系列データの成分
#ドイツ株価指数（Rに標準で入っているデータ）
EuStockMarkets
head(EuStockMarkets)
nrow(EuStockMarkets)
class(EuStockMarkets)
plot(EuStockMarkets[,"DAX"],type="l")#1(いち)じゃなくてL

#データの成分を分解
decomp <- decompose(EuStockMarkets[,"DAX"])
head(decomp)
#トレンド＝長期的な変化の傾向
#季節成分＝必ず繰り返される周期的な変動（毎週月曜日に・・とか）
#ノイズ（random）＝トレンドと季節成分で説明できない残りの部分＝邪魔なものではなく、ノイズの原因を考えることで社会の変化が読み取れる
plot(decomp)

#3-6
#時系列データの分析に回帰分析は適さない。なぜなら・・
#回帰分析はトレンドや季節成分がない線形モデルだから

#3-7分析の流れ
#1．時系列データの読み込み、ts型に変換する
#2.データをプロットしてみてみる（ts.plotもしくはplot.ts）
#3.単位根（たんいこん）検定を行って定常過程か非定常過程かを判別
#基本的には定常過程になっていることが望ましい
#4.非定常データであれば差分または対数に変換
#5.モデルの適用・予測

#utility billsの読み込み
bills <- read.csv("utility_bills.csv")
str(bills)
#時系列データへ変換
gas <- ts(bills$gas, freq=12, start=c(2014,8))
electric <- ts(bills$electric, freq=12, start=c(2014,8))

#水道代は2か月おきなのでna.omit必要
water <- na.omit(bills$water)

gas <- na.omit(bills$gas)
#そののちfreq=6でtsに
water <- ts(water, freq=6, start=c(2014,8))
str(gas)
str(electric)
str(water)

plot(bills$gas,bills$electric)
#単一データの描写
plot.ts(gas, xlab="年", ylab="ガス代")
plot.ts(electric, xlab="年", ylab="電気代")
plot.ts(water, xlab="年", ylab="水道代")

#複数のデータを重ねて描写するには
ts.plot(gas, electric, col=c("blue","red"), lty=c(3,1))
#freqが異なると描けない
ts.plot(gas, water, col=c(4,2), lty=c(3,1))

#3-9
#自己相関関数、過去の自分との相関
#過去のどの時点と現在の時点との関係が強いか、がわかる
#相関の高いデータを使うことで予測の精度が高まる（直近のものより）
acf(water,lag.max=48)#auto correlation function
#左端の１は自分自身
#負の相関がある時点（例えば3か月前）をみて、3か月前のガス代が高いと、今月は安いとなる
#純粋に1年前の相関を見たい場合は偏自己相関関数を見る（自己相関関数は累積するようなイメージ）
##たしかに右肩下がり
pacf(water,lag.max=48)#auto correlation function
#一本目から先月のデータ（自己相関関数とは異なる）
#青い点線は信頼区間＝突き抜けない部分はほぼ誤差

#3-10
#単位根検定
#adf.testを使って定常過程なのか非定常過程なのかを判別
#定常過程の方が予測の精度が上がる
#非定常過程＝変化が不規則→なんとか解消する手段を用いる必要がある
#帰無仮説が「非定常過程である」ということ
library(tseries)
adf.test(gas)
adf.test(water)

#3-11
#非定常データの変換
#非定常過程であれば「差分」をとったり「対数化」することで定常過程に変換する（ことを試してみる）
#差分をとる関数
difference <- diff(EuStockMarkets[,"DAX"])
#もともとは非定常過程のデータだったが、差分をとると定常過程になる
adf.test(difference)
plot(EuStockMarkets[,"DAX"])
plot(difference)
#対数＝eの何乗か
eulog <- log(EuStockMarkets[,"DAX"])
adf.test(eulog)
#これでもまだ非定常過程
#両方ともダメなら差分をとって、さらに対数に変換する、という方法もある
diflog <- log(diff(EuStockMarkets[,"DAX"]))
#こっちのほうがうまくいく
logdif <- diff(log(EuStockMarkets[,"DAX"]))
head(logdif)

#3-13
#過去のデータから予測式を立てるARモデル（auto regression）
#データの読み込み
bills <- read.csv("utility_bills.csv")
gas <- ts(bills$gas, freq=12, start=c(2014,8))
gas <- na.omit(gas)
#予測に使うデータを取り出す
gasmodel <- gas[2:37]
#ar関数でモデルを作成
armodel <- ar(gasmodel, method="ols") #ordinary least square
#predict(予測に使うモデル、予測に使うデータ、n.aheadで期間)
armodel2 <- predict(armodel, gasmodel, n.ahead=24)
#予測した結果をtime seriesに。予測のスタート時点を指定
armodel3 <- ts(armodel2$pred, start=c(2019,1), freq=12)
#元データを描写
plot(gas, xlim=c(2015,2020))
#linesで予測値を重ねる
lines(armodel3, col="red")
#予測値はここに入っている
armodel3

#3-14MA
#moving average＝移動平均モデル、でも移動平均ではなく自己相関に基づくモデル
#ARIMA関数で疑似的にMAモデルを実現（orderで指定）
mamodel <- arima(gasmodel)
mamodel <- arima(gasmodel, order=c(0,0,1)) #一次のMAモデルを適用
#予測値を描画するためのforecastパッケージを使用
library(forecast)
mamodel2 <- ts(forecast(mamodel, h=24)$mean, start=c(2015,1), freq=12)
#描写
plot(gas)
lines(mamodel2,col="red")

#3-15 ARIMA自己回帰和分移動平均モデル、ARとMAモデルを組み合わせたもの
#Iがintegretion=和分＝離散データに対する積分
#time series is discrete
#離散データを和分することによって変化が滑らかになる＝非定常過程のものを定常過程に近づけられる
#ARIMAモデル自身に非定常過程から定常過程への変換が含まれる
result <- arima(gasmodel, order=c(0,1,1),
		    seasonal=list(order=c(0,1,1), period=12))
plot(forecast(result, h=24))

#3-16
#forecastパッケージのauto.arima関数で最適なモデルを推定できる
#Inf(AIC)が小さいほど良いモデル
result.arima <- auto.arima(gas, trace=T, stepwise=T)
result.arima
plot(forecast(result.arima, h=24))

#3-17,様々な時系列データ分析手法
#ベイジアンネットワーク、階層ベイズモデル
#決定木モデル
#非定常過程のデータをより精度よく予測するにはどうすればいいか、という課題

#4-1、周辺システムとの連携
#RDBMS
#RからSQL文を投げて、指定されたデータだけ取り出して、それを分析する
#Rとプログラム言語との連携＝書いたプログラムの一部にRの機能を呼び出す部分を含める

#4-2 RとRDBMSとの連携
#パッケージRODBC→RからODBCドライバを呼び出してDBに接続する
#ODBCはMSが開発した汎用のデータベース規格

#以下は32ビット版でしかできない！？
#Accessに接続して、その中のDBからSQL文を発行してデータを取り出す
library(RODBC)
#Access形式のデータを読み込み
PriceDB <- odbcConnectAccess2007("主要品目小売価格.accdb")
#DBのテーブルの一覧
sqlTables(PriceDB)
#テーブル内の全項目を取得してデータフレームに格納
Price_Table <- sqlQuery(PriceDB,"select * from 主要品目小売価格")
#1960年の各品目の価格を取得してデータフレームに格納
Price_1960 <- sqlQuery(PriceDB,"select * from 主要品目小売価格 where 年次 = 1960")
#1960年代の各品目の価格を取得してデータフレームに格納
Price_196X <- sqlQuery(PriceDB,"select * from 主要品目小売価格 where 年次 between 1960 and 1969")

odbcClose(PriceDB)


#4-3
#Rは通常はCPUの1コアしか使わない→マルチコアで計算する場合には
#
install.packages(c("snow","snowfall"))
library(snowfall)
sfInit(paralle=T, cpus=3)#ローカルPCの3コアを使用（マルチコア環境の初期化）
#Init＝initiallization
#paralle=分散並列処理をしますよ

#ここでは核CPUに割り当てる作業を関数の中で定義
f <- function(x){
	data <- EuStockMarkets[,"DAX"]
	arima(data, order=c(x,x,x))
}
#system.timeで関数の実行にかかった時間を図る
#sLapplyで各コアごとに異なる命令が渡される
system.time(sLapply(1:3, f))
#並列処理を終了
sfStop()

#4-4 RとHadoopとの連携
#たくさんのサーバを組み合わせて早く処理を行うための分散並列処理の仕組み
#1代1代のサーバに小分けにした仕事を割り振る
#普通はJAVAで書く
#Hadoop Streamingを使って、標準入出力を使ってほかのプログラムにデータを渡して処理させる
#そしてそのプログラムが返してきた結果をまた受け取って集計する（Hadoopによる処理の途中でほかのプログラムに処理を投げる）
#Hadoopから分析データを投げる先としてRを指定する
#その結果をまたHadoopに投げて集計していく
#Hadoopで処理した結果は最終的に「分散ファイルシステム」に格納される

### Map処理： mapper.R、　Reduce処理：reducer.R
#!/usr/bin/Rscript

#標準入力からデータ読み取り
input <- file(description="stdin", open="r")
#1行ずつ読み込み
while(length(line <- readLines(input, n=1, warn=FALSE) >0){
#・・・
close(input)


###Hadoop streamingで処理
$HADOOP_HOME/bin/hadoop jar $HADOOP_HOME/contrib/streaming/hadoop-1.1.2-
streaming.jap -files mapper.r, reducer.r -input 入力ファイル名 -output 出力ディレクトリ名
-mapper mapper.R -reducer reducer.r


###処理結果の確認
$HADOOP_HOME/bin/hadoop fs -cat 出力ディレクトリ名/part-00000

#4-5
#rmr2パッケージでRからHadoopの機能を利用
#Hadoop streamingはHadoopからRを操作するが、これは逆
#Rの文法でプログラムが書ける（HadoopのためにJAVAはたいへん）

#4-6　Rを組み込んだプログラム開発
#RはもともとC言語
#C言語との間でやり取りするAPIをR自身が持っている
#C言語のインターフェースを介してJAVAとの間でデータのやり取りができるパッケージがある
#ｒJAVAパッケージ

#4-7　Rと統計ソフトウェアとの連携
#SASやSPSS、JMP、でもオプション機能なので料金がかかる
#Excelとならxlsxパッケージで可能（XLConnectパッケージもある）
#Apache POIらいぶらりをしようしているため、Windows,office環境がなくても処理可能
library(xlsx)
data <- read.xlsx("data.xls", 1) #シート1のデータを読み込む
library(XLConnect)
data <- readWorkSheetFromFile("data.xls", 1) #シート1のデータを読み込む












gas
mrmodel
forecast(mrmodel, h=24)

str(gas)
class(gas)
head(eulog)
gas
armodel2
plot(armodel3)
plot(EuStockMarkets[,"DAX"])

gas <- ts(bills$gas, freq=12, start=c(2014,8))
electric <- ts(bills$electric, freq=12, start=c(2014,8))
water <- na.omit(bills$water)
gas <- na.omit(bills$gas)
water <- ts(water, freq=6, start=c(2014,8))
str(gas)
