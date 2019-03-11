#updateのためのパッケージ
install.packages("installr")

#ライブラリ
library(installr)

#以下を実施してアップデート開始
updateR(install_R = TRUE)

#パッケージを新たなバージョンにコピーするか、など聞かれる
#古いバージョンのファイルはコピーでなくカット＆ペーストしたはずだが残っている？
#Macでの方法は未知