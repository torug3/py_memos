#5-73

library(tidyverse)

test <- tibble(umare=c(1990,1992,1997,1991),
		   height=c(180,176.2,165.5,172.3),
		   weight=c(70.2,80.3,65.3,61.1))

mutate(.data=test,


test %>% mutate(name=c("suzuki","honda","toyota","nihon"))


test <- test %>% mutate(bmi=weight/(height/100)^2)


#5-74
#列の名前を変える rename

test <- test %>% rename(birth_year = umare)

#selectで、列の一部だけを抽出
test %>% select(birth_year,weight)
#除去することも可能
test %>% select(-height,-weight)

test2 <- tibble(v1=c(10:20),v2_99=c(20:30),v100=c(30:40),
		    x1=c(40:50),x2_30=c(50:60),x31=c(31))

#抽出
test2 %>% select(starts_with("v"))
test2 %>% select(ends_with("1"))
test2 %>% select(contains("2"))
#並び替え
test2 %>% select(x31,everything())

#practice
test %>% select(contains("b"))
test2 %>% select(starts_with("x"),everything())
test2 %>% select(starts_with("x"),starts_with("v"))

test2 %>% select(x31,x2_30,x1,v100,v2_99,v1)
#simplify->
col2 <- colnames(test2)
rev(col2)
test2 %>% select(rev(col2))
test2 %>% {select %>% colnames() %>% rev() }
test2 %>% select({test2 %>% colnames() %>% rev()})

#5-76
#testの身長を順番に並び替える

test %>% arrange(height)#昇順
test %>% arrange(desc(height))#昇順(descending-ascending)

test3 <- tibble(grp1=c(rep(c(1:10),2),rep(c(10:1),2)),
		    grp2=c(rep(c(5:1),4),rep(c(1:5),4)),
		    grp3=c(rep(c("a","b","c","d"),10))   )

View(test3)

test3 %>% arrange(grp1)
test3 %>% arrange(desc(grp2))
test3 %>% arrange(grp1,grp3)
test3 %>% arrange(grp3,grp2)
test3 %>% arrange(desc(grp1),grp2,desc(grp3))

#5-77,78

450==450
#<（小なり）
#>（大なり）

450!=45
# !=は同じでない、を表す


#5-79

vec <- c(1:20)

vec==1
vec<10

#include?
c(1,2,3,4,5) %in% c(3,4)

#あるベクトルから特定のベクトルを抜き出したい
vec <- c(1:5)

vec[c(TRUE,TRUE,TRUE,TRUE,TRUE)]
vec[c(TRUE,TRUE,TRUE,TRUE,FALSE)]
vec[c(TRUE,TRUE,FALSE,TRUE,FALSE)]

vec <3
vec[vec<3]

#つまり、logicalのベクトルは、ベクトルから特定の要素を抜き出すときに使う

#5-80 and と or

# &はand

TRUE & TRUE
TRUE & FALSE
FALSE & FALSE

5==5 & 7>3

5==5 & 7<3
5==5 | 7<3


#5-80

# 

vec <- c(NA,1,2,3,NA,4,5,6)

vec[is.na(vec)]

vec[!is.na(vec)]


#5-81

# 文字列にしるしをつける＝正規表現

vec <- c("HbA1c:9.2%","ALT:12OUI","WBC:9.3*10^3")

#↑の数字だけを取り出す（難しい？？）

library(stringr)
str_extract(vec, "(?>=:)(\\d+\\.\\d)|(?<=:)\\d+")

#5-82
#正規表現とは、文字列をパターンでひっかける方法

vec <- c("1","120","34.3","ab123","5b","6 5","7","b","ac4235432","45.3mg/dl","abc500ml 3unit 3:40AM","^ is start","this sign($)represents end.","....")

str_detect(vec,"1")
#str_detectはlogicalを返してくれるので、以下のようにすれば取り出せる。
vec[str_detect(vec,"1")]

#複数の文字列をひっかけたい場合（1か2か3か4）
check <- str_detect(vec,"[1234]")
vec[check]

#str_viewを使うと見やすい
#いちばん最初に来る数字が当たってる
str_view(vec,"[1234]")

#以下の2つは同じ
str_detect(vec,"[1234567890]")
str_detect(vec,"[0-9]")
str_detect(vec,"\\d")

#2文字ひっかけたい場合は
str_view(vec,"[0-2][0-2]")
#もしくは
str_detect(vec,"[0-2]{2}")

#1から3まで
str_detect(vec,"[0-5]{1,3}")
str_view(vec,"[0-5]{1,3}")
str_view(vec,"[0-9]{,3}")#最大3回
str_view(vec,"[0-9]{1,}")#1回以上
str_view(vec,"[0-9]+")#上の1回以上、は+で書き換えることも可能
str_detect(vec,"\\d+")

#なんでもいい、ときは
str_detect(vec,".")
str_detect(vec,".+")#すべての文字列の長さ

#5-83
#bが文字列の先頭にあるかどうか
str_detect(vec,"^b")
str_detect(vec,"^H")

#bが文字列の最後にあるかどうか
str_detect(vec,"b$")

str_detect(vec,"^b$")#bだけをあてる
str_detect(vec,"^\\d+$")#数字だけをあてる
str_view(vec,"^\\d+$")#数字だけ（の文字列）をあてる


#記号としての^や.をあてるときは？
str_view(vec,"\\$")
str_view(vec,"\\.+")


#5-85

#このあたり冗長な内容が続く、、、内容もわからないので振り返る必要

str_detect(vec,"(?>=room)\\d+")

#すべての文字
str_view(vec,"\\w+")
#すべての非文字
str_view(vec,"\\W+")
#すべての数字
str_view(vec,"\\d")
#すべての非数字
str_view(vec,"\\D+")
#すべてのスペース
str_view(vec,"\\s+")
#すべての非スペース
str_view(vec,"\\S+")


#5-87
test$umare > 1995

#このBooleanを、filterはtibbleの列に適用してTRUEであるものを抜き出す

#filterの中にはlogicalが入る
#どんなlogicalが入るか、試行錯誤してみること

test %>% filter(test$umare>1995)
test %>% filter(test$height>=175)

#diamondsにfilterを適用してみる
diamonds %>% filter(color=="E")

diamonds %>% filter(clarity=="SI1" | clarity=="SI2")
#でもいいけど
diamonds %>% filter(str_detect(clarity,"^SI\\d+$"))

diamonds %>% filter(str_detect(clarity,"\\d"))
#でもいいし、
diamonds %>% filter(clarity != "IF")
#でも同じ。どんなlogicalを作るか。

#5-88 20180918 練習問題
dft <- tibble(
	target1 = c(
	"abc500ml 3unit",
	"def200ml 4unit",
	"ghi100ml 5unit"
	),
	target2 = c(
	"AST 50IU",
	"HbA1c 5.0%",
	"BMI 23.1g/m^2"
	),
	target3 = c(
	"opeA:4.5hr 80ml",
	"opeB:3hr 10ml",
	"opeC:12.5hr 100ml"
	)
)
dft

#練習１
#わからん。これだといけるのに
dft$target1 %>% str_extract("\\d+(?=unit)")

#これはダメなのは何で？
dft %>% str_extract(target1,"\\d+(?=unit)")

dft$target1 %>% str_extract("\\d")
str_detect(dft$target1,"\\d")
#これもダメ
dft$target1 %>% filter(str_detect(dft$target1,"//d"))

#練習２は1と同じ。unitがmlにかわるだけ
#練習３

#これが一応正解みたい
dft$target1 %>% str_extract("[a-z]+(?=\\d)")
#これでもOK（違いは何だろう？）
dft$target1 %>% str_extract("^[a-z]+(?=\\d)")
#これだとだめなのは何でだろう
dft$target1 %>% str_extract("^\\w+(?=\\d)")
dft$target1 %>% str_extract("\\w+")
#練習4
dft$target2 %>% str_extract("\\d+")
dft
str_detect(dft$target2 %>% 
#練習5
#".+"ですべての文字の繰り返し
#str_replace、マッチした結果を別の文字列に置き換えることができる
#置き換えの対象を指定しなければ（空白にすれば）

#練習６，７、
#正規表現については復習の必要大いにあり

#5-91 正規表現の補足
#日本語の問題と全角数字の問題
#"Nippon"というライブラリを使えば全角から半角へ変換ができる

#5-92 if_elseとcase_when
q()