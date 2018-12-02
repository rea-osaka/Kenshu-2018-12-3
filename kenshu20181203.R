########### 2018/12/3 研修用

options(digits=10)
options(scipen=100)

#-----下記は適宜修正を要す
setwd("C:/Users/fu300/OneDrive/デスクトップ/torihiki")

# install.packages("tidyverse")
library(tidyverse)

fnames <- dir(pattern="^\\d{2}_.*.csv") #ファイル名の取得
fnames
fnames <-fnames[34:35] #27番目が大阪府

dl <- lapply(fnames, read.csv)
torihiki <- do.call(rbind, dl)
rm(dl)
head(torihiki)
str(torihiki)

col_list <- c("No","種類","地域","code","県名","市名","地区名","駅","駅分","取引総額","坪単価","間取り",
              "m2","m2単価","土地形状","間口","延床面積","建築年","建物構造","建物用途","今後の利用目的","道路方位","道路種類","道路幅員",
              "都市計画","建ぺい率","容積率","取引時点","改装","備考" )
names(torihiki) <- col_list   # 列名を変更

num1_4 <- function(x) {
  x <- sub("１","1",x, fixed=T)
  x <- sub("２","2",x, fixed=T)
  x <- sub("３","3",x, fixed=T)
  x <- sub("４","4",x, fixed=T)
  return(x)
} 
torihiki$取引時点 <- as.factor(torihiki$取引時点)
levels(torihiki$取引時点) <- num1_4(levels(torihiki$取引時点))  # 全角数字を半角に 
torihiki$jiten <- as.integer(substr(torihiki[,28],3,4))+1988-.125+as.numeric(substr(torihiki[,28],7,7))*.25
torihiki$qtr <- paste(as.integer(substr(torihiki[,28],3,4))+1988,"Q",as.numeric(substr(torihiki[,28],7,7)),sep = "")
torihiki$tori_year <- as.integer(torihiki$jiten)
kami <- function(x) { ifelse(log10(x)<2, round(x, digits =0), signif(x,digits=2)) }
torihiki$推定m2 <- with( torihiki,ifelse(m2 == "2000㎡以上" & 種類 == "宅地(土地)",
                                       kami(取引総額/as.numeric(as.character(m2単価))),as.numeric(as.character(m2))))
torihiki$推定m2単価 <- with( torihiki, ifelse( 種類 == "中古マンション等", kami(取引総額/as.numeric(as.character(m2))),
                                          ifelse( (種類=="農地" | 種類 == "林地") & m2 != "5000㎡以上",kami(取引総額/as.numeric(as.character(m2))),as.numeric(m2単価))))
torihiki$建築年 <- as.character(torihiki$建築年)
torihiki$build_year <- with( torihiki, ifelse( 建築年=="戦前",1945,
                                              ifelse(建築年=="平成元年",1989,
                                                        ifelse(substr(建築年,1,2)=="昭和", as.integer(substr(建築年,3,nchar(建築年)-1))+1925,
                                                               ifelse(substr(建築年,1,2)=="平成", as.integer(substr(建築年,3,nchar(建築年)-1))+1988,NA)))))
torihiki$chikugo_year <- with(torihiki,round(jiten-build_year,digits = 0))
torihiki$eki_hun <- with(torihiki,as.integer(as.character(駅分))) #30分以上はNA
torihiki$yuka.m2 <- as.integer(as.character(torihiki$延床面積))
torihiki <- droplevels(torihiki)
torihiki$地域  <- torihiki$地域 %>% factor(levels=c("住宅地", "商業地", "工業地", "宅地見込地"))

torihiki$kibo <- cut(torihiki$推定m2,breaks=c(0,200,500,1000,5000,10000,30000, 10000000), right=FALSE)
levels(torihiki$kibo) <- c("200未満","500未満" ,"1000未満","5000未満","10000未満","30000未満","30000以上")

##### 事例絞り込み

# 大竹市の取引総額の分析
otake <- torihiki %>% subset(市名=="大竹市") %>% droplevels()
table(otake$種類)
otake %>% ggplot(aes(x=jiten, y=取引総額)) + 
  geom_point(aes(color=種類, shape=地域 ), size=5, alpha=0.8) +scale_shape(solid = TRUE) + theme_bw() +
  ggtitle("大竹市の種類・地域別、取引総額・時点plot")
otake %>% ggplot(aes(x=jiten, y=取引総額)) + 
  geom_point( size=3, alpha=0.3) + facet_grid(種類~地域)+theme_bw() +
  ggtitle("大竹市の種類・地域別、取引総額・時点plot")
otake_over_1oku <- otake %>% subset(取引総額>=100000000)
write.csv(otake_over_1oku, "otake_over_1oku.csv")
table(otake$種類, otake$地域)

otake %>% subset(種類=="宅地(土地)") %>% ggplot(aes(x=jiten,y=推定m2単価)) +
  geom_point(size=2, alpha=0.3) + facet_grid(地域~kibo)

##### 広島県・山口県

### 大竹市と周辺市町（廿日市市と山口県岩国市・玖珂郡和木町
shuhen <- torihiki %>% subset(市名=="大竹市" | 市名=="廿日市市" | 市名=="岩国市" | 市名=="玖珂郡和木町")
shuhen <- shuhen %>% droplevels()
table(shuhen$市名, shuhen$種類)
shuhen %>% ggplot(aes(x=jiten, y=取引総額)) + 
  geom_point(aes(shape=種類), size=3, alpha=0.4) +
  ggtitle("大竹市周辺の種類別、取引総額・時点plot")
# 岩国市の170億を除外 上限20億で市・種類悦
shuhen %>% ggplot(aes(x=jiten, y=取引総額)) + 
  geom_point( aes(color=地域), size=3, alpha=0.5) + facet_grid(種類~市名)+ylim(0,2000000000) +theme_bw() +
  ggtitle("大竹市周辺の種類別、取引総額・時点plot   岩国市平田の170億円除く")

# 1億円以上
shuhen_1oku <- shuhen %>% subset(取引総額 >= 100000000)
write.csv(shuhen_1oku, "shuhen_1oku.csv")  

### 周辺で、規模と単価
shuhen_tochi <- shuhen %>% subset(種類=="宅地(土地)")

shuhen_tochi %>% ggplot(aes(jiten, m2単価 )) + geom_point( aes(shape=市名),size=2, alpha=0.3) + 
  geom_smooth(se=FALSE) + facet_grid(地域~kibo) + theme_bw()+ ggtitle("大竹市周辺の用途・規模別の時点・m2単価プロット")

shuhen_tochi_1man <- shuhen_tochi %>% subset(推定m2 >= 10000)

shuhen_tochi_1man %>% ggplot(aes(jiten, m2単価, shape=市名, color=地域 )) + geom_point(size=5, alpha=1) + 
  theme_bw()+ ggtitle("大竹市周辺の1万m2以上の時点・m2単価プロット")
shuhen_tochi_1man %>% ggplot(aes(jiten, m2単価, shape=地域, color=地域 )) + geom_point(size=5, alpha=1) + 
  theme_bw()+facet_grid(市名~.)  +ggtitle("大竹市周辺の1万m2以上の時点・m2単価プロット")

### 2県全体
# 総額
torihiki %>% ggplot(aes(x=jiten,y=取引総額)) + geom_point()
torihiki %>% ggplot(aes(x=jiten,y=取引総額)) + geom_point(size=2.5, alpha=0.3)+facet_grid(県名~種類)

# m2単価
tochi <- torihiki %>% subset(種類=="宅地(土地)")

tochi %>% ggplot(aes(jiten, m2単価 )) + geom_point(size=2, alpha=0.3) + 
  geom_smooth(se=FALSE) + facet_grid(地域~kibo) + theme_bw()+ ggtitle("広島県と山口県の用途・規模別の時点・m2単価プロット")

tochi %>% subset(推定m2 >= 30000) %>% 
  ggplot(aes(jiten, m2単価 )) + geom_point(size=2, alpha=0.3)+ # ylim(0,200000) + 
  geom_smooth(se=FALSE) + facet_grid(地域~県名) +theme_bw() + ggtitle("県・用途別の時点・m2単価プロット(3万m2以上) ")

table(otake$推定m2)
table(otake[str_detect(otake$種類,"宅地"),]$推定m2 )


#############################################
######  大阪府  ###ここから全く別のplot
###  ファイル名の取得 
fnames <- dir(pattern="^\\d{2}_.+_\\d{5}_\\d{5}.csv")  #ファイル名の取得
fnames <- fnames[c(27)]  # c( )内に数字  京都と兵庫c(26, 28) 三重と大阪～和歌山c(24,27:30)
fnames

### ファイルの結合
dl <- lapply(fnames, read.csv,encoding = "Shift_JIS",na.strings = c("","NULL"))
torihiki <- do.call(rbind, dl)
rm(dl)

col_list <- c("No","種類","地域","code","県名","市名","地区名","駅","駅分","取引総額","坪単価","間取り",
              "m2","m2単価","土地形状","間口","延床面積","建築年","建物構造","建物用途","今後の利用目的","道路方位","道路種類","道路幅員",
              "都市計画","建ぺい率","容積率","取引時点","改装","備考" )
names(torihiki) <- col_list   # 列名を変更

num1_4 <- function(x) {
  x <- sub("１","1",x, fixed=T)
  x <- sub("２","2",x, fixed=T)
  x <- sub("３","3",x, fixed=T)
  x <- sub("４","4",x, fixed=T)
  return(x)
} 
torihiki$取引時点 <- as.factor(torihiki$取引時点)
levels(torihiki$取引時点) <- num1_4(levels(torihiki$取引時点))  # 全角数字を半角に 
torihiki$jiten <- as.integer(substr(torihiki[,28],3,4))+1988-.125+as.numeric(substr(torihiki[,28],7,7))*.25
torihiki$qtr <- paste(as.integer(substr(torihiki[,28],3,4))+1988,"Q",as.numeric(substr(torihiki[,28],7,7)),sep = "")
torihiki$tori_year <- as.integer(torihiki$jiten)
kami <- function(x) { ifelse(log10(x)<2, round(x, digits =0), signif(x,digits=2)) }
torihiki$推定m2 <- with( torihiki,ifelse(m2 == "2000㎡以上" & 種類 == "宅地(土地)",
                                       kami(取引総額/as.numeric(as.character(m2単価))),as.numeric(as.character(m2))))
torihiki$推定m2単価 <- with( torihiki, ifelse( 種類 == "中古マンション等", kami(取引総額/as.numeric(as.character(m2))),
                                          ifelse( (種類=="農地" | 種類 == "林地") & m2 != "5000㎡以上",kami(取引総額/as.numeric(as.character(m2))),as.numeric(m2単価))))
torihiki$建築年 <- as.character(torihiki$建築年)
torihiki$build_year <- with( torihiki, ifelse( 建築年=="戦前",1945,
                                              ifelse(建築年=="平成元年",1989,
                                                        ifelse(substr(建築年,1,2)=="昭和", as.integer(substr(建築年,3,nchar(建築年)-1))+1925,
                                                               ifelse(substr(建築年,1,2)=="平成", as.integer(substr(建築年,3,nchar(建築年)-1))+1988,NA)))))
torihiki$chikugo_year <- with(torihiki,round(jiten-build_year,digits = 0))
torihiki$eki_hun <- with(torihiki,as.integer(as.character(駅分))) #30分以上はNA
torihiki$yuka.m2 <- as.integer(as.character(torihiki$延床面積))
torihiki$市地区 <- paste0(torihiki$市名, torihiki$地区名)
torihiki <- droplevels(torihiki)
torihiki$地域  <- torihiki$地域 %>% factor(levels=c("住宅地", "商業地", "工業地", "宅地見込地"))

torihiki$city <- as.character(torihiki$市名)
torihiki$city[str_detect(torihiki$市名,"^大阪市")] <- "大阪市"
torihiki$city[str_detect(torihiki$市名,"堺市")] <- "堺市"
torihiki$city <- as.factor(torihiki$city)

### 絞り込み
city_n <- torihiki %>% group_by(市名) %>% summarise(
            chiku_n=length(unique(市地区)),
            eki_n = length(unique(駅)) )
torihiki %>% subset(市名=="阪南市") %>% ggplot(aes(x=jiten, y=取引総額)) + 
  geom_point(aes(color=地域,shape=種類),size=3, alpha=0.5) +
  ggtitle("時点・取引総額plot")  


torihiki %>% subset(市名=="阪南市") %>% ggplot(aes(x=jiten, y=取引総額)) + 
  geom_point(aes(color=地域,shape=種類),size=3, alpha=0.5) +
  ggtitle("地区毎の時点・取引総額plot")  + facet_wrap(地区名~.)

torihiki %>% subset(市名=="大阪市城東区" & 種類=="宅地(土地)") %>% ggplot(aes(x=jiten, y=取引総額)) + 
  geom_point(aes(color=地域,shape=地域),size=3, alpha=0.5) +
  ggtitle("土地の地区毎の時点・m2単価plot")  + facet_wrap(地区名~.)



torihiki %>% subset(市名=="南河内郡河南町") %>% ggplot(aes(x=jiten, y=m2単価)) + 
  geom_point(aes(color=地域,shape=種類),size=3, alpha=0.5) +
  ggtitle("土地の地区毎の時点・m2単価plot")  + facet_wrap(地区名~.)
torihiki %>% subset(市名=="東大阪市") %>% ggplot(aes(x=jiten, y=m2単価)) + 
  geom_point(aes(color=地域,shape=種類),size=3, alpha=0.5) +
  ggtitle("土地の地区毎の時点・m2単価plot")  + facet_wrap(駅~.)




#======  東大阪市の更地、住宅・商業・工業の規模悦単価
hosaka <- torihiki[torihiki$市名=="東大阪市",]
hosaka_tochi <- hosaka[hosaka$種類 == "宅地(土地)",]                            
table(hosaka_tochi$地域)                          
#宅地見込地は少ないので除外
hosaka_tochi <- hosaka_tochi[hosaka_tochi$地域 != "宅地見込地",]
hosaka_tochi <- droplevels(hosaka_tochi)
# 地域factorを住宅・商業・工業の順に並びかえる
hosaka_tochi$地域 <- fct_relevel(hosaka_tochi$地域,c("住宅地","商業地","工業地"))

# cut関数を使って、地積を区分(right=TRUEだと例えば100と200の間なら、100以上200未満、FALSEだと100超200ｍ以下となる)
hosaka_tochi$kibo <- cut(hosaka_tochi$推定m2,breaks=c(0,100,250,500,1000,5000,100000), right=FALSE)
levels(hosaka_tochi$kibo) <- c("100未満","100以上250未満","250以上500未満","500以上1000未満","1000以上5000未満","5000以上")
table(hosaka_tochi$kibo)

#####描画
# 全体の規模別
hosaka_tochi %>% ggplot(aes(jiten,m2単価)) + geom_point(size=2, alpha=0.5)+geom_smooth() +  facet_wrap(kibo~.)
# 規模と地域別
hosaka_tochi %>% ggplot(aes(jiten,m2単価)) + geom_point(size=2, alpha=0.5)+geom_smooth() +  facet_grid(kibo~地域)
hosaka_tochi %>% ggplot(aes(jiten,m2単価)) + geom_point(size=2, alpha=0.5)+geom_smooth() +  facet_grid(地域~kibo)

#上記のサンプル数
aggregate(kibo ~ 地域, data=hosaka_tochi, table)

#=================  工業地・更地のみで単価・時点のグラフを作成
osaka_kogyo <- torihiki[torihiki$地域=="工業地" & torihiki$種類=="宅地(土地)",]
osaka_kogyo$city %>% table() %>% sort() %>% tail()
# 大阪市、東大阪市、堺市のデータに絞り込み
osaka3_kogyo <- osaka_kogyo[str_detect(osaka_kogyo$city,"大阪市") | str_detect(osaka_kogyo$city,"堺市"),]
osaka3_kogyo <- osaka3_kogyo %>% droplevels()


# cut関数を使って、地積を区分(right=TRUEだと例えば100と200の間なら、100以上200未満、FALSEだと100超200ｍ以下となる)
# サンプル数に応じてcut関数のbreaks=c(  )内を適宜修正, 併せてlevelの数や名称も修正
osaka3_kogyo$kibo <- cut(osaka3_kogyo$推定m2,breaks=c(0,100,250,500,1000,5000,100000), right=FALSE)
levels(osaka3_kogyo$kibo) <- c("100未満","100以上250未満","250以上500未満","500以上1000未満","1000以上5000未満","5000以上")

osaka3_kogyo$city <- fct_relevel(osaka3_kogyo$city, c("大阪市", "堺市", "東大阪市"))
aggregate(data=osaka3_kogyo,kibo~city,table)


#---グラフ描画
# 年ごとの箱ひげ図
ggplot(data=osaka3_kogyo) + geom_boxplot(aes(x=as.factor(tori_year), y=m2単価))
# 四半期と単価の散布図
ggplot(data=osaka3_kogyo,aes(x=jiten, y=m2単価)) + geom_point(aes(alpha=0.5))+geom_smooth()
# 見にくいので、規模別に描画
osaka3_kogyo %>% ggplot(aes(jiten, m2単価 )) + geom_hline(yintercept=c(100000,200000),size=0.3) +
  geom_point(size=2, alpha=0.3) + geom_smooth() + facet_grid(city~kibo) + ggtitle("大阪市・堺市・東大阪市の工業地  規模別 土地m2単価")

# テーマ変更
osaka3_kogyo %>% ggplot(aes(jiten, m2単価 )) + geom_point(size=2, alpha=0.3) + geom_smooth() + facet_grid(city~kibo) + theme_bw()

