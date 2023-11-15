df <- read.csv("최종데이터프레임_편의점추가.csv", header = T)
head(df)

univ_d = df$univ_d
univ_in5 = df$univ_5

bank_d = df$closest_bank_d
bank_in5 = df$bank_in2

fa1_d = df$closest_fa_d
fa2_d = df$closest_fa2_d
fa1_in2 = df$fa_in2
fa2_in2 = df$fa2_in2

market_d = df$closest_market_d
market_in3 = df$market_in3

metro_d = df$closest_metro_d
metro_in2 = df$metro_in2

bus_d = df$closest_bus_d
bus_in2 = df$bus_in2

culture_d = df$closest_culture_d
culture_in2 = df$culture_in2

cafe_d = df$closest_cafe_d
cafe_in2 = df$cafe_in2

conv_d = df$closest_conv_d
conv_in1 = df$conv_in1

df_all <- data.frame(univ_d=univ_d, univ_in5=univ_in5, bank_d=bank_d, bank_in5=bank_in5,fa1_d=fa1_d,fa1_in2=fa1_in2, fa2_d=fa2_d, fa2_in2=fa2_in2,market_d=market_d, market_in3=market_in3,metro_d=metro_d,metro_in2=metro_in2, bus_d=bus_d, bus_in2=bus_in2, cafe_d=cafe_d, cafe_in2=cafe_in2, conv_d=conv_d, conv_in1=conv_in1)

all_cor <- cor(df_all)
all_cor

write.csv(all_cor, file="모든 수치 상관계수.csv")

png("모든수치 상관분석2.png")
corrplot(all_cor, method="number", tl.srt=30, tl.col="black", diag=F, order = "AOE")
dev.off()

df_d =data.frame(univ_d=univ_d, bank_d=bank_d, fa1_d=fa1_d, fa2_d=fa2_d, market_d=market_d, metro_d=metro_d, bus_d=bus_d, cafe_d=cafe_d, conv_d=conv_d)
png("거리별 상관분석.png")
plot(df_d)
dev.off()

d_cor <- cor(df_d)
png("거리별 상관분석2.png")
corrplot(d_cor, method="number", tl.srt=30, tl.col="black", diag=F, order = "AOE")
dev.off()

df_cnt <- data.frame(univ_in5=univ_in5, bank_in5=bank_in5, fa1_in2=fa1_in2, fa2_in2=fa2_in2, market_in3=market_in3, metro_in2=metro_in2,bus_in2=bus_in2,cafe_in2=cafe_in2, conv_in1=conv_in1)
png("개수별 상관분석.png")
plot(df_cnt)
dev.off()

cnt_cor <- cor(df_cnt)
png("개수별 상관분석2.png")
corrplot(cnt_cor, method="number", tl.srt=30, tl.col="black", diag=F, order = "AOE")
dev.off()

install.packages("corrplot")
library(corrplot)


## 교통 수단 별 상관관계

df_trans <- data.frame(univ_d=univ_d, univ_in5=univ_in5, bus_d=bus_d, bus_in2=bus_in2, metro_d=metro_d, metro_in2=metro_in2)
plot(df_trans)

trans_cor <- cor(df_trans)
trans_cor

png("교통 및 대학가 별 상관분석.png")
corrplot(trans_cor, method="circle", tl.col="black", addCoef.col = "black", tl.srt = 0, diag = F)
dev.off()

## 편의시설 별 상관관계

df_uti <- data.frame(univ_d=univ_d, univ_in5=univ_in5, bank_d=bank_d, bank_in5=bank_in5, fa1_d=fa1_d, fa1_in2=fa1_in2, fa2_d=fa2_d, fa2_in2=fa2_in2)

uti_cor <- cor(df_uti)
uti_cor

png("편의시설 별 상관분석.png")
corrplot(uti_cor, method="shade", tl.col="black", addCoef.col = "black", tl.srt = 0, diag = F)
dev.off()

## 판매 시설 별 상관관계

df_pur <- data.frame(market_d=market_d, market_in3=market_in3, conv_d=conv_d, conv_in1=conv_in1, cafe_d=cafe_d, cafe_in2=cafe_in2)

pur_cor <- cor(df_pur)

png("판매시설 별 상관분석.png")
corrplot(pur_cor, method="pie", tl.col="black", addCoef.col = "black", tl.srt = 0, diag = F)
dev.off()

## 변수 분석

install.packages("tidyverse")
library(ggplot2)
library(dplyr)

univ_raw <- read.csv("Seoul_univ.csv", header=T)
head(univ_raw)

univ_x <- univ_raw$위도
univ_y <- univ_raw$경도

min_univ_x <- min(univ_x)
max_univ_x <- max(univ_x)
min_univ_y <- min(univ_y)
max_univ_y <- max(univ_y)

ggplot(data=univ_raw, aes(x=위도,y=경도)) + geom_bar(stat="identity")

## 편의점 

conv_raw <- read.csv("Seoul_conv.csv", header=T)
head(conv_raw)

conv_x <- conv_raw[,"좌표정보.X."]
conv_y <- conv_raw[,"좌표정보.Y."]

## 은행

bank_raw <- read.csv("Seoul_bank.csv", header=T)
head(bank_raw)

bank_x <- bank_raw[,"ypos_la"]
bank_y <- bank_raw[,"xpos_lo"]

## 근린시설1

fa1_raw <- read.csv("Seoul_fa1.csv", header=T)
head(fa1_raw)

fa1_x <- fa1_raw$Y
fa1_y <- fa1_raw$X

## 근린시설2

fa2_raw <- read.csv("Seoul_fa2.csv", header=T)
head(fa2_raw)

fa2_x <- fa2_raw$Y
fa2_y <- fa2_raw$X

## 대규모 점포

market_raw <- read.csv("Seoul_market.csv", header=T)
head(market_raw)

market_x <- market_raw$y
market_y <- market_raw$x

## 지하철 

metro_raw <- read.csv("Seoul_metro.csv", header=T)
head(metro_raw)

metro_x <- metro_raw$Y
metro_y <- metro_raw$X

## 버스

bus_raw <- read.csv("Seoul_bus.csv", header=T)
head(bus_raw)

bus_x <- bus_raw$Y좌표
bus_y <- bus_raw$X좌표

## 문화시설

cult1_raw <- read.csv("Seoul_mus.csv", header=T)
cult2_raw <- read.csv("Seoul_the.csv", header=T)

head(cult1_raw)
head(cult2_raw)

cult_x <- c(cult1_raw$y, cult2_raw$y)
cult_y <- c(cult1_raw$x, cult2_raw$x)

## 카페

cafe_raw <- read.csv("Seoul_cafe.csv", header=T)
head(cafe_raw)

cafe_x <- cafe_raw$위도
cafe_y <- cafe_raw$경도

## 편의점

conv_raw <- read.csv("Seoul_conv.csv")
head(conv_raw)

conv_x <- conv_raw[,"좌표정보.Y."]
conv_y <- conv_raw[,"좌표정보.X."]

## 시각화 시작

png("raw data 산점도.png", width=1500, height = 1000)
par(mfrow=c(2, 5))

par("mar")

par(mar=c(1,1,1,1))
plot(univ_y, univ_x, main="univ")
plot(bank_y, bank_x, main="bank")
plot(fa1_y, fa1_x, main="neighborhood living facility 1")
plot(fa2_y, fa2_x, main="neighborhood living facility 2")
plot(market_y, market_x, main="market")
plot(metro_y, metro_x, main="subway")
plot(bus_y, bus_x, main="bus stop")
plot(cult_y, cult_x, main="culture")
plot(cafe_y, cafe_x, main="cafe")
plot(conv_y, conv_x,main="convenience store")
dev.off()

## univ 결측치 제거

univ_x[9] <- 37.5887034223667
univ_y[9] <- 127.031698331241

## fa1 결측치 제거

idx <- which(fa1_y < 125)

fa1_x <- c(fa1_x[1:60845], fa1_x[60847:length(fa1_y)])
fa1_y <- c(fa1_y[1:60845], fa1_y[60847:length(fa1_y)])

png("결측치 수정 - raw data 산점도.png", width=1500, height = 1000)
par(mfrow=c(2, 5))

par("mar")

par(mar=c(1,1,1,1))
plot(univ_y, univ_x, main="univ")
plot(bank_y, bank_x, main="bank")
plot(fa1_y, fa1_x, main="neighborhood living facility 1")
plot(fa2_y, fa2_x, main="neighborhood living facility 2")
plot(market_y, market_x, main="market")
plot(metro_y, metro_x, main="subway")
plot(bus_y, bus_x, main="bus stop")
plot(cult_y, cult_x, main="culture")
plot(cafe_y, cafe_x, main="cafe")
plot(conv_y, conv_x,main="convenience store")
dev.off()

## 변수 단순화 시각화

install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(ggplot2)

round_func <- function(x) {
  round(x, digits = 1)
}

sum_func <- function(m, n) {
  m <- as.character(m)
  n <- as.character(n)
  result <- paste('(',m,',',n,')')
  return(result)
}

### 편의점

new_conv_y <- sapply(conv_y, round_func)
new_conv_x <- sapply(conv_x, round_func)


df_conv <- data.frame(x=new_conv_x, y=new_conv_y)
table_conv <- data.frame(table(df_conv))
table_conv <- table_conv[table_conv$Freq != 0,]
table_conv

table_conv <- mutate(table_conv, coordinate=sum_func(x, y))
table_conv <- arrange(table_conv, x, y)
table_conv

### 대학

new_univ_y <- sapply(univ_y, round_func)
new_univ_x <- sapply(univ_x, round_func)

df_univ <- data.frame(x=new_univ_x, y=new_univ_y)
table_univ <- data.frame(table(df_univ))
table_univ <- table_univ[table_univ$Freq !=0,]
table_univ <- mutate(table_univ, coordinate=sum_func(x,y))
table_univ <- arrange(table_univ,x, y)
table_univ

### 은행

#### 전처리 필요

new_bank_y <- sapply(bank_y, round_func)
new_bank_x <- sapply(bank_x, round_func)

df_bank <- data.frame(x=new_bank_x, y=new_bank_y)
df_bank <- filter(df_bank, x >= 37.4 & x <= 37.7)
df_bank <- filter(df_bank, y >= 126.8 & y < 127.2)

table_bank <- data.frame(table(df_bank))
table_bank <- table_bank[table_bank$Freq != 0,] %>% mutate(coordinate=sum_func(x,y)) %>% arrange(x, y)
table_bank

### 근린시설 1

new_fa1_y <- sapply(fa1_y, round_func)
new_fa1_x <- sapply(fa1_x, round_func)

df_fa1 <- data.frame(x=new_fa1_x, y=new_fa1_y)
table_fa1 <- data.frame(table(df_fa1))
table_fa1 <- table_fa1[table_fa1$Freq != 0,] %>% mutate(coordinate=sum_func(x,y)) %>% arrange(x, y)
table_fa1

## 근린시설 2

make_num <- function(s) {
  s = as.numeric(s)
  return(s)
}

round_func_str<- function(x) {
  mode(x) <- "numeric"
  return(x)
}

fa2_y <- as.numeric(fa2_y)
fa2_x <- as.numeric(fa2_x)

new_fa2_y <- sapply(fa2_y, round_func)
new_fa2_x <- sapply(fa2_x, round_func)

df_fa2 <- data.frame(x=new_fa2_x, y= new_fa2_y)
table_fa2 <- data.frame(table(df_fa2))
table_fa2 <- table_fa2[table_fa2$Freq != 0,] %>% mutate(coordinate=sum_func(x,y)) %>% arrange(x, y)
table_fa2

### market

market_y <- sapply(market_y, round_func)
market_x <- sapply(market_x, round_func)

df_market <- data.frame(x= market_x, y = market_y)
table_market <- data.frame(table(df_market))
table_market <- table_market[table_market$Freq != 0, ] %>% mutate(coordinate=sum_func(x,y)) %>% arrange(x, y)
table_market

### metro

metro_y <- sapply(metro_y, round_func)
metro_x <- sapply(metro_x, round_func)

df_metro <- data.frame(x= metro_x, y = metro_y)

df_metro <- filter(df_metro, x >= 37.4 & x <= 37.7)
df_metro <- filter(df_metro, y >= 126.8 & y < 127.2)

table_metro <- data.frame(table(df_metro))
table_metro <- table_metro[table_metro$Freq != 0, ] %>% mutate(coordinate=sum_func(x,y)) %>% arrange(x, y)
table_metro 

### bus

bus_y <- sapply(bus_y, round_func)
bus_x <- sapply(bus_x, round_func)

df_bus <- data.frame(x=bus_x, y=bus_y)

table_bus <- data.frame(table(df_bus))
table_bus <- table_bus[table_bus$Freq !=0,] %>% mutate(coordinate=sum_func(x,y)) %>% arrange(x, y)
table_bus

### 문화시설

cult_x <- sapply(cult_x, round_func)
cult_y <- sapply(cult_y, round_func)

df_cult <- data.frame(x=cult_x, y=cult_y)
df_cult <- filter(df_cult, x >= 37.4 & x <= 37.7)
df_cult <- filter(df_cult, y >= 126.8 & y < 127.2)


table_cult <- data.frame(table(df_cult))
table_cult <- table_cult[table_cult$Freq !=0,] %>% mutate(coordinate=sum_func(x,y)) %>% arrange(x, y)
table_cult

### 카페

cafe_x
cafe_y

cafe_y <- sapply(cafe_y, round_func)
cafe_x <- sapply(cafe_x, round_func)

df_cafe <- data.frame(x=cafe_x, y=cafe_y)
df_cafe

table_cafe <- data.frame(table(df_cafe))
table_cafe <- table_cafe[table_cafe$Freq !=0,] %>% mutate(coordinate=sum_func(x,y)) %>% arrange(x, y)
table_cafe

install.packages('patchwork')
library('patchwork')

png("변수별 분포 파악-test3.png", width=1200, height = 900)

p1 <- ggplot(table_univ, aes(x=coordinate, y=Freq, group=1)) + geom_line(color="blue", linetype="dashed") + geom_point(color="black") + ggtitle("Univ") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7)) 
p2 <- ggplot(table_bank, aes(x=coordinate, y=Freq, group=1)) + geom_line(color="red", linetype="dashed") + geom_point(color="black") + ggtitle("Bank") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7))
p3 <- ggplot(table_fa1, aes(x=coordinate, y=Freq, group=1)) + geom_line(color="brown", linetype="dashed") + geom_point(color="black") + ggtitle("Neighborhood living facility 1") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7)) 
p4 <- ggplot(table_fa2, aes(x=coordinate, y=Freq, group=1)) + geom_line(color="purple", linetype="dashed") + geom_point(color="black") + ggtitle("Neighborhood living facility 2") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7))
p5 <- ggplot(table_market, aes(x=coordinate, y=Freq, group=1)) + geom_line(color="black", linetype="dashed") + geom_point(color="black") + ggtitle("Market") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7))
p6 <- ggplot(table_metro, aes(x=coordinate, y=Freq, group=1)) + geom_line(color="goldenrod", linetype="dashed") + geom_point(color="black") + ggtitle("Metro") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7))
p7 <- ggplot(table_bus, aes(x=coordinate, y=Freq, group=1)) + geom_line(color="light coral", linetype="dashed") + geom_point(color="black") + ggtitle("Bus stop") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7))
p8 <- ggplot(table_cult, aes(x=coordinate, y=Freq, group=1)) + geom_line(color="light sky blue", linetype="dashed") + geom_point(color="black") + ggtitle("Culture facility") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7)) 
p9 <- ggplot(table_cafe, aes(x=coordinate, y=Freq, group=1)) + geom_line(color="dark cyan", linetype="dashed") + geom_point(color="black") + ggtitle("Cafe") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7)) 
p10 <- ggplot(table_conv, aes(x=coordinate, y=Freq, group=1)) + geom_line(color="orange", linetype="dashed") + geom_point(color="black") + ggtitle("Convinence Store") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7))

(p1 +p2+p3+p4+p5) / (p6+p7+p8+p9+p10)
dev.off()


### input 데이터 분석

main <- read.csv("판매시설.csv")
head(main)

coor_to_num_x <- function(s) {
  s <- strsplit(s, ", ")[[1]]
  s <- strsplit(s, '')
  s <- s[[1]][-1]
  s <- paste(s, collapse = '')
  return(as.numeric(s))
}

coor_to_num_y <- function(s) {
  s <- strsplit(s, ", ")[[1]]
  s <- strsplit(s, '')
  s_len <- length(s[[2]])
  s <- s[[2]][-s_len]
  s <- paste(s, collapse = '')
  return(as.numeric(s))
}

main_coor <- main$X2
main_coor

main_x <- sapply(main_coor, coor_to_num_x)
main_y <- sapply(main_coor, coor_to_num_y)

main_x <- unname(main_x)
main_y <- unname(main_y)

png("Input1.png", width=1500, height = 1000)
plot(main_y, main_x, main="Input Data", bg="gray")
dev.off()

main_y <- sapply(main_y, round_func)
main_x <- sapply(main_x, round_func)

df_main <- data.frame(x=main_x, y=main_y)
df_main

table_main <- data.frame(table(df_main))
table_main <- table_main[table_main$Freq !=0,] %>% mutate(coordinate=sum_func(x,y)) %>% arrange(x, y)
table_main

png("Input2.png")
ggplot(table_main, aes(x=coordinate, y=Freq, group=1)) + geom_bar(stat="identity", color="royal blue", fill="royal blue") + ggtitle("Input Data") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=30, hjust=1, size = 7))
dev.off()

## 빈도수

univ_d = df$univ_d
univ_in5 = df$univ_5

bank_d = df$closest_bank_d
bank_in5 = df$bank_in2

fa1_d = df$closest_fa_d
fa2_d = df$closest_fa2_d
fa1_in2 = df$fa_in2
fa2_in2 = df$fa2_in2

market_d = df$closest_market_d
market_in3 = df$market_in3

metro_d = df$closest_metro_d
metro_in2 = df$metro_in2

bus_d = df$closest_bus_d
bus_in2 = df$bus_in2

culture_d = df$closest_culture_d
culture_in2 = df$culture_in2

cafe_d = df$closest_cafe_d
cafe_in2 = df$cafe_in2

conv_d = df$closest_conv_d
conv_in1 = df$conv_in1


