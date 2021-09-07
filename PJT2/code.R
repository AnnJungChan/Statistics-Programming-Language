# 자유도가 14인 t-분포에서 구한 검정통계량 0.73에 대한 유의확률 P(T>0.753)
1-pt(0.73,df=14)

#여아 신생아 18명의 몸무게에 대한 t-검정
###데이터 불러오기
data<-read.table("C:/Users/sec/Desktop/baby_weight.txt",header=T)
str(data)
names(data)<-c("gender","weight")
tmp<-subset(data,gender==1)
###데이터 확인하기
weight<-tmp[[2]]
weight
###평균과 표준편차, 표본의 개수 구하기
xbar<-mean(weight)
xbar
s<-sd(weight)
s
n<-length(weight)
n
###유의수준이 0.05일 때, 자유도 17에서 t_a값(분위수) 구하기
alpha<-0.05 # 유의수준 0.05로 놓기
(c.u<-qt(1-alpha, df=n-1))# 자유도가 n-1인 t-분포에서 P(T>cu)=0.05가 되는 분위수 구하기
###검정통계량에 대한 유의확률 구하기 (P(T>2.233))
1-pt(2.23318, df=n-1)

#야구공 100개에 대한 모비율에 대한 가설 검정
###데이터 불러오기
tmp<-read.table("C:/Users/sec/Desktop/restitution.txt",header=T)
head(tmp)
###데이터 불러오기
tmp<-read.table("C:/Users/sec/Desktop/restitution.txt",header=T)
head(tmp)
###ifelse 함수를 통하여 각 샘플이 불량인지 아닌지 파악하기
rel<-ifelse(tmp$rst<0.4134|tmp$rst>0.4374,1,0)
rel
###phat 정의
n <- length(rel)
n
sumrel <- sum(rel)
sumrel
phat<- sumrel / n
phat
###p_0정의
p0 <- 0.1
###검정통계량 구하기
(z <- (phat - p0) / sqrt( ( p0*(1-p0) )/n ) )
###유의수준이 0.05일 때, Z_a값(분위수) 구하기
alpha<-0.05
(c.u<-qnorm(1-alpha))
###p 값 구하기
(p.value<-1-pnorm(z))
###prop.test() 함수
prop.test( sumrel, n, p=0.1, alternative="greater", correct=FALSE)
# F-분포(양쪽검정, 자유도 17, 25)의 임계값
qf(0.975, df1=17,df2=25)
# 등분산 검정
data <- read.table("C:/Users/sec/Desktop/baby_weight.txt", header=T)
var.test(data$weight ~ data$gender)
#서로 독립인 두 모집단: 모평균의 차이 검정
t.test(data$weight~data$gender,mu=0,alternative="less",var.equal=T)
###t-분포(단측검정, 자유도 42)의 임계값
qt(0.05, df=42)
#서로 대응인 두 모집단: 모평균의 차이 검정
###데이터 불러오기
data <- read.csv("C:/Users/sec/Desktop/anorexia.csv",header=T)
str(data)
data
###검정통계량 구하기
(n <- length(data$Prior - data$Post))
(m <- mean( data$Prior - data$Post ))
(s <- sd (data$Prior - data$Post))
(round( t.t <- m/(s / sqrt(n)),3) )
###t.test() 함수를 통해 더 쉽게 검정통계량 구하기
t.test(data$Prior, data$Post, paired=T, alternative="less")
###유의수준이 0.05일 때 자유도 16의 t_a 분위수 구하기
alpha <- 0.05
qt(alpha, df=16)
###유의확률 구하기
pt(t.t, df=16)
pt(-4.1849, df=16)
#모집단이 3개 이상일  경우 평균 비교 검정
###오차 제곱 합 구하기
##데이터 읽어오기
ad <- read.csv("C:/Users/sec/Desktop/age.data.csv",header=T)
str(ad)
View(ad)
## 각 집단별로 나누어 저장하기
y1 <- ad$age[ad$scale=="1"]
y2 <- ad$age[ad$scale=="2"]
y3 <- ad$age[ad$scale=="3"]
##각 집단별 평균 구하기
y1.mean <- mean(y1)
y2.mean <- mean(y2)
y3.mean <- mean(y3)
y1.mean
y2.mean
y3.mean
##각 집단별 편차 제곱합 구하기
sse.1 <- sum( (y1 - y1.mean)^2 )
sse.2 <- sum( (y2 - y2.mean)^2 )
sse.3 <- sum( (y3 - y3.mean)^2 )
sse.1
sse.2
sse.3
##각 집단별 편차 제곱합을 모두 더해 오차 제곱 합 구하고 sse에 저장한 후 출력하기
(sse <- sse.1 + sse.2 + sse.3)
## 자유도를 식에 맞게 코드를 짜고 dfe에 저장하고 출력
(dfe <- (length(y1)) + (length(y2)) + (length(y3))-3)

###처리제곱합 구하기
##전체 평균을 구해 변수 y에 저장
y <- mean(ad$age)
## 각 처리별로 처리의 평균과 전체 평균과의 편차제곱합을 구하고 각 처리의 표본의 개수와 곱한다.
sst.1 <- length(y1) * sum((y1.mean - y)^2)
sst.2 <- length(y2) * sum((y2.mean - y)^2)
sst.3 <- length(y3) * sum((y3.mean - y)^2)
sst.1
sst.2
sst.3
## 각 처리별로 구한 값을 모두 더해 처리제곱합을 구해 변수 sst에 저장하고 출력
(sst <- sst.1 + sst.2 + sst.3)
##(처리집단의 수-1)로 처리간 제곱합의 자유도를 구하고 출력

###총 제곱합을 통하여 오차제곱합과 처리제곱합이 잘 나왔는지 검산하기
##총편차제곱합을 구해 tsq에 저장하고 출력
( tsq <- sum( (ad$age - y)^2 ) )
## 처리의 제곱합(sst)와 오차제곱합(sse)의 합을 ss에 저장하고 출력(tsq와 ss가 동일)
( ss <- sst + sse )

###검정통계량 구하기
##처리평균제곱합을 구해 변수 mst에 저장
mst <- sst / dft
mst
#오차평균제곱합을 구해 변수 mse에 저장
mse <- sse / dfe
mse
##처리평균제곱합(mst)를 오차평균제곱합(mse)로 나눈값을 f.t에 저장하고 출력
(f.t <- mst / mse)

###검정
##유의수준이 0.05라고 놓았으므로, alpha를 0.05라고 놓고 시작
alpha<-0.05
##유의수준이 0.05일 때, qf()함수를 통하여 자유도가 2,147인 F-분포에서 유의수준 0.05일 때 분위수를 구함
(tol<-qf(1-alpha,2,147))

###유의확률 구하기
##검정통계량이 저장된 f.t값보다 클 확률을 구해 p.value에 저장하고 출력
(p.value <- 1 - pf(f.t, 2, 147))

#평균의 동일성 검증을 boxplot을 통해 시각화 시키기
ad$scale = factor(ad$scale, labels = c("1", "2", "3"))
require(ggplot2)
ggplot(ad, aes(x = scale, y = age)) + 
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("Dried weight of plants")

#data.mod를 통해 데이터가 어떻게 분포되어있는지 시각화 하기
data.mod=data.frame(Fitted = fitted(data.mod1),
                    Residuals=resid(data.mod1),Treatment=ad$scale)
ggplot(data.mod, aes(Fitted, Residuals, color = Treatment))+geom_point()

#Im() 함수를 이용하여 회귀분석과 F-검정 단순화 시키기
data.mod1 = lm(age ~ scale, data = ad)
summary(data.mod1)

#anova() 함수로 분산분석표 제시하기
anova(data.mod1)

#confint() 함수로 95% 신뢰구간 구하기
confint(data.mod1)

#aov 함수로 분산분석표 제시하기
r.aov<-aov(age~scale, data=ad)
summary(r.aov)

#Bartlett.test() 함수로 바틀렛 검정을 통하여 검정하기
bartlett.test(age~scale,data=ad)

#일원배치 분산분석
##데이터 만들기
###매일 측정하였을 때, 온도 상태에 따라서 나오는 결과의 차이가 존재하는가?
###라는 가설을 세우고, 귀무가설은 차이가 없다. 대립가설은 차이가 있다로 검정은 시작된다.
###이 때, 온도 상태의 그룹은 3개가 있는데, group 1,2,3이 바로 그 것이다.
# group 1 : temperature condition 1 
# group 2 : temperature condition 2
# group 3 : temperature condition 3
###이 때, 우리는 그 온도 상태에 대한 각각의 결과값들을 r에 넣으려고 한다.
###group 1,2,3에 해당하는 각각의 결과값들을 y1,2,3에 저장한다.
y1 <- c(50.5, 52.1, 51.9, 52.4, 50.6, 51.4, 51.2, 52.2, 51.5, 50.8)
y2 <- c(47.5, 47.7, 46.6, 47.1, 47.2, 47.8, 45.2, 47.4, 45.0, 47.9)
y3 <- c(46.0, 47.1, 45.6, 47.1, 47.2, 46.4, 45.9, 47.1, 44.9, 46.2)
y1
y2
y3
###그 후, 모든 결과값들을 y에 저장한다.
y <- c(y1, y2, y3)
y
###그 다음, 이 값들을 데이터 프레임으로 옮기기 위한 사전 절차를 실시할 것이다.
###데이터 프레임에 그냥 자료를 넣게되면, 어지러워질 수도 있기 때문에, 각각의 그룹에 해당하는
###자료들 앞에 1,2,3,을 붙여주어야 한다. 그 사전작업을 실시한다.
n <- rep(10, 3)
n

group <- rep(1:3, n)
group
###현재, rep()함수를 사용하여, 3개의 10을 만들어준 후, 각각의 1,2,3을 10번씩 순차대로 반복하라는
###명령을 취해주었다.
# data.frame으로  표 만들어 주기. 이 때, 1열에는 y의 값을 순차적으로 넣어주고, 2열에는
# group의 값을 넣어주라고 명령한다.
group_df <- data.frame(y, group)
group_df
###그 다음, 이 1,2,3들이 숫자가 아니라 범주형 자료이므로, 이를 변환하기 위한 작업을 한다.
sapply(group_df, class)
group_df <- transform(group_df, group = factor(group))
sapply(group_df, class)

# boxplot을 그려서 우선 각각의 평균이 비슷한지 안비슷한지 눈대중으로 봐보자.
attach(group_df)

boxplot(y ~ group, 
        main = "Boxplot of Daily Outcome by Temperature condition 1/2/3", 
        xlab = "Factor Levels : Temperature condition 1/2/3", 
        ylab = "Daily Outcome")

# 그룹 1,2,3에 대한 최소, 최대, 사분위수를 비교해보자.
tapply(y, group, summary)
detach(group_df)

#일원배치분산분석 시행
aov(y ~ group, data = group_df)
##sst와 sse 구해주고, 각각의 자유도 구해주기
summary(aov(y ~ group, data = group_df)) 
##sst,sse, F-검정통계량, 유의확률 구해주기
##유의 확률이 유의수준 0.05보다 더 매우 작게 나와서 귀무가설을 기각하고 대립가설을 
##채택하게 되어 온도 조건 1/2/3에 따라서 결과의 차이가 있다라고 말할 수 있다.

#bartlett.test() 함수로 오차의 등분산성 검정하기
bartlett.test(y ~ group, data = group_df)
#이 때는 유의확률의 유의수준인 0.05보다 더 크게 나왔으므로, 오차의 등분산성 가정을 만족한다고
#말할 수 있을 것이다.

###R에서 제공하는 데이터인 PlantGrowth를 이용하여 boxplot을 그려보고
###회귀분석 데이터와 F검정값과 유의확률 이끌어내기
data <- PlantGrowth
View(data)
data$group = factor(data$group, labels = c("Control", "Treatment 1", "Treatment 2"))
require(ggplot2)
ggplot(data, aes(x = group, y = weight)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("Dried weight of plants")
data.mod1 = lm(weight ~ group, data = data)
summary(data.mod1)