rm(list=ls())

install.packages("tidyverse")
library(tidyverse)
library(gridExtra)
library(splines)
install.packages("gam")
library(gam)
library(car)
dat <- read.csv("가계금융(2017)데이터 전처리.csv", header = T)
glimpse(dat)
View(dat)

##EDA
#자산총액
summary(dat$자산총액)
a1 <- ggplot(data = dat) +
  geom_density(mapping = aes(x = 자산총액)) + 
  ggtitle("자산총액 분포") +
  xlab(" 자산총액(만원)") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )

a2 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_density(mapping = aes(x = 자산총액)) + 
  ggtitle("자산총액 분포(10억이하)") +
  xlab(" 자산총액(만원)") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )
grid.arrange(a1, a2, nrow = 2, ncol= 1)
### 자산총액 분포  작은 값으로 치우쳐 있어 정규분포변환필요 box-cox transformation



#나이
summary(dat$나이)

b1 <- ggplot(data = dat) +
  geom_density(mapping = aes(x = 나이))  + 
  ggtitle("나이 분포") +
  xlab("나이") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
    ) 

b2 <- ggplot(data = dat) +
  geom_point(mapping = aes(x = 나이, y = 자산총액)) +
  ggtitle("나이 vs 자산총액") +
  xlab("나이") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

b3 <- ggplot(data = dat) +
  geom_smooth(mapping = aes(x = 나이, y = 자산총액)) +
  ggtitle("나이 vs 자산총액") +
  xlab("나이") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

grid.arrange(b1, b2, b3, nrow = 2, ncol= 2)
#경상소득
summary(dat$경상소득)

c1 <- ggplot(data = dat) +
  geom_density(mapping = aes(x = 경상소득))  + 
  ggtitle("경상소득 분포") +
  xlab("경상소득(만원)") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

c2 <- ggplot(data = dat) +
  geom_point(mapping = aes(x = 경상소득, y = 자산총액))+ 
  ggtitle("경상소득 vs 자산총액") +
  xlab("경상소득(만원)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

c3 <- ggplot(data = dat) +
  geom_smooth(mapping = aes(x = 경상소득, y = 자산총액))+ 
  ggtitle("경상소득 vs 자산총액") +
  xlab("경상소득(만원)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

grid.arrange(c1, c2, c3, nrow = 2, ncol= 2)


#세금
summary(dat$세금)

d1 <- ggplot(data = dat) +
  geom_density(mapping = aes(x = 세금))   + 
  ggtitle("세금 분포") +
  xlab("세금(만원)") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  


d2 <- ggplot(data = dat) +
  geom_point(mapping = aes(x = 세금, y = 자산총액)) + 
  ggtitle("세금 vs 자산총액") +
  xlab("세금(만원)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

d3 <- ggplot(data = dat) +
  geom_smooth(mapping = aes(x = 세금, y = 자산총액)) +
  ggtitle("세금 vs 자산총액") +
  xlab("세금(만원)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

grid.arrange(d1, d2, d3, nrow = 2, ncol= 2)

#전용면적
summary(dat$전용면적)

e1 <- ggplot(data = dat) +
  geom_density(mapping = aes(x = 전용면적))   + 
  ggtitle("전용면적 분포") +
  xlab("전용면적(m2)") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

e2 <- ggplot(data = dat) +
  geom_point(mapping = aes(x = 전용면적, y = 자산총액))+ 
  ggtitle("전용면적 vs 자산총액") +
  xlab("전용면적(m2)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

e3 <- ggplot(data = dat) +
  geom_smooth(mapping = aes(x = 전용면적, y = 자산총액))+ 
  ggtitle("전용면적 vs 자산총액") +
  xlab("전용면적(m2)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

grid.arrange(e1, e2, e3, nrow = 2, ncol= 2)


#성별
summary(dat$성별)

f1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 성별)) + 
  ggtitle("성별 분포") +
  xlab("성별") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

f2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 성별, y = 자산총액))+ 
  ggtitle("성별 vs 자산총액") +
  xlab("성별") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

f3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 성별, y = 자산총액))+ 
  ggtitle("성별 vs 자산총액(10억이하)") +
  xlab("성별") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(f1, f2, f3, nrow = 2, ncol= 2)


#혼인상태
summary(dat$혼인상태)

g1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 혼인상태))+ 
  ggtitle("혼인상태 분포") +
  xlab("혼인상태") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

g2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 혼인상태, y = 자산총액))+ 
  ggtitle("혼인상태 vs 자산총액") +
  xlab("혼인상태") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

g3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 혼인상태, y = 자산총액))+
  ggtitle("혼인상태 vs 자산총액(10억이하)") +
  xlab("혼인상태") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(g1, g2, g3, nrow = 2, ncol= 2)

#직업군
summary(dat$직업군)

h1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 직업군))+ 
  ggtitle("직업군 분포") +
  xlab("직업군") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

h2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 직업군, y = 자산총액))+ 
  ggtitle("직업군 vs 자산총액") +
  xlab("직업군") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

h3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 직업군, y = 자산총액))+
  ggtitle("직업군 vs 자산총액(10억이하)") +
  xlab("직업군") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(h1, h2, h3, nrow = 2, ncol= 2)

#가구원수
table(dat$가구원수)

i1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 가구원수))+ 
  ggtitle("가구원수 분포") +
  xlab("가구원수(명)") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

i2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 가구원수, y = 자산총액))+ 
  ggtitle("가구원수 vs 자산총액") +
  xlab("가구원수(명)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

i3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 가구원수, y = 자산총액))+
  ggtitle("가구원수 vs 자산총액(10억이하)") +
  xlab("가구원수(명)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(i1, i2, i3, nrow = 2, ncol= 2)

#주거
summary(dat$주거)

j1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 주거))+ 
  ggtitle("주거 분포") +
  xlab("주거") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

j2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 주거, y = 자산총액))+ 
  ggtitle("주거 vs 자산총액") +
  xlab("주거") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

j3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 주거, y = 자산총액))+
  ggtitle("주거 vs 자산총액(10억이하)") +
  xlab("주거") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(j1, j2, j3, nrow = 2, ncol= 2)
#입주형태
summary(dat$입주형태)

k1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 입주형태))+ 
  ggtitle("입주형태 분포") +
  xlab("입주형태") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

k2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 입주형태, y = 자산총액))+ 
  ggtitle("입주형태 vs 자산총액") +
  xlab("입주형태") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

k3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 입주형태, y = 자산총액))+
  ggtitle("입주형태 vs 자산총액(10억이하)") +
  xlab("입주형태") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(k1, k2, k3, nrow = 2, ncol= 2)

#교육정도
summary(dat$교육정도)

l1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 교육정도))+ 
  ggtitle("교육정도 분포") +
  xlab("교육정도") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

l2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 교육정도, y = 자산총액))+ 
  ggtitle("교육정도 vs 자산총액") +
  xlab("교육정도)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

l3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 교육정도, y = 자산총액))+
  ggtitle("교육정도 vs 자산총액(10억이하)") +
  xlab("교육정도") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(l1, l2, l3, nrow = 2, ncol= 2)


#가구유형
summary(dat$가구유형)

m1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 가구유형))+ 
  ggtitle("가구유형 분포") +
  xlab("가구유형") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

m2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 가구유형, y = 자산총액))+ 
  ggtitle("가구유형 vs 자산총액") +
  xlab("가구유형)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

m3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 가구유형, y = 자산총액))+
  ggtitle("가구유형 vs 자산총액(10억이하)") +
  xlab("가구유형") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(m1, m2, m3, nrow = 2, ncol= 2)



#부채여부
summary(dat$부채여부)

n1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 부채여부))+ 
  ggtitle("부채여부 분포") +
  xlab("부채여부") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

n2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 부채여부, y = 자산총액))+ 
  ggtitle("부채여부 vs 자산총액") +
  xlab("부채여부)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

n3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 부채여부, y = 자산총액))+
  ggtitle("부채여부 vs 자산총액(10억이하)") +
  xlab("부채여부") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(n1, n2, n3, nrow = 2, ncol= 2)
#수도권여부
summary(dat$수도권여부)

o1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 수도권여부))+ 
  ggtitle("수도권여부 분포") +
  xlab("수도권여부") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

o2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 수도권여부, y = 자산총액))+ 
  ggtitle("수도권여부 vs 자산총액") +
  xlab("수도권여부)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

o3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 수도권여부, y = 자산총액))+
  ggtitle("수도권여부 vs 자산총액(10억이하)") +
  xlab("수도권여부") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(o1, o2, o3, nrow = 2, ncol= 2)

#한부모가구
summary(dat$한부모가구)

p1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 한부모가구))+ 
  ggtitle("한부모가구 분포") +
  xlab("한부모가구") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

p2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 한부모가구, y = 자산총액))+ 
  ggtitle("한부모가구 vs 자산총액") +
  xlab("한부모가구)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

p3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 한부모가구, y = 자산총액))+
  ggtitle("한부모가구 vs 자산총액(10억이하)") +
  xlab("한부모가구") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(p1, p2, p3, nrow = 2, ncol= 2)

#조손가구
summary(dat$조손가구)

r1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 조손가구))+ 
  ggtitle("조손가구 분포") +
  xlab("조손가구") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

r2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 조손가구, y = 자산총액))+ 
  ggtitle("조손가구 vs 자산총액") +
  xlab("조손가구)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

r3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 조손가구, y = 자산총액))+
  ggtitle("조손가구 vs 자산총액(10억이하)") +
  xlab("조손가구") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 
grid.arrange(r1, r2, r3, nrow = 2, ncol= 2)
#노인가구
summary(dat$노인가구)

s1 <- ggplot(data = dat) + 
  geom_bar(mapping = aes(x = 노인가구))+ 
  ggtitle("노인가구 분포") +
  xlab("노인가구") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )  

s2 <- ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = 노인가구, y = 자산총액))+ 
  ggtitle("노인가구 vs 자산총액") +
  xlab("노인가구)") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

s3 <- ggplot(data = filter(dat,자산총액<=100000)) +
  geom_boxplot(mapping = aes(x = 노인가구, y = 자산총액))+
  ggtitle("노인가구 vs 자산총액(10억이하)") +
  xlab("노인가구") + 
  ylab("자산총액(만원)") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 

grid.arrange(s1, s2, s3, nrow = 2, ncol= 2)



###first regression
fit <- lm(자산총액 ~ ., data = dat)
summary(fit)
fit <- glm(자산총액 ~ ., data = dat)
summary(fit)
#0.3909 455958

#box - cox transformation
func1 <- function(l){
  ((dat$자산총액)^l-1)/l
}

func2 <- function(l){
  ((dat$경상소득)^l-1)/l
}

func3 <- function(l){
  ((dat$세금)^l-1)/l
}

func4 <- function(l){
  ((dat$전용면적)^l-1)/l
}

#box-cox 최적화 된 lamda
library("MASS") 
bc<-boxcox(lm(자산총액 ~ .,data=dat),lambda=seq(-1,1,by=.1))
lambda <- bc$x[which.max(bc$y)]

dat2 <- mutate(dat, 변환한_자산총액 = func1(0.089), 변환한_경상소득 = func2(0.35), 
               변환한_세금 = func3(0.21), 변환한_전용면적 = func4(0.15))
dat3 <- select(dat2,-자산총액)
final_dat <- select(dat3,-(경상소득 : 전용면적))
#변환한_자산총액
ggplot(data = final_dat) +
  geom_density(mapping = aes(x = 변환한_자산총액)) + 
  ggtitle("변환한_자산총액 분포") +
  xlab(" 변환한_자산총액") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  )

#나이 vs변환한_자산총액
bb1 <- ggplot(data = final_dat) +
  geom_density(mapping = aes(x = 나이))  + 
  ggtitle("나이 분포") +
  xlab("나이") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  ) 

bb2 <- ggplot(data = final_dat) +
  geom_point(mapping = aes(x = 나이, y = 변환한_자산총액)) +
  geom_smooth(mapping = aes(x = 나이, y = 변환한_자산총액)) +
  ggtitle("나이 vs 변환한_자산총액") +
  xlab("나이") + 
  ylab("변환한_자산총액") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 


grid.arrange(bb1, bb2, nrow = 2, ncol= 1)
#변환한_경상소득 vs변환한_자산총액
cc1 <- ggplot(data = final_dat) +
  geom_density(mapping = aes(x = 변환한_경상소득))  + 
  ggtitle("변환한_경상소득 분포") +
  xlab("변환한_경상소득") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  ) 

cc2 <- ggplot(data = final_dat) +
  geom_point(mapping = aes(x = 변환한_경상소득, y = 변환한_자산총액)) +
  geom_smooth(mapping = aes(x = 변환한_경상소득, y = 변환한_자산총액)) +
  ggtitle("변환한_경상소득 vs 변환한_자산총액") +
  xlab("변환한_경상소득") + 
  ylab("변환한_자산총액") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 


grid.arrange(cc1, cc2, nrow = 2, ncol= 1)

#변환한_세금 vs변환한_자산총액
dd1 <- ggplot(data = final_dat) +
  geom_density(mapping = aes(x = 변환한_세금))  + 
  ggtitle("변환한_세금 분포") +
  xlab("변환한_세금") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  ) 

dd2 <- ggplot(data = final_dat) +
  geom_point(mapping = aes(x = 변환한_세금, y = 변환한_자산총액)) +
  geom_smooth(mapping = aes(x = 변환한_세금, y = 변환한_자산총액)) +
  ggtitle("변환한_세금 vs 변환한_자산총액") +
  xlab("변환한_세금") + 
  ylab("변환한_자산총액") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 


grid.arrange(dd1, dd2, nrow = 2, ncol= 1)

#변환한_전용면적 vs변환한_자산총액
ee1 <- ggplot(data = final_dat) +
  geom_density(mapping = aes(x = 변환한_전용면적))  + 
  ggtitle("변환한_전용면적 분포") +
  xlab("변환한_전용면적") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold")
  ) 

ee2 <- ggplot(data = final_dat) +
  geom_point(mapping = aes(x = 변환한_전용면적, y = 변환한_자산총액)) +
  geom_smooth(mapping = aes(x = 변환한_전용면적, y = 변환한_자산총액)) +
  ggtitle("변환한_전용면적 vs 변환한_자산총액") +
  xlab("변환한_전용면적") + 
  ylab("변환한_자산총액") +
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold")
  ) 


grid.arrange(ee1, ee2, nrow = 2, ncol= 1)


###regression
#X변환전
new <- lm(변환한_자산총액 ~ . - 변환한_경상소득 - 변환한_세금 - 변환한_전용면적, data = dat3)
summary(new)
new <- glm(변환한_자산총액 ~ . - 변환한_경상소득 - 변환한_세금 - 변환한_전용면적, data = dat3)
summary(new)
#0.6999 79756

#X변환후
new1 <- lm(변환한_자산총액 ~ ., data = final_dat)
summary(new1)
new1 <- glm(변환한_자산총액 ~ ., data = final_dat)
summary(new1)
#0.7517 76252


##변수선택
#가구유형 제거
new2 <- lm(변환한_자산총액 ~ . - 가구유형, data = final_dat)
summary(new2)
new2 <- glm(변환한_자산총액 ~ . - 가구유형, data = final_dat)
summary(new2)
#0.7517 76249

#한부모가구 제거
new3 <- lm(변환한_자산총액 ~ . - 가구유형 - 한부모가구, data = final_dat)
summary(new3)
new3 <- glm(변환한_자산총액 ~ . - 가구유형 - 한부모가구, data = final_dat)
summary(new3)
#0.7517 76247

#조손가구 제거
new4 <- lm(변환한_자산총액 ~ . - 가구유형 - 한부모가구 - 조손가구, data = final_dat)
summary(new4)
new4 <- glm(변환한_자산총액 ~ . - 가구유형 - 한부모가구 - 조손가구, data = final_dat)
summary(new4)
#0.7517 76246 
vif(new4)

###spline 적용
final_dat2 <- select(final_dat, -c(가구유형, 한부모가구, 조손가구))
View(final_dat2)
final <- glm(변환한_자산총액 ~ . - 나이 - 변환한_경상소득 - 변환한_세금 - 변환한_전용면적 + ns(나이, df = 3)  + ns(변환한_경상소득, df = 5)  + ns(변환한_세금, df = 5) + ns(변환한_전용면적, df = 5), data = final_dat2)
summary(final)
AIC(final)

#나이
agelims = range(final_dat2$나이) 
age.grid=seq(from=agelims [1],to=agelims [2]) 
fit=lm(변환한_자산총액 ~ ns(나이,df = 3),data=final_dat2)  
pred=predict (fit ,newdata =list(나이=age.grid),se=T) 
plot(final_dat2$나이 ,final_dat2$변환한_자산총액 ,col="gray") 
lines(age.grid ,pred$fit ,lwd=2) 
#변환한_경상소득
incomelims = range(final_dat2$변환한_경상소득) 
income.grid=seq(from=incomelims [1],to=incomelims [2]) 
fit=lm(변환한_자산총액 ~ ns(변환한_경상소득,df = 5),data=final_dat2)  
pred=predict (fit ,newdata =list(변환한_경상소득=income.grid),se=T) 
plot(final_dat2$변환한_경상소득 ,final_dat2$변환한_자산총액 ,col="gray") 
lines(income.grid ,pred$fit ,lwd=2)

#변환한_세금
taxlims = range(final_dat2$변환한_세금) 
tax.grid=seq(from=taxlims [1],to=taxlims [2]) 
fit=lm(변환한_자산총액 ~ ns(변환한_세금,df = 5),data=final_dat2)  
pred=predict (fit ,newdata =list(변환한_세금=tax.grid),se=T) 
plot(final_dat2$변환한_세금 ,final_dat2$변환한_자산총액 ,col="gray") 
lines(tax.grid ,pred$fit ,lwd=2)

#변환한_전용면적
widelims = range(final_dat2$변환한_전용면적) 
wide.grid=seq(from=widelims [1],to=widelims [2]) 
fit=lm(변환한_자산총액 ~ ns(변환한_전용면적,df = 5),data=final_dat2)  
pred=predict (fit ,newdata =list(변환한_전용면적=wide.grid),se=T) 
plot(final_dat2$변환한_전용면적 ,final_dat2$변환한_자산총액 ,col="gray") 
lines(wide.grid ,pred$fit ,lwd=2)


