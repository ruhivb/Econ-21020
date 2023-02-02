library(dplyr)
library(readxl)
setwd("/Users/ruhi/Desktop/ECON 21020/Econ-21020")

# load data
df <- read_excel("caschool.xlsx")

# number of observations (total number of districts)
count(df)

# new income variable
df$income <- df$avginc * 1000

# mean & standard deviation of avgin and income
mean(df$avginc)
sd(df$avginc)

mean(df$income)
sd(df$income)

# mean math score
mean(df$math_scr)

# math scores, avg class size <= 20
l20 <- filter(df, df$str <= 20)
count(l20)/count(df)
x1 <- mean(l20$math_scr)
se1 <- sd(l20$math_scr)/ sqrt(length(l20$math_scr))

# math scores, avg class size >20
g20 <- filter(df,df$str > 20)
count(g20)/count(df)
x2 <- mean(g20$math_scr)
se2 <- sd(g20$math_scr)/ sqrt(length(g20$math_scr))

# test difference in means
a <- .1
t <- (x1-x2)/(sqrt((se1*se1) + (se2*se2)))
1-pnorm(t)

# covariance
cov(df$avginc, df$math_scr)
cov(df$income, df$math_scr)

# correlation
cor(df$avginc, df$math_scr)
cor(df$income, df$math_scr)







