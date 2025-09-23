
#上のSessionタブからでもwdを設定できる
setwd("C:/Users/chiak/OneDrive - BI Norwegian Business School (BIEDU)/Bologna_2025_Fall/96342_BigData_in_Social_Sciences/Lecture02/Group_Activity-20250923")

#Fileを読み込む
x = read.csv("UNdata.txt")

#必要なパッケージのみを読み込む
suppressMessages(library(tidyverse))
library(ggplot2) 
library(dplyr) 

options(scipen = 999)

set.seed(123)

attach(x)

print("HEAD:")
print(head(x))
print(summary(world.pop))


x$ratio <- x$world.pop / x$world.pop[1]
x$perc  <- round((x$ratio - 1) * 100)


x <- x %>% mutate(RATIO = world.pop/world.pop[1], percent = (RATIO-1)*100)


x$ratio <- x$RATIO
x$Perc <- round(x$percent, 1)


a <- x[,2]
b <- a / a[1]
c <- (b-1)*100
x$p <- c


threshold <- 120.5
x$flag <- ifelse(x$p>threshold, TRUE, FALSE)


write.csv(x, "temp.csv", row.names = FALSE) 


plot(x$year, x$p, type="l", col="blue", lwd=3, main="Pop % inc (???)")
abline(h=120, col="red", lty=2)

g <- ggplot(x, aes(year, p)) + geom_line() + geom_point() + 
  geom_hline(yintercept=120.0, linetype="dashed", color="red") +
  labs(title="World population growth (% change since 1950)", y="Percent change (%)", x="Year") +
  theme_minimal()
print(g)


x  =  x |> mutate(   R = world.pop / first(world.pop),   P = round((R-1)*100,1)  )

## ここで一度デタッチ（この後で列名を変えるため）
detach("x")

names(x)[names(x)=="world.pop"] <- "WorldPop"
x$WORLD_pop <- x$WorldPop


x$year <- as.character(x$year)
x$year_num <- as.numeric(x$year)

xx <- subset(x, year_num >= 1960)
yy = xx
zzz <- yy
rm(yy) # but keep zzz, sure

xx$rat2 <- xx$WORLD_pop / xx$WORLD_pop[1]
xx$pc2  <- round((xx$rat2-1)*100, 2)


xx <- xx[, c("year", "year_num", "WORLD_pop", "pc2", "p", "R", "P", "flag", "RATIO", "Perc")]

#Windowsで動くパスに変える
write.csv(xx, "output_final.csv", row.names = FALSE)

print(xx[ , c(1,3,4,5)])

## すでに detach 済みだが、二重で呼んでも安全にするなら存在チェックを挟む
if("x" %in% search()) detach("x")

# THE END (I guess)

