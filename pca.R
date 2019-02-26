library(corrplot)
# read data
city <- read.csv("D:/PKU/EDA/data/city2.csv", header = T)
summary(city)
# EDA
# GDP
barplot(sort(city$GDP, decreasing = T), names.arg = city$城市[order(city$GDP, decreasing = T)], 
        las = 2, col = "#008080", ylim = c(0, 12000))
loc <- par("usr")
text(loc[1]-0.5, loc[4]+1200, "GDP", pos = 2, xpd = T)
barplot(sort(city$人均可支配收入, decreasing = T), names.arg = city$城市[order(city$人均可支配收入, decreasing = T)], 
        las = 2, col = "#008080", ylim = c(0, 30000))
loc <- par("usr")
text(loc[1]+0.5, loc[4]+3000, "人均\n可支配收入", pos = 2, xpd = T)
barplot(sort(city$商品房銷售均價, decreasing = T), names.arg = city$城市[order(city$商品房銷售均價, decreasing = T)], 
        las = 2, col = "#008080", ylim = c(0, 10000))
loc <- par("usr")
text(loc[1]+0.5, loc[4]+1000, "商品房\n銷售均價", pos = 2, xpd = T)
par(mfrow = c(2, 4))
for(i in 2:9){
  boxplot(city[, i], col = "#007399", main = names(city)[i], axes = F)
  axis(1, c(0.6, 1.4), col = "#19334d", col.axis = "#19334d", labels = F)
  axis(2, col = "#19334d", col.ticks = "#19334d", col.axis = "#19334d")
}

outlier <- rep(-1, 8)
out <- data.frame()

for(i in 1:8){
  outlier[i] <- boxplot(city[, i+1])$out
  out <- rbind(out, city[city[, i+1] == outlier[i],])
}
outlier
out.data <- unique(out)
out <- table(out$城市)[table(out$城市) > 0]

# 很多變數間為正相關
# 由R^2看出人均可之收入與人均使用面積此兩變數與其他變數較無相關性
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2), 
       cex = 1.8, col = "#008080") 
}
pairs(city[, -1], upper.panel = panel.pearson, pch = 19,  cex = 0.8, col = "#008080")
mycor <- cor(city[, -1])
corrplot(mycor, tl.col = "black")
# 將不相關的變數移除
pairs(city[, -c(1, 3, 5)], upper.panel = panel.pearson, pch = 19,  cex = 0.8, col = "#008080")
mycor <- cor(city[, -c(1, 3, 5)])
corrplot(mycor, tl.col = "black")

###############
city.pca <- princomp(city[, -1], cor = T)
summary(city.pca) # 選前兩個
plot(city.pca$sdev, type = "b", col = "#008080", axes = F, ylim = c(0, 2.5),
     ylab = "Standard deviation ", xlab = "Components", main = "碎石圖") 
axis(1, col = "#19334d", col.axis = "#19334d")
axis(2, seq(0, 2.5, 0.5), as.character(seq(0, 2.5, 0.5)),col = "#19334d", col.ticks = "#19334d", col.axis = "#19334d")

city.pca$loadings
city.pca$loadings[, 1:2]

######################
# row pca
######################
plot(city.pca$scores[,1:2], axes = F, col =  "#008080",
     xlim = c(-7, 7), ylim = c(-6, 6))
axis(1, col = "#19334d", col.ticks = "#19334d", col.axis = "#19334d")
axis(2, col = "#19334d", col.ticks = "#19334d", col.axis = "#19334d")

text(city.pca$scores[(city$城市 %in% names(out)[1]), 1], 
     city.pca$scores[(city$城市 %in% names(out)[1]), 2] - 0.5, 
     names(out)[1])
text(city.pca$scores[(city$城市 %in% names(out)[2]), 1], 
     city.pca$scores[(city$城市 %in% names(out)[2]), 2] - 0.5, 
     names(out)[2])
text(city.pca$scores[(city$城市 %in% names(out)[3]), 1], 
     city.pca$scores[(city$城市 %in% names(out)[3]), 2] - 0.5, 
     names(out)[3])
text(city.pca$scores[(city$城市 %in% names(out)[4]), 1], 
     city.pca$scores[(city$城市 %in% names(out)[4]), 2] - 0.5, 
     names(out)[4])

# drop outlier
# 上海、北京
city.drop <- city[!c(city$城市 %in% names(out)[1:2]),]
dim(city.drop)
city.pca1 <- princomp(city.drop[, -1], cor = T)
summary(city.pca1)
city.pca1$loadings
out.comp1 <- apply(city.pca1$loadings[, 1]*scale(city[c(city$城市 %in% names(out)[1:2]), -1]), 1, sum)
out.comp2 <- apply(city.pca1$loadings[, 2]*scale(city[c(city$城市 %in% names(out)[1:2]), -1]), 1, sum)

plot(city.pca1$scores[,1:2], axes = F, col =  "#008080", 
     xlim = c(-6000, 15000), ylim = c(-10000, 6000))
axis(1, col = "#19334d", col.ticks = "#19334d", col.axis = "#19334d")
axis(2, col = "#19334d", col.ticks = "#19334d", col.axis = "#19334d")
text(out.comp1, out.comp2, paste("*", names(out)[1:2]))
# drop outlier
# 上海、北京、東莞、深圳 
city.drop4 <- city[!c(city$城市 %in% names(out)),]
dim(city.drop4)
city.pca2 <- princomp(city.drop4[, -1], cor = T)
summary(city.pca2)
city.pca2$loadings

plot(city.pca2$scores[,1:2], axes = F, col =  "#008080", 
     xlim = c(-6, 6), ylim = c(-4, 4))
axis(1, col = "#19334d", col.ticks = "#19334d", col.axis = "#19334d")
axis(2, col = "#19334d", col.ticks = "#19334d", col.axis = "#19334d")
out.comp1 <- apply(city.pca2$loadings[, 1]*scale(city[c(city$城市 %in% names(out)), -1]), 1, sum)
out.comp2 <- apply(city.pca2$loadings[, 2]*scale(city[c(city$城市 %in% names(out)), -1]), 1, sum)
text(out.comp1, out.comp2, paste("*", names(out)))

###
new <- city$人均使用面積*city$戶籍人口數量
city.new <- cbind(city, new)
colnames(city.new) <- c(names(city), "面積")
city.drop4.new <- city.new[!c(city.new$城市 %in% names(out)),]
city.pca3 <- princomp(city.drop4.new[, -c(1, 5:6)], cor = T)
summary(city.pca3)
city.pca3$loadings

plot(city.pca3$scores[,1:2], axes = F, col =  "#008080", 
     xlim = c(-6, 6), ylim = c(-4, 4))
axis(1, col = "#19334d", col.ticks = "#19334d", col.axis = "#19334d")
axis(2, col = "#19334d", col.ticks = "#19334d", col.axis = "#19334d")
out.comp1 <- apply(city.pca3$loadings[, 1]*scale(city.new[c(city.new$城市 %in% names(out)), -c(1, 5:6)]), 1,
                   sum)
out.comp2 <- apply(city.pca3$loadings[, 2]*scale(city.new[c(city.new$城市 %in% names(out)), -c(1, 5:6)]), 1,
                   sum)
text(out.comp1, out.comp2, paste("*", names(out)))


invest <- sort(c(city.pca3$scores[, 1], out.comp1), decreasing = T)
city[as.numeric(names(invest)), ]
