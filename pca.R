library(corrplot)
# read data
city <- read.csv("D:/PKU/EDA/data/city2.csv", header = T)

outlier <- rep(-1, 8)
out <- data.frame()
for(i in 1:8){
  outlier[i] <- boxplot(city[, i+1])$out
  out <- rbind(out, city[city[, i+1] == outlier[i],])
}
outlier
out.data <- unique(out)
out <- table(out$城市)[table(out$城市) > 0]
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
