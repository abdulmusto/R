###################### PART A ################

# N1 : Loading and observing the trees data

library(datasets)
View(trees)

# N2 : 5-number summary numbers

summary(trees)

# N3 : Graphing straight line Regression for all pairs (3 pairs)

par(mfrow = c(1,3),pch = 16)

plot(trees$Girth,trees$Volume,pch = 16,cex = 1.3, col = "blue",main = "Volume against Girth",xlab = "Girth",ylab = "Volume")
abline(lm(trees$Volume ~ trees$Girth))

plot(trees$Girth,trees$Height,pch = 16,cex = 1.3, col = "blue",main = "Height against Girth",xlab = "Girth",ylab = "Height")
abline(lm(trees$Height ~ trees$Girth))

plot(trees$Height,trees$Volume,pch = 16,cex = 1.3, col = "blue",main = "Volume against Height",xlab = "Height",ylab = "Volume")
abline(lm(trees$Volume ~ trees$Height))

par(mfrow = c(1,2),pch = 16)
trees.mrl <- lm(Volume ~ Height+Girth,data = trees)
termplot(trees.mrl,partial = TRUE)

  # N4 : Histograms and Density Plots

par(mfrow = c(1,2),pch = 16)


hist(trees$Girth,main = "Histogram for Girth",xlab ="Girth size" )
dens <- density(trees$Girth)
xlim <- range(dens$x)
ylim <- range(dens$y)
hist(trees$Girth,probability = TRUE,xlim = xlim,ylim = ylim,main = "Density graph for Girth",xlab = "Girth size")
lines(dens)

hist(trees$Height,main = "Histogram for Height",xlab = "Height")
dens <- density(trees$Height)
xlim <- range(dens$x)
ylim <- range(dens$y)
hist(trees$Height,probability = TRUE,xlim = xlim,ylim = ylim,main = "Density graph for Height",xlab = "Height")
lines(dens)

hist(trees$Volume,main = "Histogram for Volume",xlab = "Volume")
dens <- density(trees$Volume)
xlim <- range(dens$x)
ylim <- range(dens$y)
hist(trees$Volume,probability = TRUE,xlim = xlim,ylim = ylim,main = "Density graph for Volume",xlab = "Volume")
lines(dens)

par(mfrow = c(1,1),pch=1)

# N5 : Boxplot

par(mfrow = c(3,1),pch = 16)

boxplot(trees$Girth,horizontal = TRUE,main = "Girth Boxplot",xlab = "Girth Size",outline = TRUE,col = "red")
rug(trees$Girth,side = 1)

boxplot(trees$Volume,horizontal = TRUE,main = "Volume Boxplot",xlab = "Volume",outline = TRUE,col = "green")
rug(trees$Volume,side = 1)

boxplot(trees$Height,horizontal = TRUE,main = "Height Boxplot",xlab = "Height",outline = TRUE,col = "blue")
rug(trees$Height,side = 1)

par(mfrow = c(1,1),pch = 1)

# N6 : Normal Probability plots

par(mfrow = c(1,3),pch = 16)

qqnorm(trees$Volume,col = "red",main = "Normal Probability Plot for Volume")
qqnorm(trees$Girth,col = "red",main ="Normal Probability Plot for Girth")
qqnorm(trees$Height,col = "red",main = "Normal Probability Plot for Height")

par(mfrow = c(1,1),pch = 1)


#################### PART B #####################


require("DAAG")
require("MASS")
require("ggplot2")
require("ggcorrplot")

View(oddbooks)
View(Rubber)

pairs(Rubber)
pairs(oddbooks)

####### Rubber

ggcorrplot(cor(Rubber),method = "square", show.diag = TRUE,colors = c("#6D9EC1", "white", "#E46726"))

par(mfrow = c(1,2),pch = 16)

Rubber.lm <- lm(loss ~ hard + tens, data = Rubber)
summary(Rubber.lm)

termplot(Rubber.lm,partial=TRUE,smooth = panel.smooth)
par(mfrow = c(1,1),pch = 16)


####### oddbooks 

ggcorrplot(cor(oddbooks),method = "square", colors = c("#6D9EC1", "white", "#E46726"))
summary(oddbooks)

logbooks <- log(oddbooks)

par(mfrow = c(1,1),pch = 16)
logbooks.lm1 <- lm(weight ~ thick,data = logbooks)
summary(logbooks.lm1)
termplot(logbooks.lm1,partial = TRUE,smooth = panel.smooth)
par(mfrow = c(1,1),pch = 16)

par(mfrow = c(1,2),pch = 16)
logbooks.lm2 <- lm(weight ~ thick+height ,data = logbooks)
summary(logbooks.lm2)
termplot(logbooks.lm2,partial = TRUE,smooth = panel.smooth)
par(mfrow = c(1,1),pch = 16)

par(mfrow = c(1,3),pch = 16)
logbooks.lm3 <- lm(weight ~ thick+height+breadth,data = logbooks)
summary(logbooks.lm3)
termplot(logbooks.lm3,partial = TRUE,smooth = panel.smooth)
par(mfrow = c(1,1),pch = 16)




