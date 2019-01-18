df <- read.csv("C:/Users/LENOVO/Desktop/project data/data.csv",header=TRUE)
View(df)
colnames
colnames(df)#the name of our clumns
summary(df)# the summary of each feachure
library(dataMaid)
makeDataReport(df)
library(FactoMineR)
#res.pca = PCA(df[df, scale.unit=TRUE, ncp=5, graph=T)
#res.pca = PCA(df, scale.unit=TRUE, ncp=4, graph=T)



attach(df)
df$X.stream = as.factor(df$X.stream)
attach(df)
summary(df)

install.packages("reshape2")
names(df)
library(reshape2)
dd <- melt(df) #les mettre dans une seule variable
attach(dd)
View(dd)

model <- lm(value ~ variable+ isp + browser + connected, data= dd)
summary(model)
summary(model)$coefficient
confint(model)
attributes(model)

library(FactoMineR)
library(factoextra)
df1 <- na.omit(dd)
names(df1)
res.mca = MCA(df1, quanti.sup=6)
plot.MCA(res.mca, invisible=c("ind")) #corr entre les modalité 
plotellipses(res.mca,keepvar=4)

table(df$qualif, df$p2p)
str(df)#structure du data frame
aov1<-aov(df$isp~df$p2p) #différence significative 
summary(aov1)
install.packages("psych")
library(psych)
pairs.panels(df[1:4],gap=0,bg=c("red","green","blue")[df$p2p],pch=21)
permutest(df.MHV)

