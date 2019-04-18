library(mgcv)
load("/Users/xmly/Documents/chenyue/data/NatalBirthData.rData")
#print(sdata)
train<- sdata[sdata$ORIGRANDGROUP<=5,]
#print(train)
test<-sdata[sdata$ORIGRANDGROUP>5,]
#print(test)

#线性分析
form.lin<-as.formula("DBWT ~ PWGT + WTGAIN + MAGER + UPREVIS")
linmodel<-lm(form.lin,data=train)
summary(linmodel)

#gam分析
form.glin<-as.formula("DBWT ~ s(PWGT) + s(WTGAIN) + s(MAGER) + s(UPREVIS)")
glinmodel<-gam(form.glin,data=train)
glinmodel$converted
summary(glinmodel)

#绘图
library(ggplot2)
terms <- predict(glinmodel, type = "terms")
tframe <- cbind(DBWT = train$DBWT, as.data.frame(terms))
colnames(tframe) <- gsub('[()]', '', colnames(tframe))
pframe <- cbind(tframe, train[,c("PWGT", "WTGAIN", "MAGER", "UPREVIS")])
p1 <- ggplot(pframe, aes(x = PWGT)) +
  geom_point(aes(y = scale(sPWGT, scale = F))) +
  geom_smooth(aes(y = scale(DBWT, scale = F))) 
p1