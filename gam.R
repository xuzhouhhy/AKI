library(mgcv)
akiData<-read.xlsx("/Users/xmly/Documents/chenyue/data/input.xlsx",sheetIndex = 3)
#print(akiData)

#gam分析
#form.glin<-as.formula("AKI ~ s(SSFS) +s(IABP) + s(ZZG) + s(EF) + s(CPB) + s(GENDER) + s(HT) + s(DIA) + s(HF) + s(JX) + s(NXG) + s(SQZ) + s(ACEI) + s(STATINS) + s(AGE) + s(Hb) + s(HXBSZ) + s(eGFR) + s(DAY)")
form.glin<-as.formula("DAY ~ s(HBA)+s(AGE)")
glinmodel<-gam(form.glin,data=akiData)
glinmodel$converted
summary(glinmodel)

#绘图
library(ggplot2)
terms <- predict(glinmodel, type = "terms")
tframe <- cbind(DAY = akiData$DAY, as.data.frame(terms))
colnames(tframe) <- gsub('[()]', '', colnames(tframe))
pframe <- cbind(tframe, akiData[,c("HBA","AGE")])
p1 <- ggplot(pframe, aes(x = HBA)) +
  geom_point(aes(y = scale(sHBA, scale = F))) +
  geom_line(aes(y = scale(DAY, scale = F))) 
p1