library(mgcv)
akiData<-read.xlsx("/Users/xmly/Documents/chenyue/data/input.xlsx",sheetIndex = 3)
#print(akiData)

#gam分析
form.glin<-as.formula("AKI ~ s(SSFS) +s(IABP)")
#form.glin<-as.formula("AKI ~ s(SSFS) +s(IABP) + s(ZZG) + s(EF) + s(CPB) + s(GENDER) + s(HT) + s(DIA) + s(HF) + s(JX) + s(NXG) + s(SQZ) + s(ACEI) + s(STATINS) + s(AGE) + s(SQHB) + s(HXBSZ) + s(EGFR) + s(DAY)")
glinmodel<-gam(form.glin,data=akiData)
glinmodel$converted
summary(glinmodel)

#绘图
# library(ggplot2)
# terms <- predict(glinmodel, type = "terms")
# tframe <- cbind(AKI = akiData$AKI, as.data.frame(terms))
# colnames(tframe) <- gsub('[()]', '', colnames(tframe))
# pframe <- cbind(tframe, akiData[,c("SSFS" ,"IABP" , "ZZG" , "EF", "CPB" , "GENDER", "HT" , "DIA" , "HF" , "JX" , "NXG" , "SQZ" , "ACEI" , "STATINS" , "AGE" , "SQHB" , "HXBSZ" , "EGFR" , "DAY")])
# p1 <- ggplot(pframe, aes(x = SSFS)) +
#   geom_point(aes(y = scale(sSSFS, scale = F))) +
#   geom_line(aes(y = scale(AKI, scale = F)))
# p1