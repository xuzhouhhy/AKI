require(xlsx)
akiData<-read.xlsx("/Users/xmly/Documents/chenyue/data/input.xlsx",sheetIndex = 3)

#Logestic分析
formIABP.lin<-as.formula("AKI ~ IABP")
linmodel<-glm(formIABP.lin,data=akiData,family=binomial())
summary(linmodel)

formHF.lin<-as.formula("AKI ~ HF")
linmodel<-glm(formHF.lin,data=akiData,family=binomial())
summary(linmodel)

require(rms)
f <- lrm(AKI ~ SSFS)
print(f)
