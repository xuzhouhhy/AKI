akiData<-read.xlsx("/Users/xmly/Documents/chenyue/data/input.xlsx",sheetIndex = 3)
#print(akiData)

#线性分析
form.lin<-as.formula("AKI ~ SSFS  + IABP + ZZG + EF + CPB + GENDER + HT + DIA + HF + JX + NXG + SQZ + ACEI + STATINS + AGE + SQHB + HXBSZ + EGFR + DAY")
linmodel<-lm(form.lin,data=akiData)
summary(linmodel)
plot(linmodel)
library(car)
outlierTest(linmodel)
influence.measures(linmodel)