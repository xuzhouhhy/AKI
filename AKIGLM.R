require(xlsx)
akiData<-read.xlsx("/Users/xmly/Documents/chenyue/data/input.xlsx",sheetIndex = 3)
#print(akiData)

#Logestic分析
#没有HbA1c
form.lin<-as.formula("AKI ~ SSFS  + IABP + ZZG + EF + CPB + GENDER + HT + HF + JX + NXG + SQZ + ACEI + STATINS + AGE + SQHB + HXBSZ + EGFR + DAY")
linmodel<-glm(form.lin,data=akiData,family=binomial())
summary(linmodel)
#anova()

#有HbA1c
hbaicform.lin<-as.formula("AKI ~ SSFS  + IABP + ZZG + EF + CPB + GENDER + HT + DIA + HF + JX + NXG + SQZ + ACEI + STATINS + AGE + SQHB + HXBSZ + EGFR + HBA + DAY")
hbaiclinmodel<-glm(hbaicform.lin,data=akiData,family=binomial())
summary(hbaiclinmodel)