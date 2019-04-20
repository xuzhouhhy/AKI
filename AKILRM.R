require(rms)
akiData<-read.xlsx("/Users/xmly/Documents/chenyue/data/input.xlsx",sheetIndex = 3)
print(akiData$SSFS)
lrmmodel<-li(AKI ~ SSFS  + IABP + ZZG + EF + CPB + GENDER + HT + DIA + HF + JX + NXG + SQZ + ACEI + STATINS + AGE + SQHB + HXBSZ + EGFR + DAY)
summary(lrmmodel)

#设定nomogram的参数
AKI<-akiData$AKI
SSFS<-akiData$SSFS
IABP<-akiData$IABP
ZZG<-akiData$ZZG
EF<-akiData$EF
CPB<-akiData$CPB
GENDER<-akiData$GENDER
HT<-akiData$HT
DIA<-akiData$DIA
HF<-akiData$HF
JX<-akiData$JX
NXG<-akiData$NXG
SQZ<-akiData$SQZ
ACEI<-akiData$ACEI
STATINS<-akiData$STATINS
AGE<-akiData$AGE
SQHB<-akiData$SQHB
HXBSZ<-akiData$HXBSZ
EGFR<-akiData$EGFR
DAY<-akiData$DAY
HBA<-akiData$HBA
  
print(SSFS)
# print(IABP)
# print(ZZG)

ddist<-datadist(SSFS,IABP,ZZG,EF, CPB , GENDER , HT , DIA , HF , JX , NXG , SQZ , ACEI , STATINS , AGE , SQHB , HXBSZ , EGFR , DAY ,HBA)
# ddist<-datadist(SSFS)
print(ddist)
options(datadist='ddist')

#logistic回归,no include HbA1c
#f <- lrm(AKI ~ SSFS + IABP + ZZG + EF + CPB  + GENDER + HT + HF + JX + NXG + SQZ + ACEI + STATINS + AGE + SQHB + HXBSZ + EGFR + DAY )
f <- lrm(AKI ~ IABP + ZZG + EF + CPB  + GENDER + HT + HF + JX + NXG + SQZ + ACEI + STATINS + AGE + SQHB + HXBSZ + EGFR + DAY )
print(f)
nom <- nomogram(f, fun=plogis,
                fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),
                lp=F, funlabel="Risk")
plot(nom)

#logistic回归,include HbA1c
hbaf <- lrm(AKI ~ SSFS + IABP + ZZG + EF + CPB  + GENDER + HT + HF + JX + NXG + SQZ + ACEI + STATINS + AGE + SQHB + HXBSZ + EGFR + DAY + HBA)
print(hbaf)
hbanom <- nomogram(hbaf, fun=plogis,
                fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),
                lp=F, funlabel="Risk")
plot(hbanom)