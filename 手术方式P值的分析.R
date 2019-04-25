require(xlsx)
akiData<-read.xlsx("/Users/xmly/Documents/chenyue/data/input.xlsx",sheetIndex = 3)

AKI<-akiData$AKI
SSFSN<-akiData$SSFSN
SSFS<-akiData$SSFS
IABP<-akiData$IABP
HT<-akiData$HT
SQHB<-akiData$SQHB
AGE<-akiData$AGE
HXBSZ<-akiData$HXBSZ
EGFR<-akiData$EGFR
DAY<-akiData$DAY
HBA<-akiData$HBA

#设定nomogram的参数
ddist<-datadist(SSFSN,SSFS,IABP,HT,SQHB,AGE,HXBSZ,EGFR,DAY,HBA)
print(ddist)
options(datadist='ddist')

require(rms)
noHBA <- lrm(AKI ~  SSFS + IABP + HT + SQHB + AGE + HXBSZ + EGFR + DAY)
print(noHBA)
summary(noHBA)
anova(noHBA,test = "Chisq")
cbind(coed<-coef(noHBA),confint(noHBA))
exp(cbind(OR=coef(noHBA),confint(noHBA)))

noHBAGLM <- glm(AKI ~  SSFS + IABP + HT + SQHB + AGE + HXBSZ + EGFR + DAY,data=akiData,family = binomial(link='logit'))
print(noHBAGLM)
summary(noHBAGLM)
anova(noHBAGLM, test="Chisq")
cbind(coed<-coef(noHBAGLM),confint(noHBAGLM))
exp(cbind(OR=coef(noHBAGLM),confint(noHBAGLM)))

HBAGLM <-  lrm(AKI ~ SSFS + IABP + HT + SQHB + AGE + HXBSZ + EGFR + DAY + HBA)
print(HBAGLM)
summary(HBAGLM)
anova(HBAGLM,test="Chisq")

cbind(coed<-coef(HBAGLM),confint(HBAGLM))
exp(cbind(OR=coef(HBAGLM),confint(HBAGLM)))
akiData$preHBAGLM<-predict(newdata=akiData,HBAGLM,"response")

modelA <- glm(AKI ~ IABP + HT + HXBSZ + EGFR + DAY ,data=akiData,family=binomial())
print(modelA)
summary(modelA)

cbind(coed<-coef(modelA),confint(modelA))
exp(cbind(OR=coef(modelA),confint(modelA)))
akiData$preModelA<-predict(newdata=akiData,modelA,"response")


nomoModelA <- lrm(AKI ~ IABP + HT + HXBSZ + EGFR + DAY)
print(nomoModelA)
summary(nomoModelA)
nom <- nomogram(nomoModelA, fun=plogis,
                fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),
                lp=F, funlabel="Risk")
plot(nom)
