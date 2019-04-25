require(xlsx)
#读取excel数据
data<-read.xlsx("/Users/xmly/Documents/chenyue/data/input.xlsx",sheetIndex = 4)

#提取需要的数据
AKI<-data$AKI
operation.type<-data$operation.type
IABP<-data$IABP
LMD<-data$LMD
LVEF<-data$LVEF
CPB.used<-data$CPB.used
gender<-data$gender
hypertension<-data$hypertension
hyperlipidemia<-data$hyperlipidemia
ACS<-data$ACS
cerebrovascular.diseases<-data$cerebrovascular.diseases
contrast.medium.used<-data$contrast.medium.used
ACEI.ARB<-data$ACEI.ARB
statin<-data$statin
age<-data$age
anemia<-data$anemia
RBCT<-data$RBCT
eGFR<-data$eGFR
A1c<-data$A1c
PO.BG<-data$PO.BG

#为logistics分析lrm函数分析作准备
ddist<-datadist(operation.type,IABP,LMD,LVEF,CPB.used,gender,hypertension,hyperlipidemia,ACS,cerebrovascular.diseases,contrast.medium.used,ACEI.ARB,
         statin,age,anemia,RBCT,eGFR,A1c,PO.BG)
options(datadist = 'ddist')

require(rms)

#单因素分析，手术方式
otLrmModel<-lrm(AKI ~ operation.type)
otLrmModel
summary(otLrmModel)
anova(otLrmModel,test = "Chisq")

#单因素分析，IABP
iabpLrmModel<-lrm(AKI ~ IABP)
iabpLrmModel
summary(iabpLrmModel)
anova(iabpLrmModel,test="Chisq")

#单因素分析，LMD
lmdLrmModel<-lrm(AKI ~ LMD)
lmdLrmModel
summary(lmdLrmModel)
anova(lmdLrmModel,test="Chisq")

#单因素分析，LVEF
lvefLrmModel<-lrm(AKI ~ LVEF)
lvefLrmModel
summary(lvefLrmModel)
anova(lvefLrmModel,test="Chisq")

#单因素分析，CPB.used
cpbUSedLrmModel<-lrm(AKI ~ CPB.used)
cpbUSedLrmModel
summary(cpbUSedLrmModel)
anova(cpbUSedLrmModel,test="Chisq")

#下面这个单因素分析和你给的结果一样
cpbUsedGlmModel<-glm(AKI ~ CPB.used,data=data,family=binomial())
cpbUsedGlmModel

#单因素分析，gender
genderLrmModel<-lrm(AKI ~ gender)
genderLrmModel
summary(genderLrmModel)
anova(genderLrmModel,test="Chisq")

genderGlmModel<-glm(AKI~gender,data = data,family = binomial())
genderGlmModel
summary(genderGlmModel)

#单因素分析，hypertension
hypertensionLrmModel<-lrm(AKI ~ hypertension)
hypertensionLrmModel
summary(hypertensionLrmModel)
anova(hypertensionLrmModel)

#单因素分析，hyperlipidemia
hyperlipidemiaLrmModel<-lrm(AKI ~ hyperlipidemia)
hyperlipidemiaLrmModel
summary(hyperlipidemiaLrmModel)
anova(hyperlipidemiaLrmModel,test="Chisq")

#单因素分析，ACS
acsLrmModel<-lrm(AKI ~ ACS)
acsLrmModel
summary(acsLrmModel)
anova(acsLrmModel,test="Chisq")

#单因素分析，cerebrovascular diseases
cerebrovascularDiseasesLrmModel<-lrm(AKI ~ cerebrovascular.diseases)
cerebrovascularDiseasesLrmModel
summary(cerebrovascularDiseasesLrmModel)
anova(cerebrovascularDiseasesLrmModel,test="Chisq")

#单因素分析，contrast.medium.used
contrast.medium.usedLrmModel<-lrm(AKI ~ contrast.medium.used)
contrast.medium.usedLrmModel
summary(contrast.medium.used)
anova(contrast.medium.usedLrmModel,test="Chisq")

#单因素分析，ACEI.ARB
ACEI.ARBLrmModel<-lrm(AKI ~ ACEI.ARB)
ACEI.ARBLrmModel
summary(ACEI.ARBLrmModel)
anova(ACEI.ARBLrmModel)

#单因素分析，statin
statinLrmModel<-lrm(AKI ~ statin)
statinLrmModel
summary(statinLrmModel)
anova(statinLrmModel)

#单因素分析，age
ageLrmModel<-lrm(AKI ~ age)
ageLrmModel
summary(ageLrmModel)
anova(ageLrmModel)

#单因素分析，anemia
anemiaLrmModel<-lrm(AKI ~ anemia)
anemiaLrmModel
summary(anemiaLrmModel)
anova(anemiaLrmModel)

#单因素分析，RBCT
rbctLrmModel<-lrm(AKI ~ RBCT)
rbctLrmModel
summary(rbctLrmModel)
anova(rbctLrmModel)

#单因素分析，eGFR
egfrLrmModel<-lrm(AKI ~ eGFR)
egfrLrmModel
summary(egfrLrmModel)
anova(egfrLrmModel)

#单因素分析，A1c
a1cLrmModel<-lrm(AKI ~ A1c)
a1cLrmModel
summary(a1cLrmModel)
anova(a1cLrmModel)

#单因素分析，PO.BG
PO.BGLrmModel<-lrm(AKI ~ PO.BG)
PO.BGLrmModel
summary(PO.BGLrmModel)
anova(PO.BGLrmModel)

#选出单因素分析有效的数据建模分析
noHBALrmModel <- lrm(AKI ~ age + hypertension + anemia + IABP + eGFR + operation.type +  RBCT + PO.BG)
print(noHBALrmModel)
summary(noHBALrmModel)
anova(noHBALrmModel,test = "Chisq")

#选出单因素分析有效的数据建模分析,包含A1c
HBALrmModel <- lrm(AKI ~ age + hypertension + anemia + IABP + eGFR + operation.type +  RBCT + PO.BG + A1c)
print(HBALrmModel)
summary(HBALrmModel)
anova(HBALrmModel,test = "Chisq")

#从HBALrmModel中筛选出有效的自变量分析
effectLrmModel<-lrm(AKI ~ hypertension + IABP + eGFR + RBCT + PO.BG)
print(effectLrmModel)
summary(effectLrmModel)
anova(effectLrmModel,test="Chisq")
nom <- nomogram(effectLrmModel, fun=plogis,
                fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),
                lp=F, funlabel="Risk")
plot(nom)
