set.seed(602957)

x <- rnorm(1000)
noise <- rnorm(1000, sd = 1.5)
y <- 3*sin(2*x) + cos(0.75*x) - 1.5*(x^2) + noise

#x<-"hi"
#print(x)

select<- runif(1000)
#print(select)

frame<-data.frame(y=y,x=x)
#print(frame)

train<-frame[select>0.1,]
test<-frame[select<=0.1,]

#print(train)
#print (test)

#线性回归拟合
lin.model<-lm(y~x,data=train)
summary(lin.model)

#GAM拟合
glin.model<-gam(y~s(x),data = train)
glin.model$converged
summary(glin.model)

actual <- test$y
print(actual)

pred.lin <- predict(lin.model,newdata = test)
pred.glin <- predict(glin.model,newdata = test)
resid.lin <- actual - pred.lin
resid.glin <- actual-pred.glin
sqrt(mean(resid.lin^2))
sqrt(mean(resid.glin^2))
cor(actual,pred.lin)^2
cor(actual,pred.glin)^2

sx <- predict(glin.model,type="terms")
summary(sx)

xframe<- cbind(train,sx = sx[,1])

library(ggplot2)
ggplot(xframe,aes(x=x))+
  geom_point(aes(y=y),alpha=0.4)
  +geom_line(aes(y=sx))