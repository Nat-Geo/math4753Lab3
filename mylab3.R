#Task1
getwd()
#Task2
library(s20x)
library(ggplot2)

spruce.df = read.csv("SPRUCE.csv")

head(spruce.df)
#Task3
with(spruce.df,
  plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2, ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter))))
with(spruce.df,{layout(matrix(1:3,nr=3))
  trendscatter(x = BHDiameter,y = Height,f = 0.5)
  trendscatter(x = BHDiameter,y = Height,f=0.6)
  trendscatter(x=BHDiameter,y=Height,f=0.7)
  }
)

spruce.lm<-with(spruce.df, lm(Height~BHDiameter))
abline(spruce.lm)
#Task4
layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))
layout.show(4)
with(spruce.df,
     plot(Height~BHDiameter, bg="Blue",pch=21,cex=1.2,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter))))
abline(spruce.lm)

yhat=fitted(spruce.lm)

with(spruce.df, plot(Height~BHDiameter, bg = "Blue", pch=21, cex=1.2, ylim = c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter))))
with(spruce.df, {
  segments(BHDiameter, Height, BHDiameter, yhat)
})
abline(spruce.lm)

RSS = with(spruce.df, sum((Height-yhat)^2))

with(spruce.df,
     plot(Height~BHDiameter, bg = "Blue", pch=21, cex=1.2, ylim = c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter))))

with(spruce.df,abline(h=mean(Height)))     
with(spruce.df,
     segments(BHDiameter,mean(Height),BHDiameter,yhat,col="Red"))
abline(spruce.lm)
MSS = with(spruce.df, sum((yhat-mean(Height))^2))

with(spruce.df,
     plot(Height~BHDiameter, bg = "Blue", pch=21, cex=1.2, ylim = c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter))))
with(spruce.df, abline(h=mean(Height)))
with(spruce.df,segments(BHDiameter, Height, BHDiameter, mean(Height),col="Green"))
TSS = with(spruce.df,sum((Height-mean(Height))^2))

TSS
MSS
RSS
MSS/TSS
MSS+RSS

#Task5

summary(spruce.lm)

predict(spruce.lm, data.frame(BHDiameter=c(15,18,20)))

#Task6

ggplot(data=spruce.df)+geom_point(mapping=aes(x=BHDiameter, y=Height))+geom_line(mapping=aes(x=BHDiameter,y=Height))+geom_abline(mapping=aes(intercept = spruce.lm$coefficients["(Intercept)"],slope=spruce.lm$coefficients["BHDiameter"],color = "Blue"
                                                                                                                                             ))
