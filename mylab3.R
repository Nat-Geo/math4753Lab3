getwd()
library(s20x)

spruce.df = read.csv("SPRUCE.csv")

head(spruce.df)

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

layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))
layout.show(4)
with(spruce.df,
     plot(Height~BHDiameter, bg="Blue",pch=21,cex=1.2,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter))))
abline(spruce.lm)

yhat=fitted(spruce.lm)
