# Set working directory, load data files, load libraries
setwd("/home/zonination/Dropbox/R/Representatives")
uspop <- read.csv("~/Dropbox/R/Representatives/uspop.csv")
nrep <- read.csv("~/Dropbox/R/Representatives/nreps.csv")
library(ggplot2)

# Linearly interpolate US Population
df<-data.frame("year"=NA,nrep=NA,uspop=NA)
for(n in 1790:2010){
  df<-rbind(df,c(
    n,
    subset(nrep,year<=n)$nrep[nrow(subset(nrep,year<=n))],
    approx(x=uspop$year,y=uspop$uspop,xout=n)$y
  ))
}
df<-df[2:nrow(df),]

df$ratio<-df$nrep*100000/df$uspop

ggplot(df,aes(year,ratio))+
  geom_line(color="firebrick3",size=2)+
  geom_smooth(method=lm,color="black",size=.5,linetype=4,se=F)+
  scale_y_log10(breaks=c(.1,1,10),limits=c(.1,3),minor_breaks=c(seq(.1,1,.1),seq(1,10,1)))+
  scale_x_continuous(breaks=seq(1800,2000,50),minor_breaks=seq(1790,2010,10))+
  ylab("Number of Representatives per 100,000 People")+
  xlab("Year")+
  ggtitle("Representation in the U.S.")+
  theme_bw()