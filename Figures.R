#Separate phyla from total data 
View(Total.data)
Arthropoda<-Total.data[1:1027,1:8]
Brachiopoda<-Total.data[1028:4771,1:8]
Bryozoa<-Total.data[4772:5133,1:8]
Cnidaria<-Total.data[5134:6571,1:8]
Echinodermata<-Total.data[6572:6899,1:8]
Mollusca<-Total.data[7212:15910,1:8]
Porifera<-Total.data[6900:7211,1:8]

#Incorporate documents with average range and effect sizes for each phylum 
Arthropoda.data<-read.csv("Arthropoda data.csv")
Brachiopoda.data<-read.csv("Brachiopoda data.csv")
Bryozoa.data<-read.csv("Bryozoa data.csv")
Cnidaria.data<-read.csv("Cnidaria data.csv")
Echinodermata.data<-read.csv("Echinodermata data.csv")
Mollusca.data<-read.csv("Mollusca data.csv")
Porifera.data<-read.csv("Porifera data.csv")

#Midpoint vs. log(range) graphs per phylum 
quartz()
dev.new(height=10, width=8)
par(mfrow=c(4,2))
par(mar=c(1,4,2,1))
plot(Arthropoda$midpoint,log(Arthropoda$range))
points(Arthropoda.data$Midpoint,log(Arthropoda.data$Mean.Range..Arthropoda.),pch=19,cex=1.5,col="red",xlab="Midpoint",ylab="log(range)")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
plot(Brachiopoda$midpoint,log(Brachiopoda$range))
points(Brachiopoda.data$Midpoint,log(Brachiopoda.data$Mean.range..Brachiopoda.),pch=19,cex=1.5,col="red",xlab="Midpoint",ylab="log(range)")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
plot(Bryozoa$midpoint,log(Bryozoa$range))
points(Bryozoa.data$Midpoint,log(Bryozoa.data$Mean.range..Bryozoa.),pch=19,cex=1.5,col="red",xlab="Midpoint",ylab="log(range)")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
plot(Cnidaria$midpoint,log(Cnidaria$range))
points(Cnidaria.data$Midpoint,log(Cnidaria.data$Mean.range..Cnidaria.),pch=19,cex=1.5,col="red",xlab="Midpoint",ylab="log(range)")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
plot(Echinodermata$midpoint,log(Echinodermata$range))
points(Echinodermata.data$Midpoint,log(Echinodermata.data$Mean.range..Echinodermata.),pch=19,cex=1.5,col="red",xlab="Midpoint",ylab="log(range)")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
plot(Mollusca$midpoint,log(Mollusca$range))
points(Mollusca.data$Midpoint,log(Mollusca.data$Mean.range..Mollusca.),pch=19,cex=1.5,col="red",xlab="Midpoint",ylab="log(range)")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
plot(Porifera$midpoint,log(Porifera$range))
points(Porifera.data$Midpoint,log(Porifera.data$Mean.range..Porifera.),pch=19,cex=1.5,col="red",xlab="Midpoint",ylab="log(range)")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)

#Midpoint vs. effect size graphs per phylum 
quartz()
dev.new(height=10, width=8)
par(mfrow=c(4,2))
par(mar=c(1,4,2,1))
plot(Arthropoda$midpoint,Arthropoda$ES,ylim=c(-12,12))
points(Arthropoda.data$Midpoint,Arthropoda.data$Mean.Effect.Size..Arthropoda.,pch=19,cex=1,col="red")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
abline(h=-2, col="blue", lty=3)
abline(h=2, col="blue", lty=3)
plot(Brachiopoda$midpoint,Brachiopoda$ES,ylim=c(-12,12))
points(Brachiopoda.data$Midpoint,Brachiopoda.data$Mean.effect.size..Brachiopoda,pch=19,cex=1,col="red")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
abline(h=-2, col="blue", lty=3)
abline(h=2, col="blue", lty=3)
plot(Bryozoa$midpoint,Bryozoa$ES,ylim=c(-12,12))
points(Bryozoa.data$Midpoint,Bryozoa.data$Mean.effect.size..Bryozoa.,pch=19,cex=1,col="red")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
abline(h=-2, col="blue", lty=3)
abline(h=2, col="blue", lty=3)
plot(Cnidaria$midpoint,Cnidaria$ES,ylim=c(-12,12))
points(Cnidaria.data$Midpoint,Cnidaria.data$Mean.effect.size..Cnidaria.,pch=19,cex=1,col="red")
abline(v=-370.8, col="blue", lty=3)
abline(h=-2, col="blue", lty=3)
abline(h=2, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
plot(Echinodermata$midpoint,Echinodermata$ES,ylim=c(-12,12))
points(Echinodermata.data$Midpoint,Echinodermata.data$Mean.effect.size..Echinodermata.,pch=19,cex=1,col="red")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
abline(h=-2, col="blue", lty=3)
abline(h=2, col="blue", lty=3)
plot(Mollusca$midpoint,Mollusca$ES,ylim=c(-12,12))
points(Mollusca.data$Midpoint,Mollusca.data$Mean.effect.size..Mollusca.,pch=19,cex=1,col="red")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
abline(h=-2, col="blue", lty=3)
abline(h=2, col="blue", lty=3)
plot(Porifera$midpoint,Porifera$ES,ylim=c(-12,12))
points(Porifera.data$Midpoint,Porifera.data$Mean.effect.size..Porifera.,pch=19,cex=1,col="red")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
abline(h=-2, col="blue", lty=3)
abline(h=2, col="blue", lty=3)

#General data figures 
getwd()
Total.data.MZ<-Total.data
Mean.general.data<-data_FA_df
quartz()
dev.new(height=10, width=16)
par(mfrow=c(3,2))
par(mar=c(1,4,2,1))
plot(Total.data.MZ$midpoint,log(Total.data.MZ$range))
x<-as.numeric(Mean.general.data$midpoint)
y<-log(as.numeric(Mean.general.data$mean_range))
points(x,y,pch=19,cex=1,col="red")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
plot(Total.data.MZ$midpoint,Total.data.MZ$ES)
a<-as.numeric(Mean.general.data$midpoint)
b<-as.numeric(Mean.general.data$mean_ES)
points(a,b,pch=19,cex=1,col="red")
abline(v=-370.8, col="blue", lty=3)
abline(v=-262.235, col="blue", lty=3)
abline(h=-2, col="blue", lty=3)
abline(h=2, col="blue", lty=3)
plot(Total.data.MZ$frag,log(Total.data.MZ$range))
c<-as.numeric(Mean.general.data$frag)
d<-log(as.numeric(Mean.general.data$mean_range))
points(c,d,pch=19,cex=1,col="red")
plot(Total.data.MZ$frag,Total.data.MZ$ES)
e<-as.numeric(Mean.general.data$frag)
f<-as.numeric(Mean.general.data$mean_ES)
points(e,f,pch=19,cex=1,col="red")
abline(h=-2, col="blue", lty=3)
abline(h=2, col="blue", lty=3)
g<-as.numeric(Mean.general.data$midpoint)
h<-as.numeric(Mean.general.data$mean_range)
plot(g,log(h),pch=19,cex=1,col="red")

#Correlation tests 
cor.test(as.numeric(Mean.general.data$midpoint),as.numeric(Mean.general.data$mean_range),method=c("spearman"),exact=FALSE)
cor.test(as.numeric(Mean.general.data$midpoint),as.numeric(Mean.general.data$mean_range),method=c("kendall"),exact=FALSE)
cor.test(as.numeric(Mean.general.data$midpoint),as.numeric(Mean.general.data$mean_ES),method=c("spearman"),exact=FALSE)
cor.test(as.numeric(Mean.general.data$midpoint),as.numeric(Mean.general.data$mean_ES),method=c("kendall"),exact=FALSE)
cor.test(as.numeric(Mean.general.data$frag),as.numeric(Mean.general.data$mean_range),method=c("spearman"),exact=FALSE)
cor.test(as.numeric(Mean.general.data$frag),as.numeric(Mean.general.data$mean_range),method=c("kendall"),exact=FALSE)
cor.test(as.numeric(Mean.general.data$frag),as.numeric(Mean.general.data$mean_ES),method=c("spearman"),exact=FALSE)
cor.test(as.numeric(Mean.general.data$frag),as.numeric(Mean.general.data$mean_ES),method=c("kendall"),exact=FALSE)




