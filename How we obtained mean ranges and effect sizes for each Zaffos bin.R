### short code looking at range size distributions and corrs with continental fragmentation

# get data
data<-read.csv("/Users/matiaszenklusen/Desktop/Research Project Implementation Documents/total.ranges.ccd.csv")
colnames(data)

# get look-up chart
lookup<-read.csv("/Users/matiaszenklusen/Desktop/Research Project Implementation Documents/Look-up chart.csv")

# and midpoint data
midpoints<-read.csv("/Users/matiaszenklusen/Desktop/Research Project Implementation Documents/midpoints.csv")
mid.data<-midpoints$Midpoint

# fragmentation data
frags<-read.csv("/Users/matiaszenklusen/Desktop/Research Project Implementation Documents/fragmentation_data.csv")

# make a results file
data_FA<-array(NA, dim=c(length(unique(lookup$Zaffos.bin)), 5))
colnames(data_FA)<-c("ZaffosBin","midpoint","mean_range","mean_ES","frag")
data_FA[,1]<-c(unique(lookup$Zaffos.bin))
data_FA[,2]<-c(midpoints$Midpoint*-1)
data_FA[,5]<-c(frags$Frag_index)

# write a silly loop
Ztimebins<-unique(lookup$Zaffos.bin)

for (i in 1:length(Ztimebins)){
  ttb<-Ztimebins[i]
  ttbd<-lookup[ which(lookup$Zaffos.bin==ttb), ]
  ttbd_numbers<-ttbd$Number.DarrochCode.
  
  # now extract the range data we want
  range_td<-data[ which(data$Time %in% ttbd_numbers) , ]
  mean_range<-mean(range_td$Area)
  mean_ES<-mean(range_td$ES)
  data_FA[i,3]<-mean_range/10^6 # to convert into square km
  data_FA[i,4]<-mean_ES
}

# turn into a dataframe so we can plot
data_FA_df<-as.data.frame(data_FA)

# some basic plots
dev.new(height=6, width=8)
plot(data_FA_df$midpoint, data_FA_df$mean_range, pch=19, type="b", xlab="Time(mya)", ylab="RangeSize(km2)")

# how about range size vs. fragmentation index
dev.new(height=7, width=7)
plot(data_FA_df$mean_range, data_FA_df$frag, pch=19, xlab="RangeSize(km2)", ylab="Frag. index")

# experiment with log axes
dev.new(height=6, width=8)
plot(data_FA_df$midpoint, log(as.numeric(data_FA_df$mean_range)), pch=19, type="b", xlab="Time(mya)", ylab="RangeSize(km2)")

dev.new(height=7, width=7)
plot(log(as.numeric(data_FA_df$mean_range)), data_FA_df$frag, pch=19, xlab="RangeSize(km2)", ylab="Frag. index")
