
#Import libraries. 
library("IgorR")
library(ggplot2)
library(dplyr)
library(hms)
library(signal)

#Read in PXP file 
traces = read.pxp("~/burst/WedJul262017c0_001.pxp", ReturnTimeSeries= TRUE, 
                 regex = "RecordA")
sampleInterval = traces$vars$SampleInterval
numberOfWaves = traces$vars$NumWaves
sample = (traces$vars$SamplesPerWave)*sampleInterval
rate = numberOfWaves * sample
notes = traces$Notes


traceTimeCourse <- as.hms(seq(0,numberOfWaves, by = 1/sample))
traceTimeCourse <- traceTimeCourse[-1]  

trace.ts<- scale(trace[[2]])
trace.ts<-decimate(trace.ts, 50)
# tracelist<- 
for(i in 2:numwaves){
    
    trace.tsi <- ts(scale(trace[[i]]), start = 0,  
                    frequency = trace$vars$SamplesPerWave, 
                    deltat = trace$vars$SampleInterval)
    trace.tsi<-decimate(trace.tsi, 50)
    trace.ts<- c(trace.ts, trace.tsi)
    
}
trace.ts<-ts(trace.ts)
x.ts<-(x[1:length(trace.ts)])

#Plot
Series <- ggplot( ) +  geom_line(aes(x.ts, trace.ts))

#dev.off()
#big_plot<-ggplot( ) +  geom_line(aes(x.ts, trace.ts)) + scale_x_time(0,3) + ylim(-5, 5) + geom_segment(mapping=aes(x=400, y=-20, xend=582, yend=-5), arrow=arrow(), size=1, color="gray")  + geom_segment(mapping=aes(x=1000, y=-20, xend=645, yend=-10), arrow=arrow(), size=1, color="gray") + geom_segment(mapping=aes(x=1700, y=-20, xend=1700, yend=-5), arrow=arrow(), size=1, color="gray")  + geom_segment(mapping=aes(x=61, y=-20, xend=61, yend=-5), arrow=arrow(), size=1, color="gray") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#  ggsave("isoguavacine.png", plot=big_plot, width = 4, height = 4, units = "in", device = "png")
# small_plot<-ggplot( ) + geom_line(aes(x.ts[570000:632000], trace.ts[570000:632000]))+ scale_x_time() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#   ggsave("isoguavacine_zoom.png", plot=small_plot, width = 4, height = 4, units = "in", device = "png")
print(big_plot)
#x.trace<- as.hms(seq(0,1, by= 0.00002))
#x.trace<- x.trace[-1]
#tiny_plot1<-ggplot( ) + geom_line(aes(x=x.trace,y=trace[[61]]))+ ylim(-10, 35) + scale_x_time() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#ggsave("isoguavacine_zoom2_baseline.png", plot=tiny_plot1, width = 4, height = 4, units = "in", device = "png")
#tiny_plot2<-ggplot( ) + geom_line(aes(x=x.trace,y=trace[[582]]))+ ylim(-10, 35) + scale_x_time() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#ggsave("isoguavacine_zoom2_peak.png", plot=tiny_plot2, width = 4, heighfint = 4, units = "in", device = "png")
#tiny_plot3<-ggplot( ) + geom_line(aes(x=x.trace,y=trace[[645]]))+ ylim(-10, 35) + scale_x_time() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#ggsave("isoguavacine_zoom2_spread_depression.png", plot=tiny_plot3, width = 4, height = 4, units = "in", device = "png")
#tiny_plot4<-ggplot( ) + geom_line(aes(x=x.trace,y=trace[[1700]]))+ ylim(-10, 35) + scale_x_time() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#ggsave("isoguavacine_zoom2_last.png", plot=tiny_plot4, width = 4, height = 4, units = "in", device = "png")


# library(grid)
# vp <- viewport(width = 300000, height = 5, x = 15000000, y = 5)

find_peaks <- function (x, m = 3){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
        z <- i - m + 1
        z <- ifelse (z > 0, z, 1)
        w <- 
    }
}
