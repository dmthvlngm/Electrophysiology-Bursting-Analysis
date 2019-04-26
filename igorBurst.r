#original file: ~/burst/data/WedJul262017c0_001.pxp

#Import libraries. 
library(IgorR)
library(ggplot2)
library(dplyr)
library(hms)
library(signal)
library(ggpubr)
library(ggsignif)
library(pracma)
library(quantmod)


#Read in PXP file 
igorRecords = read.pxp("~/burst/data/WedJul262017c0_001.pxp", ReturnTimeSeries= TRUE, 
                       regex = "RecordA")
#igorRecords[[i]]
#i = 1 -> header
#i = 2....n -> individual RecordsAi 

#Parse out experiment/recording info from header
sampleInterval = igorRecords$vars$SampleInterval
numberOfRecords = igorRecords$vars$NumWaves
experimentFrequency = igorRecords$vars$SamplesPerWave
timeWindowPerRecord = (experimentFrequency)*sampleInterval #in millisec
numberOfSamples = numberOfRecords * timeWindowPerRecord
experimentalNotes = igorRecords$Notes



#Since records will be stored as a timeseries ?dataframe
#the time data must be generated. HMS package will create 
#appropriate time window.
experimentTimeCourse <- (as.hms(seq(0,numberOfRecords, by = 1/timeWindowPerRecord)))[-1] 

#scale(), with default settings, will calculate the mean 
#and standard deviation of the entire vector, then "scale" 
#each element by those values by subtracting the mean and 
#dividing by the sd

experimentalTrace <- decimate(scale(igorRecords[[2]]),(1/sampleInterval)) 
maxAmplitude <- max(experimentalTrace, na.rm=TRUE)

for(i in 2:numberOfRecords){
    
    ithExperimentalRecord <- decimate(ts(scale(igorRecords[[i]]), start = 0,
                                         frequency = experimentFrequency, 
                                         deltat = sampleInterval),(1/sampleInterval))
    ithMaxAmplitude <- max(ithExperimentalRecord, na.rm = TRUE)
    maxAmplitude <- c(maxAmplitude, ithMaxAmplitude)
    experimentalTrace <- ts(c(experimentalTrace, ithExperimentalRecord))
    
}

#last half of maxAmp vector
#maxAmplitude <- maxAmplitude[(length(maxAmplitude)/2):(length(maxAmplitude))]

#filter max amplitude
vectorBursts <- vector()
estimatedBurstThreshold <- (median(maxAmplitude) + sd(maxAmplitude))

for(i in 1:length(maxAmplitude)){
    
    if(maxAmplitude[i] > estimatedBurstThreshold) {
        vectorBursts <- c(vectorBursts, maxAmplitude[i])
    }
}

burstPerMin <- length(vectorBursts)/10
burstPerMin

summary(maxAmplitude)

#Determine threshold for peak-finding
#(Quiroga et. al 2004) Threshold = 5*(median(x/0.6745)) where x is bandpass
#filtered signal, and (median(x/0.6745)) is an estimate of the SD of background

vectorizedExperimentalTrace <- as.vector(experimentalTrace)
thresholdValue = 5 * (median(vectorizedExperimentalTrace/0.6745))

#Use findpeaks() from pracma package
#Returns a matrix where each row represents one peak found. 
#The first column [,1] gives the height, the second the position/index 
#where the maximum is reached, the third and forth the indices of 
#where the peak begins and ends --- in the sense of where the pattern 
#starts and ends.

dataFramePeaks <- findpeaks(vectorizedExperimentalTrace, threshold = thresholdValue)
numberOfPeaks <- nrow(dataFramePeaks)


#create vector with height of peaks
peakHeights <- as.numeric(dataFramePeaks[,1])
summary(peakHeights)


#################### PLOTS ###############################

#Plot all records from the entire experiment
fullExperimentalTrace <- data.frame(experimentTimeCourse, experimentalTrace)
fullExperimentalTracePlot <- ggplot() + 
    geom_line(aes(experimentTimeCourse, experimentalTrace), color= "seagreen3") +
    xlab("Time in minutes") + ylab("mV") +
    ylim(-2,2) + 
    labs(title = "MonApr202015", 
         subtitle = "LaylaFelix: Slice1 perfused with HEPES - 95% O2") + 
    theme_classic()
ggsave("~/burst/data/MonApr202015c0_000.png")


#Plot all maxAmp distribution from the entire experimental file
dfMaxAmplitude <- data.frame(maxAmplitude)
maxAmpPlot <- ggplot(dfMaxAmplitude, aes(x=maxAmplitude)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
    labs(title = "MonApr202015", 
         subtitle = "LaylaFelix: Slice1 perfused with HEPES - 100%O2") + 
    theme_classic()

ggsave("~/burst/data/slice1_peakDist_WedJul262017c0_001.png")


#######################################
#plotting burst/min (percent)
burstcount.data <- read.csv("/Users/Maiuran/burst/data/hepesburstdata.csv", header = TRUE)
names(burstcount.data) <- c("SliceID", "Treatment", "BurstsperMinute", "PercentChange")


burstcountplot <- ggplot(burstcount.data, aes(x=SliceID, y=PercentChange, fill=Treatment)) + 
    geom_bar(stat="identity", position=position_dodge()) + 
    geom_errorbar(aes(ymin=PercentChange-sd(PercentChange), ymax=PercentChange+sd(PercentChange)), width=.2,
                  position=position_dodge(.9)) +
    scale_fill_grey() +
    labs(x = "SliceID", y = "% Change in Burst Frequency",
         title = "ACSF Oxygen Content vs. Slice Burst Frequency") +
    theme_classic()
burstcountplot

ggsave("~/burst/data/plotsdec132018/burstfreqbyslice_percent.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 800, limitsize = TRUE)


#plot avg bpm between treatments (percent)
oxy100 <- burstcount.data[which(burstcount.data$Treatment == '100%O"[2]'),]
oxy95 <- burstcount.data[which(burstcount.data$Treatment == '95%O"[2]'),]

ggplot(burstcount.data, aes(x=Treatment, y=PercentChange, fill = Treatment)) + 
    geom_bar(stat="identity", position=position_dodge()) +  scale_fill_grey() +
    geom_point(data=oxy100, aes(x=oxy100$Treatment, y=oxy100$PercentChange)) +
    geom_point(data=oxy95, aes(x=oxy95$Treatment, y=oxy95$PercentChange)) +
    geom_signif(comparisons = list(c("100%O2", "95%O2")), 
                map_signif_level=TRUE) + 
    labs(x = "Slice Treatment", y = "% Change in Burst Frequency",
         title = "ACSF Oxygen Content vs. Slice Burst Frequency") +
    theme_classic()
ggsave("~/burst/data/O2HepesBursting/burstfreq_percent.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 800, limitsize = TRUE)

#plot avg bpm between treatments (burstpermin)
oxy100 <- burstcount.data[which(burstcount.data$Treatment == '100%O2'),]
oxy95 <- burstcount.data[which(burstcount.data$Treatment == '95%O2'),]

ggplot(burstcount.data, aes(x=Treatment, y=BurstsperMinute)) + 
    geom_boxplot(outlier.shape=NA) + 
    geom_point(data=oxy100, aes(x=oxy100$Treatment, y=oxy100$BurstsperMinute, color = SliceID )) +
    geom_point(data=oxy95, aes(x=oxy95$Treatment, y=oxy95$BurstsperMinute, color = SliceID)) +
    geom_signif(comparisons = list(c("100%O2", "95%O2")), 
                map_signif_level=TRUE) + 
    theme(panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 14),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) + 
    labs(x = "Slice Treatment", y = "Bursts per Minute",
         title = "ACSF Oxygen Content vs. Slice Burst Frequency") +
    scale_y_continuous(breaks = seq(0.0, 30.0, by=10.0), limits=c(0.0,30.0)) +
    theme_classic()


ggsave("~/burst/data/plotsdec132018/burstfreq.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 800, limitsize = TRUE)
