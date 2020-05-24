library(ggplot2)
library(tidyverse)
library(tidyr)
library(pastecs)
library(plotly)

realestate.raw <- read.csv("CSV_Housing.csv",header=TRUE)
names(realestate.raw)[1]<-paste("ZIP")
realestate.raw <- realestate.raw[1:3]
realestate.raw[,3] <- as.numeric(gsub("%","",realestate.raw[,3]))
realestate.raw$ZIP <- as.factor(realestate.raw$ZIP)

commodities.raw <- read.csv("CSV_Commodities_General.csv",header=TRUE)
names(commodities.raw)[1]<-paste("AssetType")
commodities.raw<-commodities.raw[1:3]
commodities.raw[,3]<- as.numeric(gsub("%","",commodities.raw[,3]))
commodities.raw$AssetType<-as.factor(commodities.raw$AssetType)


bonds.raw <- read.csv("CSV_Bonds.csv",header=TRUE)
names(bonds.raw)[1]<-paste("AssetType")
bonds.raw<-bonds.raw[1:3]
bonds.raw[,3]<- as.numeric(gsub("%","",bonds.raw[,3]))
bonds.raw$AssetType<-as.factor(bonds.raw$AssetType)


metals.raw <- read.csv("Metal Commodities Dataset.csv",header=TRUE)
names(metals.raw)[1]<-paste("AssetType")
metals.raw[,3]<- as.numeric(gsub("%","",metals.raw[,3]))
metals.raw$AssetType<-as.factor(metals.raw$AssetType)


metals.subset <- read.csv("Metal subset.csv",header=TRUE)
names(metals.subset)[1]<-paste("AssetType")
metals.subset<-metals.subset[1:3]
metals.subset[,3]<- as.numeric(gsub("%","",metals.subset[,3]))
metals.subset$AssetType<-as.factor(metals.subset$AssetType)


stocks.raw <- read.csv("CSV_Stocks.csv",header=TRUE)
names(stocks.raw)[1]<-paste("AssetType")
stocks.raw<-stocks.raw[1:3]
stocks.raw[,3]<- as.numeric(gsub("%","",stocks.raw[,3]))
stocks.raw$AssetType<-as.factor(stocks.raw$AssetType)


REIT.raw <- read.csv("REIT Dataset.csv",header=TRUE)
names(REIT.raw)[1]<-paste("AssetType")
REIT.raw<-REIT.raw[1:3]
REIT.raw[,3]<- as.numeric(gsub("%","",REIT.raw[,3]))
REIT.raw$AssetType<-as.factor(REIT.raw$AssetType)


nearcash.raw <- read.csv("CSV_NearCash.csv",header=TRUE)
names(nearcash.raw)[1]<-paste("AssetType")
nearcash.raw<-nearcash.raw[1:3]
nearcash.raw[,3]<- as.numeric(gsub("%","",nearcash.raw[,3]))
nearcash.raw$AssetType<-as.factor(nearcash.raw$AssetType)



byyear.summary <- read.csv("ByRetirementYear.csv",header=TRUE)
names(byyear.summary)[1]<-paste("AssetType")
byyear.summary<-byyear.summary[1:3]
byyear.summary[,3]<- as.numeric(gsub("%","",byyear.summary[,3]))
byyear.summary$AssetType<-as.factor(byyear.summary$AssetType)


summarydf <- read.csv("summaryData.csv",header=TRUE)
names(summarydf)[1]<-paste("AssetType")
summarydf<-summarydf[1:3]
summarydf[,2]<- as.numeric(gsub("%","",summarydf[,2]))
summarydf[,3]<- as.numeric(gsub("%","",summarydf[,3]))
summarydf$AssetType<-as.factor(summarydf$AssetType)



g <- ggplot(data = realestate.raw, aes(Growth))+
        geom_histogram(binwidth=1)+
        ggtitle("Histogram of YOY Real Estate Value Growth")+
        xlab("% Growth Over Prior Year")+
        ylab("Count")

print(g)
ggsave("Real Estate Histogram.png",height = 5, width = 5)



c <- ggplot(data = realestate.raw, aes(x = Year, y = Growth))+
        geom_point(shape=1)+
        geom_smooth()+
        ggtitle("Scatter of YOY Real Estate Value Growth")+
        xlab("Year")+
        ylab("% Growth Over Prior Year")
ggplotly(c)
print(c)
ggsave("Real Estate Scatterplot.png",height = 5, width = 5)


b <- ggplot(data=commodities.raw, aes(x = Year, y = Growth))+
        geom_boxplot(aes(AssetType))+
        ggtitle("General Commodities Annual Return")+
        xlab("Asset Type")+
        ylab("% Growth Over Prior Year")+
        theme(axis.text.x = element_text(size=10,angle=-90,vjust = 0.5, hjust=1))


print(b)
ggsave("General Commodities Boxplot.png",height = 10,width = 10)


a <- ggplot(data=metals.raw, aes(x = Year, y = Growth, color = AssetType))+
        geom_jitter(alpha=0.7)+
        ggtitle("Metals Annual Return")+
        geom_smooth()+
        xlab("Asset Type")+
        ylab("% Growth Over Prior Year")+
        xlim(1975,2019)
        
        
print(a)
ggsave("Metals Jitterchart.png",height = 10,width = 10)


d <- ggplot(data=metals.raw, aes(x = Year, y = Growth))+
        geom_boxplot(aes(AssetType))+
        ggtitle("Metal Commodities Annual Return")+
        xlab("Asset Type")+
        ylim(-50,150)+
        ylab("% Growth Over Prior Year")+
        theme(axis.text.x = element_text(size=10,angle=-90,vjust = 0.5, hjust=1))



print(d)
ggsave("Metal Commodities Boxplot.png",height = 10,width = 10)


e <- ggplot(data=REIT.raw, aes(x = Year, y = Growth))+
        geom_boxplot(aes(AssetType))+
        ggtitle("REIT Annual Return")+
        xlab("Asset Type")+
        ylab("% Growth Over Prior Year")

print(e)
ggsave("REIT Boxplot.png",height = 10,width = 10)



f <- ggplot(data=stocks.raw, aes(x = Year, y = Growth, color = AssetType))+
        geom_jitter(alpha=0.7)+
        geom_smooth()+
        ggtitle("Stock Market Annual Return")+
        xlab("Asset Type")+
        ylab("% Growth Over Prior Year")+
        

print(f)
ggsave("Stocks Scatterplot.png",height = 10,width = 10)


h <- ggplot(data=bonds.raw, aes(x = Year, y = Growth, color = AssetType))+
        geom_jitter(alpha=0.7)+
        geom_smooth()+
        ggtitle("Bond Market Annual Return")+
        xlab("Asset Type")+
        ylab("% Growth Over Prior Year")+
        
        
        print(h)
ggsave("Bonds Scatterplot.png",height = 10,width = 10)


i <- ggplot(data=metals.subset, aes(x = Year, y = Growth, color = AssetType))+
        geom_jitter(alpha=0.7)+
        geom_smooth()+
        ggtitle("Selected Metal Commodities Annual Return")+
        xlim(1950,2019)+
        ylim(-50,100)+
        xlab("Asset Type")+
        ylab("% Growth Over Prior Year")+
        
        
        print(i)
ggsave("Selected Metals Timeseries.png",height = 10,width = 10)



j <- ggplot(data=nearcash.raw, aes(x = Year, y = Growth, color = AssetType))+
        geom_jitter(alpha=0.7)+
        geom_smooth()+
        ggtitle("Cash and Near Cash Annual Return")+
        xlab("Asset Type")+
        ylab("% Growth Over Prior Year")+
        
        
        print(j)
ggsave("Cash and Near Cash Timeseries.png",height = 10,width = 10)



k <- ggplot(data=summarydf, aes(x = Year, y = Growth, color = AssetType))+
        geom_point()+
        facet_wrap(~AssetType, ncol=3)+
        geom_smooth()+
        ggtitle("Expected Impact of Each Asset per Year")+
        xlab("Year")+
        ylab("% Growth Over Prior Year")+
        ylim(-50,50)
        
        print(k)
ggsave("Per Year Asset Facet.png",height = 10,width = 10)


