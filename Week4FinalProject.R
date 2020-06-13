#要用的包
library(ggplot2)
library(plyr)
#下载数据
if(!(file.exists("summarySCC_PM25.rds") && file.exists("Source_Classification_Code.rds"))){
    archiveFile <- "NEI_data.zip"
    if(!file.exists(archiveFile)){
        archiveURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(url = archiveURL, destfile = archiveFile, method = "curl")
    }
    unzip(archiveFile)
}
#导入数据
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
head(SCC)
#因素
colToFactor <- c("fips", "SCC", "Pollutant", "type", "year")
NEI[,colToFactor] <- lapply(NEI[,colToFactor], factor)
head(levels(NEI$fips))
#舍去NA值
levels(NEI$fips)[1]  = NA
NEIdata <- NEI[complete.cases(NEI),]
colSums(is.na(NEIdata))
#Q1
totalEmission <- aggregate(Emissions~year, NEIdata, sum)
totalEmission
barplot(
    (totalEmission$Emissions)/10^6,
    #整理数据
    names.arg = totalEmission$year,
    #y-x轴
    xlab = "Year", ylab = "PM2.5 Emission (10^6 Tons)",
    #y-x轴名字
    main =  "Total PM2.5 Emissions From All US Source"
    #题目
)
dev.copy(png, "plot1.png", width = 480, height = 480)
dev.off()
#Q2
NEIdata_Baltimore <- subset(NEIdata, fips == "24510")
totalEmission_Baltimore <- aggregate(Emissions~year, NEIdata_Baltimore, sum)
barplot(
    (totalEmission_Baltimore$Emissions)/10^6,
    #整理数据
    names.arg = totalEmission_Baltimore$year,
    #y-x轴
    xlab = "Year", ylab = "PM2.5 Emission (10^6 Tons)",
    #y-x轴名字
    main =  "Total PM2.5 Emissions All Baltimore City Sources"
    #题目
)
dev.copy(png, "plot2.png", width = 480, height = 480)
dev.off()
#Q3 Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.
g <- ggplot(aes(x = year, y = Emissions, fill = type), data = NEIdata_Baltimore)
g + geom_bar(stat = "identity") +
    facet_grid(.~type) +
    labs(x = "year", y = expression("Total PM"[2.5]*"Emission (Tons)")) +
    labs(title = expression("PM"[2.5]*"Emissions, Baltimore City 1999-2008 by Source Type"))+
    guides(fill = F)
dev.copy(png, "plot3.png", width = 480, height = 480)
dev.off()
#Q4 
names(SCC)
names(SCC) <- gsub("\\.", "", names(SCC))
#移除列名中的.
SCCcombustion <- grepl(pattern ="comb", SCC$SCCLevelOne, ignore.case = T)
SCCcoal <- grepl(pattern = "coal", SCC$SCCLevelFour, ignore.case = T)
SCCcoalCombustionSCC <- SCC[SCCcombustion & SCCcoal,]$SCC
NIECoalCombustionValues <- NEIdata[NEIdata$SCC %in% SCCcoalCombustionSCC,]
NIECoalCombustionTotalEm <- aggregate(Emissions~year, NIECoalCombustionValues, sum)

g <- ggplot(aes(year, Emissions/10^5), data = NIECoalCombustionTotalEm)
g+geom_bar(stat = "identity", fill = "grey", width = 0.75)+
    guides(fill = F)+
    labs(x = "year", y = expression("Total PM"[2.5]*"Emission (10^5 Tons)")) +
    labs(title = expression("PM"[2.5]*"Coal Combustion Source Emission Across US from 1999-2000"))
dev.copy(png, "plot4.png", width = 480, height = 480)
dev.off()

#Q5
SCCvehicle <- grepl(pattern = "vehicle", SCC$EISector, ignore.case = T)
SCCvehicleSCC <- SCC[SCCvehicle,]$SCC

NEIvehicleSSC <- NEIdata[NEIdata$SCC %in% SCCvehicleSCC,]
NEIvehicleBaltimore <- subset(NEIvehicleSSC, fips == "24510")
NIEvehicleBaltimoreTotEm<-aggregate(Emissions~year, NEIvehicleBaltimore, sum)

g<-ggplot(aes(year, Emissions/10^5), data=NIEvehicleBaltimoreTotEm)
g+geom_bar(stat="identity",fill="grey",width=0.75) +
    guides(fill=FALSE) +
    labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
    labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))

dev.copy(png, "plot5.png", width = 480, height = 480)
dev.off()

#Q6

NEIvehicleBalti<-subset(NEIvehicleSSC, fips == "24510")
NEIvehicleBalti$city <- "Baltimore City"
NEIvehiclela<-subset(NEIvehicleSSC, fips == "06037")
NEIvehiclela$city <- "Los Angeles County"
NEIBothCity <- rbind(NEIvehicleBalti, NEIvehiclela)
ggplot(NEIBothCity, aes(x=year, y=Emissions, fill=city)) +
    geom_bar(aes(fill=year),stat="identity") +
    facet_grid(.~city) +
    guides(fill=FALSE) + theme_bw() +
    labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
    labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))

dev.copy(png, "plot6.png", width = 480, height = 480)
dev.off()

aggregateEmissions <- aggregate(Emissions~city+year, data=NEIBothCity, sum)
aggregate(Emissions~city, data=aggregateEmissions, range)


