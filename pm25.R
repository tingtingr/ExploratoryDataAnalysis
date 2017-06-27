# load the data
pm25sum <- readRDS('ExploratoryDataAnalysis/summarySCC_PM25.rds')
pm25 <- readRDS('ExploratoryDataAnalysis/Source_Classification_Code.rds')
# > dim(pm25sum)
# [1] 6497651       6
# > dim(pm25)
# [1] 11717    15

#convert to data.table
pm25sum <- data.table(pm25sum)
pm25 <- data.table(pm25)

#Question 1. 
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.


# > str(pm25sum)
# Classes 'data.table' and 'data.frame':	6497651 obs. of  6 variables:
#   $ fips     : chr  "09001" "09001" "09001" "09001" ...
# $ SCC      : chr  "10100401" "10100404" "10100501" "10200401" ...
# $ Pollutant: chr  "PM25-PRI" "PM25-PRI" "PM25-PRI" "PM25-PRI" ...
# $ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
# $ type     : chr  "POINT" "POINT" "POINT" "POINT" ...
# $ year     : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...
# - attr(*, ".internal.selfref")=<externalptr> 

#convert year to factor
pm25sum$year <- as.factor(pm25sum$year)

# sum of emissions
totalEmission <- tapply(pm25sum$Emissions,pm25sum$year,sum)

# > totalEmission
# 1999    2002    2005    2008 
# 7332967 5635780 5454703 3464206

png('ExploratoryDataAnalysis/plot01.png')

plot(names(totalEmission), totalEmission, type = "l", color = 'red', xlab = "Year", ylab = "Total Emissions (tons)",main = "Total Emissions by Year")

dev.off()

#Answer: Yes. See plot01.png

#Question 2. 
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.

mdEmission <- pm25sum[(fips == '24510'),]
# > dim(MDEmission)
# [1] 2096    6
totalmdEmission <- tapply(mdEmission$Emissions, mdEmission$year, sum)
# > head(totalmdEmission)
# 1999     2002     2005     2008 
# 3274.180 2453.916 3091.354 1862.282 
png('ExploratoryDataAnalysis/plot02.png')
g<-plot(names(totalmdEmission),totalmdEmission, type = 'l', col = "red", xlab='Year', ylab = 'Total Emissions in MD (tons)',main = 'Total Emissions in MD by Year')
dev.off()

# Answer: Yes. See plot02.png

# Question 3.
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

# table(mdEmission$type)
# 
# NON-ROAD NONPOINT  ON-ROAD    POINT 
# 416      142     1119      419 
# > str(mdEmission)
# Classes 'data.table' and 'data.frame':	2096 obs. of  6 variables:
#   $ fips     : chr  "24510" "24510" "24510" "24510" ...
# $ SCC      : chr  "10100601" "10200601" "10200602" "30100699" ...
# $ Pollutant: chr  "PM25-PRI" "PM25-PRI" "PM25-PRI" "PM25-PRI" ...
# $ Emissions: num  6.53 78.88 0.92 10.38 10.86 ...
# $ type     : chr  "POINT" "POINT" "POINT" "POINT" ...
# $ year     : Factor w/ 4 levels "1999","2002",..: 1 1 1 1 1 1 1 1 1 1 ...
# - attr(*, ".internal.selfref")=<externalptr> 
#   > 

# Noticed, type is a chr. Let\'s change it to factor

mdEmission$type <- as.factor(mdEmission$type)
# class(mdEmission$type)
# [1] "factor"

mdEmissionSumTypeYear <- tapply(mdEmission$Emissions, list(mdEmission$type,mdEmission$year), sum)
mdEmissionSumTypeYear <- melt(mdEmissionSumTypeYear,id = 'Year')
colnames(mdEmissionSumTypeYear) = c('Types','Year','Emissions')

# > mdEmissionSumTypeYear
# Type Year  Emissions
# 1  NON-ROAD 1999  522.94000
# 2  NONPOINT 1999 2107.62500
# 3   ON-ROAD 1999  346.82000
# 4     POINT 1999  296.79500
# 5  NON-ROAD 2002  240.84692
# 6  NONPOINT 2002 1509.50000
# 7   ON-ROAD 2002  134.30882
# 8     POINT 2002  569.26000
# 9  NON-ROAD 2005  248.93369
# 10 NONPOINT 2005 1509.50000
# 11  ON-ROAD 2005  130.43038
# 12    POINT 2005 1202.49000
# 13 NON-ROAD 2008   55.82356
# 14 NONPOINT 2008 1373.20731
# 15  ON-ROAD 2008   88.27546
# 16    POINT 2008  344.97518
png('ExploratoryDataAnalysis/plot03.png')
qplot(Year, Emissions, data = mdEmissionSumTypeYear, color = Types, facets = . ~ Types, geom=c('point','smooth'),main='Baltimore Total Emissions based on Type from 1999 - 2008')
dev.off()

# Answer: 
# Decreased: NONPOINT, NON-ROAD, ON-ROAD
# Increased(slightly): POINT

#Question 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

#Examine data, noticed the categorization is in 'EI.Sector' 

# dim(pm25)
# [1] 11717    15
# > head(pm25,3)
# SCC Data.Category
# 1: 10100101         Point
# 2: 10100102         Point
# 3: 10100201         Point
# Short.Name
# 1:                   Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal
# 2: Ext Comb /Electric Gen /Anthracite Coal /Traveling Grate (Overfeed) Stoker
# 3:       Ext Comb /Electric Gen /Bituminous Coal /Pulverized Coal: Wet Bottom
# EI.Sector Option.Group Option.Set
# 1: Fuel Comb - Electric Generation - Coal                        
# 2: Fuel Comb - Electric Generation - Coal                        
# 3: Fuel Comb - Electric Generation - Coal                        
# SCC.Level.One       SCC.Level.Two               SCC.Level.Three
# 1: External Combustion Boilers Electric Generation               Anthracite Coal
# 2: External Combustion Boilers Electric Generation               Anthracite Coal
# 3: External Combustion Boilers Electric Generation Bituminous/Subbituminous Coal
# SCC.Level.Four Map.To Last.Inventory.Year Created_Date
# 1:                               Pulverized Coal     NA                  NA             
# 2:             Traveling Grate (Overfeed) Stoker     NA                  NA             
# 3: Pulverized Coal: Wet Bottom (Bituminous Coal)     NA                  NA             
# Revised_Date Usage.Notes

coalnames <- grep(".*Comb.*Coal*",names(table(pm25$EI.Sector)),value = T, ignore.case = T)

# > coalnames
# [1] "Fuel Comb - Comm/Institutional - Coal"       "Fuel Comb - Electric Generation - Coal"     
# [3] "Fuel Comb - Industrial Boilers, ICEs - Coal"
x <- pm25[pm25$EI.Sector == coalnames,]

coalSCC <- unique(x$SCC)
# > length(coalSCC)
# [1] 33

coalEmission <- pm25sum[(pm25sum$SCC == coalSCC),]
# > dim(coalEmission)
# [1] 403   6

coalEmissionSum <- tapply(coalEmission$Emissions, list(coalEmission$year, coalEmission$type), sum)

# > coalEmissionSum
# NONPOINT     POINT
# 1999 126.29300 9580.6910
# 2002 292.80657 3774.8789
# 2005 165.12883  486.3100
# 2008  32.28742  883.7379

coalEmissionSum <- melt(coalEmissionSum)
names(coalEmissionSum) <- c('Year','Type','Emissions')
# > coalEmissionSum
# Year     Type  Emissions
# 1 1999 NONPOINT  126.29300
# 2 2002 NONPOINT  292.80657
# 3 2005 NONPOINT  165.12883
# 4 2008 NONPOINT   32.28742
# 5 1999    POINT 9580.69100
# 6 2002    POINT 3774.87887
# 7 2005    POINT  486.31000
# 8 2008    POINT  883.73794

png('ExploratoryDataAnalysis/plot04.png')
plot(names(coalEmissionSum),coalEmissionSum, type ='l', col='red', xlab = 'Year', ylab = 'Coal Emission Sum', main = 'Coal Emission Sum from 1999 - 2008')
dev.off()