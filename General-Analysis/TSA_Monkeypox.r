# Set directory, import libraries, and import dataframe from csv
setwd("C:/Users/xedua/OneDrive/Escritorio/MCC-I/Semester_1/Applied Mathematics/FPA/Exploratory")
library(readr)
library(ggplot2)
library(tibble)
library(hrbrthemes)
library(tseries)
df <- read_csv("Daily_Country_Wise_Confirmed_Cases.csv")

# Save the column names
Countries <- df$Country

# Transpose all but the first column (Country)
df <- as.data.frame(t(df[ , -1]))
colnames(df) <- Countries
df <- tibble::rownames_to_column(df, "Date")

# Adds up a World Count in the dataframe
Sum <- rowSums(df[ , -1])
Sum <- matrix(Sum)
df[ ,114] <- Sum
Countries[113] <- 'World'
Countries2 <- 'Date'
Countries2 <- append(Countries2, Countries)
colnames(df) <- Countries2

#Plot the time series with ggplot

require(gridExtra)

p1_1 <- ggplot(df, aes(x = as.Date(Date))) +
          geom_line(aes(y = `World`), color = "black") +
          labs(x ="", y ="World") +
          scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
          theme(axis.text.x = element_text(angle=80, hjust=1), title = element_text(face = "bold"),
                panel.background = element_rect(fill = 'White', colour = 'black'),
                panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_line(colour = "gray"))

p1_2 <- ggplot(df, aes(x = as.Date(Date))) +
          geom_line(aes(y = `United States`), color = "black") +
          labs(x ="", y ="United States") +
          scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
          theme(axis.text.x = element_text(angle=80, hjust=1), title = element_text(face = "bold"),
                panel.background = element_rect(fill = 'White', colour = 'black'),
                panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_line(colour = "gray"))

p1_3 <- ggplot(df, aes(x = as.Date(Date))) +
          geom_line(aes(y = Mexico), color = "black") +
          labs(x ="Date", y ="Mexico") +
          scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
          theme(axis.text.x = element_text(angle=80, hjust=1), title = element_text(face = "bold"),
                panel.background = element_rect(fill = 'White', colour = 'black'),
                panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_line(colour = "gray"))
p1 <- grid.arrange(p1_1, p1_2, p1_3, ncol = 1)

# Import dataframe from csv, create new dataframe with accumulated cases for plotting
df2 <- read_csv("Monkey_Pox_Cases_Worldwide.csv")
Cases <- data.frame(Countries = df2$Country, Count = df2$Confirmed_Cases)
Hospitalized <- data.frame(Countries = df2$Country, Count = df2$Hospitalized)
NumberHospitalized <- sum(Hospitalized$Count)
NumberCases <- sum(Cases$Count) - NumberHospitalized
#Arrange from largest to smallest, and recover the top 20 countries with the most cases
library(dplyr)
New_cases <- Cases %>%
  arrange(desc(Count)) %>%
  slice(1:20)
New_hospitalized <- Hospitalized %>%
  arrange(desc(Count)) %>%
  slice(1:20) 
#Plot the top 20 countries, viridis changes the palette color to a fancy one
library(viridis)
p2 <- ggplot(New_cases, aes(x = reorder(Countries, +Count), y=Count, fill = Countries)) +
        geom_bar(stat="identity") +
        scale_fill_viridis_d() +
        labs(x = 'Accumulated Cases', y = 'Countries') +
        theme(panel.background = element_rect(fill = 'White', colour = 'black'), 
              panel.grid.major = element_line(colour = "gray"), 
              panel.grid.minor = element_line(colour = "gray"),
              legend.position = 'none') +
        coord_flip()
print(p2)
p3 <- ggplot(New_hospitalized, aes(x = reorder(Countries, +Count), y=Count, fill = Countries)) +
        geom_bar(stat="identity") +
        scale_fill_viridis_d() +
        labs(x = 'Accumulated Hospitalized Cases', y = 'Countries') +
        theme(panel.background = element_rect(fill = 'White', colour = 'black'), 
              panel.grid.major = element_line(colour = "gray"), 
              panel.grid.minor = element_line(colour = "gray"),
              legend.position = 'none') +
        coord_flip()
print(p3)


# Donut plot of cases and hospitalized cases worldwide
donut <- data.frame(Cases = c('Infections', 'Hospitalizations'), count = c(NumberCases, NumberHospitalized))
donut$fraction = donut$count / sum(donut$count)
donut$ymax = cumsum(donut$fraction)
donut$ymin = c(0, head(donut$ymax, n = -1))
# Compute labels
donut$labelPosition <- (donut$ymax + donut$ymin) / 2
donut$label <- paste0(donut$Cases, "\n Value: ", donut$count)
# Plot the donut
p4 <- ggplot(donut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Cases)) +
  geom_rect() +
  geom_label(x = 3.5, aes(y=labelPosition, label=label), size = 5) +
  coord_polar(theta = "y") +
  xlim(c(2.2, 4)) +
  theme(panel.background = element_rect(fill = 'White', colour = 'black')) +
  labs(y ="") +
  scale_color_manual(name = "Cases:")
print(p4)


# Wordcloud of the symptoms
library(wordcloud)
df3 <- read_csv('Worldwide_Case_Detection_Timeline.csv')
df3 <- subset(df3, !is.na(df3$Symptoms))
Symptoms <- unlist(strsplit(df3$Symptoms, ','))
Symptoms <- unlist(strsplit(Symptoms,';'))
Symptoms <- unlist(strsplit(Symptoms, " +"))
Symptoms <- subset(Symptoms, nchar(Symptoms) > 3)
df3 <- data.frame(Symptoms = Symptoms)
df3 <- transform(df3, Symptoms_Frequency = ave(seq(nrow(df3)), Symptoms, FUN = length))
df3 <- df3[!duplicated(df3), ]

p5 <- wordcloud(words = df3$Symptoms,freq = df3$Symptoms_Frequency, scale = c(8, 0.7), random.color = F, random.order = F,
          colors = rev(colorRampPalette(brewer.pal(30,"Reds"))(32)), shape = 'circle', fixed.asp = T)
print(p5)

library(patchwork)
ggsave("TimeSeries.png", plot = p1, units="cm", width=30, height=15, dpi=600)
ggsave("Top10Barplot.png", plot = p2 + p3, units="cm", width = 42.5, height = 20, dpi=600)
ggsave("WorldwideDonut.png", plot = p4, units="cm", width=30, height=30, dpi=600)
ggsave("SymptomsWordmap.png", plot = p5, units="cm", width=30, height=30, dpi=600)

# install webshot
library(webshot)
# webshot::install_phantomjs()

# save it in html
library("htmlwidgets")
saveWidget(p5,"SymptomsWordmap.html", selfcontained = F)

# and in png or pdf
webshot("SymptomsWordmap.html","SymptomsWordmap.png", delay =5, vwidth = 800, vheight=800, dpi = 600)

library(rgdal)
library(choroplethrMaps)
library(RColorBrewer)

ConfirmedCases <- data.frame(Countries = df2$Country, Cases =df2$Confirmed_Cases)

ggplot(ConfirmedCases, aes(x = long, y = lat, group = group, fill = Assault)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic")

library(rgdal)        # for readOGR(...)
library(RColorBrewer) # for brewer.pal(...)
library(ggplot2)

library(rworldmap)

#join data to a map
CasesMap <- joinCountryData2Map(ConfirmedCases, nameJoinColumn="Countries", joinCode = "NAME" )

mapDevice('x11') #create a world shaped window

#plot the map
mapCountryData(CasesMap, nameColumnToPlot='Cases', catMethod='fixedWidth', numCats=100 )


# library(tseries)
# y <- df$Mexico
# print(y)
# d.y <- diff(y)
# t <- as.Date(df$Date)
# summary(y)
# summary(d.y)
# plot(t, y)
# plot(d.y)
# adf.test(y, alternative = 'stationary', k = 0)
# adf.test(y, alternative = 'explosive', k = 0)
# # IT IS EXPLOSIVE, THE SERIES IS NOT STATIONARY
# 
# 
# acf(y)
# #
# pacf(y)
# #
# 
# arima(d.y, order = c(2, 1, 3))
# df.arima101 <- arima(y, order = c(2,1,3))
# df.pred1 <- predict(df.arima101, n.ahead = 100)
# plot(y)
# lines(df.pred1$pred, col = 'blue')
# lines(df.pred1$pred+2*df.pred1$se, col = 'red')
# lines(df.pred1$pred-2*df.pred1$se, col = 'red')
