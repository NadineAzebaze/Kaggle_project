#Installer les librairies (Utile seulement la première fois !)
##install.packages("devtools")
#install.packages("RColorBrewer")
#devtools::install_github("hadley/tidyverse")
#install.packages("tidytext")
#install.packages("viridis")  # Installer

#Charger les librairies
library("viridis")           
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)
library(tidytext)
library(sqldf)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(wordcloud2)
library(tm)
################################

library(dplyr)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(grid)
library(gridExtra)
library(scales)
library(knitr)

dataset = read.csv("Hotel_Reviews.csv")

# check for if any null values are present in which columns
sapply(dataset, function(x)sum(is.na(x)))


head(summary(as.factor(dataset$Reviewer_Nationality)), n = 1) # getting top Nationality for na's


dataset$Review_Range <- cut(dataset$Reviewer_Score, breaks = 1.5:11, include.lowest = T)

#dataset$lng[is.na(dataset_f$lng)] <- 0
#dataset_f$lat[is.na(dataset_f$lat)] <- 0
#dataset_f$Country <- coords2country(cbind(dataset_f$lng, dataset_f$lat))

dataset$Total_Words <- dataset$Review_Total_Negative_Word_Counts + dataset$Review_Total_Positive_Word_Counts
dataset$Positive_Word_Rate <- dataset$Review_Total_Positive_Word_Counts / dataset$Total_Words

dataset$Review_Range <- as.factor(cut(dataset$Reviewer_Score, breaks = 1.5:11, include.lowest = T))
dataset$Review_Range <- gsub("]", ")", dataset$Review_Range)
dataset$Review_Range <- factor(dataset$Review_Range, levels = c(
    "(9.5,10.5)", "(8.5,9.5)",
    "(7.5,8.5)", "(6.5,7.5)",
    "(5.5,6.5)", "(4.5,5.5)",
    "(3.5,4.5)", "(2.5,3.5)",
    "[1.5,2.5)"
))

review_range <- dataset %>%
    select(
        Review_Range, Total_Number_of_Reviews,
        Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts, Hotel_Name
    ) %>%
    # #Remove the 17 records without geo coordinates
    # filter(lat != 0 & lng != 0) %>%
    group_by(Review_Range) %>%
    summarise(
        Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
        Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
        Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
        Pos_Word_Rate = percent(Tot_Pos_Words / Total_Words),
        Neg_Word_Rate = percent(Tot_Neg_Words / Total_Words),
        Num_Reviews = n(),
        Avg_Words_Per_Review = format(Total_Words / Num_Reviews, digits = 4)
    )

kable(as.data.frame(review_range))

# can only run once without getting lat & lng errors!
dataset <- dataset %>%
  select(colnames(dataset),-lat, -lng, -Review_Total_Negative_Word_Counts, -Review_Total_Positive_Word_Counts, -Additional_Number_of_Scoring) %>%
  mutate(Positive_Review = ifelse(is.na(Positive_Review),
                                  "Nothing",
                                  Positive_Review)) %>%
  mutate(Negative_Review = ifelse(is.na(Negative_Review),
                                  "Nothing",
                                  Negative_Review))
sapply(dataset, function(x)sum(is.na(x)))
# good no more NAs!

## Get hotel details
dataset <- dataset%>%
    filter(!duplicated(Hotel_Address))->dataset_clean
    dataset_clean$Country=sapply(str_split(dataset_clean$Hotel_Address," "),function(x){x[length(x)]})
    dataset_clean$city=sapply(str_split(dataset_clean$Hotel_Address," "),function(x){x[length(x)-1]})
    ## Remove the mention of "United" as "London" in the city column and "Kingdom" as "United Kingdom" in the country column
    dataset_clean$city=str_replace(dataset_clean$city,"United","London")
    dataset_clean$Country=str_replace(dataset_clean$Country,"Kingdom","United Kingdom")


dataset_clean <- dataset_clean %>%
  mutate(Review_Date = lubridate::mdy(Review_Date))
#summary(df)
# Time Series Analysis
# is the score dependent on time of year?
dataset_clean <- dataset_clean %>%
  mutate(Year = year(Review_Date)) %>%
  mutate(Month = month(Review_Date))
  


# subsetting out lengthy columns, keeping hotel address and review data if
# needed to join
dataset_nlp <- dataset_clean %>%
  select(Hotel_Name, Country, Review_Date, Year, Month,
         Total_Number_of_Reviews_Reviewer_Has_Given, Negative_Review, Reviewer_Nationality,
         Positive_Review,Total_Number_of_Reviews_Reviewer_Has_Given, Tags, Reviewer_Score)
dataset_clean <- dataset_clean %>%
  select(Hotel_Name, Country, Review_Date, Year, Month, Average_Score,
         Reviewer_Nationality,Reviewer_Score,Total_Number_of_Reviews_Reviewer_Has_Given, Negative_Review,
         Positive_Review)

print("Our dataset cleaned:" )
head(dataset_clean)
print("First column of NLP:" )
head(dataset_nlp, n = 1)

dataset_clean <- dataset_clean %>%
  mutate(Reviewer_Nationality = trimws(dataset_clean$Reviewer_Nationality, which = "both"))

dataset_clean

#date non utile car par autant de données dans chaque année
df_eda <- dataset_clean %>%
    select(Hotel_Name, Country, Review_Date, Year, Month, Average_Score,
    Reviewer_Nationality,Reviewer_Score,Total_Number_of_Reviews_Reviewer_Has_Given)

ggplot(df_eda, aes(Reviewer_Score)) +
  geom_histogram(bins = 20) + 
  facet_grid(Year~Month)

dataset_clean%>%select(Average_Score,Country)%>%distinct(Average_Score,Country)%>%ggplot(aes(x=Average_Score))+geom_histogram(color='blue',fill='blue',alpha=0.3,bins=30)+xlab("Average Review Score")+ylab("Counts")

ggplot(dataset_clean, aes(Reviewer_Score))+
  geom_histogram(bins = 20) +
  ggtitle("Distribution of All Reviewer Scores")

dataset%>%ggplot(aes(x=as.factor(Country),y=Average_Score))+geom_boxplot()+xlab("Country")+ylab("Average Score")

dataset_clean%>%select(Average_Score,Hotel_Name)%>%distinct(Average_Score,Hotel_Name)%>%ggplot(aes(x=Average_Score))+geom_histogram(color='blue',fill='blue',alpha=0.3,bins=30)+xlab("Average Review Score")+ylab("Counts")

dataset_clean%>%ggplot(aes(x=as.factor(Country),y=Average_Score))+geom_boxplot()+xlab("Country")+ylab("Average Score")

# re-importing the dataset because we need different threatment of the data
dataset_f <- read.csv('Hotel_Reviews.csv')

# Function to extract country from lat/lon (will be switching to getting name from address field)

#from:https://stackoverflow.com/a/14342127
install.packages("sp")
install.packages("rworldmap")
library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country <- function(points) {
    countriesSP <- getMap(resolution = "low")
    # countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail

    # convert our list of points to a SpatialPoints object

    # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

    # setting CRS directly to that from rworldmap
    pointsSP <- SpatialPoints(points, proj4string = CRS(proj4string(countriesSP)))


    # use 'over' to get indices of the Polygons object containing each point
    indices <- over(pointsSP, countriesSP)

    # return the ADMIN names of each country
    indices$ADMIN
    # indices$ISO3 # returns the ISO3 code
    # indices$continent   # returns the continent (6 continent model)
    # indices$REGION   # returns the continent (7 continent model)
}

p1 <- ggplot(review_range, aes(
    x = Review_Range,
    y = Num_Reviews,
    fill = Review_Range
)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(label = review_range$Num_Reviews, nudge_y = 0.5) +
    ggtitle(label = "Number of Reviews by Range of Score") +
    labs(xlab("Review Range"), ylab("Number of Reviews")) +
    theme(legend.position = "off")

g1 <- ggplotGrob(p1)

grid.draw(g1)

p2 <- ggplot(review_range, aes(
    x = Review_Range,
    y = Pos_Word_Rate,
    fill = Review_Range
)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(label = review_range$Pos_Word_Rate, nudge_y = 0.2) +
    ggtitle(label = "Percent Positive Words by Range of Score") +
    labs(xlab("Review Range"), ylab("Percent(%) Positive Words")) +
    theme(legend.position = "bottom")

g2 <- ggplotGrob(p2)

grid.draw(g2)

p3 <- ggplot(review_range, aes(
    x = Review_Range,
    y = Avg_Words_Per_Review,
    fill = Review_Range
)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(label = review_range$Avg_Words_Per_Review, nudge_y = 0.2) +
    ggtitle(label = "Average Words per Review by Range of Score") +
    labs(xlab("Review Range"), ylab("Avg. Words per Review")) +
    theme(legend.position = "off")

g3 <- ggplotGrob(p3)


grid.draw(g3)

grid.draw(rbind(g1, g3, g2, size = "last"))

#PIECHART

countryTable <- table(dataset_clean$Country)
pieNames <- paste(countryTable, sep="")
pieLabels <- paste(names(countryTable), "\n", countryTable, sep="")
pie(countryTable, labels = pieLabels, col = viridis(length(countryTable)),
    main="Pie Chart of Nb de personnes allant dans chaque pays\n with sample size")

#Barplot 
barplot(countryTable, 
        col = viridis(length(countryTable)),
        border = "dark blue"
        )
title(main = "Barplot of Species\n with sample size")

#PIECHART
nationalityTable <- table(dataset$Reviewer_Nationality)
pieNames <- paste(nationalityTable, sep="")
pieLabels <- paste(names(nationalityTable), "\n", nationalityTable, sep="")
#pie(nationalityTable, labels = pieLabels, main="Pie Chart of Nationality\n with sample size")

#Barplot 
barplot(nationalityTable,
        col = viridis(length(nationalityTable)),
        border = "dark blue"
       )
title(main = "Barplot of Nationality\n with sample size")

wordcloud2(data = nationalityTable, minRotation = 0, maxRotation = 45)

#ind=which(is.na(dataset_clean$Reviewer_Nationality))
#data_model=dataset_clean[-ind,]"""

dataset_clean$tourist=ifelse(dataset_clean$Reviewer_Nationality==dataset_clean$Country,"Yes","No")
dataset_clean$tourist=as.factor(dataset_clean$tourist)
table(dataset_clean$tourist)
dataset_clean%>%group_by(Country,tourist)%>%summarise(average_score=mean(Average_Score))%>%ungroup()%>%mutate(Average_Score=average_score**7)%>%ggplot(aes(x=Country,y=Average_Score,color=tourist,fill=tourist))+geom_bar(stat='identity',position='dodge')+xlab("Country")+ylab("Average Score")+scale_y_continuous(breaks = NULL)


touristTable <- table(dataset_clean$tourist)
touristTable
#pieNames <- paste(touristTable, sep="")
pieLabels <- paste0(round(100 * touristTable/sum(touristTable), 2), "%")
pie(touristTable, labels = pieLabels, col = viridis(length(touristTable)),
    main="Pie Chart of Tourist\n in the dataset")

legend("topleft", legend = c("Locals", "Tourist"),
       fill =  viridis(length(touristTable)))

atTable <- table(((dataset_clean%>%filter(Country=='Austria'))$tourist))
pieLabels <- paste0(round(100 * atTable/sum(atTable), 2), "%")
pie(atTable, labels = pieLabels, col = viridis(length(atTable)),
    main="Pie Chart of Tourist\n in Austria") -> at

legend("topleft", legend = c("Locals", "Tourist"),
       fill =  viridis(length(atTable)))


frTable <- table(((dataset_clean%>%filter(Country=='France'))$tourist))
pieLabels <- paste0(round(100 * frTable/sum(frTable), 2), "%")
pie(frTable, labels = pieLabels, col = viridis(length(atTable)),
    main="Pie Chart of Tourist\n in France") -> fr

legend("topleft", legend = c("Locals", "Tourist"),
       fill =  viridis(length(atTable)))


spTable <- table(((dataset_clean%>%filter(Country=='Spain'))$tourist))
pieLabels <- paste0(round(100 * spTable/sum(spTable), 2), "%")
pie(spTable, labels = pieLabels, col = viridis(length(atTable)),
    main="Pie Chart of Tourist\n in Spain")

legend("topleft", legend = c("Locals", "Tourist"),
       fill =  viridis(length(atTable)))

nTable <- table(((dataset_clean%>%filter(Country=='Netherlands'))$tourist))
pieLabels <- paste0(round(100 * nTable/sum(nTable), 2), "%")
pie(nTable, labels = pieLabels, col = viridis(length(atTable)),
    main="Pie Chart of Tourist\n in Netherlands")

legend("topleft", legend = c("Locals", "Tourist"),
       fill =  viridis(length(atTable)))


itTable <- table(((dataset_clean%>%filter(Country=='Italy'))$tourist))
pieLabels <- paste0(round(100 * itTable/sum(itTable), 2), "%")
pie(itTable, labels = pieLabels, col = viridis(length(atTable)),
    main="Pie Chart of Tourist\n in Italy")

legend("topleft", legend = c("Locals", "Tourist"),
       fill =  viridis(length(atTable)))


#grid.arrange(at,fr,ncol=2)

# set the plotting area into a 1*2 array
par(mfrow=c(2,2))

# Draw the two pie chart using above datasets
pie(atTable, paste0(round(100 * atTable/sum(atTable), 2), "%"),main="Tourists in Austria")
legend("topleft", legend = c("Locals", "Tourist"),
       fill =  c("white", "lightblue"))

pie(frTable, paste0(round(100 * frTable/sum(frTable), 2), "%"),main="Tourists in France")
pie(atTable, paste0(round(100 * itTable/sum(itTable), 2), "%"),main="Tourists in Italy")
pie(spTable, paste0(round(100 * spTable/sum(spTable), 2), "%"),main="Tourists in Spain")




#plot_world_cloud
reviews <- dataset_clean[sample(nrow(dataset_clean), 40000), ]
reviews <- reviews[reviews$Positive_Review!='Nothing',]
reviews <- reviews[reviews$Negative_Review!='Nothing',]
term_freq <- function(dataset_clean,sent){
  if(sent=='pos'){
    corpus <- Corpus(VectorSource(dataset_clean$Positive_Review))
  }else{
    corpus <- Corpus(VectorSource(dataset_clean$Negative_Review))
  }
  corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  dtm <-TermDocumentMatrix(corpus)
  mat_dtm <- as.matrix(dtm)
  v_dtm <- sort(rowSums(mat_dtm),decreasing = TRUE)
  FreqMat <- data.frame(word = names(v_dtm), Freq = v_dtm)
  FreqMat <- FreqMat[1:50,]
  return(FreqMat)
}

wordcloud2(data = term_freq(reviews,'neg'), minRotation = 0, maxRotation = 0)

dataset_country <- dataset %>%
  select(Hotel_Name, Country, Review_Date, Year, Month,
         Total_Number_of_Reviews_Reviewer_Has_Given, Negative_Review, Reviewer_Nationality,
         Positive_Review,Total_Number_of_Reviews_Reviewer_Has_Given, Tags, Reviewer_Score)
data = subset(dataset_country, Country == c("Austria"))
#plot_world_cloud
reviews <- data[sample(nrow(data), 4000), ]
reviews <- reviews[reviews$Positive_Review!='Nothing',]
reviews <- reviews[reviews$Negative_Review!='Nothing',]
term_freq <- function(data,sent){
  if(sent=='pos'){
    corpus <- Corpus(VectorSource(data$Positive_Review))
  }else{
    corpus <- Corpus(VectorSource(data$Negative_Review))
  }
  corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  dtm <-TermDocumentMatrix(corpus)
  mat_dtm <- as.matrix(dtm)
  v_dtm <- sort(rowSums(mat_dtm),decreasing = TRUE)
  FreqMat <- data.frame(word = names(v_dtm), Freq = v_dtm)
  FreqMat <- FreqMat[1:50,]
  return(FreqMat)
}
wordcloud2(data = term_freq(reviews,'pos'),minRotation = 0,maxRotation = 0)

#WorkCloud

dataset_country <- dataset %>%
  select(Hotel_Name, Country, Review_Date, Year, Month,
         Total_Number_of_Reviews_Reviewer_Has_Given, Negative_Review, Reviewer_Nationality,
         Positive_Review,Total_Number_of_Reviews_Reviewer_Has_Given, Tags, Reviewer_Score)
#plot_world_cloud
reviews <- dataset_clean[sample(nrow(data), 4000), ]

term_freqNat <- function(dataset){
    corpus <- Corpus(VectorSource(dataset$Reviewer_Nationality))
    corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
    corpus <- tm_map(corpus, stripWhitespace)
    dtm <-TermDocumentMatrix(corpus)
    mat_dtm <- as.matrix(dtm)
    v_dtm <- sort(rowSums(mat_dtm),decreasing = TRUE)
    FreqMat <- data.frame(word = names(v_dtm), Freq = v_dtm)
    FreqMat <- FreqMat[1:50,]
    return(FreqMat)
}
wordcloud2(data = term_freqNat(reviews), minRotation = 0, maxRotation = 45)

#dataset_country = dataset_clean[0:2]
#print(dataset_country)

#PIECHART

countryTable <- table(dataset_clean$Country)
pieNames <- paste(countryTable, sep="")
pieLabels <- paste(names(countryTable), "\n", countryTable, sep="")
pie(countryTable, labels = pieLabels, col = viridis(length(countryTable)),
    main="Pie Chart of Nb de personnes allant dans chaque pays\n with sample size")

#Barplot 
barplot(countryTable, 
        col = viridis(length(countryTable)),
        border = "dark blue"
        )
title(main = "Barplot of Species\n with sample size")

## Get hotel details
dataset_clean <- 
    select(Hotel_Name,lat,lng,Hotel_Address)%>%group_by(Hotel_Address) <- 
        filter(!duplicated(Hotel_Address))->hotel_details
hotel_details$country=sapply(str_split(hotel_details$Hotel_Address," "),function(x){x[length(x)]})
hotel_details$city=sapply(str_split(hotel_details$Hotel_Address," "),function(x){x[length(x)-1]})
## Remove the mention of "United" as "London" in the city column and "Kingdom" as "United Kingdom" in the country column
hotel_details$city=str_replace(hotel_details$city,"United","London")
hotel_details$country=str_replace(hotel_details$country,"Kingdom","United Kingdom")
data%>%left_join(hotel_details[,4:6],by = 'Hotel_Address')->data
countries=paste(unique(hotel_details$country),collapse=",")
message=paste("The countries mentioned in the dataset are:", countries)
print(message)

# https://www.kaggle.com/code/sampsonsimpson/map-of-hotels-rmd/report
# https://www.kaggle.com/code/sampsonsimpson/exploring-515k-european-hotel-reviews/report

# re-importing the dataset because we need different threatment of the data
dataset_f <- read.csv('../dataset/Hotel_Reviews.csv')

# Function to extract country from lat/lon (will be switching to getting name from address field)

# from:https://stackoverflow.com/a/14342127
library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country <- function(points) {
    countriesSP <- getMap(resolution = "low")
    # countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail

    # convert our list of points to a SpatialPoints object

    # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

    # setting CRS directly to that from rworldmap
    pointsSP <- SpatialPoints(points, proj4string = CRS(proj4string(countriesSP)))


    # use 'over' to get indices of the Polygons object containing each point
    indices <- over(pointsSP, countriesSP)

    # return the ADMIN names of each country
    indices$ADMIN
    # indices$ISO3 # returns the ISO3 code
    # indices$continent   # returns the continent (6 continent model)
    # indices$REGION   # returns the continent (7 continent model)
}

dataset_f$Review_Range <- cut(dataset_f$Reviewer_Score, breaks = 1.5:11, include.lowest = T)
dataset_f$lng[is.na(dataset_f$lng)] <- 0
dataset_f$lat[is.na(dataset_f$lat)] <- 0
dataset_f$Country <- coords2country(cbind(dataset_f$lng, dataset_f$lat))
dataset_f$Total_Words <- dataset_f$Review_Total_Negative_Word_Counts + dataset_f$Review_Total_Positive_Word_Counts
dataset_f$Positive_Word_Rate <- dataset_f$Review_Total_Positive_Word_Counts / dataset_f$Total_Words

dataset_f$Review_Range <- as.factor(cut(dataset_f$Reviewer_Score, breaks = 1.5:11, include.lowest = T))
dataset_f$Review_Range <- gsub("]", ")", dataset_f$Review_Range)
dataset_f$Review_Range <- factor(dataset_f$Review_Range, levels = c(
    "(9.5,10.5)", "(8.5,9.5)",
    "(7.5,8.5)", "(6.5,7.5)",
    "(5.5,6.5)", "(4.5,5.5)",
    "(3.5,4.5)", "(2.5,3.5)",
    "[1.5,2.5)"
))


head(dataset_f)

review_range <- dataset_f %>%
    select(
        Review_Range, Total_Number_of_Reviews,
        Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts, Hotel_Name
    ) %>%
    # #Remove the 17 records without geo coordinates
    # filter(lat != 0 & lng != 0) %>%
    group_by(Review_Range) %>%
    summarise(
        Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
        Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
        Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
        Pos_Word_Rate = percent(Tot_Pos_Words / Total_Words),
        Neg_Word_Rate = percent(Tot_Neg_Words / Total_Words),
        Num_Reviews = n(),
        Avg_Words_Per_Review = format(Total_Words / Num_Reviews, digits = 4)
    )

kable(as.data.frame(review_range))

p1 <- ggplot(review_range, aes(
    x = Review_Range,
    y = Num_Reviews,
    fill = Review_Range
)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(label = review_range$Num_Reviews, nudge_y = 0.5) +
    ggtitle(label = "Number of Reviews by Range of Score") +
    labs(xlab("Review Range"), ylab("Number of Reviews")) +
    theme(legend.position = "off")

g1 <- ggplotGrob(p1)

grid.draw(g1)

p2 <- ggplot(review_range, aes(
    x = Review_Range,
    y = Pos_Word_Rate,
    fill = Review_Range
)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(label = review_range$Pos_Word_Rate, nudge_y = 0.2) +
    ggtitle(label = "Percent Positive Words by Range of Score") +
    labs(xlab("Review Range"), ylab("Percent(%) Positive Words")) +
    theme(legend.position = "bottom")

g2 <- ggplotGrob(p2)

grid.draw(g2)

p3 <- ggplot(review_range, aes(
    x = Review_Range,
    y = Avg_Words_Per_Review,
    fill = Review_Range
)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(label = review_range$Avg_Words_Per_Review, nudge_y = 0.2) +
    ggtitle(label = "Average Words per Review by Range of Score") +
    labs(xlab("Review Range"), ylab("Avg. Words per Review")) +
    theme(legend.position = "off")

g3 <- ggplotGrob(p3)


grid.draw(g3)

grid.draw(rbind(g1, g3, g2, size = "last"))


