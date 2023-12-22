library(rvest)
library(httr)
library(dplyr) 
library(polite)

library(kableExtra)

polite::use_manners(save_as = 'polite_scrape.R')


url <- 'https://www.airlinequality.com/airline-reviews/airasia/'
session <- bow(url,
               user_agent = "Educational")




title <- character(0)


titles_list <- scrape(session) %>%
  html_nodes('h3.text_sub_header') %>% 
  html_text

titles_list_sub <- as.data.frame(titles_list[1:10])
colnames(titles_list_sub) <- "ranks"


split_df <- strsplit((titles_list_sub$ranks),"\r\n","reviews",fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))

title<- data.frame(
  title= split_df)

title<-title[,c(-1,-2,-3)]
split_df <- strsplit((title),"(",fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))
name<-as.data.frame(split_df[,1])

split_df <- strsplit((split_df$X2),")",fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))
num1<-cbind(name,split_df)



xname<-array(2:30)
xone<-array(2:30)
xtwo<-array(2:30)

for (i in 2:30) {
  url<-paste("https://www.airlinequality.com/airline-reviews/airasia/page/",i,"/", sep = "")

  session <- bow(url,
                 user_agent = "Educational")
  
  
  title <- character(0)
  
  
  titles_list <- scrape(session) %>%
    html_nodes('h3.text_sub_header') %>% 
    html_text
  
  titles_list_sub <- as.data.frame(titles_list[1:10])
  colnames(titles_list_sub) <- "ranks"
  
  
  split_df <- strsplit((titles_list_sub$ranks),"\r\n","reviews",fixed = TRUE)
  split_df <- data.frame(do.call(rbind,split_df))
  
  title<- data.frame(
    title= split_df)
  title
  title<-title[,c(-1,-2,-3)]
  title
  split_df<- strsplit((title),"(",fixed = TRUE)
  split_df<- data.frame(do.call(rbind,split_df))

  name<-as.data.frame(split_df[,1])

  split_df<- strsplit((split_df$X2),")",fixed = TRUE)
  split_df<- data.frame(do.call(rbind,split_df))

  xname[i]<-name
  xone[i]<-as.data.frame(split_df$X1)
  xtwo[i]<-as.data.frame(split_df$X2)
}
for (i in 2:30){
  df1<-data.frame(
    data=c(xone[i])
  ) 
  colnames(df1)<-"data"
  
  if(i==2){
    resc<-df1
  }
  else if(i>2){
    resc<-rbind(resc,df1)
  }
}

for (i in 2:30){
  df1<-data.frame(
    data=c(xname[i])
  ) 
  colnames(df1)<-"data"
  
  if(i==2){
    resn<-df1
  }
  else if(i>2){
    resn<-rbind(resn,df1)
  }
}

for (i in 2:30){
  df1<-data.frame(
    data=c(xtwo[i])
  ) 
  colnames(df1)<-"data"
  
  if(i==2){
    resd<-df1
  }
  else if(i>2){
    resd<-rbind(resd,df1)
  }
}
num2<-cbind(resn,resc,resd)
colnames(num1)<-c("Name","Country","Date")
colnames(num2)<-c("Name","Country","Date")
part3<-rbind(as.data.frame(num1),as.data.frame(num2))

#-----------------------------------------------------------------------
name <- character(0)

title_list <- scrape(session) %>%
  html_nodes('td.review-value') %>% 
  html_text
title_list_sub <- as.data.frame(title_list[])


name<- title_list_sub
colnames(name)<-c("Name")

split_df <- strsplit((name[,1]),"|", fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))

colnames(split_df)<-"details"

split_df<-subset(split_df, details!="A320")
b1y5<-seq(1,50,by=5)
g1<-as.data.frame(split_df[b1y5,])
b2y5<-seq(2,50,by=5)
g2<-as.data.frame(split_df[b2y5,])

b3y5<-seq(3,50,by=5)
g3<-as.data.frame(split_df[b3y5,])



b4y5<-seq(4,50,by=5)
g4<-as.data.frame(split_df[b4y5,])
b5y5<-seq(5,50,by=5)
g5<-as.data.frame(split_df[b5y5,])

group1<-cbind(g1,g2,g3,g4,g5)

gc1<-array(2:30)
gc2<-array(2:30)
gc3<-array(2:30)
gc4<-array(2:30)
gc5<-array(2:30)

# Example for loop
for (i in 2:30) {
  url<-paste("https://www.airlinequality.com/airline-reviews/airasia/page/",i,"/", sep = "")

  session <- bow(url,
                 user_agent = "Educational")
  
  
  group<- character(0)
  
  
  group_list <- scrape(session) %>%
    html_nodes('td.review-value') %>% 
    html_text
  
  group_list_sub <- as.data.frame(group_list[])
  colnames(group_list_sub) <- "details"
  
  split_df<-subset(group_list_sub, details!="A320" & details!="A320-200" & details!="Airbu" & details!="A320 Neo" & details!="A330" & details!="A330-300" & details!="A330 Neo" & details!="a320" & details!="AirAsia airline") 
  b1y5<-seq(1,50,by=5)
  g1<-as.data.frame(split_df[b1y5,])
  colnames(g1)<-c("TypeofTraveller")
  gc1[i]<-g1
  
  b2y5<-seq(2,50,by=5)
  g2<-as.data.frame(split_df[b2y5,])
  colnames(g2)<-c("SeatType")
  gc2[i]<-g2
  
  b3y5<-seq(3,50,by=5)
  gc3[i]<-as.data.frame(split_df[b3y5,])
  
  
  b4y5<-seq(4,50,by=5)
  g4<-as.data.frame(split_df[b4y5,])
  gc4[i]<-g4
  
  b5y5<-seq(5,50,by=5)
  g5<-as.data.frame(split_df[b5y5,])
  gc5[i]<-g5
  
  
}
for (i in 2:30){
  df1<-data.frame(
    data=c(gc1[i])
  ) 
  colnames(df1)<-"TypeofTraveller"
  
  if(i==2){
    restot<-df1
  }
  else if(i>2){
    restot<-rbind(restot,df1)
  }
}
for (i in 2:30){
  df2<-data.frame(
    data=c(gc2[i])
  ) 
  colnames(df2)<-"SeatType"
  
  if(i==2){
    resst<-df2
  }
  else if(i>2){
    resst<-rbind(resst,df2)
  }
}
for (i in 2:30){
  df3<-data.frame(
    data=c(gc3[i])
  ) 
  colnames(df3)<-"Route"
  
  if(i==2){
    resroute<-df3
  }
  else if(i>2){
    resroute<-rbind(resroute,df3)
  }
}
for (i in 2:30){
  df4<-data.frame(
    data=c(gc4[i])
  ) 
  colnames(df4)<-"DateFlown"
  
  if(i==2){
    resd<-df4
  }
  else if(i>2){
    resd<-rbind(resd,df4)
  }
}
for (i in 2:30){
  df5<-data.frame(
    data=c(gc5[i])
  ) 
  colnames(df5)<-"Recomended"
  
  if(i==2){
    resrec<-df5
  }
  else if(i>2){
    resrec<-rbind(resrec,df5)
  }
}

group2<-cbind(restot,resst,resroute,resd,resrec)
colnames(group1)<-c("TypeofTraveller","SeatType","Route","DateFlown","Recommended")
colnames(group2)<-c("TypeofTraveller","SeatType","Route","DateFlown","Recommended")
part4<-rbind(group1,group2)

#---------------------------------------------------------------------------------
title <- character(0)


titles_list <- scrape(session) %>%
  html_nodes('h2.text_header') %>% 
  html_text

titles_list_sub <- as.data.frame(titles_list[1:10])
colnames(titles_list_sub) <- "Title"


title<- data.frame(
  title= titles_list_sub)

tilt<-as.data.frame(title)
split_df <- strsplit((tilt[,1]),"\"", fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))
tilt<-split_df
tilt<-tilt[,-1]
tilt<-as.data.frame(tilt)

xtitle<-array(2:30)


for (i in 2:30) {
  url<-paste("https://www.airlinequality.com/airline-reviews/airasia/page/",i,"/", sep = "")

  session <- bow(url,
                 user_agent = "Educational")
  
  
  title <- character(0)
  
  
  titles_list <- scrape(session) %>%
    html_nodes('h2.text_header') %>% 
    html_text
  
  titles_list_sub <- as.data.frame(titles_list[1:10])
  colnames(titles_list_sub) <- "Title"
  
  title<- data.frame(
    title= titles_list_sub)
  
  title<-as.data.frame(title)

  xtitle[i]<-as.data.frame(title[,1])
  
}
for (i in 2:30){
  df1<-data.frame(
    data=c(xtitle[i])
  ) 
  colnames(df1)<-"Title"
  
  if(i==2){
    rest<-df1
  }
  else if(i>2){
    rest<-rbind(rest,df1)
  }
}
colnames(tilt)<-"Title"
part5<-rbind(tilt,rest)

#-------------------------------------------------------------------------------

textrev<- character(0)

treviews <- scrape(session) %>%
  html_nodes('div.text_content') %>% 
  html_text
treviews_sub <- as.data.frame(treviews[1:10])


textrev<- treviews_sub


split_df <- strsplit((textrev[,1]),"|", fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))



reviews1<-as.data.frame(split_df$X2)



reviews2<-array(2:30)
for (i in 2:30) {
  
  url<-paste("https://www.airlinequality.com/airline-reviews/airasia/page/",i,"/", sep = "")
  
  session <- bow(url,
                 user_agent = "Educational")
  
  
  textrev2 <- character(0)
  
  
  treviews2<- scrape(session) %>%
    html_nodes('div.text_content') %>% 
    html_text
  treviews_sub2 <- as.data.frame(treviews2[1:10])
  
  textrev2<- treviews_sub2

  
  split_df <- strsplit((textrev2[,1]),"|", fixed = TRUE)
  split_df <- data.frame(do.call(rbind,split_df))

  r1<-split_df
  reviews2[i]<-as.data.frame(split_df$X2)
  
}

for (i in 2:30){
  dr1<-data.frame(
    data=c(reviews2[i])
  ) 
  colnames(dr1)<-"Reviews"
  
  if(i==2){
    resreview<-dr1
  }
  else if(i>2){
    resreview<-rbind(resreview,dr1)
  }
}
reviews1<-as.data.frame(reviews1)
colnames(reviews1)<-"Reviews"
resreview<-as.data.frame(resreview)
colnames(resreview)<-"Reviews"
part6<-rbind(reviews1,resreview)
finaloutput<-cbind(part3,part4,part5,part6)
finaloutput
#---------------------------------------------------------------------
summary(finaloutput)
library(ggplot2)
ggplot(finaloutput, aes(x =name, y = country),colors=class) + geom_point()
# Malaysia has the most number of passengers that review the Airasia.
library(ggplot2)

library(dplyr)

groupofcountry<- finaloutput %>%
  group_by(country) %>%
  summarise(count=n())

ggplot(groupofcountry, aes(x = country, y = count, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Number Reviews per Country",
       x = "Country",
       y = "Number of Reviews") +
  scale_fill_hue()+ guides(fill=FALSE)

recommendedfactor<-factor(finaloutput$recommended)
barcolors <- c("red", "green")
plot(recommendedfactor,col=c(barcolors),main="Passenger Recommendations for Air Asia",xlab = "Recommendation", ylab = "Number of Reviews")
legend("topright", legend = levels(recommendedfactor), fill = barcolors)  
#Passenger's recommendation reviews to Air Asia Airline. Most passengers prefer/voted to not recommend the airline.
#--------------------------------------------------------
library(dplyr)

groupoftraveller<- finaloutput %>%
  group_by(typeoftraveller) %>%
  summarise(count=n())

colors <- c("maroon", "pink", "skyblue", "purple")


pie(groupoftraveller$count, labels = groupoftraveller$count, col = colors, main = "Traveller Types Distribution on Air Asia")
legend(x = 1.2, y = 1.2,cex = 0.8, legend = groupoftraveller$typeoftraveller, fill = colors, title = "Type of Travellers")

#The analysis of passenger data for Air Asia reveals a diverse distribution of traveler types, with 30 business travelers, 70 couples on leisure trips, 86 family leisure , and 114 solo leisure travelers.

#--------------------------------------------------------
library(wordcloud)
library(tm)


reviewCorpus <- Corpus(VectorSource(finaloutput$review))

reviewCorpus <- tm_map(reviewCorpus, content_transformer(tolower))
reviewCorpus <- tm_map(reviewCorpus, removePunctuation)
reviewCorpus <- tm_map(reviewCorpus, removeNumbers)
reviewCorpus <- tm_map(reviewCorpus, removeWords, stopwords("english"))
reviewCorpus <- tm_map(reviewCorpus, stripWhitespace)
reviewCorpus <- tm_map(reviewCorpus, removeWords, c("the","just","even","will","my","kg", "us", "said", "via"))

str(reviewCorpus)
pal <- brewer.pal(9, "Reds")
pal<- pal[-(1:3)]

set.seed(1234)
wordcloud(words = reviewCorpus, min.freq = 1, scale = c(2, 0.2), max.words = 100, random.order = FALSE, rot.per = 0.35, colors = pal)
#The most word that has been spotted in the comment reviewsis the word flight followed by the airasia,service,refund,airline,time,get,flights, and customer.
#-----------------------------------------------------

library(syuzhet)
library(SentimentAnalysis)
library(ggplot2)
library(SnowballC)

reviewP <- data.frame(text = sapply (reviewCorpus, as.character), stringsAsFactors= FALSE)

reviewSentiments <- get_sentiment (reviewP$text,method = "syuzhet")

reviewsen <- cbind(reviewP, reviewSentiments)

#positive and negative sentiments
reviewsen$sentiment <- ifelse(reviewsen$reviewSentiments <= -0.5, "1) very negative",
                              ifelse(reviewsen$reviewSentiments > -0.5 & reviewsen$reviewSentiments < 0, "2) negative",
                                     ifelse(reviewsen$reviewSentiments == 0, "3) neutral",
                                            ifelse(reviewsen$reviewSentiments > 0 & reviewsen$reviewSentiments < 0.5, "4) positive",
                                                   ifelse(reviewsen$reviewSentiments >= 0.5, "5) very positive", NA)))))
df <- head(reviewsen, n = 10)
class(reviewsen)

sentiment_counts <- table(reviewsen$sentiment)


ggplot(reviewsen, aes(x = sentiment, fill = sentiment)) +
geom_bar() +
  labs(title = "Air Asia Customer Sentiments",
       x = "Sentiment",
       y = "Count",
       subtitle = paste("Total Counts: Very Negative =", sentiment_counts["1) very negative"],
                        "| Negative =", sentiment_counts["2) negative"],
                        "| Neutral =", sentiment_counts["3) neutral"],
                        "| Positive =", sentiment_counts["4) positive"],
                        "| Very Positive =", sentiment_counts["5) very positive"])) +
  scale_fill_manual(values = c("1) very negative" = "darkred", 
                               "2) negative" = "red", 
                               "3) neutral" = "yellow",
                               "4) positive" = "lightgreen", 
                               "5) very positive" = "darkgreen")) +
  scale_y_continuous(breaks = seq(0, 150, 25)) +
  theme_minimal()
#There are more very positive sentiments with the number of 136 found in AirAsia followed by the very negative=107,negative=37,positive=18,neutral=2.

#---------------------------------------
library(ggplot2)
library(viridis)

ggplot(data = finaloutput, aes(x = date)) +
  geom_bar(aes(fill = ..count..), stat = "count") +
  theme(legend.position = "none") +
  xlab("Date") + ylab("Number of Reviews") +
  scale_fill_viridis_c()

md<-max(dateof$count)
subset(dateof,count==md)
subset(dateof,count==4)
subset(dateof,count==3)
subset(dateof,count==2)
subset(dateof,count==1)

#The maximum number of the number of reviews per date is in the date of September 26, 2023 that has 6 reviews, followed by 4 reviews in date of February 18,2020, 5counts of 3 reviews per date,30counts of 2 reviews per date, and 215counts per 1 review per date in the remaining dates. 
