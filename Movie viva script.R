# Viva Script For Movie Dataset
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)


movies=read.csv('D:\\Data Science\\R programming\\Viva dataset and questions\\tmdb_5000_movies.csv'
                ,na.strings = "")
View(movies)
dim(movies)
# If we want class of every column we have three methods
# Method 1
str(movies)
# or Method 2
glimpse(movies)
# or Method 3
sapply(movies,class)


# Q-1 Discuss steps to load data from specific columns like "id", "budget", 
# "original_language", "original_title", "overview", "popularity", "status", 
# "title", "vote_average", "vote_count"
movie=movies[,c(1,4,6,7,8,9,16,18,19,20)]
# head(movie)

# Q-2 Discuss pseudo code to plot most expensive (budget wise) top 10 movies  
# Select the columns which we have to plot
budget=movie[,c(1,4)]
# head(budget)
# Order the data budgetwise in descending order
budget=budget[rev(order(movie$budget)),]
budget=head(budget,10)
# Take top 10 high budget movies
# Plot them
ggplot(data.frame(budget),aes(x=reorder(original_title,budget),y=budget))+
  geom_bar(stat="identity", width=.6,fill="orange") +
  labs(title="Top 10 expensive movies ",y="budget",x="movie title")+
  theme(axis.text.x = element_text(angle=90,hjust=1))


# Q-3 Discuss steps to print list of movies which were produced by 
# "Columbia Pictures"  and "Walt Disney Picture"
# Method 1
patt=str_extract(movies$production_companies,c("Columbia Pictures","Walt Disney Pictures"))
title=movies$title
df=data.frame(title,patt)
head(df)
table=filter(df,patt=="Columbia Pictures" | patt=="Walt Disney Pictures")
head(table)
# Method 2
pattern="(Walt Disney Pictures)|(Columbia Pictures)"
index=which(str_detect(movies$production_companies,pattern)==TRUE)
mov=which(colnames(movies)=="title")
movies[index,mov]
# It gives us the list of movies which are Produced by Columbia Pictures and Walt Disney Pictures

# Q-4 Explain command to draw boxplot of month Average votes received by Month 
movies$year <- year(ymd(movies$release_date))
movies$month <- month(ymd(movies$release_date)) 
movies$day <- day(ymd(movies$release_date))
head(movies)
dim(movies)

df=data.frame(movies$vote_average,movies$month)
boxplot(movies$vote_average~movies$month,data=movies,main="Month average votes",
        xlab="month",ylab="Average votes",col="orange",border="brown")
# The average votes recieved by all months is near about 6. 

# Q-5 Command to plot movies with highest popularity. 
m_pop= movies[order(-movies$popularity),] %>% head(n=12)
m_pop[,c(7,9)]
ggplot(m_pop,aes(x=reorder(title,popularity),y=popularity)) +
  geom_bar(stat="identity", width=.6,fill="dark red") +
  labs(title="Top 12 popular movie ",y="popularity",x="movie title")+
  theme(axis.text.x = element_text(angle=90,hjust=1))

# Q-6 Steps to print list of movies which has no tag lines.  
t=movies$tagline
no_tag=movies$title[is.na(t)]
head(no_tag)
# Q-7 Which year has seen maximum release of movies? Discuss appropriate 
# visualization to display that information. 
table=table(movies$year)
barplot(table,col="red",xlab="year",ylab="No. of movies",
        main="Max. Release Of Movies",border="dark blue")


which.max(table)
# Here we can see in 2009 maximum release of movies

# Q-8 Which Month has minimum release? Discuss the appropriate 
# visualization to be used to display the information.
month=table(movies$month)
barplot(month,col="yellow",xlab="Month",ylab="No. of movies",
        main="Min. Release Of Movies",border="dark blue")
which.min(month)
# Here we can see there are minimum releases in the month of february

#Trend analysis of movies over the years
movies %>%
  group_by(year) %>%
  summarise(movie_count = n()) %>%
  filter(movie_count >= 10)  %>%
  ggplot(aes(x = year, y = movie_count)) +
  geom_line(col="blue") +
  geom_point(col="red") +
  xlab("Year") +
  ylab("Number of Movies") +
  theme_classic()
# Analysis of movies over the years is shown using line plot. Year 2009 saw the 
# highest number with 247 movies. It is seen that number of movies steadily 
# increased till year 1999, after which there have been ups and downs. As the data 
#set contains information about movies till 2016, We see a sharp decline in number 
#of movies after 2015.

#top 10 movies with highest rating
movies %>%
  filter(vote_count >= 1000) %>%
  select(title, vote_average) %>%
  arrange(desc(vote_average)) %>%
  head(n = 10) %>%
  ggplot(aes(x = reorder(title, -vote_average), y = vote_average)) +
  geom_bar(stat = "identity",fill="orange",col="purple") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 highest rated movies of all time") +
  xlab("Movie Title") +
  ylab("Average Vote")
# Bar chart not suitable for this , use line graph
# Top 10 movies with highest rating. Only movies which have more than 1000 vote 
#counts have been considered.

#vote_average and vote_count over the years
movies %>%
  filter(vote_count >= 1000) %>%
  group_by(year) %>%
  summarise(avg_vote = mean(vote_average)) %>%
  ggplot(aes(x = year, y = avg_vote)) +
  geom_point(col="red") +
  geom_smooth(stat = "smooth", position = "identity",col="dark blue") +
  xlab("Year") +
  ylab("Average Vote")
# Average vote of movies was analysed over the years and only movies which had more 
# than 1000 votes were included in the plot. It is observed that average vote of 
#movies was highest in mid 1960s, after which it has steadily declined.

# Original language was used for the analysis.
#vote_average vs original_language
movies %>%
  filter(vote_count >= 100) %>%
  ggplot(aes(x = original_language, y = vote_average)) +
  geom_boxplot(col="orange")
# A look of how average vote varies across languages. 

#Run time over the years
movies %>%
  group_by(year) %>%
  summarise(avg_runtime = mean(runtime)) %>%
  ggplot(aes(x = year, y = avg_runtime)) +
  geom_point(col="dark blue") +
  xlab("Year") +
  ylab("Average Runtime")
#Average runtime of the movies over the years was analysed using scatter plot. 
# We see that average runtime has come down over the years. after 1970, the average 
#runtime has remained in the range of 100 to 125 minutes.

#runtime vs original_language
# There are two methods-
# Method 1- Using boxplot
movies %>%
  ggplot(aes(x = original_language, y = runtime)) +
  geom_boxplot(col="dark green") +
  xlab("Original Language") +
  ylab("Runtime")

# Method 2- Using Barplot
movies %>%
  ggplot(aes(x = original_language, y = runtime)) +
  stat_summary(fun.y = mean, geom = "bar",col="dark red",fill="dark blue")
#Analysis of runtime across different languages is shown using boxplot and bar chart. 
# The column 'original_language' was used for the analysis. Movies made in English 
# language have an average runtime of around 110 minutes and several outliers were 
# observed. Interestingly, the longest and shortest movies in the data set are also 
# made in English language originally. Among the seven languages, which have average
# runtime of more than 125 minutes, most of them are Asian languages. in fact, two 
# Indian languages - Telugu and Tamil, have average runtime of more than 150 minutes



# By using line graph
#top 10 movies with highest rating
movies %>%
  filter(vote_count >= 1000) %>%
  select(title, vote_average) %>%
  arrange(desc(vote_average)) %>%
  head(n = 10) %>%
  ggplot(aes(x=title,y=vote_average),xlab("Title of movies"),ylab("Vote_average"))+
  geom_point(col="green")+
  geom_smooth(position="identity",col="orange")



