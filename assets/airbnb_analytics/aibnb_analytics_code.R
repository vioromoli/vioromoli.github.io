# This file contains code for Sentiment Analysis, Topic Modelling, Seasonal Topic Modelling and Regression Analysis

airbnb2 <- read.csv(file="reviews.csv", header= TRUE)

nrow(airbnb2) 
range(airbnb2$date)
airbnb2$date <- as.Date(airbnb2$date)
nrow(airbnb2[airbnb2$date >= as.Date("2024-09-17"), ])

airbnb2 <- airbnb2[airbnb2$date >= as.Date("2024-09-17"), ]
range(airbnb2$date)
nrow(airbnb2) 

install.packages("cld2")  # Google's Compact Language Detector
library(cld2)

airbnb2$language <- detect_language(airbnb2$comments)

airbnb2 <- airbnb2[!is.na(airbnb2$language) & airbnb2$language == "en", ]

# data cleaning for sentiment analysis

airbnb2$comments <- tolower(airbnb2$comments)
airbnb2$comments <- gsub("http\\S+|www\\.\\S+", " ", airbnb2$comments)

#removing punctuations

airbnb2$comments <- gsub("[[:punct:]]", " ", airbnb2$comments)

#removing numbers

airbnb2$comments <- gsub("[[:digit:]]", " ", airbnb2$comments)

# removing non-english characters

airbnb2 <- airbnb2[which(!grepl("[^\x01-\x7F]+", airbnb2$comments)),]
airbnb2$comments <- gsub("\\s+", " ", airbnb2$comments)
airbnb2$comments <- trimws(airbnb2$comments)
airbnb2 <- airbnb2[!is.na(airbnb2$comments), ]

nrow(airbnb2) 

# Sentiment Analysis

install.packages(c("sentimentr","stm"))
library(sentimentr)

senti <- sentiment_by(airbnb2$comments) 
airbnb2$sentiment_score <- senti$ave_sentiment
summary(airbnb2$sentiment_score)

# Topic Modelling

install.packages("stm")
library(stm)

install.packages("tm")
library(tm)

# Create season column

airbnb2$month  <- as.integer(format(airbnb2$date, "%m"))
airbnb2$season <- ifelse(airbnb2$month %in% c(12,1,2),  "Winter",
                         ifelse(airbnb2$month %in% c(3,4,5),   "Spring",
                                ifelse(airbnb2$month %in% c(6,7,8),   "Summer", "Autumn")))
library(ggplot2)

#WINTER
sub_winter <- airbnb2[airbnb2$season == 'Winter', ]
processed_winter <- textProcessor(sub_winter$comments, metadata=sub_winter)
out_Winter <- prepDocuments(processed_winter$documents, processed_winter$vocab, processed_winter$meta)
docs_winter <- out_Winter$documents
vocab_winter <- out_Winter$vocab
meta_winter <- out_Winter$meta

tm_Winter <- stm(out_Winter$documents, out_Winter$vocab, K=8, prevalence=~sentiment_score, 
            max.em.its=75, data=out_Winter$meta, init.type="Spectral", 
            seed=3559)

#SPRING
sub_spring <- airbnb2[airbnb2$season == 'Spring', ]
processed_spring <- textProcessor(sub_spring$comments, metadata=sub_spring)
out_Spring <- prepDocuments(processed_spring$documents, processed_spring$vocab, processed_spring$meta)
docs_spring <- out_Spring$documents
vocab_spring <- out_Spring$vocab
meta_spring <- out_Spring$meta

tm_Spring <- stm(out_Spring$documents, out_Spring$vocab, K=8, prevalence=~sentiment_score, 
                 max.em.its=75, data=out_Spring$meta, init.type="Spectral", 
                 seed=3559)

#SUMMER
sub_summer <- airbnb2[airbnb2$season == 'Summer', ]
processed_summer <- textProcessor(sub_summer$comments, metadata=sub_summer)
out_Summer <- prepDocuments(processed_summer$documents, processed_summer$vocab, processed_summer$meta)
docs_summer <- out_Summer$documents
vocab_summer <- out_Summer$vocab
meta_summer <- out_Summer$meta

tm_Summer <- stm(out_Summer$documents, out_Summer$vocab, K=8, prevalence=~sentiment_score, 
                 max.em.its=75, data=out_Summer$meta, init.type="Spectral", 
                 seed=3559)

#AUTUMN
sub_autumn <- airbnb2[airbnb2$season == 'Autumn', ]
processed_autumn <- textProcessor(sub_autumn$comments, metadata=sub_autumn)
out_Autumn <- prepDocuments(processed_autumn$documents, processed_autumn$vocab, processed_autumn$meta)
docs_autumn <- out_Autumn$documents
vocab_autumn <- out_Autumn$vocab
meta_autumn <- out_Autumn$meta

tm_Autumn <- stm(out_Autumn$documents, out_Autumn$vocab, K=8, prevalence=~sentiment_score, 
                 max.em.its=75, data=out_Autumn$meta, init.type="Spectral", 
                 seed=3559)


# Compare topics by season

plot(tm_Winter,  type="summary", main="Winter")
plot(tm_Spring,  type="summary", main="Spring")
plot(tm_Summer,  type="summary", main="Summer")
plot(tm_Autumn,    type="summary", main="Autumn")# Crea colonna stagione

# plot the top words in each topic

plot(tm_Winter, type="labels", topics=c(1,2,3,4,5,6,7,8),text.cex=0.4)
plot(tm_Spring, type="labels", topics=c(1,2,3,4,5,6,7,8),text.cex=0.4)
plot(tm_Summer, type="labels", topics=c(1,2,3,4,5,6,7,8),text.cex=0.4)
plot(tm_Autumn, type="labels", topics=c(1,2,3,4,5,6,7,8),text.cex=0.4)



##########################################
## Topic Modelling (II) - Advanced Analysis ##

# Identified eight topics the Airbnb London customers talked about between Sept 17, 2024 to Sept 17, 2025. 
# Research question 1:
# What do Airbnb London customers care about (in the given period)?  

#WINTER
findThoughts(tm_Winter, texts = as.character(meta_winter$comments),
             n = 2, topics = 1)$docs[[1]]
findThoughts(tm_Winter, texts = as.character(meta_winter$comments),
             n = 2, topics = 2)$docs[[1]]
findThoughts(tm_Winter, texts = as.character(meta_winter$comments),
             n = 2, topics = 3)$docs[[1]]
findThoughts(tm_Winter, texts = as.character(meta_winter$comments),
             n = 2, topics = 4)$docs[[1]]
findThoughts(tm_Winter, texts = as.character(meta_winter$comments),
             n = 2, topics = 5)$docs[[1]]
findThoughts(tm_Winter, texts = as.character(meta_winter$comments),
             n = 2, topics = 6)$docs[[1]]
findThoughts(tm_Winter, texts = as.character(meta_winter$comments),
             n = 2, topics = 7)$docs[[1]]
findThoughts(tm_Winter, texts = as.character(meta_winter$comments),
             n = 2, topics = 8)$docs[[1]]

#FALL
findThoughts(tm_Autumn, texts = as.character(meta_autumn$comments),
             n = 2, topics = 1)$docs[[1]]
findThoughts(tm_Autumn, texts = as.character(meta_autumn$comments),
             n = 2, topics = 2)$docs[[1]]
findThoughts(tm_Autumn, texts = as.character(meta_autumn$comments),
             n = 2, topics = 3)$docs[[1]]
findThoughts(tm_Autumn, texts = as.character(meta_autumn$comments),
             n = 2, topics = 4)$docs[[1]]
findThoughts(tm_Autumn, texts = as.character(meta_autumn$comments),
             n = 2, topics = 5)$docs[[1]]
findThoughts(tm_Autumn, texts = as.character(meta_autumn$comments),
             n = 2, topics = 6)$docs[[1]]
findThoughts(tm_Autumn, texts = as.character(meta_autumn$comments),
             n = 2, topics = 7)$docs[[1]]
findThoughts(tm_Autumn, texts = as.character(meta_autumn$comments),
             n = 2, topics = 8)$docs[[1]]


#SUMMER
findThoughts(tm_Summer, texts = as.character(meta_summer$comments),
             n = 2, topics = 1)$docs[[1]]
findThoughts(tm_Summer, texts = as.character(meta_summer$comments),
             n = 2, topics = 2)$docs[[1]]
findThoughts(tm_Summer, texts = as.character(meta_summer$comments),
             n = 2, topics = 3)$docs[[1]]
findThoughts(tm_Summer, texts = as.character(meta_summer$comments),
             n = 2, topics = 4)$docs[[1]]
findThoughts(tm_Summer, texts = as.character(meta_summer$comments),
             n = 2, topics = 5)$docs[[1]]
findThoughts(tm_Summer, texts = as.character(meta_summer$comments),
             n = 2, topics = 6)$docs[[1]]
findThoughts(tm_Summer, texts = as.character(meta_summer$comments),
             n = 2, topics = 7)$docs[[1]]
findThoughts(tm_Summer, texts = as.character(meta_summer$comments),
             n = 2, topics = 8)$docs[[1]]


#SPRING
findThoughts(tm_Spring, texts = as.character(meta_spring$comments),
             n = 2, topics = 1)$docs[[1]]
findThoughts(tm_Spring, texts = as.character(meta_spring$comments),
             n = 2, topics = 2)$docs[[1]]
findThoughts(tm_Spring, texts = as.character(meta_spring$comments),
             n = 2, topics = 3)$docs[[1]]
findThoughts(tm_Spring, texts = as.character(meta_spring$comments),
             n = 2, topics = 4)$docs[[1]]
findThoughts(tm_Spring, texts = as.character(meta_spring$comments),
             n = 2, topics = 5)$docs[[1]]
findThoughts(tm_Spring, texts = as.character(meta_spring$comments),
             n = 2, topics = 6)$docs[[1]]
findThoughts(tm_Spring, texts = as.character(meta_spring$comments),
             n = 2, topics = 7)$docs[[1]]
findThoughts(tm_Spring, texts = as.character(meta_spring$comments),
             n = 2, topics = 8)$docs[[1]]


# Get the proportions of topics

proportion_summer <- as.data.frame(colSums(tm_Summer$theta/nrow(tm_Summer$theta)))
print(proportion_summer)
proportion_spring <- as.data.frame(colSums(tm_Spring$theta/nrow(tm_Spring$theta)))
print(proportion_spring)
proportion_winter <- as.data.frame(colSums(tm_Winter$theta/nrow(tm_Winter$theta)))
print(proportion_winter)
proportion_autumn <- as.data.frame(colSums(tm_Autumn$theta/nrow(tm_Autumn$theta)))
print(proportion_autumn)

# Research Question 2: 
# What topics are more negative? What topics are more positive?

prep_summer <- estimateEffect(1:8 ~sentiment_score, tm_Summer,
                              meta = out_Summer$meta, uncertainty = "Global")
prep_spring <- estimateEffect(1:8 ~sentiment_score, tm_Spring,
                              meta = out_Spring$meta, uncertainty = "Global")
prep_autumn <- estimateEffect(1:8 ~sentiment_score, tm_Autumn,
                              meta = out_Autumn$meta, uncertainty = "Global")
prep_winter <- estimateEffect(1:8 ~sentiment_score, tm_Winter,
                              meta = out_Winter$meta, uncertainty = "Global")

plot(prep_winter, covariate = "sentiment_score", topics = c(1,2,3,4,5,6, 7,8),
     model = tm_Winter, method = "difference",
     cov.value1 = "Postive", cov.value2 = "Negative",
     xlab = "More Postive ... More Negative",
     main = "Effect of content sentiment",
     xlim = c(.15, -.20), labeltype = "custom", 
     custom.labels = c('Topic 1', 'Topic 2','Topic 3', 'Topic 4', 
                       'Topic 5','Topic 6','Topic 7','Topic 8'))
plot(prep_summer, covariate = "sentiment_score", topics = c(1,2,3,4,5,6, 7,8),
     model = tm_Summer, method = "difference",
     cov.value1 = "Postive", cov.value2 = "Negative",
     xlab = "More Postive ... More Negative",
     main = "Effect of content sentiment",
     xlim = c(.15, -.20), labeltype = "custom", 
     custom.labels = c('Topic 1', 'Topic 2','Topic 3', 'Topic 4', 
                       'Topic 5','Topic 6','Topic 7','Topic 8'))
plot(prep_autumn, covariate = "sentiment_score", topics = c(1,2,3,4,5,6, 7,8),
     model = tm_Autumn, method = "difference",
     cov.value1 = "Postive", cov.value2 = "Negative",
     xlab = "More Postive ... More Negative",
     main = "Effect of content sentiment",
     xlim = c(.15, -.20), labeltype = "custom", 
     custom.labels = c('Topic 1', 'Topic 2','Topic 3', 'Topic 4', 
                       'Topic 5','Topic 6','Topic 7','Topic 8'))
plot(prep_spring, covariate = "sentiment_score", topics = c(1,2,3,4,5,6, 7,8),
     model = tm_Spring, method = "difference",
     cov.value1 = "Postive", cov.value2 = "Negative",
     xlab = "More Postive ... More Negative",
     main = "Effect of content sentiment",
     xlim = c(.15, -.22), labeltype = "custom", 
     custom.labels = c('Topic 1', 'Topic 2','Topic 3', 'Topic 4', 
                       'Topic 5','Topic 6','Topic 7','Topic 8'))

# Research Question 3: 
# How the topic changes over time?

par(mar = c(5, 4, 4, 10), xpd = TRUE) 

plot(prep_winter, "date_rank", method = "continuous", topics = c(1,2,3,4,5,6,7,8), 
     printlegend = FALSE, 
     ylab = "Topic proportion over time", xlab = "time",
     main = "Topic proportion over time")

legend("topright", 
       inset = c(-0.35, 0), 
       legend = c('Topic 1', 'Topic 2', 'Topic 3', 'Topic 4',
                  'Topic 5', 'Topic 6', 'Topic 7', 'Topic 8'),
       lty = 1:8,   
       col = 1:8,   
       cex = 0.8,
       bty = "n") 

par(mar = c(5, 4, 4, 2), xpd = FALSE)

par(mar = c(5, 4, 4, 10), xpd = TRUE) 

plot(prep_autumn, "date_rank", method = "continuous", topics = c(1,2,3,4,5,6,7,8), 
     printlegend = FALSE,
     ylab = "Topic proportion over time", xlab = "time",
     main = "Topic proportion over time")

legend("topright", 
       inset = c(-0.35, 0),
       legend = c('Topic 1', 'Topic 2', 'Topic 3', 'Topic 4',
                  'Topic 5', 'Topic 6', 'Topic 7', 'Topic 8'),
       lty = 1:8,   
       col = 1:8,   
       cex = 0.8,
       bty = "n")  

par(mar = c(5, 4, 4, 2), xpd = FALSE)

par(mar = c(5, 4, 4, 10), xpd = TRUE)

plot(prep_summer, "date_rank", method = "continuous", topics = c(1,2,3,4,5,6,7,8), 
     printlegend = FALSE, 
     ylab = "Topic proportion over time", xlab = "time",
     main = "Topic proportion over time")

legend("topright", 
       inset = c(-0.35, 0), 
       legend = c('Topic 1', 'Topic 2', 'Topic 3', 'Topic 4',
                  'Topic 5', 'Topic 6', 'Topic 7', 'Topic 8'),
       lty = 1:8,  
       col = 1:8,  
       cex = 0.8,
       bty = "n")  

par(mar = c(5, 4, 4, 2), xpd = FALSE)

par(mar = c(5, 4, 4, 10), xpd = TRUE)  

plot(prep_spring, "date_rank", method = "continuous", topics = c(1,2,3,4,5,6,7,8), 
     printlegend = FALSE,  
     ylab = "Topic proportion over time", xlab = "time",
     main = "Topic proportion over time")

legend("topright", 
       inset = c(-0.35, 0), 
       legend = c('Topic 1', 'Topic 2', 'Topic 3', 'Topic 4',
                  'Topic 5', 'Topic 6', 'Topic 7', 'Topic 8'),
       lty = 1:8,  
       col = 1:8,  
       cex = 0.8,
       bty = "n")   

par(mar = c(5, 4, 4, 2), xpd = FALSE)

#Regression Analysis 

# theta contains the proportion of each topic for each document

topic_proportions <- as.data.frame(tm_1$theta)
colnames(topic_proportions) <- c("Topic1", "Topic2", "Topic3", "Topic4", 
                                 "Topic5", "Topic6", "Topic7", "Topic8")

# Merge with the original dataset

airbnb2_reg <- cbind(meta, topic_proportions)

reg_sentiment <- lm(sentiment_score ~ Topic1 + Topic2 + Topic3 + Topic4 + 
                      Topic5 + Topic6 + Topic7 + Topic8, 
                    data = airbnb2_reg)

summary(reg_sentiment)
