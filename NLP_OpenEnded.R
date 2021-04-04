library(SnowballC)
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(readxl)


setwd("/Users/mb/Desktop/Thesis Data/Script")

# Open ended question: 
# What is one process or practice that you would like to see from your management regarding communication during a crisis?

# "Open" - Responses to the open ended question above
open<-read_excel("/Users/mb/Desktop/Thesis Data/Maria/Me to work/T20 Responses.xlsx")

# View data set  
glimpse(open)
head(open)
tail(open)

# frequency of number of na in the data set
table(is.na(open))

# remove na from data set
open<-na.omit(open)

## Cleaning data set

# Adding header to data frame 
colnames(open) <- c("response")

# duplicate column to clean it 
open <- open %>% 
  mutate(`Clean Response` = `response`)

# All lowercase
open <- open %>% mutate(`Clean Response` = tolower(open$`Clean Response`))

# Remove punctuation
open <- open %>% mutate(`Clean Response` = removePunctuation(open$`Clean Response`))

# Remove Numbers
open <- open %>% mutate(`Clean Response` = replace_number(open$`Clean Response`))

# Remove whitespace
open <- open %>% mutate(`Clean Response` = stripWhitespace(open$`Clean Response`))

# Replace contractions
open <- open %>% mutate(`Clean Response` = replace_contraction(open$`Clean Response`))

# Replace abbreviations
open <- open %>% mutate(`Clean Response` = replace_abbreviation(open$`Clean Response`))

# Replace symbols with words
open <- open %>% mutate(`Clean Response` = replace_symbol(open$`Clean Response`))

# Print text without standard stop words
open <- open %>% mutate(`Clean Response` = removeWords(open$`Clean Response`, stopwords('en')))
                          

# Make the vector a VCorpus object (volatile corpus, which is held in your computer’s RAM rather than saved to disk, just to be more memory efficient. )
# Make a vector source
open_source<-VectorSource(open$`Clean Response`)

# Make a volatile corpus
open_corpus<- VCorpus(open_source)

# print
open_corpus

# Print data on the  open_corpus
open_corpus[[4]]$content

# Create the dtm from the corpus: open_corpus
open_dtm <- DocumentTermMatrix(open_corpus)

# Print out coffee_dtm data
open_dtm

# Convert coffee_dtm to a matrix: coffee_m
open_m <- as.matrix(open_dtm)

# Print the dimensions of coffee_m
dim(open_m)

# Review a portion of the matrix
open_m[14:16, 100:105]

## Make a term document matrix
# Create a TDM from clean_corp: coffee_tdm
open_tdm <- TermDocumentMatrix(open_corpus)

# Print coffee_tdm data
open_tdm

# Convert coffee_tdm to a matrix: coffee_m
open_m <- as.matrix(open_tdm)

# Print the dimensions of the matrix
dim(open_m)

## Word Clouds

# Create a matrix: coffee_m
open_m <- as.matrix(open_tdm)

# Calculate the rowSums: term_frequency
term_frequency <- rowSums(open_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = T)

# View the top 10 most common words
term_frequency[1:10]

# Plot a barchart of the 10 most common words
barplot(term_frequency[1:10], col = "tan", las = 2)

## Word Cloud

open_word_freqs <- data.frame(
  term = names(term_frequency),
  num = term_frequency
)

head(open_word_freqs)

# Create a wordcloud for the values in word_freqs
wordcloud(open_word_freqs$term, open_word_freqs$num,
          max.words = 100, colors = "red")