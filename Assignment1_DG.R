# Dante Goss 
# Assignment 1 - Text Mining
path="C:/Users/Dante/OneDrive - University of Virginia/PSYC 5710 Machine Learning/Week 2 - Text Mining"
setwd(path)

# Load Libraries
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)

# Load data
beatles<-read.csv("lyrics_beatles.csv",header = TRUE)
head(beatles,1)
str(beatles)
# Question 1: What code accomplishes the following tasks?
# 1. Transform data to corpus object using as input a vector from a dataframe
corp<-Corpus(VectorSource(beatles[,3]))
corp[[1]]

# 2. Convert to lower case
corp1<-tm_map(corp,tolower)
corp1
# 3. Remove punctuation
corp2<-tm_map(corp1,removePunctuation)
# 4. Remove numbers
corp3<-tm_map(corp2,removeNumbers)
# 5. Stopword removal
corp4<-tm_map(corp3,removeWords,stopwords("en"))
# 6. Stem the data
corp5<- tm_map(corp4,stemDocument)
# 7. Convert the corpus object into a document term matrix
dtm<- DocumentTermMatrix(corp5)
dtm
# Question 2: Observe the document term matrix and answer the following Questions.

# 2.1 What does Documents: 187 mean?
## Total number of documents in the corpus
## There are 187 total documnets or rows in the matrix

# 2.2 What does terms: 1719 mean?
## Total number of terms in all the documents
## There are 1719 words or terms or tokens (columns) in the dataset.

# 2.3 What does Non -/Sparse entries: 7000/314453 mean?
# There are 7000 cells that are non zero and 314453 that are zero

# 2.4 What does Sparsity: 98% Mean?
# 98% of the terms are zero.

# Question 3: Without changing the max level of sparsity presented in the document
# term matrix, the resulting dataset will have lots of 0's. How can you reduce the sparsity
# of the document - term matrix via the tm pakcage?
#a. removeSparsity()
#b. decreaseSparsity()
#c. removeSparseTerms()  ----This one
#d. removeSparseWords
dtm1<-removeSparseTerms(dtm,sparse = 0.99999999)
dtm1
# Question 4: Iteratively change the maximum level of sparsity in the document-term 
# matrix setting the sparse argument to: 0.99, then to 0.98, then to 0.97, and finally 
# to 0.96. What happens with the document term matrices as the level of sparsity decreases?
#a. The level of sparsity decreases
#b. The level of sparsity stays the same
#c. The level of sparsity decreases
C=seq(0.99,0.96,-0.01)
for (i in C){
  dtm2<-removeSparseTerms(dtm,sparse=i)
  print(dtm2)
  print("/n")
}
## The number of terms decreases, and sparsity decreases.

# Question 5: Create a new document term matrix named dtm.beatles with the upper 
# bound of sparsity set as .90. Convert this document-term matrix into a dataframe 
# named beatles.lyrics. How many terms your dtm.beatles object has?
dtm.beatles<-removeSparseTerms(dtm,sparse = 0.9)
beatles.lyrics<-as.data.frame(as.matrix(dtm.beatles))
length(beatles.lyrics) ##72
head(beatles.lyrics)

# Question 6: Create a plot showing the distribution of the words frequency using 
# the ggplot2 package.
beatles.freq<-sort(colSums(beatles.lyrics),decreasing=TRUE)
beatles.freq2<-data.frame(word=names(beatles.freq),freq=beatles.freq)
ggplot(beatles.freq2,aes(reorder(word,freq),freq))+geom_col()+coord_flip()

# 6.1 What are the three most frequent words?
head(beatles.freq2)
## Love, Know, Dont

# 6.2 What are the three least frequent words?
tail(beatles.freq2)
## Hear, Show, Turn

# Question 7: Create a dynamical heatmap of the words correlation matrix using the 
# plotly package. Save the resulting plot into a HTML file named beatles.cor.html. 
# For computing the correlation, use the cor_auto function from the qgraph package.
library(qgraph)
library(plotly)
library(dplyr)

cor.terms<-cor_auto(beatles.lyrics)
a<- list(showticklabels=TRUE,tickangel=-45)
plot.cor<- plot_ly(x=colnames(beatles.lyrics),
        y=colnames(beatles.lyrics),
        z=cor.terms,type="heatmap") %>% 
  layout(xaxis=a,showlegend=FALSE,margin=
           list(l=100,b=100,r=100,u=100))
htmlwidgets::saveWidget(widget = plot.cor, file = "beatles.cor.html")

# 7.1 Based on the correlation heatmap, which pair of variables are more strongly 
# positively correlated?
sort(cor.terms,decreasing = TRUE)
# Love and Need
