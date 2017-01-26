setwd("C:/Users/Maryam/Desktop/Datascientists/Data science capstone/final/en_US")

library(qdap)
library(RWeka)
library(ggplot2)
library(stringi)
library(quanteda)
library(slam)
library(tm)
library(SnowballC)
library(rJava)

blogs<-readLines("en_US.blogs.txt")
twitter<-readLines("en_US.twitter.txt")
news<-readLines("en_US.news.txt", warn=T)

blog_size<- round(file.info("en_US.blogs.txt")$size*1e-6, 1)
twitter_size<- round(file.info("en_US.twitter.txt")$size*1e-6, 1)
news_size<- round(file.info("en_US.news.txt")$size*1e-6, 1)

blogs_words<-sum(stri_count_words(blogs))
twitter_words<-sum(stri_count_words(twitter))
news_words<-sum(stri_count_words(news))

blogs_gen<-stri_stats_general(blogs)
twitter_gen<-stri_stats_general(twitter)
news_gen<-stri_stats_general(news)

table<- data.frame(source= c("Blogs", "Twitters", "News"),
                   Words= c(blogs_words, twitter_words, news_words),
                   Lines= c(blogs_gen[1], twitter_gen[1], news_gen[1]),
                   Characters= c(blogs_gen[3], twitter_gen[3], news_gen[3]),
                   Size= c(blog_size, twitter_size, news_size)
)
table 

##############

set.seed(1234)
blogs_sample<- sample(blogs, length(blogs)*0.02, replace=F)
twitter_sample<- sample(twitter, length(twitter)*0.02, replace=F)
news_sample<- sample(news, length(news)*0.02, replace=F)
data_sample<- c(blogs_sample, twitter_sample, news_sample)

data_sample<- gsub("â", "'", data_sample)
data_sample <- gsub(" #\\S*","", data_sample) 
data_sample <- gsub("(f|ht)(tp)(s?)(://)(\\S*)", "", data_sample)  
data_sample <- gsub("[^0-9A-Za-z///' ]", "", data_sample)  
data_sample<- Trim(clean(data_sample))

#################

data_vec<-VectorSource(data_sample)

data_corpus<-corpus(data_sample)
tkn<- tokenize(data_corpus, removeNumbers = T, removePunct = T, removeSeparators = T)
tkn_dfm<- dfm(tkn, stem=T, ignoredFeatures = stopwords("english"))
topfeatures(tkn_dfm, 20)
plot(tkn_dfm, max.word=50, colors=brewer.pal(8, "Dark2"))

########################

tkn_dfm_1<- dfm(data_corpus, ngrams=1, verbose= T, concatenator=" ", stopwords=T) 
tkn_dfm_2<- dfm(data_corpus, ngrams=2, verbose= T, concatenator=" ", stopwords=T) 
tkn_dfm_3<- dfm(data_corpus, ngrams=3, verbose= T, concatenator=" ", stopwords=T) 

one_gram<-as.data.frame(as.matrix(docfreq(tkn_dfm_1)))
two_gram<-as.data.frame(as.matrix(docfreq(tkn_dfm_2)))
three_gram<-as.data.frame(as.matrix(docfreq(tkn_dfm_3)))



# plot 1
one_gram_sorted<-sort(rowSums(one_gram), decreasing=T)
one_gram_table<-data.frame(Words=names(one_gram_sorted), Frequency=one_gram_sorted)
one_gram_plot<-ggplot(within(one_gram_table[1:30, ], Words<-factor(Words, levels=Words)), aes(Words, Frequency))+
  geom_bar(stat="identity", fill="green")+
  ggtitle("Top 30 one gram")+ theme(axis.text.x=element_text(angle=45, hjust=1))
one_gram_plot

# plot 2
two_gram_sorted<-sort(rowSums(two_gram), decreasing=T)
two_gram_table<-data.frame(Words=names(two_gram_sorted), Frequency=two_gram_sorted)
two_gram_plot<-ggplot(within(two_gram_table[1:30, ], Words<-factor(Words, levels=Words)), aes(Words, Frequency))+
  geom_bar(stat="identity", fill="green")+
  ggtitle("Top 30 two gram")+ theme(axis.text.x=element_text(angle=45, hjust=1))
two_gram_plot

# plot 3
three_gram_sorted<-sort(rowSums(three_gram), decreasing=T)
three_gram_table<-data.frame(Words=names(three_gram_sorted), Frequency=three_gram_sorted)
three_gram_plot<-ggplot(within(three_gram_table[1:30, ], Words<-factor(Words, levels=Words)), aes(Words, Frequency))+
  geom_bar(stat="identity", fill="green")+
  ggtitle("Top 30 three gram")+ theme(axis.text.x=element_text(angle=45, hjust=1))
three_gram_plot


