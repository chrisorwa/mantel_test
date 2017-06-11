#load required library
library(tm)
library(ggplot2)
library(ggthemes)
library(ape)
library(RWeka)

#load data
dfile <-read.csv()

#Create Bigram Tokenizer 
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

#create term-document matrix
corpus <-Corpus(VectorSource(dfile$text))
tdm <-DocumentTermMatrix(corpus,control = list(stopwords=stopwords('SMART')))
tdm2 <-removeSparseTerms(tdm,0.99)

#convert to matrix
b <-as.data.frame(as.matrix(tdm2))

#define euclidean function
euclidean <-function(x2,y2,x1,y1){
  a <-(x2-x1)*(x2-x1)
  b <-(y2-y1)*(y2-y1)
  return(sqrt(a+b))
}

#initialize loop variables
k = 500
v = nrow(b)
record = c()

while(v-k > 0 )
{
  c <-prcomp(b[1:k,])
  d <-as.data.frame(c$rotation)
  e <-d[c('PC1','PC2')]
  
  #create empty matrix
  n <-matrix(nrow = nrow(e),ncol = nrow(e))
  colnames(n) <-rownames(e)
  rownames(n) <-rownames(e)
  
  #populate matrix
  for (i in 1:nrow(e)){
    for (j in 1:nrow(e)){
      n[i,j] <-round(euclidean(e$PC2[i],e$PC1[i],e$PC2[j],e$PC1[j]),digits = 4)
    }
  }
  
  c <-prcomp(b[1:v,])
  d <-as.data.frame(c$rotation)
  e <-d[c('PC1','PC2')]
  
  #create empty matrix
  m <-matrix(nrow = nrow(e),ncol = nrow(e))
  colnames(m) <-rownames(e)
  rownames(m) <-rownames(e)
  
  #populate matrix
  for (i in 1:nrow(e)){
    for (j in 1:nrow(e)){
      m[i,j] <-round(euclidean(e$PC2[i],e$PC1[i],e$PC2[j],e$PC1[j]),digits = 4)
    }
  }
  
  
  #mantel's permutation test
  mantle <-mantel.test(m,n,graph = FALSE)
  
  rec = c(k,v,mantle$p)
  record = rbind(record,rec)
  
  #increment counter
  k = k+500
  v = v-500
} 

#give column names
colnames(record) <-c("k","j","p-value")
record <-as.data.frame(record)

#create new variable
record$diff <-record$j - record$k

#plotting
ggplot(record, aes(x=record$diff,y=record$`p-value`)) + 
  xlab("Difference")+
  ylab("P-value")+
  theme_economist(base_size = 10,horizontal = TRUE)+
  geom_line(aes(x = record$diff,y = record$`p-value`,color='red',size=1)) + 
  theme(text=element_text(size=10))+
  ggtitle("Mantels Test \'n")

qplot(x = record$diff,y = record$`p-value`)

#===========================================================================================

#perform PCA
c <-prcomp(b)
d <-as.data.frame(c$rotation)
e <-d[c('PC1','PC2')]

#plotting
qplot(PC1,PC2,data = e,main = 'Principal Component Analysis\n',label=rownames(e),geom='text')
