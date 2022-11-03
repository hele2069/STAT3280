papers1 <- scan("/Users/a/Desktop/STAT 3280/HW6/statisticians/Data/paperList.txt",sep="\n",what="")
papers <- scan("/Users/a/Desktop/STAT 3280/HW6/statisticians/Data/paperList_Abstracts_Keyword.txt",sep="\n",what="")

head(papers1)

head(papers)

papers1[2]

papers[2]

papers1[5]
papers[5]


#### notice that items in the two data sets match in their order

papers <- papers[-1]

#### So papers1 contains the papers by title and time, papers contains papers by abstract and time

dt <- scan("/Users/a/Desktop/STAT 3280/HW6/statisticians/Data/paperList_Abstracts_Keyword.txt",what="",sep="\n")
length(dt)
dt <- dt[-1]

dt[1]
dt[2]


### now extract year

tmp1 <- lapply(dt,function(s)unlist(strsplit(s,'"')))
tmp1[[3]]
tmp1[[2]]


years <- unlist(lapply(tmp1,function(x) return(x[3])))



head(years)
years <- gsub(",","",years)
head(years)


years <- as.numeric(years)

table(years)


abss <- unlist(lapply(tmp1,function(x) return(x[4])))



head(abss)


tail(abss)


### want to remove some strange and not so meaningful characters
abss <- gsub('\\{','',abss)
abss <- gsub("}","",abss)
abss <- gsub("#","",abss)
abss <- gsub("&","",abss)
abss <- gsub("<","",abss)
abss <- gsub(">","",abss)
abss <- gsub("/","",abss)
abss <- gsub(")","",abss)
abss <- gsub("\\(","",abss)
abss <- gsub("amp","",abss)

abss[1]

head(abss)

for(k in 1:length(abss)){
    
    write(abss[k],file=paste("/Users/a/Desktop/STAT 3280/HW6/statisticians/Data/TextFolder/",k,sep=""))
}

###### you can save all the abstracts in a separate folder for future use

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

cname <- "/Users/a/Desktop/STAT 3280/HW6/statisticians/Data/TextFolder"

dir(cname)

## read in all the documents as a Corpus

docs <- Corpus(DirSource(cname))   
meta(docs,"id")
document.names <- unlist(meta(docs,"id"))
document.names[2]

#### notice the index order -- alphabetical

length(document.names)

head(document.names)

#summary(docs)

inspect(docs[2])

writeLines(as.character(docs[[2]])) ### print out the whole string

as.character(docs[[1]])

lapply(docs[1:2], as.character)

### why do we need that: we need to clean the text data


## Removing punctuation:
new.docs <- tm_map(docs, removePunctuation)

docs <- new.docs

lapply(docs[1:2], as.character)
lapply(docs[52:53], as.character)

## Removing numbers

docs <- tm_map(docs, removeNumbers)

lapply(docs[1:2], as.character)

## To lower

docs <- tm_map(docs, tolower)

## remove stopwords
stopwords("english")

docs <- tm_map(docs, removeWords, stopwords("english"))

lapply(docs[1:2], as.character)


## stemming

new.docs <- tm_map(docs, stemDocument)   

writeLines(as.character(docs[[5]]))

writeLines(as.character(new.docs[[5]]))


## Notice that stems are used a lot in processing daily data. 
## Titles are very regular and typically do not need stemming, but there is no conclusion for abstracts
## It may or may not be effective in processing abstracts (people hold different opinions)


new.docs <- tm_map(new.docs, stripWhitespace)   

docs <- new.docs

lapply(docs[1:2], as.character)

dtm <- DocumentTermMatrix(docs)   

##dtm = the number of occurrences of term t j in doc i

class(dtm)
dim(dtm)
dtm

tdm <- TermDocumentMatrix(docs)
dim(tdm)

freq <- colSums(as.matrix(dtm))   
length(freq)

hist(log(freq))

### very skewed

ord <- order(freq)

 findFreqTerms(dtm, 10) ## find terms with at frequency >= 10
 
 findFreqTerms(dtm, 30,40)

dtms <- removeSparseTerms(dtm, 0.995)  ### remove terms that appear in no more than 0.5% entries

dtms

DTM <- as.matrix(dtms)
## for full terms
# DTM <- as.matrix(dtm)
dim(DTM)
colnames(DTM)

document.names

head(document.names,20)

doc.index <- as.numeric(document.names)
#### now, need to make sure everything is back in the original order --- because we will have to use the same order to match other informaiton

final.DTM <- matrix(0,nrow=nrow(DTM),ncol=ncol(DTM))
final.DTM[doc.index,] <- DTM

head(rownames(DTM))

sum(abs(final.DTM[10,]-DTM[2,]))

colnames(final.DTM) <- colnames(DTM)

### example: visualize the word cloud
install.packages("wordcloud")
library(wordcloud)



wordcloud(
  words = colnames(final.DTM),
  freq = colSums(final.DTM),
  scale = c(2, 0.25),min.freq = 100)


wordcloud(
  words = colnames(final.DTM),
  freq = colSums(final.DTM),
  scale = c(2, 0.25),min.freq = 200)


wordcloud(
  words = colnames(final.DTM),
  freq = colSums(final.DTM),
  scale = c(2, 0.25),min.freq = 300)


wordcloud(
  words = colnames(final.DTM),
  freq = colSums(final.DTM),
  scale = c(2, 0.5),min.freq = 300)


### look at the cloud, what to improve?

#### Note that we only remove very sparse terms. However, another cateogory of non-informative terms is
###  the class of too-common terms. That means, you may not
### want to purely filter words by frequency. In NLP, there is a more mature procedure as tf-idf filtering
###  tf-idf: term frequency–inverse document frequency
### And further filter the current dictionary to a smaller size (say, 500). Check wiki for more details.

TF <- final.DTM/(0.5+rowSums(final.DTM))
iDF <- log(nrow(final.DTM)/colSums(final.DTM>0))
TFiDF <- t(t(TF) * iDF)
term.TFiDF <- colSums(TFiDF)
ix <- sort(term.TFiDF,decreasing=TRUE,index.return=TRUE)$ix

colnames(final.DTM)[ix[1:100]]


### make sense except for the first one

final.DTM <- final.DTM[,ix[2:501]]



wordcloud(
  words = colnames(final.DTM),
  freq = colSums(final.DTM),
  scale = c(3, 0.25),min.freq = 300)



#### Next: how to calculate authors' frequency table?

A2P <- read.table("/Users/a/Desktop/STAT 3280/HW6/statisticians/Data/authorPaperBiadj.txt")

dim(A2P)


rownames(A2P)

### papers are the columns. They are in the same order as the previous documents, 

A2P <- as.matrix(A2P)


A.DTM <- A2P%*%final.DTM
dim(A.DTM)
#### matrix multiplication. Convince yourself why this works. Check wiki to understand matrix multiplication



### Now get author names --- still in the same author order

authors <- read.table("/Users/a/Desktop/STAT 3280/HW6/statisticians/Data/authorList.txt",stringsAsFactors=FALSE)
head(authors)
authors <- unlist(lapply(authors,function(x)gsub(" ","_",x)))
head(authors)

length(authors)

rownames(A.DTM)  <- authors


### final coauthorship: think about how to efficiently find it?
### matrix multiplication again

Coauthor.Adj <- A2P%*%t(A2P)

dim(Coauthor.Adj)



#### convince yourself the meaning of this matrix

### create coauthorship network

weighted.A <- Coauthor.Adj
A <- weighted.A
A[weighted.A>0] <- 1   ### remove the weights
diag(A) <- 0

author.degree <- rowSums(A2P) #### how many collaborators each author has


library(igraph)


g <- graph.adjacency(A,mode="undirected")

plot(g,vertex.size=3,edge.width=0.5,vertex.label=NA)

##### see what happens? And this shows why visualization is important


#### focus on the core component ~~~~ the largest connected component

cl <- clusters(g)

cl$csize

cl$membership

which.max(cl$csize)

lcc <- which(cl$membership==which.max(cl$csize))

A.lcc <- A[lcc,lcc]

g.lcc <- graph.adjacency(A.lcc,mode="undirected")

plot(g.lcc,vertex.size=3,edge.width=0.5,vertex.label=NA)

lo <- layout_with_graphopt(g.lcc) 
#### can be slow
plot(g.lcc,vertex.size=3,edge.width=0.5,vertex.label=NA,layout=lo)

lo <- layout_with_lgl(g.lcc) 

plot(g.lcc,vertex.size=3,edge.width=0.5,vertex.label=NA,layout=lo)

###  not informative

### too large and complicated




### In the homework, you will focus on ego network -- the subnetwork induced by a particular node and all its connections
### That will be smaller and easier for visual exploration

### However, you will need to use the year information to stratify the corpus, so you can analyze the pattern for each period

