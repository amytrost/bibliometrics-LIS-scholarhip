# Scraping google scholar (GS) profiles from a list of google scholar IDs
# borrowed from https://www.r-bloggers.com/yet-another-post-on-google-scholar-data-analysis/
#
# Questions? Please contact Amy Trost at UMD libraries

library(scholar)
library(rvest)


# test with single record
author_pub <- get_publications("GkI8GhIAAAAJ", pagesize = 1000)

# initialize variables
author_pub<-NULL
author_piece <- NULL

# read in list of UMD faculty google scholar ids
gsids <- read.csv("GoogleScholar ID list.csv", stringsAsFactors = FALSE)
dim(gsids)

# loop thru IDs, create table of bibliographic info
for (i in 1:41){
    author_piece <- cbind(StaffID=gsids$StaffIDs[[i]], get_publications(gsids$StaffIDs[[i]], pagesize = 1000))    
    author_pub <- rbind(author_pub, author_piece)
    author_piece <- NULL
}


View(author_pub)
write.csv(author_pub,"pubslist2.csv")

# Create word co-occurence network for title terms
library(bibliometrix)
library(tm)
library(dplyr)

GS_files <- data.frame(read.csv("pubslist_all.csv"))
View(GS_files)

# Create individual title files
for (i in 1:514){
  write(toString(GS_files$Title[[i]]), paste("./title indiv/",i, "Title.txt"))
}


#Create Corpus
docs <- Corpus(DirSource("./title indiv"))
View(docs)

#create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

# clean up corpus
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)

docs_stemmed <- tm_map(docs,stemDocument)



# Creating term document matrix
tdm <- TermDocumentMatrix(docs_stemmed)
tdm2 <- TermDocumentMatrix(docs)
tdm2_sum <- data.frame(rowSums(as.matrix(tdm2)))

View(tdm)
View(tdm2_sum)

#creating nodes table
write.csv(tdm2_sum,"fullwords.csv")


# This sequence transforms tdm into a matrix
# showing word co-occurrence
tdm.matrix <- as.matrix(tdm)
tdm.matrix[tdm.matrix>=1] <- 1
tdm.matrix2 <- tdm.matrix %*% t(tdm.matrix)

tdm.matrix2[6:20,6:20]

# flattens co occurence from matrix to 3 columns
desired <- as.data.frame(as.table(tdm.matrix2))
head(desired$Freq)
edges <-  subset(desired, Freq>1)

# Create Node Table, add id
nodes <- data.frame(rowSums(tdm.matrix))
View(nodes)
Terms <- rownames(nodes)
nodes <- cbind(Terms,nodes)
nodes <- cbind("id"=1:nrow(nodes),nodes)
colnames(nodes)[[3]] <- "Frequency"


# create second nodes table to help with edges
nodes.1 <- nodes
colnames(nodes.1)[[1]] <- "Target"
colnames(nodes.1)[[2]] <- "Terms.1"
View(nodes.1)

View(edges)

# Substitute stemmed term for ID on edge table
edges_labeled <- merge(edges, nodes, by.x="Terms")

View(edges_labeled)
edges_labeled$Frequency <- NULL

edges_labeled2 <- merge(edges_labeled, nodes.1, by.x="Terms.1")
View(nodes)
View(edges_labeled2)
edges_labeled2$Frequency <- NULL

write.csv(edges_labeled2,"edges.csv")
write.csv(nodes,"nodes.csv")




