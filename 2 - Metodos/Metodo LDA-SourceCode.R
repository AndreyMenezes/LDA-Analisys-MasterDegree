# 
# Andrey Menezes, 02/05/2016

library(tm)
library(topicmodels)

SourceCodes = read.csv2("../Results/Experimento/AllSourceCode.csv")

dtm = Corpus(VectorSource(SourceCodes$Code))
dtm = tm_map(dtm, content_transformer(tolower))
dtm = DocumentTermMatrix(dtm)


#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 15

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics = cbind(as.matrix(temp), ldaOut.topics)
colnames(ldaOut.topics) = c("Class", "Topico")
write.csv(ldaOut.topics,file=paste("../Results/Experimento/LDA/Code/LDAGibbs",k,"DocsToTopicsSourceCode.csv"), row.names = F)

#docs to three most probable topics
ldaOut.three.topics = as.matrix(topics(ldaOut,3))
ldaOut.three.topics = t(ldaOut.three.topics)
ldaOut.three.topics = cbind(as.matrix(temp), ldaOut.three.topics)
colnames(ldaOut.three.topics) = c("Class", "1 Topico", "2 Topico", "3 Topico")
write.csv(ldaOut.three.topics,file=paste("../Results/Experimento/LDA/Code/LDAGibbs",k,"DocsToThreeProbableTopicsSourceCode.csv"), row.names = F)

#top 8 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,8))
write.csv(ldaOut.terms,file=paste("../Results/Experimento/LDA/Code/LDAGibbs",k,"TopicsToTermsSourceCode.csv"), row.names = F)

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("../Results/Experimento/LDA/Code/LDAGibbs",k,"TopicProbabilitiesSourceCode.csv"), row.names = F)

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

#write to file
write.csv(topic1ToTopic2,file=paste("../Results/Experimento/LDA/Code/LDAGibbs",k,"Topic1ToTopic2SourceCode.csv"), row.names = F)
write.csv(topic2ToTopic3,file=paste("../Results/Experimento/LDA/Code/LDAGibbs",k,"Topic2ToTopic3SourceCode.csv"), row.names = F)

