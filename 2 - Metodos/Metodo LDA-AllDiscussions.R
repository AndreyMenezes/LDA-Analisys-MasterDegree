# 
# Andrey Menezes, 26/04/2016

library(tm)
library(topicmodels)

MessagesVocabulary = read.csv2("../../Results/DiscussionsWithRoots.csv")

dtm = Corpus(VectorSource(MessagesVocabulary$TreatedMessage))
dtm = tm_map(dtm, content_transformer(tolower))
dtm = DocumentTermMatrix(dtm)


#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 20

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics = cbind(as.matrix(MessagesVocabulary$Identify), ldaOut.topics)
colnames(ldaOut.topics) = c("Identify", "Topico")
write.csv(ldaOut.topics,file=paste("../../Results/LDA/LDAGibbs",k,"DocsToTopicsDiscussions.csv"), row.names = F)

#docs to three most probable topics
ldaOut.three.topics = as.matrix(topics(ldaOut,3))
ldaOut.three.topics = t(ldaOut.three.topics)
ldaOut.three.topics = cbind(as.matrix(MessagesVocabulary$Identify), ldaOut.three.topics)
colnames(ldaOut.three.topics) = c("Identify", "1 Topico", "2 Topico", "3 Topico")
write.csv(ldaOut.three.topics,file=paste("../../Results/LDA/LDAGibbs",k,"DocsToThreeProbableTopicsDiscussions.csv"), row.names = F)

#top 8 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,8))
write.csv(ldaOut.terms,file=paste("../../Results/LDA/LDAGibbs",k,"TopicsToTermsDiscussions.csv"), row.names = F)

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("../../Results/LDA/LDAGibbs",k,"TopicProbabilitiesDiscussions.csv"), row.names = F)

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

#write to file
write.csv(topic1ToTopic2,file=paste("../../Results/LDA/LDAGibbs",k,"Topic1ToTopic2Discussions.csv"), row.names = F)
write.csv(topic2ToTopic3,file=paste("../../Results/LDA/LDAGibbs",k,"Topic2ToTopic3Discussions.csv"), row.names = F)

