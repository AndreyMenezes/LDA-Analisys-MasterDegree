# LDA aplication for each discussion
# Andrey Menezes, 09/12/2016

library(tm)
library(topicmodels)
library(doMC)

MessagesVocabulary = read.csv2("../../Results/DiscussionsWithRoots.csv")
dir.create(file.path(getwd(), "../../Results/LDA/"))

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 3

# Extract topic of discussion (most representative terms)
registerDoMC(4)
tableLDATermosCode = foreach( i=c(1:nrow(MessagesVocabulary)), .combine="rbind") %dopar% {
  dtm = Corpus(VectorSource(MessagesVocabulary$TreatedMessage[i]))
  dtm = tm_map(dtm, content_transformer(tolower))
  dtm = DocumentTermMatrix(dtm)
  
  # Run LDA using Gibbs sampling
  ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  
  # Docs to three most probable topics
  ldaOut.three.topics = as.matrix(topics(ldaOut,3))
  ldaOut.three.topics = t(ldaOut.three.topics)
  
  # Top 8 terms in each topic
  ldaOut.terms <- as.matrix(terms(ldaOut,8))
  
  data.frame("Identify" = as.character(MessagesVocabulary$Identify[i]),
             "Topico 1" = paste(ldaOut.terms[,ldaOut.three.topics[,1]], sep = " ", collapse = ' '),
             "Topico 2" = paste(ldaOut.terms[,ldaOut.three.topics[,2]], sep = " ", collapse = ' '),
             "Topico 3" = paste(ldaOut.terms[,ldaOut.three.topics[,3]], sep = " ", collapse = ' '))
}

write.csv(tableLDATermosCode, file="../../Results/LDA/LDAGibbs Each Discussion.csv", row.names = F)
