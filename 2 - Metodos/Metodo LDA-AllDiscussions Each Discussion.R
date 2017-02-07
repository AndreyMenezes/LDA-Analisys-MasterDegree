# LDA aplication for each discussion
# Andrey Menezes, 26/04/2016

library(tm)
library(topicmodels)

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
tableTermosToDiscussions = data.frame("Identify" = 0, "Topico 1" = 0, "Topico 2" = 0, "Topico 3" = 0)
for (i in 1:nrow(MessagesVocabulary)) {
  
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
  
  tableTermosToDiscussions[i,1] = as.character(MessagesVocabulary$Identify[i])
  tableTermosToDiscussions[i,2] = paste(ldaOut.terms[,ldaOut.three.topics[,1]], sep = " ", collapse = ' ')
  tableTermosToDiscussions[i,3] = paste(ldaOut.terms[,ldaOut.three.topics[,2]], sep = " ", collapse = ' ')
  tableTermosToDiscussions[i,4] = paste(ldaOut.terms[,ldaOut.three.topics[,3]], sep = " ", collapse = ' ')
}

write.csv(tableTermosToDiscussions, file="../../Results/LDA/LDAGibbs Each Discussion.csv", row.names = F)
