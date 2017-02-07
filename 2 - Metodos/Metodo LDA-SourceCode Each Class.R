# 
# Andrey Menezes, 02/05/2016

library(tm)
library(topicmodels)

SourceCodes = read.csv2("../../Results/AllSourceCode.csv")

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 3

tableTermosToClass = data.frame("Class" = 0, "Topico 1" = 0, "Topico 2" = 0, "Topico 3" = 0)
for (i in 1:nrow(SourceCodes)) {
  
  dtm = Corpus(VectorSource(SourceCodes$Code[i]))
  dtm = tm_map(dtm, content_transformer(tolower))
  dtm = DocumentTermMatrix(dtm)
  
  #Run LDA using Gibbs sampling
  ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  
  #docs to three most probable topics
  ldaOut.three.topics = as.matrix(topics(ldaOut,3))
  ldaOut.three.topics = t(ldaOut.three.topics)
  
  #top 8 terms in each topic
  ldaOut.terms <- as.matrix(terms(ldaOut,8))
  
  tableTermosToClass[i,1] = as.character(SourceCodes$Class[i])
  tableTermosToClass[i,2] = paste(ldaOut.terms[,ldaOut.three.topics[,1]], sep = " ", collapse = ' ')
  tableTermosToClass[i,3] = paste(ldaOut.terms[,ldaOut.three.topics[,2]], sep = " ", collapse = ' ')
  tableTermosToClass[i,4] = paste(ldaOut.terms[,ldaOut.three.topics[,3]], sep = " ", collapse = ' ')
  
}

write.csv(tableTermosToClass,file=paste("../../Results/LDA/LDAGibbs Each ClassTestezao.csv"), row.names = F)
