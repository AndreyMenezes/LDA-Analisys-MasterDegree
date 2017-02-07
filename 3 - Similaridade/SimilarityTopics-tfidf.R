library(lsa)
library(tm)


LDACodeTopics = read.csv( paste("../Results/Experimento/LDA/Code/AllOneRepetition//LDAGibbs 50 TopicsToTermsSourceCode 6 .csv") )
LDADiscussionsTopics = read.csv( paste("../Results/Experimento/LDA/Discussions/AllOneRepetition/LDAGibbs 50 TopicsToTermsDiscussions 6 .csv") )

CodeTopicsPaste = c()
for (i in 1:ncol(LDACodeTopics)) {
  CodeTopicsPaste[i] = paste(as.vector(LDACodeTopics[,i]), sep = " ", collapse = ' ')
}
    
tableSimilarities = matrix(nrow = ncol(LDADiscussionsTopics), ncol = ncol(LDACodeTopics))
for (e in 1:ncol(LDADiscussionsTopics)) {
  DiscussionsTopicPaste = paste(as.vector(LDADiscussionsTopics[,e]), sep = " ", collapse = ' ')
    
  DocumentsTfIdf = rbind(as.matrix(DiscussionsTopicPaste), as.matrix(CodeTopicsPaste))
  DocumentsTfIdf = Corpus(VectorSource(DocumentsTfIdf))
  DocumentsTfIdf = DocumentTermMatrix(DocumentsTfIdf, control = list(weighting = weightTfIdf))
      
  #similarity = dist(DocumentsTfIdf, diag=T, method = "pearson")
  #lineSimilarity = as.vector(as.matrix(similarity)[1,][-1])
  cos = cosine(t(as.matrix(DocumentsTfIdf)))
  

  tableSimilarities[e,] = lineSimilarity
}

write.csv(tableSimilarities, "../Results/Experimento/TableSimilaritiesAllOneRepetition.csv")



colnames(tableSummary) = c("topics","words","min","max","mean","median")
write.csv(tableSummary, "../Results/Experimento/TableSimilaritiesOneRepetition.csv")
