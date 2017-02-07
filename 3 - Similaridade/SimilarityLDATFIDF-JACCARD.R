#
# Andrey Menezes, 23/05/2016

LDACodeTopics = read.csv("../Results/Experimento/LDA/Code/LDAGibbs 20 TopicsToTermsSourceCode.csv")
LDACodeClassify = read.csv("../Results/Experimento/LDA/Code/LDAGibbs 20 DocsToThreeProbableTopicsSourceCode.csv")

LDADiscussionsTopics = read.csv("../Results/Experimento/LDA/Discussions/LDAGibbs 20 TopicsToTermsDiscussions.csv")
LDADiscussionsClassify = read.csv("../Results/Experimento/LDA/Discussions/LDAGibbs 20 DocsToThreeProbableTopicsDiscussions.csv")

# Similaridade entre os topicos do codigo e discussoes TFIDF
CodeTopicsPaste = c()
for (i in 1:ncol(LDACodeTopics)) {
  CodeTopicsPaste[i] = paste(as.vector(LDACodeTopics[,i]), sep = " ", collapse = ' ')
}

tableSimilarities = matrix(nrow = ncol(LDADiscussionsTopics), ncol = ncol(LDACodeTopics))
for (i in 1:ncol(LDADiscussionsTopics)) {
  DiscussionsTopicPaste = paste(as.vector(LDADiscussionsTopics[,i]), sep = " ", collapse = ' ')
  
  DocumentsTfIdf = rbind(as.matrix(DiscussionsTopicPaste), as.matrix(CodeTopicsPaste))
  DocumentsTfIdf = Corpus(VectorSource(DocumentsTfIdf))
  DocumentsTfIdf = DocumentTermMatrix(DocumentsTfIdf, control = list(weighting = weightTfIdf))
  
  similarity = dist(DocumentsTfIdf, diag=T)
  lineSimilarity = as.vector(as.matrix(similarity)[1,][-1])
  
  tableSimilarities[i,] = lineSimilarity
}
write.csv(tableSimilarities, "../Results/Experimento/TableSimilaritiesTopicsTFidf.csv")


# Similaridade entre discussoes e codigo fonte
TableRelations = matrix( nrow = nrow(LDADiscussionsClassify), ncol = (ncol(LDADiscussionsClassify)-1) )
for (i in 1:nrow(LDADiscussionsClassify)) {
  Similarity = 0
  Discussion = 0
  Code = 0
  
  for (e in 1:nrow(LDACodeClassify)) {
    Actual_Similarity = ( tableSimilarities[LDADiscussionsClassify[i,2],LDACodeClassify[e,2]] ) + ( tableSimilarities[LDADiscussionsClassify[i,2],LDACodeClassify[e,3]] ) + ( tableSimilarities[LDADiscussionsClassify[i,2],LDACodeClassify[e,4]] ) +
      ( tableSimilarities[LDADiscussionsClassify[i,3],LDACodeClassify[e,2]] ) + ( tableSimilarities[LDADiscussionsClassify[i,3],LDACodeClassify[e,3]] ) + ( tableSimilarities[LDADiscussionsClassify[i,3],LDACodeClassify[e,4]] ) +
      ( tableSimilarities[LDADiscussionsClassify[i,4],LDACodeClassify[e,2]] ) + ( tableSimilarities[LDADiscussionsClassify[i,4],LDACodeClassify[e,3]] ) + ( tableSimilarities[LDADiscussionsClassify[i,4],LDACodeClassify[e,4]] )
    
    if(Actual_Similarity > Similarity) {
      Similarity = Actual_Similarity
      Code = as.character(LDACodeClassify[i,1])
      Discussion = as.character(LDADiscussionsClassify[e,1])
    }
  }
  
  TableRelations[i,1] = Code
  TableRelations[i,2] = Discussion
  TableRelations[i,3] = Similarity
}

colnames(TableRelations) = c("TopicCode", "TopicDiscussion", "Similarity")
write.csv(TableRelations, "../Results/Experimento/TableRelationsBetweenTopics.csv", row.names = F)
