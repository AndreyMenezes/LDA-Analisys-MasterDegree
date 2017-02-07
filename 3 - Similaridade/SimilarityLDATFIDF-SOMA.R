#
# Andrey Menezes, 23/05/2016

library(tm)

LDACodeTopics = read.csv("../Results/Experimento/LDA/Code/LDAGibbs 100 TopicsToTermsSourceCode.csv")
LDACodeClassify = read.csv("../Results/Experimento/LDA/Code/LDAGibbs 100 DocsToThreeProbableTopicsSourceCode.csv")

LDADiscussionsTopics = read.csv("../Results/Experimento/LDA/Discussions/LDAGibbs 100 TopicsToTermsDiscussions.csv")
LDADiscussionsClassify = read.csv("../Results/Experimento/LDA/Discussions/LDAGibbs 100 DocsToThreeProbableTopicsDiscussions.csv")

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
write.csv(tableSimilarities, "../Results/Experimento/TableSimilaritiesTopicsTFidf T = 100 w = 15.csv")


# Similaridade entre discussoes e codigo fonte
TableRelations = matrix( nrow = nrow(LDADiscussionsClassify), ncol = ( (2*ncol(LDADiscussionsClassify)) -1) )
for (i in 1:nrow(LDADiscussionsClassify)) {
  Similarity1 = 0
  Similarity2 = 0
  Similarity3 = 0
  Discussion = 0
  Code1 = 0
  Code2 = 0
  Code3 = 0
  
  for (e in 1:nrow(LDACodeClassify)) {
    Actual_Similarity = ( tableSimilarities[LDADiscussionsClassify[i,2],LDACodeClassify[e,2]] ) + ( tableSimilarities[LDADiscussionsClassify[i,2],LDACodeClassify[e,3]] ) + ( tableSimilarities[LDADiscussionsClassify[i,2],LDACodeClassify[e,4]] ) +
                        ( tableSimilarities[LDADiscussionsClassify[i,3],LDACodeClassify[e,2]] ) + ( tableSimilarities[LDADiscussionsClassify[i,3],LDACodeClassify[e,3]] ) + ( tableSimilarities[LDADiscussionsClassify[i,3],LDACodeClassify[e,4]] ) +
                        ( tableSimilarities[LDADiscussionsClassify[i,4],LDACodeClassify[e,2]] ) + ( tableSimilarities[LDADiscussionsClassify[i,4],LDACodeClassify[e,3]] ) + ( tableSimilarities[LDADiscussionsClassify[i,4],LDACodeClassify[e,4]] )
    
    if(Actual_Similarity > Similarity1) {
      Similarity1 = Actual_Similarity
      Code1 = as.character(LDACodeClassify[e,1])
    } else if(Actual_Similarity > Similarity2) {
      Similarity2 = Actual_Similarity
      Code2 = as.character(LDACodeClassify[e,1])
    } else if(Actual_Similarity > Similarity3) {
      Similarity3 = Actual_Similarity
      Code3 = as.character(LDACodeClassify[e,1])
    }
  }
  Discussion = as.character(LDADiscussionsClassify[i,1])
  
  TableRelations[i,1] = Discussion
  TableRelations[i,2] = Code1
  TableRelations[i,3] = Similarity1
  TableRelations[i,4] = Code2
  TableRelations[i,5] = Similarity2
  TableRelations[i,6] = Code3
  TableRelations[i,7] = Similarity3
}

colnames(TableRelations) = c("TopicDiscussion", "TopicCode1", "Similarity1", "TopicCode2", "Similarity2", "TopicCode3", "Similarity3")
write.csv(TableRelations, "../Results/Experimento/TableRelationsBetweenTopics TFIDF-Soma.csv", row.names = F)
