#
# Andrey Menezes, 23/05/2016

LDACodeTopics = read.csv("../Results/Experimento/LDA/Code/LDAGibbs 20 TopicsToTermsSourceCode.csv")
LDACodeClassify = read.csv("../Results/Experimento/LDA/Code/LDAGibbs 20 DocsToThreeProbableTopicsSourceCode.csv")

LDADiscussionsTopics = read.csv("../Results/Experimento/LDA/Discussions/LDAGibbs 20 TopicsToTermsDiscussions.csv")
LDADiscussionsClassify = read.csv("../Results/Experimento/LDA/Discussions/LDAGibbs 20 DocsToThreeProbableTopicsDiscussions.csv")

# Similaridade entre os topicos do codigo e discussoes Jaccard
tableSimilarities = matrix(nrow = ncol(LDACodeTopics), ncol = ncol(LDADiscussionsTopics))
for (i in 1:ncol(LDACodeTopics)) {
  for (e in 1:ncol(LDADiscussionsTopics)) {
    tableSimilarities[i,e] = length( intersect(as.vector(LDACodeTopics[,i]), as.vector(LDADiscussionsTopics[,e])) ) / length( union(as.vector(LDACodeTopics[,i]), as.vector(LDADiscussionsTopics[,e])) )
  }
}
write.csv(tableSimilarities, "../Results/Experimento/TableSimilaritiesTopics.csv")


# Similaridade entre discussoes e codigo fonte Jaccard
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
    Actual_Similarity = 
    
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
write.csv(TableRelations, "../Results/Experimento/TableRelationsBetweenTopics Jaccard-Soma.csv", row.names = F)
