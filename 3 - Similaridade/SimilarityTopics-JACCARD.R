#
# Andrey Menezes, 23/05/2016

LDACodeTopics = read.csv("../Results/Experimento/LDA/Code/LDAGibbs 100 TopicsToTermsSourceCode.csv")
LDACodeClassify = read.csv("../Results/Experimento/LDA/Code/LDAGibbs 100 DocsToThreeProbableTopicsSourceCode.csv")

LDADiscussionsTopics = read.csv("../Results/Experimento/LDA/Discussions/LDAGibbs 100 TopicsToTermsDiscussions.csv")
LDADiscussionsClassify = read.csv("../Results/Experimento/LDA/Discussions/LDAGibbs 100 DocsToThreeProbableTopicsDiscussions.csv")

# Similarity between source code topics and discussions topics
tableSimilarities = matrix(nrow = ncol(LDACodeTopics), ncol = ncol(LDADiscussionsTopics))
for (i in 1:ncol(LDACodeTopics)) {
  for (e in 1:ncol(LDADiscussionsTopics)) {
    tableSimilarities[i,e] = length( intersect(as.vector(LDACodeTopics[,i]), as.vector(LDADiscussionsTopics[,e])) ) / length( union(as.vector(LDACodeTopics[,i]), as.vector(LDADiscussionsTopics[,e])) )
  }
}
write.csv(tableSimilarities, "../Results/Experimento/TableSimilaritiesTopics T = 100 w = 15.csv")
