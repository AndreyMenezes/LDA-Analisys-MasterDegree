library(tm)

t = c(15,20,30,50)

tableSummary = matrix( nrow = (length(t) * length(w)), ncol = 6 )
m = 1
for (i in 1:length(t)) {
  for (e in 1:length(w)){
    LDACodeTopics = read.csv( paste("../Results/Experimento/LDA/Code/OneRepetition/LDAGibbs",t[i],"TopicsToTermsSourceCode",w[e],".csv") )
    LDADiscussionsTopics = read.csv( paste("../Results/Experimento/LDA/Discussions/OneRepetition/LDAGibbs",t[i],"TopicsToTermsDiscussions",w[e],".csv") )
    
    CodeTopicsPaste = c()
    for (d in 1:ncol(LDACodeTopics)) {
      CodeTopicsPaste[d] = paste(as.vector(LDACodeTopics[,d]), sep = " ", collapse = ' ')
    }
    
    tableSimilarities = matrix(nrow = ncol(LDADiscussionsTopics), ncol = ncol(LDACodeTopics))
    for (u in 1:ncol(LDADiscussionsTopics)) {
      DiscussionsTopicPaste = paste(as.vector(LDADiscussionsTopics[,u]), sep = " ", collapse = ' ')
      
      DocumentsTfIdf = rbind(as.matrix(DiscussionsTopicPaste), as.matrix(CodeTopicsPaste))
      DocumentsTfIdf = Corpus(VectorSource(DocumentsTfIdf))
      DocumentsTfIdf = DocumentTermMatrix(DocumentsTfIdf, control = list(weighting = weightTfIdf))
      
      similarity = dist(DocumentsTfIdf, diag=T)
      lineSimilarity = as.vector(as.matrix(similarity)[1,][-1])
      
      tableSimilarities[u,] = lineSimilarity
    }
    
    tableSummary[m,1] = t[i]
    tableSummary[m,2] = w[e]
    tableSummary[m,3] = min(tableSimilarities)
    tableSummary[m,4] = max(tableSimilarities)
    tableSummary[m,5] = mean(tableSimilarities)
    tableSummary[m,6] = median(tableSimilarities)
    m = m +1
  }
}

colnames(tableSummary) = c("topics","words","min","max","mean","median")
write.csv(tableSummary, "../Results/Experimento/TableSimilaritiesOneRepetition.csv")
