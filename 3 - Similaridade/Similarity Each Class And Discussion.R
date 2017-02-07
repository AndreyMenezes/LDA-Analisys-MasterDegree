library(tm)

DadosCode = read.csv("../../Results/LDA/LDAGibbs Each Class.csv")
DadosDiscussion = read.csv("../../Results/LDA/LDAGibbs Each Discussion.csv")

# Similarity between discussions and source code
tableSimilarities = matrix(nrow = nrow(DadosDiscussion), ncol = 7)
for (i in 1:nrow(DadosDiscussion)) {
  
  DocumentsTfIdf = rbind( as.matrix(DadosDiscussion[i,2]), as.matrix(DadosCode[,2]) )
  DocumentsTfIdf = Corpus(VectorSource(DocumentsTfIdf))
  DocumentsTfIdf = DocumentTermMatrix(DocumentsTfIdf, control = list(weighting = weightTfIdf))
  
  similarity = dist(DocumentsTfIdf, diag=T)
  lineSimilarity = as.matrix(similarity)[1,][-1]
  positions = sort.list(lineSimilarity)
  
  tableSimilarities[i,1] = as.character(DadosDiscussion$Identify[i])
  tableSimilarities[i,2] = as.character(DadosCode$Class[ positions[1] ])
  tableSimilarities[i,3] = lineSimilarity[positions[1]]
  tableSimilarities[i,4] = as.character(DadosCode$Class[ positions[2] ])
  tableSimilarities[i,5] = lineSimilarity[positions[2]]
  tableSimilarities[i,6] = as.character(DadosCode$Class[ positions[3] ])
  tableSimilarities[i,7] = lineSimilarity[positions[3]]
  
}

write.csv(tableSimilarities, "../../Results/TableSimilarities LDA EachClassAndDiscussion.csv")