# Similarity TF-IDF with euclidian distance
# Andrey Menezes, 26/05/2016

library(tm)
library(doMC)

DadosCode = read.csv("../../Results/LDA/LDAGibbs Each Class.csv")
DadosDiscussion = read.csv("../../Results/LDA/LDAGibbs Each Discussion.csv")

# Similarity between discussions and source code
registerDoMC(8)
tableSimilarities = foreach( i=c(1:nrow(DadosDiscussion)), .combine="rbind") %dopar% {
  DocumentsTfIdf = rbind( as.matrix(DadosDiscussion[i,2]), as.matrix(DadosCode[,2]) )
  DocumentsTfIdf = Corpus(VectorSource(DocumentsTfIdf))
  DocumentsTfIdf = DocumentTermMatrix(DocumentsTfIdf, control = list(weighting = weightTfIdf))
  
  similarity = dist(DocumentsTfIdf, diag=T)
  lineSimilarity = as.matrix(similarity)[1,][-1]
  positions = sort.list(lineSimilarity)

  data.frame("Identify" = as.character(DadosDiscussion$Identify[i]),
             "Class1" = as.character(DadosCode$Class[ positions[1] ]),
             "Discussion1" = lineSimilarity[positions[1]],
             "Class2" = as.character(DadosCode$Class[ positions[2] ]),
             "Discussion1" = lineSimilarity[positions[2]],
             "Class3" = as.character(DadosCode$Class[ positions[3] ]),
             "Discussion3" = lineSimilarity[positions[3]] )
}

write.csv(tableSimilarities, "../../Results/Similarities/TableSimilarities LDA EachClassAndDiscussion.csv")