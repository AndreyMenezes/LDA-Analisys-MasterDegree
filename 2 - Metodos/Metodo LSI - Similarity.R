# LSI Aplication
# Andrey Menezes, 23/05/2016

library(tm)
library(lsa)

SourceCodes = read.csv2("../../Results/AllSourceCode.csv")
MessagesVocabulary = read.csv2("../../Results/DiscussionsWithRoots.csv")

# Funcao para calcular a similaridade
SearchDocuments <- function(p) {
  q = fold_in(query(p, rownames(LSASpaceMatrix)),LSASpace)
  qd = 0
  for (i in 1:ncol(LSASpaceMatrix)) {
    qd[i] = cosine(as.vector(q),as.vector(LSASpaceMatrix[,i]))
  }
  qd
}

# Construcao da tabela LSI
dtmLSI = Corpus(VectorSource(as.vector(SourceCodes$Code)))
dtmLSI = tm_map(dtmLSI, content_transformer(tolower))
dtmLSI = as.matrix(TermDocumentMatrix(dtmLSI))
  
td = lw_bintf(dtmLSI) * gw_idf(dtmLSI)
LSASpace = lsa(td)
LSASpaceMatrix = as.textmatrix(LSASpace)

tableSimilarities = matrix(nrow = nrow(MessagesVocabulary), ncol = 7)
for (i in 1:nrow(MessagesVocabulary)) {
  
  message = MessagesVocabulary$TreatedMessage[i]
  query = SearchDocuments(message)
  positions = sort.list(query, decreasing = T)

  tableSimilarities[i,1] = as.character(MessagesVocabulary$Identify[i])
  tableSimilarities[i,2] = as.character(SourceCodes$Class[ as.integer(positions[1]) ])
  tableSimilarities[i,3] = query[positions[1]]
  tableSimilarities[i,4] = as.character(SourceCodes$Class[ as.integer(positions[2]) ])
  tableSimilarities[i,5] = query[positions[2]]
  tableSimilarities[i,6] = as.character(SourceCodes$Class[ as.integer(positions[3]) ])
  tableSimilarities[i,7] = query[positions[3]]
  
}

write.csv(tableSimilarities, "../../Results/LSI/TableSimilarities LSI EachClassAndDiscussion.csv")
