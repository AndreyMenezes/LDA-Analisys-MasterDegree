# Study of words frequency in the project, analises of class and messages
# Andrey Menezes, 21/09/2015

discussionRoots = read.csv2("../../Results/DiscussionsWithRoots.csv")
allSourceCodes = read.csv2("../../Results/AllSourceCode.csv")

# Frequency words of all discussions
discussions = paste(discussionRoots[1,4], sep = " ", collapse = ' ')
discussions = strsplit(discussions, " ")
vectorCount = c(discussions[[1]])
for (i in 2:nrow(discussionRoots)) {
  discussions = paste(discussionRoots[i,4], sep = " ", collapse = ' ')
  discussions = strsplit(discussions, " ")
  vectorCount = c(vectorCount, discussions[[1]])
}

tableCountDiscussions = as.data.frame(table(vectorCount))

# Frequency words of all source code
sourceCode = paste(allSourceCodes[1,1], sep = " ", collapse = ' ')
sourceCode = strsplit(sourceCode, " ")
vectorCount = c(sourceCode[[1]])
for (i in 2:nrow(allSourceCodes)) {
  sourceCode = paste(allSourceCodes[i,1], sep = " ", collapse = ' ')
  sourceCode = strsplit(sourceCode, " ")
  vectorCount = c(vectorCount, sourceCode[[1]])
}

tableCountSourceCode = as.data.frame(table(vectorCount))


# Remove one repettitions words and the most communs words of join the discussions table and source code table
allKeywordsFrenquency = rbind(tableCountDiscussions, tableCountSourceCode)
allKeywordsFrenquency = with(allKeywordsFrenquency, aggregate(Freq, list(vectorCount), sum))
colnames(allKeywordsFrenquency) = c("Character", "Freq")
allKeywordsFrenquency = allKeywordsFrenquency[order(allKeywordsFrenquency$Freq, decreasing = T),]

# Graph for analysis
png(filename = "../../Results/Images/CumulativeFrequencyAllTerms.png")
plot(ecdf(allKeywordsFrenquency$Freq), cex=0.75, main = "Frequencia cumulativa termos classes", xlab="Frequencia", ylab="Acumulado", las=2)
axis(1, at = 750, las=2, cex.axis = 0.75)
axis(2, at = 0.965, las=1, cex.axis = 0.75)
segments(x0=1400, y0=0, x1=1400, y1=0.985, col="red")
#segments(x0=0, y0=0.96, x1=690, y1=0.96, col="red")
dev.off()


mykeywordsTop = subset(allKeywordsFrenquency, Freq > 1400)
mykeywordsOneRepettition = subset(allKeywordsFrenquency, Freq == 1)
write.table(mykeywordsTop, "../../Results/mykeywordsTop.csv", row.names = F, quote=F)
write.table(mykeywordsOneRepettition, "../../Results/mykeywordsRemoveOneRepettition.csv", row.names = F, quote=F)
