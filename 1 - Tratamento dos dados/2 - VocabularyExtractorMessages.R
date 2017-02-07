# Extracts the vocabulary of discussions of developers, creating a root list of words for each message.
# Andrey Menezes, 21/09/2015

library(tm)
library(qdap)

data = read.csv2("../../Results/DiscussionsAggregateAndClean.csv")
keywordsJava = read.table("../../../JavaKeywords.txt", sep="\t")
keywordsJava = as.vector(as.matrix(keywordsJava))

# Remove English stop words, spaces, punctuation and numbers
data$TreatedMessage = gsub("[._-]", " ", as.character(data$Comment))
data$TreatedMessage = gsub("/", " ", as.character(data$TreatedMessage))
data$TreatedMessage = gsub("'\'", " ", as.character(data$TreatedMessage))
data$TreatedMessage = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", data$TreatedMessage)
data$TreatedMessage = gsub("[[:punct:]]", " ", data$TreatedMessage)

data$TreatedMessage = iconv(data$TreatedMessage, to="ASCII//TRANSLIT")
data$TreatedMessage = tolower(data$TreatedMessage)
data$TreatedMessage = removeWords(data$TreatedMessage, stopwords("en"))
data$TreatedMessage = removeWords(data$TreatedMessage, keywordsJava)
data$TreatedMessage = gsub('"', ' ', as.character(data$TreatedMessage))
data$TreatedMessage = gsub("'", ' ', as.character(data$TreatedMessage))
data$TreatedMessage = removeNumbers(data$TreatedMessage)

# Remove others words
cleanData = data
cleanData$TreatedMessage = stripWhitespace(as.character(cleanData$TreatedMessage))

# Extract the roots of words and eliminate roots with less 3 characters
cleanData$TreatedMessage = stemmer(cleanData$TreatedMessage, warn = F)
cleanData$TreatedMessage = gsub("\\b[a-zA-Z0-9]{1,2}\\b", "", cleanData$TreatedMessage)
cleanData$TreatedMessage = tolower(cleanData$TreatedMessage)
cleanData$TreatedMessage = stripWhitespace(as.character(cleanData$TreatedMessage))
cleanData = na.omit(cleanData)

# Save new table
write.csv2(cleanData, "../../Results/DiscussionsWithRoots.csv", row.names = F)