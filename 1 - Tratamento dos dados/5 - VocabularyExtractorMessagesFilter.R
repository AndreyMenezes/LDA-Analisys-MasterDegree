# Extracts the vocabulary of discussions of developers, creating a root list of words for each message.
# Andrey Menezes, 21/09/2015

library(tm)
library(qdap)

data = read.csv2("../../Results/DiscussionsAggregateAndClean.csv")

keywordsJava = read.table("../../../JavaKeywords.txt", sep="\t")
keywordsJava = as.vector(as.matrix(keywordsJava))

mykeywordsTop = read.table("../../Results/mykeywordsTop.csv", header=T, sep=" ")
mykeywordsTop = as.vector(as.matrix(mykeywordsTop[,1]))

mykeywords = read.table("../../Results/mykeywordsRemoveOneRepettition.csv", header=T, sep=" ")
mykeywords = as.vector(as.matrix(mykeywords[,1]))


# Remove English stop words, spaces, punctuation and numbers
data$TreatedMessage = gsub("[._-]", " ", as.character(data$Comment))
data$TreatedMessage = gsub("/", " ", as.character(data$TreatedMessage))
data$TreatedMessage = gsub("'\'", " ", as.character(data$TreatedMessage))
data$TreatedMessage = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", data$TreatedMessage)
data$TreatedMessage = gsub("[[:punct:]]", " ", data$TreatedMessage)

data$TreatedMessage = iconv(data$TreatedMessage,to="ASCII//TRANSLIT")
data$TreatedMessage = tolower(data$TreatedMessage)
data$TreatedMessage = removeWords(data$TreatedMessage, stopwords("en"))
data$TreatedMessage = removeWords(data$TreatedMessage, keywordsJava)
data$TreatedMessage = gsub('"', ' ', as.character(data$TreatedMessage))
data$TreatedMessage = gsub("'", ' ', as.character(data$TreatedMessage))
data$TreatedMessage = removeNumbers(data$TreatedMessage)


########## Part two: Data clean using JavaKeyowrds and limit of top frequency ##########
########################################################################################

# Remove others words
cleanData = data
cleanData$TreatedMessage = removeWords(cleanData$TreatedMessage, mykeywordsTop)
cleanData$TreatedMessage = stripWhitespace(as.character(cleanData$TreatedMessage))

# Remove others words
cleanData$TreatedMessage = stemmer(cleanData$TreatedMessage, warn = F)
cleanData$TreatedMessage = gsub("\\b[a-zA-Z0-9]{1,2}\\b", "", cleanData$TreatedMessage)
cleanData$TreatedMessage = tolower(cleanData$TreatedMessage)
cleanData$TreatedMessage = stripWhitespace(as.character(cleanData$TreatedMessage))

# Save new table
write.csv2(cleanData, "../../Results/DiscussionsWithRootsTopLimit.csv", row.names = F)


######### Part Three: Data clean using JavaKeywords and limit botton frequency ##########
#########################################################################################

# Remove others words
cleanData = data
begin = 1
parts = (length(mykeywords)/50)
end = parts
for (i in 1:49) {
  cleanData$TreatedMessage = removeWords(cleanData$TreatedMessage, mykeywords[begin:end])
  begin = end+1
  end = end + parts
}
cleanData$TreatedMessage = stripWhitespace(as.character(cleanData$TreatedMessage))

# Extract the roots of words and eliminate roots with less 3 characters
cleanData$TreatedMessage = stemmer(cleanData$TreatedMessage, warn = F)
cleanData$TreatedMessage = gsub("\\b[a-zA-Z0-9]{1,2}\\b", "", cleanData$TreatedMessage)
cleanData$TreatedMessage = tolower(cleanData$TreatedMessage)
cleanData$TreatedMessage = stripWhitespace(as.character(cleanData$TreatedMessage))

# Save new table
write.csv2(cleanData, "../../Results/DiscussionsWithRootsRemoveOneRepetittion.csv", row.names = F)


###### Part Four: Data clean using JavaKeywords and limit botton and top frequency #######
##########################################################################################

# Remove others words
cleanData = data
begin = 1
parts = (length(mykeywords)/50)
end = parts
for (i in 1:49) {
  cleanData$TreatedMessage = removeWords(cleanData$TreatedMessage, mykeywords[begin:end])
  begin = end+1
  end = end + parts
}
cleanData$TreatedMessage = removeWords(cleanData$TreatedMessage, mykeywordsTop)
cleanData$TreatedMessage = stripWhitespace(as.character(cleanData$TreatedMessage))

# Extract the roots of words and eliminate roots with less 3 characters
cleanData$TreatedMessage = stemmer(cleanData$TreatedMessage, warn = F)
cleanData$TreatedMessage = gsub("\\b[a-zA-Z0-9]{1,2}\\b", "", cleanData$TreatedMessage)
cleanData$TreatedMessage = tolower(cleanData$TreatedMessage)
cleanData$TreatedMessage = stripWhitespace(as.character(cleanData$TreatedMessage))

# Save new table
write.csv2(cleanData, "../../Results/DiscussionsWithRootsTopAndRemoveOneRepetittion.csv", row.names = F)