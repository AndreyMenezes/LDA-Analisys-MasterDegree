# Extracts the vocabulary of source code, creating root list of words for each class.
# Andrey Menezes, 21/09/2015

library(tm)
library(qdap)

# Keywords and especify a default header size
keywordsJava = read.table("../../../JavaKeywords.txt", sep="\t")
keywordsJava = as.vector(as.matrix(keywordsJava))
header = 1:18 # Header of each class

# Read files of source code
temp = list.files(path = "../../elasticsearch-master/", pattern="*.java", recursive = TRUE)
temp = grep(pattern="*-info.java", temp, value=T, invert=T)

dir.create(file.path(getwd(), "../../Results/SourceCodeStem"))

for (i in 1:length(temp)) {
  assign(temp[i], read.table(paste("../../elasticsearch-master/", temp[i], sep=""), sep="\t", na.strings=c("", "NA"), strip.white=T, allowEscapes=T, skipNul = T, stringsAsFactors=F))
  data = na.omit(get(temp[i]))
  data = as.vector(as.matrix(data))
  if (length(data) > length(header)) {
    if (data[length(header)] == "*/") {
      data = data[-c(header)]
      
      # Text treatment (spaces, punctuation, English stop words, keywords)
      data = grep(pattern="^import", data, value=T, invert=T)
      data = grep(pattern="^package", data, value=T, invert=T)
      data = gsub("[._-]", " ", as.character(data))
      data = gsub("/", " ", as.character(data))
      data = gsub("'\'", " ", as.character(data))
      data = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", data)
      data = gsub("[[:punct:]]", " ", data)
      data = iconv(data, to="ASCII//TRANSLIT")
      data = tolower(data)
      data = removeWords(data, stopwords("en"))
      data = removeWords(data, keywordsJava)
      data = gsub('"', ' ', as.character(data))
      data = gsub("'", ' ', as.character(data))
      data = removeNumbers(data)
      data = stripWhitespace(as.character(data))
      data = paste(data, sep = " ", collapse = ' ')
      
      # Extract the roots of words and eliminate roots with less 3 characters
      stem = stemmer(data, warn = F)
      stem = stem[ nchar(stem) %in% 3:max(nchar(stem)) ]
      stem = tolower(stem)
      
      # Remove little class
      if (length(stem) > 8 ) {
        dir = as.character(temp[i])
        dir = strsplit(dir, split = "/")[[1]]
        name = paste(dir[length(dir)-1], dir[length(dir)], sep="-")
        
        write.table(stem, paste("../../Results/SourceCodeStem/", paste(name, ".csv", sep=""), sep=""), row.names = F, col.names = F)
      }
    }
  }
}


# Remove noises and create a complete table with all source code
temp = list.files(path = "../../Results/SourceCodeStem/", pattern="*.csv")
TableAllSourceCodeVocabulary = data.frame("Code" = 0)
for (i in 1:length(temp)) {
  assign(temp[i], read.table(paste("../../Results/SourceCodeStem/", temp[i], sep="")))
  SourceCodeVocabulary = as.vector(as.matrix(get(temp[i])))
  SourceCodeVocabulary = paste(SourceCodeVocabulary, sep = " ", collapse = ' ')
  SourceCodeVocabulary = stripWhitespace(as.character(SourceCodeVocabulary))
  TableAllSourceCodeVocabulary = rbind(TableAllSourceCodeVocabulary, SourceCodeVocabulary)
}

# Save the results
TableAllSourceCodeVocabulary = as.matrix(TableAllSourceCodeVocabulary[-1,])
TableAllSourceCodeVocabulary = cbind(TableAllSourceCodeVocabulary, as.matrix(temp))
colnames(TableAllSourceCodeVocabulary) = c("Code", "Class")
write.csv2(TableAllSourceCodeVocabulary, file="../../Results/AllSourceCode.csv", row.names = F)
