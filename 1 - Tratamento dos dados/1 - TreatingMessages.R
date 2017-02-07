# Treat gross messages of github
# Andrey Menezes, 15/10/2015

library(qdap)

data = read.csv("../../ElasticSearch-Discussions.csv")[,c(7,8,10)]

# Remove empty comments, default messages and discussions with only one comment
data = subset( data, Comentario != "" )
data = subset( data, word_count(Comentario) > 6 )
data = data[ as.logical(ave(1:nrow(data), data$Identificador, FUN=function(x) length(x) > 1)), ]

# Group messages by identify
DataAggregate = with(data, aggregate(Comentario, list(Titulo, Identificador), paste, collapse=" "))
colnames(DataAggregate) = c("Tittle", "Identify", "Comment")

write.csv2(DataAggregate, "../../Results/DiscussionsAggregateAndClean.csv", row.names=F)


################ Extract Sample of the data for manual classification ###############
#####################################################################################

sample = DataAggregate[sample(1:nrow(DataAggregate), 150),]
write.csv2(sample, "../../sample.csv", row.names=F)
