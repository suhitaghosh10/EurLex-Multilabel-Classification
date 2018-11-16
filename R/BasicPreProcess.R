library(stringr)
# Read a txt file, named "mtcars.txt"
data_short <- read.delim("C:\\Users\\com\\Documents\\eurolex\\EurLexClassification\\data\\acquis\\acquis.cf", stringsAsFactors = TRUE)
data_full <- read.delim("acquis.cf", header=FALSE)
even_indexes<-seq(2,9482,2)
odd_indexes<-seq(1,9481,2)
labels <- data.frame(data_full[odd_indexes,1])
rawData <- data.frame(data_full[even_indexes,1])
colnames(labels)[1] <- "labels"
colnames(rawData)[1] <- "text"

#rownames(labels)
#rownames(rawData)

#write.csv(labels,'labels.csv')
#write.csv(rawData,'rawData.csv')

#labelsData <- read.csv(file="labels.csv", header=TRUE,  stringsAsFactors = FALSE)
splitData <- data.frame(labels$labels, str_split_fixed(labels$labels, "#", 2))
df_data <- data.frame(splitData, str_split_fixed(splitData$X1, " ", 10), rawData)

write.csv(df_data,'df_data.csv')

