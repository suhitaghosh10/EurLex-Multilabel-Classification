if(!exists("raw_text_char")){
connection <- fileName  %>% file(open="r")
raw_text_char <- connection %>% readLines(encoding = "UTF-8")
close.connection(connection)
}

#for a single batch change sample_size to length(raw_text_char)
#batch of 1000 documents
batch_no <- ifelse((exists("batch_number") && batch_number>2),batch_number, 2)
doc_no <- ifelse((exists("doc_number") && doc_number>100),doc_number, 100)

sample_size <- doc_no/batch_no
offset <- 0
start_text <- 0
start_label <- 0
end <- 1

for(index in 1 :batch_no) {

start_text <- offset +2
start_label <- offset +1
end <- index * sample_size *2
text_content_list <- raw_text_char[seq (start_text, end,2)]
#text_content_list <- raw_text_char[seq (2, length(raw_text_char),2)]
class_labels_list <- raw_text_char[seq (start_label, end,2)] %>%
#class_labels_list <- raw_text_char[seq (1, length(raw_text_char),2)] %>%
strsplit("#") %>%
sapply("[[", 1) %>%
trimws() %>%
get_label_name_list()

offset <- offset+(sample_size *2)

text_corpus <- text_content_list %>%
get_clean_content()  %>%
VectorSource()  %>%
VCorpus()

label_corpus <- class_labels_list %>%
 VectorSource()  %>%
 VCorpus()


dtm_tfidf <- text_corpus %>%
 DocumentTermMatrix(control = list(wordLengths = c(3, Inf), weighting = function(x) weightTfIdf(x, normalize = FALSE) ,stopwords = TRUE))  %>%
removeSparseTerms(0.99)

dtm_incidence <-  text_corpus %>%
tm_map( content_transformer(uniqueWords)) %>%
DocumentTermMatrix(control=list(wordLengths = c(3, Inf), weight=weightBin ,stopwords = TRUE)) %>%
removeSparseTerms(0.99)

dtm_labels <- DocumentTermMatrix(label_corpus, control=list(weight=weightTfIdf))


dtm_tfidf <- cbind(dtm_tfidf,dtm_labels)
dtm_incidence <- cbind(dtm_incidence,dtm_labels)

generate_ARFF(dtm_incidence, paste(incFileName,index,".arff", sep = ""))
generate_ARFF(dtm_tfidf, paste(tfidfFileName,index,".arff", sep = ""))
print(paste("Generated arrf files for batch No:",index))

}


print("Generating XML files")

label_names <- xmlParse(labelFile) %>% xpathApply( "//LIBELLE", xmlValue) %>% get_clean_label()
xml_root = newXMLNode("labels")
for ( i in 1:length(label_names) ){
  newXMLNode("label", attrs=c(name=label_names[i]), parent=xml_root)
}
xmlFileName = paste(lang,".xml",sep = "")
saveXML(xml_root,file=xmlFileName)

for(index in 1:batch_no){
  
  current_xml_name <- paste(tfidfFileName,index,".xml",sep="")
  file.create(current_xml_name)
  file.copy(xmlFileName, current_xml_name, overwrite = TRUE)
  
  current_xml_name <- paste(incFileName,index,".xml",sep="")
  file.create(current_xml_name)
  file.copy(xmlFileName, current_xml_name, overwrite = TRUE)
  
}

print("Dataset generation done!")

