init("english")
#init("german")

connection <- fileName  %>% file(open="r")
raw_text_char <- connection %>% readLines()
close.connection(connection)

#for a single batch change sample_size to length(raw_text_char)
#batch of 1000 documents
sample_size <- 1000 
offset <- 0
iteration_no <- 24000/sample_size

for(index in 1 :iteration_no) {

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

generate_ARFF(dtm_incidence, paste(incArffFileName,index,arff, sep = ""))
generate_ARFF(dtm_tfidf, paste(tfidfArffFileName,index,arff, sep = ""))

}


label_names <- xmlParse(labelFile) %>% xpathApply( "//LIBELLE", xmlValue) %>% get_clean_label()
xml_root = newXMLNode("labels")
for ( i in 1:length(label_names) ){
  newXMLNode("label", attrs=c(name=label_names[i]), parent=xml_root)
}
saveXML(xml_root,file=XMLFileName)

