init("english")
#init("german")

connection <- fileName  %>% file(open="r")
raw_text_char <- connection %>% readLines()
close.connection(connection)

#text_content_list <- raw_text_char[seq (2, 100,2)]
text_content_list <- raw_text_char[seq (2, length(raw_text_char),2)]
#class_labels_list <- raw_text_char[seq (1, 100,2)] %>%
class_labels_list <- raw_text_char[seq (1, length(raw_text_char),2)] %>%
strsplit("#") %>%
sapply("[[", 1) %>%
trimws() %>%
get_label_name_list()

corpus <- text_content_list %>%
get_clean_content()  %>%
VectorSource()  %>%
VCorpus()

dtm_tfidf <- corpus %>% DocumentTermMatrix(control = list(wordLengths = c(3, Inf), weighting = function(x) weightTfIdf(x, normalize = FALSE) ,stopwords = TRUE))  %>%
removeSparseTerms(0.99)

dtm_incidence <-  corpus %>%
tm_map( content_transformer(uniqueWords)) %>%
DocumentTermMatrix(control=list(wordLengths = c(3, Inf), weight=weightBin ,stopwords = TRUE)) %>%
removeSparseTerms(0.99)

dtm_labels <- class_labels_list %>% VCorpus(VectorSource())  %>%
DocumentTermMatrix(control=list(weight=weightTfIdf))

dtm_tfidf <- cbind(dtm_tfidf,dtm_labels)
dtm_incidence <- cbind(dtm_incidence,dtm_labels)

generate_ARFF(dtm_incidence, incArffFileName)
generate_ARFF(dtm_tfidf, tfidfArffFileName)

label_names <- getCleanLabel(xpathApply(desc_xml, "//LIBELLE", xmlValue) )

xml_root = newXMLNode("labels")
for ( i in 1:length(label_names) ){
  newXMLNode("label", attrs=c(name=label_names[i]), parent=xml_root)
}
saveXML(xml_root,file="output/EN.xml")

