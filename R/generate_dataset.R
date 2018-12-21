init("english")
#init("german")

connection <- fileName  %>% file(open="r")
raw_text_char <- connection %>% readLines()
close.connection(connection)

#temp_text_content_list <- raw_text_char[seq (2, 100,2)]
temp_text_content_list <- raw_text_char[seq (2, length(raw_text_char),2)]
#temp_class_labels_list <- raw_text_char[seq (1, 100,2)] %>%
temp_class_labels_list <- raw_text_char[seq (1, length(raw_text_char),2)] %>%
strsplit("#") %>%
sapply("[[", 1) %>%
trimws()

temp_class_labels_list <- get_label_name_list(temp_class_labels_list)

corpus <- temp_text_content_list %>%
get_clean_content()  %>%
VectorSource()  %>%
VCorpus()

dtm <- corpus %>% DocumentTermMatrix(control = list(wordLengths = c(3, Inf), weighting = function(x) weightTfIdf(x, normalize = FALSE) ,stopwords = TRUE))  %>%
removeSparseTerms(0.99)

dtm_incidence <-  corpus %>%
tm_map( content_transformer(uniqueWords)) %>%
DocumentTermMatrix(control=list(wordLengths = c(3, Inf), weight=weightBin ,stopwords = TRUE)) %>%
removeSparseTerms(0.99)

label_corpus <- VCorpus(VectorSource(temp_class_labels_list))
dtm_labels <- DocumentTermMatrix(label_corpus,control=list(weight=weightTfIdf))
temp_dtm <- cbind(dtm,dtm_labels)
temp_dtm_inc <- cbind(dtm_incidence,dtm_labels)

generate_ARFF(temp_dtm, incArffFileName)
generate_ARFF(temp_dtm, tfidfArffFileName)
