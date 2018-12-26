get_clean_content <- function(content) {

if(lang == "english"){
  stopwords <-
    c('gt','notext','p','lt','aka','oj','n','a','eec','article','directive','shall','follow')  %>%
    append(stopwords(lang))
  }
  else if(lang == "german"){
  stopwords <-
    c('gt','notext','p','lt','aka','oj','n','a','eec','article','directive','soll','folgen')  %>%
    append(stopwords(lang))
  }
  clean_content <- content  %>%
    replace_html(replacement = " ") %>%
	{
      gsub('-', '', .)
    } %>%
    {
      gsub('[[:punct:] ]+', ' ', .)
    } %>%
    {
      gsub("\\b[IVXLCDM]+\\b", " ", .)
    } %>%
    tolower()  
	if(lang == "english")
    clean_content <- lemmatize_strings(clean_content)
	else{
	clean_content <- stri_trans_general(clean_content, "Latin-ASCII")
	lemmata <- c()
    for(index in 1: length(clean_content)){
		lemmata[index] <- mclapply(clean_content[[index]], FUN = function(x) generate_lemma_per_document(x, index))
        }
    clean_content <- sapply( lemmata, paste0, collapse=" ")
	}
	
	clean_content <- clean_content %>%
    {
      gsub("\\w*[0-9]+\\w*\\s*", "", .)
    } %>%
    removeWords(words = stopwords)  %>%
    replace_non_ascii(replacement = " ")  %>%
    trimws()
  
  clean_content
}

get_clean_label <- function(content) {
  content <- content %>%
  {
    gsub("\\s|\\.|\\[|\\]|-|\\(|\\)", "_", .)
  } %>%
  {
    gsub("'", "", .)
  } %>%
    replace_non_ascii(replacement = " ") %>%
    tolower()  %>%
    paste("tag", sep = "_")
  
  content
}

get_label_name <- function(label_id, label_id_name_df) {
  label_name = label_id_name_df[label_id_name_df$did == label_id, 2]
  if (length(label_name) > 0)
    return(as.character(label_name))
  else
    return(paste(label_id, "tag" , sep = "_"))
}

generate_ARFF <- function(dtm, arff_name) {
  write.arff(dtm, file = arff_name , eol = "\n")
  conn <- file(arff_name, open = "r")
  readLines(conn)  %>%
  {
    gsub("_tag' numeric", "_tag' {0,1}", .)
  }  %>%
    write(file = arff_name)
  close.connection(conn)
}

init <- function(language) {
  arff<<- ".arff"
  if (language == "english") {
  print("english chosen")
    lang <<- "english"
    fileName <<- "data/english/acquis.cf"
    tfidfArffFileName <<- "output/tfidf_EN"
    incArffFileName <<- "output/inc_EN"
    XMLFileName <<- "output/EN.xml"
    labelFile <<- "data/english/desc_en.xml"
  }
  else if (language == "german") {
  print("german chosen")
    lang <<- "german"
    fileName <<- "data/german/acquis_german.cf"
    tfidfArffFileName <<- "output/tfidf_DE"
    incArffFileName <<- "output/inc_DE"
    tfidfXMLFileName <<- "output/tfidf_DE.xml"
    XMLFileName <<- "output/DE.xml"
    labelFile <<- "data/german/desc_de.xml"
    model_file <<- "output/german-gsd-ud-2.3-181115.udpipe"
    if (!file.exists(model_file))
    {
      model <<- udpipe_download_model(language = "german", model_dir = "output/")
    } else {
      model <<- udpipe_load_model(model_file)
    }
  }
}

generate_lemma_per_document <- function(content, doc_id) {
print(doc_id)
  x <- as.data.table(udpipe_annotate(model, x = content, doc_id = doc_id,  tagger = "default",
                                     parser = "none"))
        lemma <- sapply(x$lemma, paste, collapse = " ")
  return(lemma)
}


get_label_name_list <- function(label_id_list){

desc_xml <- xmlParse(labelFile)

xm_df <- data.frame(
did=sapply(desc_xml["//DESCRIPTEUR_ID"], xmlValue),dname=sapply(desc_xml["//LIBELLE"], xmlValue)
)
xm_df$dname <- get_clean_label(xm_df$dname)

label_name_list <- label_id_list %>%
lapply(function(labelsets) strsplit(labelsets, " ")) %>%
sapply("[[", 1) %>%
lapply( function(label_id_array) lapply(label_id_array, function(label_id) get_label_name(label_id, xm_df))) %>%
lapply(function(label) paste(label, sep = " "))

label_name_list 
}


uniqueWords <- function(text) {
return(paste(unique(strsplit(text, " ")[[1]]), collapse = ' '))
}