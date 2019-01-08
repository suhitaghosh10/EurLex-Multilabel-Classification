get_clean_content <- function(content) {

if(lang == "english"){
  stopwords <-
    c("gt","notext","p","lt","aka","oj","n","a","eec","article","directive","follow", "accordance","chairman","necessary","comply","reference","commission","opinion","decision","annex","refer","member","european","treaty","throughout","regulation","particular","thereof","community","committee","measure","parliament","regard","amend","procedure","administrative","procedure","publication","month","date","year","enter","force","ensure","authority","take","council","act","within","national","law","main","provision","mention","approve","certain","whereas","eea","also","apply","may","can","will","shall","require","paragraph","subparagraph","official","journal","ec","b","s","c","e","na")  %>%
    append(stopwords(lang))
  }
  else if(lang == "german"){
  stopwords <-
    c("gt","keintext","p","lt","aka","oj","n","a","eec","artikel","artikels","directive","folgend", "übereinstimmung", "vorsitzender", "notwendig", "einhalten", "bezug", "kommission", "stellungnahme", "entscheidung", "anlage", "bezug", "mitglied", "europäer", "vertrag","regulierung","insbesondere","davon","gemeinschaft","ausschuss","maßnahme","parlament","betrachten","ändern","verfahren","administrativ","verfahren","veröffentlichung","monat", "datum", "jahr", "eingeben", "erzwingen", "gewähren", "autorität", "nehmen", "rat", "handeln", "innerhalb","national","recht","haupt","vorschrift","erwähnen","genehmigen","sicher","wobei","eea","auch","bewerben","kann","wird", "sollen","erfordern", "absatz", "unterabsatz", "offiziell", "journal","ec","b","s","c","e","NA","ev","fur","mitgliedstaat","agentur","europaisch","evs","beirat","verordnung","jed","mussen")  %>%
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
	lemmata <- c()
	 if (!file.exists(model_file))
    {
      model <<- udpipe_download_model(language = "german", model_dir = "../output/")
	  print("german udpipe model downloaded and loaded")
    } else {
      model <<- udpipe_load_model(model_file)
	  print("german udpipe model loaded")
    }
    for(index in 1: length(clean_content)){
		lemmata[index] <- mclapply(clean_content[[index]], FUN = function(x) generate_lemma_per_document(x, index))
        }
    clean_content <- sapply( lemmata, paste0, collapse=" ")
	}
	
	clean_content <- clean_content %>%
    removeNumbers() %>%
    removeWords(words = stopwords)  %>%
    replace_non_ascii(replacement = " ")  %>%
    trimws()
  
  clean_content
}

get_clean_label <- function(labels) {
  labels <- labels %>%
  {
    gsub("\\s|\\.|\\[|\\]|-|\\(|\\)", "_", .)
  } %>%
  {
    gsub("'", "", .)
  } %>%
    replace_non_ascii(replacement = " ") %>%
    tolower()  %>%
    paste("_", sep = "")
  
  labels
}

get_label_name <- function(label_id, label_id_name_df) {
  label_name = label_id_name_df[label_id_name_df$did == label_id, 2]
  if (length(label_name) > 0)
    return(as.character(label_name))
  else
    return(paste(label_id, "_" , sep = ""))
}

generate_ARFF <- function(dtm, arff_name) {
  write.arff(dtm, file = arff_name , eol = "\n")
  conn <- file(arff_name, open = "r")
  readLines(conn)  %>%
  {
    gsub("_' numeric", "_' {0,1}", .)
  }  %>%
    write(file = arff_name)
  close.connection(conn)
}

generate_lemma_per_document <- function(content, doc_id) {
  print(paste("generating lemma for doc-",doc_id))
  annotated_data_table <-  udpipe_annotate(model, x = content, doc_id = doc_id,  tagger = "default",
                                     parser = "none") %>% as.data.table()
  lemma <- sapply(annotated_data_table$lemma, paste, collapse = " ")
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
