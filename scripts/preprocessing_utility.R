# The script file contains utility methods used for preprocessing

# The method performs preprocessing on text content
# @input:
# content -> text content from acquis file
# @returns: list of clean/preprocessed text
get_clean_content <- function(content) {
  clean_content <- content  %>%
    #replace html tags with space
    replace_html(replacement = " ") %>%
    #remove hyphens among words
    {
      gsub('-', '', .)
    } %>%
    #replace punctuations with space
    {
      gsub('[[:punct:] ]+', ' ', .)
    } %>%
    #replace roman numerals with space
    {
      gsub("\\b[IVXLCDM]+\\b", " ", .)
    } %>%
    #transform to lower case
    tolower()
  
  if (lang == "english") #lemmatization for english corpus
    clean_content <- lemmatize_strings(clean_content)
  else{  #lemmatization for german corpus using udpipe model
    lemmata <- c()
    if (!file.exists(model_file))
    {
      udpipe_download_model(language = "german", model_dir = "../output/")
      model <- udpipe_load_model(model_file)
      print("german udpipe model downloaded and loaded")
    } else {
      model <- udpipe_load_model(model_file)
      print("german udpipe model loaded")
    }
    #generate lemma document-wise, as udpipe throws a weird error if number of words exceed by a number
    for (index in 1:length(clean_content)) {
      lemmata[index] <-
        mclapply(
          clean_content[[index]],
          #generate list of lemma for each document
          FUN = function(x)
            generate_lemma_per_document(x, index, model)
        )
    }
    #append the lemmata for each document using space
    clean_content <- sapply(lemmata, paste0, collapse = " ")
  }
  
  #English law and EurLex related stopwords
  if (lang == "english") {
    stopwords <-
      c(
        "gt",
        "notext",
        "p",
        "lt",
        "aka",
        "oj",
        "n",
        "a",
        "eec",
        "article",
        "directive",
        "follow",
        "accordance",
        "chairman",
        "necessary",
        "comply",
        "reference",
        "commission",
        "opinion",
        "decision",
        "annex",
        "refer",
        "member",
        "european",
        "treaty",
        "throughout",
        "regulation",
        "particular",
        "thereof",
        "community",
        "committee",
        "measure",
        "parliament",
        "regard",
        "amend",
        "procedure",
        "administrative",
        "procedure",
        "publication",
        "month",
        "date",
        "year",
        "enter",
        "force",
        "ensure",
        "authority",
        "take",
        "council",
        "act",
        "within",
        "national",
        "law",
        "main",
        "provision",
        "mention",
        "approve",
        "certain",
        "whereas",
        "eea",
        "also",
        "apply",
        "may",
        "can",
        "will",
        "shall",
        "require",
        "paragraph",
        "subparagraph",
        "official",
        "journal",
        "ec",
        "b",
        "s",
        "c",
        "e",
        "na"
      )  %>%
      append(stopwords(lang))
  }
  #German law and EurLex related stopwords
  else if (lang == "german") {
    stopwords <-
      c(
        "gt",
        "keintext",
        "p",
        "lt",
        "aka",
        "oj",
        "n",
        "a",
        "eec",
        "artikel",
        "artikels",
        "directive",
        "folgend",
        "übereinstimmung",
        "vorsitzender",
        "notwendig",
        "einhalten",
        "bezug",
        "kommission",
        "stellungnahme",
        "entscheidung",
        "anlage",
        "bezug",
        "mitglied",
        "europäer",
        "vertrag",
        "regulierung",
        "insbesondere",
        "davon",
        "gemeinschaft",
        "ausschuss",
        "maßnahme",
        "parlament",
        "betrachten",
        "ändern",
        "verfahren",
        "administrativ",
        "verfahren",
        "veröffentlichung",
        "monat",
        "datum",
        "jahr",
        "eingeben",
        "erzwingen",
        "gewähren",
        "autorität",
        "nehmen",
        "rat",
        "handeln",
        "innerhalb",
        "national",
        "recht",
        "haupt",
        "vorschrift",
        "erwähnen",
        "genehmigen",
        "sicher",
        "wobei",
        "eea",
        "auch",
        "bewerben",
        "kann",
        "wird",
        "sollen",
        "erfordern",
        "absatz",
        "unterabsatz",
        "offiziell",
        "journal",
        "ec",
        "b",
        "s",
        "c",
        "e",
        "NA",
        "ev",
        "fur",
        "mitgliedstaat",
        "agentur",
        "europaisch",
        "evs",
        "beirat",
        "verordnung",
        "jed",
        "mussen"
      )  %>%
      append(stopwords(lang))
  }
  clean_content <- clean_content %>%
    #remove numbers
    removeNumbers() %>%
    #remove stopwords
    removeWords(words = stopwords)  %>%
    #remove non ascii terms
    replace_non_ascii(replacement = " ")  %>%
    #remove extra space
    trimws()
  
  clean_content
}

# The method generates a list of clean label names from provided label names
# @input:
# labels -> a list of label
# @returns: list of clean/preprocessed text
get_clean_label <- function(labels) {
  
  labels <- labels %>%
  {
    #replaces characters mentioned in regex with hyphen
    gsub("\\s|\\.|\\[|\\]|-|\\(|\\)", "_", .)
  } %>%
  {
    #removes apostrophes
    gsub("'", "", .)
  } %>%
    # replaces non ascii characters
    replace_non_ascii(replacement = " ") %>%
    #transform to lower case
    tolower()  %>%
    #appends hyphen at the end
    paste("_", sep = "")
  
  labels
}

# The method generates label name against a label-id
# @input:
# label_id -> label-id
#label_id_name_df -> dataframe containing mapping between label-id and name
# @returns: label name
get_label_name <- function(label_id, label_id_name_df) {
  #gets the label name against the label id from dataframe
  label_name = label_id_name_df[label_id_name_df$did == label_id, 2]
  if (length(label_name) > 0)
    return(as.character(label_name))
  else
    return(paste(label_id, "_" , sep = "")) #returns label_id appended with hyphen if no mapping found
}

# The method generates lemma per document for udpipe model
# @input:
# content -> a character vector in UTF-8 encoding where each element of the character vector contains text to be tokenized
#doc_id -> document sequence number
# @returns: lemmata
generate_lemma_per_document <- function(content, doc_id, model) {
  
  #print(paste("generating lemma for doc-", doc_id))
  #generates a data table containing lemma and original words
  annotated_data_table <-
    udpipe_annotate(
      model,
      x = content,
      doc_id = doc_id,
      tagger = "default",
      parser = "none"
    ) %>% as.data.table()
  #retrieve lemma element
  lemma <- sapply(annotated_data_table$lemma, paste, collapse = " ")
  
  lemma
}

# The method returns text containing unique words from the given text
# @input:
# text -> a character vector
# @returns: a character vector containing unique words
  uniqueWords <- function(text) {
    return(paste(unique(strsplit(text, " ")[[1]]), collapse = ' '))
  }
 
# The method returns list label-name list from a list of label-id list
# @input:
# label_id_list -> list of label-id
# @returns:  list of label-names
  get_label_name_list <- function(label_id_list){
    
    
    #parses given xml file containing label-id and name mapping
    desc_xml <- xmlParse(labelFile)
    
    #generate a datframe containing label-id and names
    xm_df <-
      data.frame(did = sapply(desc_xml["//DESCRIPTEUR_ID"], xmlValue),
                 dname = sapply(desc_xml["//LIBELLE"], xmlValue))
    #get clean label
    xm_df$dname <- get_clean_label(xm_df$dname)
    #Every element of label_id_list contains a list of label-ids-> 134 456
    label_name_list <- label_id_list %>%
      lapply(function(labelsets)
        #apply split on every element of list to get individual label-id
        strsplit(labelsets, " ")) %>%
      #retrieve the list resulted from splitting
      sapply("[[", 1) %>%
      #apply function get_label_name on every element of the list to get label name
      lapply(function(label_id_array)
        lapply(label_id_array, function(label_id)
          get_label_name(label_id, xm_df))) %>%
      #apply paste on every element of list to append the label-names -> label1 label2
      lapply(function(label)
        paste(label, sep = " "))
    
    label_name_list
  }
  
