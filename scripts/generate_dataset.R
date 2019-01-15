# The script file contains utility methods used for generation of mldr dataset

#The minimum number of documents to be considered
min_doc_no<- 100

# The method generates clean preprocessed text
# @input:
# content -> content of the acquis file
# @returns: list of preprocessed text 
generate_clean_text <- function(content) {
  
  #content comprises labels+text, therefore number of documents is half the length of content
  max_doc_no <- length(content) / 2
  
  doc_no <-
    ifelse((
      exists("doc_number") &&
        doc_number >= min_doc_no &&  #minimum number of documents should be min_doc_no
        doc_number > batch_number &&  #minimum number of documents should be greater than batch no for classification
        doc_number <= max_doc_no  #maximum number of documents should be less than max_doc_no
    ),
    doc_number, min_doc_no )
  
  text_content_list <- 
    # every even line contains text
    content[seq (2, doc_no * 2, 2)] %>% 
    # preprocess text
    get_clean_content() 
  
  text_content_list
}

# The method generates clean preprocessed label names
# @input:
# content -> content of the acquis file
# @returns: list of preprocessed label names
generate_clean_labels <- function(content) {
  
  #content comprises labels+text, therefore number of documents is half the length of content
  max_doc_no <- length(content) / 2
  
  doc_no <-
    ifelse((
      exists("doc_number") &&
        doc_number >= min_doc_no &&  #minimum number of documents should be min_doc_no
        doc_number > batch_number &&  #minimum number of documents should be greater than batch no for classification
        doc_number <= max_doc_no  #maximum number of documents should be less than max_doc_no
    ),
    doc_number, min_doc_no )
  
  label_list <- 
    # every odd line contains text
    content[seq (1, doc_no * 2, 2)] %>%
    # split every line on # to remove doc-id
    strsplit("#") %>%
    # get only label-ids
    sapply("[[", 1) %>%
    # trim on extra space
    trimws() %>%
    # get label names for the label-ids
    get_label_name_list()
  
  label_list
}

# The method generates mldr dataset (arff and xml files)
# @input:
# is_batch -> flag whether to create to batch of datasets
# text_list -> list of text
# label_list -> list of label names list
# tfidf_flag -> flag whether to generate tfidf dataset
#incidence_flag -> flag whether to generate incidence dataset
# @returns: nothing but generates arff and xml files
generate_dataset <-
  function(is_batch,
           text_list,
           label_list,
           tfidf_flag,
           incidence_flag) {
    if (is_batch) {
      iteration_num <-
        ifelse((exists("batch_number") &&
                  batch_number > 2), batch_number, 2)
    } else {
      iteration_num <- 1
    }
    
    #content comprises labels+text, therefore number of documents is half the length of content
    max_doc_no <- length(content) / 2
    
    doc_no <-
      ifelse((
        exists("doc_number") &&
          doc_number >= min_doc_no &&
          #minimum number of documents should be min_doc_no
          doc_number > batch_number &&
          #minimum number of documents should be greater than batch no for classification
          doc_number <= max_doc_no  #maximum number of documents should be less than max_doc_no
      ),
      doc_number,
      min_doc_no
      )
    
    sample_size <-
      doc_no / iteration_num #number of documents per batch
    
    #create mldr xml for labels from the given 'labelFile'
    
    if(!file.exists(labelFile))
      stop(paste("XML file",fileName,"not found!"))
    
    label_names <-
      xmlParse(labelFile) %>% xpathApply("//LIBELLE", xmlValue) %>% get_clean_label()
    xml_root = newXMLNode("labels")
    
    for (index in 1:length(label_names)) {
      newXMLNode("label",
                 attrs = c(name = label_names[index]),
                 parent = xml_root)
    }
    
    #initialise variables
    offset <- 0
    start_text <- 0
    start_label <- 0
    end <- 1
    
    #generate datasets
    for (index in 1:iteration_num) {
      start_text <- offset + 2
      start_label <- offset + 1
      end <- index * sample_size * 2
      
      offset <- offset + (sample_size * 2)
      
      #create a corpus from text
      text_corpus <- text_list %>%
        VectorSource()  %>%
        VCorpus()
      #create a corpus from labels
      label_corpus <- label_list %>%
        VectorSource()  %>%
        VCorpus()
      #create a dtm from label corpus
      dtm_labels <-
        DocumentTermMatrix(label_corpus, control = list(weight = weightTfIdf))
      
      if (tfidf_flag == TRUE) {
        #create a dtm from text corpus
        dtm_tfidf <- text_corpus %>%
          DocumentTermMatrix(control = list(
            wordLengths = c(3, Inf),
            # keeps words having atleast 3 characters
            weighting = function(x)
              weightTfIdf(x, normalize = FALSE) ,
            #generate tfidf values
            stopwords = TRUE # remove stopwords
          ))  %>%
          # reduce the dtm matrix sparsity, maximum- 99%
          removeSparseTerms(0.99)
        
        # column bind label and text dtms to create arff file
        dtm_tfidf <- cbind(dtm_tfidf, dtm_labels)
        #generate ARFF file from dtm
        generate_ARFF(dtm_tfidf, paste(tfidfFileName, index, ".arff", sep = ""))
        #generate mldr xml file
        saveXML(xml_root, file = paste(tfidfFileName, index, ".xml", sep = ""))
      }
      if (incidence_flag == TRUE) {
        #create a dtm from text corpus
        dtm_incidence <-  text_corpus %>%
          tm_map(content_transformer(uniqueWords)) %>% #get unique words to get term incidence
          DocumentTermMatrix(control = list(
            wordLengths = c(3, Inf),
            # keeps words having atleast 3 characters
            weight = weightBin ,
            stopwords = TRUE
          )) %>%
          # reduce the dtm matrix sparsity, maximum- 99%
          removeSparseTerms(0.99)
        
        # column bind label and text dtms to create arff file
        dtm_incidence <- cbind(dtm_incidence, dtm_labels)
        #generate ARFF file from dtm
        generate_ARFF(dtm_incidence,
                      paste(incFileName, index, ".arff", sep = ""))
        #generate mldr xml file
        saveXML(xml_root, file = paste(incFileName, index, ".xml", sep = ""))
      }
    }
    print(paste("Generated arrf and XML files"))
  }

# The method generates arff file
# @input:
# dtm -> document term matrix which needs to be stored as arff file
#arff_name -> name of the arff file
# @returns: nothing but generates an arff file
generate_ARFF <- function(dtm, arff_name) {
  
  
  
  #writes an arff file
  write.arff(dtm, file = arff_name , eol = "\n")
  #retrieves arff file
  conn <- file(arff_name, open = "r")
  readLines(conn)  %>%
  {
    #need to convert labels (attributes ending with _) from numeric to nominal value {0,1}
    gsub("_' numeric", "_' {0,1}", .)
  }  %>%
    #write the final arff
    write(file = arff_name)
  close.connection(conn)
}

