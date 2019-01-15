# EurLexClassification

A single text document often has multiple semantic aspects. A single news article related to politics may have aspects related to trade, technology and defense. In the perspective of machine learning, we can interpret the various aspects as multiple class labels of an instance (a document). In this project we explore a public multi labelled legal text dataset that has been manually annotated over a decade. It contains laws related to the European Union, including treaties, legislation, case-law and legislative proposals, in 24 different languages. This is popularly known as the EUR-Lex dataset containing about twenty thousand documents and seven thousand labels. A skewed distribution of multiple labels per document, along with existence of the same data in multiple languages, makes this data set an interesting proposition. Few publications have used this dataset; the ones that have used this have reported relatively poor values in the range of 40%.

# Website URL
https://suhitaghosh10.github.io/EurLexClassification

# Screencast URL
https://www.youtube.com/watch?v=3ELi5mLlzgM

# Data URL
The English and German data is present at the *data* folder.
Please note if you are downloading the code from Web, data would not get downloaded (as it exceeds the limit provided by Github). Please perform the following:  
* Download the data from the links below:
  - [English](https://drive.google.com/drive/folders/1F5HznSWxlZno4iVQSzsXHPhCl-cD644t?usp=sharing)
  - [German](https://drive.google.com/drive/folders/1TncK4erU2ZJc9v1_-RvwM6fDmkkUYKAk?usp=sharing)
* After downloading the files:
  - English -> Place the downloaded data for English (acquis.cf, desc_en.xml) at data/english
  - German -> Place the downloaded data for German (acquis.cf, desc_de.xml) at data/german

# Repository structure
The content in the repository has been stored in the following manner:
* configuration -> The configuration files used for the project
* data -> The data used for the project
* docs -> The documents for the website (markdown, html, site xml, pictures)
* report -> R Process notebook (markdown + html)
* scripts -> R scripts

# Process Notebook configuration
Since the dataset comprises around 25,000 documents, it will take long time to execute the process notebook over the entire corpus. For trying out the process notebook on a small sample, some parameters have been defined in the file *configuration/config.R*. Please adjust the following parameters before running the notebook.
- doc_number (the number of documents to be considered)
- batch_number (the number of batches of datasets to be generated for classification)

