library(tm)
library(wordcloud)
library(memoise)
library(readxl)

# My list of data to be illustrated

key = readLines("./key.txt.gz", encoding="UTF-8")
campaign = readLines("./campaign.txt.gz", encoding="UTF-8")
parameters <<- list("Keyword" = "key",
               "Campaign" = "campaign")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(parameter) {
 # check for the name in the list
  if (!(parameter %in% parameters))
    stop("Unknown parameter")
  
  text <- readLines(sprintf("./%s.txt.gz", parameter),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})
