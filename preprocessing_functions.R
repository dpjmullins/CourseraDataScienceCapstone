## Functions

library(readr)
library(tm)

badwords <- read_lines(file = 'unsavoury_language.txt')
word_contractions <- read_csv(file = 'word_contractions.txt')

### Functions to chance specific patterns to blanks or spaces
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
toNothing <- content_transformer(function (x, pattern) gsub(pattern, "", x))
# Remove email id
RemoveEmail <- content_transformer( function(x) {
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
})
## Modify word contractions to be one word for more predictive power
pattern_to_pattern <- content_transformer(function (x, pattern1, pattern2) gsub(pattern1, pattern2, x))


### Function to perform numerous transformations over a text corpus
text_transformer <- function(text_corpus) {
    ## Remove emails
    text_corpus <- tm_map(text_corpus, RemoveEmail)
    ## Convert symbols to whitespace
    text_corpus <- tm_map(text_corpus, toNothing, "[[:punct:]]")
    ## Convert to lower case
    text_corpus <- tm_map(text_corpus, content_transformer(tolower))
    ## Remove numbers
    text_corpus <- tm_map(text_corpus, removeNumbers)
    ### Remove common English stopwords
    #text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))
    ## Remove words with 3 or more repeated letters
    text_corpus <- tm_map(text_corpus, toSpace, "(.)\\1{2,}")
    ## fix word contractions
    for (i in 1:nrow(word_contractions)) {
        text_corpus <- tm_map(text_corpus, pattern_to_pattern, word_contractions[i,1], word_contractions[i,2])
    }
    ### Remove any remaining single letter words
    #text_corpus <- tm_map(text_corpus, toSpace, "\\b(.)\\b")
    ## Remove additional stopwords and badwords
    text_corpus <- tm_map(text_corpus, removeWords, c("rt", badwords))
    ## Remove long words that may be invalid - >15 letters chosen as cut-off here
    text_corpus <- tm_map(text_corpus, toSpace, "[[:alpha:]]{15,}")
    ## Remove extra whitespace
    text_corpus <- tm_map(text_corpus, stripWhitespace)

    text_corpus
}

