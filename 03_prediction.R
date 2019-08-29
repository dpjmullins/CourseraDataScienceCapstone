# Prediction function

library(tidyverse)

predictor.func <-
    function( inputStr ) {
        ## manipulate input string to underscore delimited format
        str_list <- unlist(str_split(inputStr, pattern = " "))
        str_len <- length(str_list)

        response_found <- FALSE

        ## Search the quadgram model
        if (str_len >= 3) {
            input.predictor <- str_list[(str_len-2):str_len]
            input.predictor <- paste(input.predictor[1], input.predictor[2], input.predictor[3], sep="_")

            ## Find in quadgram model
            quadgram <- readRDS('model_quadgram.rds')
            input.response <- quadgram[quadgram$predictor == input.predictor,]$response

            ## If there is more than 1 equi-frequent answer, randomly pick 1
            if ( length(input.response) >= 1 ) {
                input.response <- sample(input.response, size=1)
                response_found <- TRUE
            }
            rm(quadgram)
        }

        ## Search the trigram model
        if (str_len >= 2 & response_found == FALSE) {
            input.predictor <- str_list[(str_len-1):str_len]
            input.predictor <- paste(input.predictor[1], input.predictor[2], sep="_")

            ## Find in trigram model
            trigram <- readRDS('model_trigram.rds')
            input.response <- trigram[trigram$predictor == input.predictor,]$response

            ## If there is more than 1 equi-frequent answer, randomly pick 1
            if ( length(input.response) >= 1 ) {
                input.response <- sample(input.response, size=1)
                response_found <- TRUE
            }
            rm(trigram)
        }

        ## Search the bigram model
        if (str_len >= 1 & response_found == FALSE) {
            input.predictor <- str_list[str_len]

            ## Find in bigram model
            bigram <- readRDS('model_bigram.rds')
            input.response <- bigram[bigram$predictor == input.predictor,]$response

            ## If there is more than 1 equi-frequent answer, randomly pick 1
            if ( length(input.response) >= 1 ) {
                input.response <- sample(input.response, size=1)
                response_found <- TRUE
            }
            rm(bigram)
        }

        ## If no answer found
        if (response_found == FALSE) {
            input.response <- "the"
        }
        return(input.response)

    }

