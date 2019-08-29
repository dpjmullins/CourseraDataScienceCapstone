# Code to pre-process the data and create n-gram models

## Libraries
library(tidyverse)
library(tm)
library(quanteda)

## Load the data
twit <- read_lines(file = 'intermediate_data/twitter_test.txt')
blog <- read_lines(file = 'intermediate_data/blogs_test.txt')
news <- read_lines(file = 'intermediate_data/news_test.txt')

source('preprocessing_functions.R')


# MAIN

## Dataset pre-processing

### Twitter
twit.corpus <- Corpus(VectorSource( twit ) )
twit.filt.corpus <- text_transformer(twit.corpus)

### Blogs
blog.corpus <- Corpus( VectorSource( blog ) )
blog.filt.corpus <- text_transformer(blog.corpus)

### News
news.corpus <- Corpus( VectorSource( news ) )
news.filt.corpus <- text_transformer(news.corpus)

datasets.list <- list(twit.filt.corpus, blog.filt.corpus, news.filt.corpus)


## Generate n-gram models

## Quadgram
quadgram <- function(ds.tm_corpus) {
    ds.tokens <- tokens(corpus(ds.tm_corpus))
    ds.quadgram <- dfm(tokens_ngrams(ds.tokens, n=4))
    ds.quadgram
}

quadgrams.list <- lapply(datasets.list, quadgram)
dataset.quadgram <- dfm_compress(rbind(quadgrams.list[[1]], quadgrams.list[[2]], quadgrams.list[[3]] ), margin="features")
dataset.quadgram.trim <- dfm_trim(dataset.quadgram, min_termfreq = 6, termfreq_type = "count")
dataset.quadgram.terms <- convert(dataset.quadgram.trim, to="tripletlist")[[2]]

dataset.quadgram.top_terms <-
    tbl_df(dataset.quadgram.terms) %>%
        mutate(split = str_split(value, "_", n=4)) %>%
        rowwise() %>%
        mutate(predictor = paste(split[1], split[2], split[3], sep="_")) %>%
        mutate(response = split[4]) %>%
        ungroup() %>%
        group_by(predictor, response) %>%
        summarize(frequency = n()) %>%
        group_by(predictor) %>%
        mutate(maximum = max(frequency)) %>%
        ungroup() %>%
        filter(frequency == maximum) %>% ## filter for maximum frequency of response
        select(-c(frequency, maximum)) ## remove frequency and maximum variables to save on memory

saveRDS(object = dataset.quadgram.top_terms, file = "model_quadgram.rds")


## Trigram
trigram <- function(ds.tm_corpus) {
    ds.tokens <- tokens(corpus(ds.tm_corpus))
    ds.trigram <- dfm(tokens_ngrams(ds.tokens, n=3))
    ds.trigram
}

trigrams.list <- lapply(datasets.list, trigram)
dataset.trigram <- dfm_compress(rbind(trigrams.list[[1]], trigrams.list[[2]], trigrams.list[[3]] ), margin="features")
dataset.trigram.trim <- dfm_trim(dataset.trigram, min_termfreq = 6, termfreq_type = "count")
dataset.trigram.terms <- convert(dataset.trigram.trim, to="tripletlist")[[2]]

dataset.trigram.top_terms <-
    tbl_df(dataset.trigram.terms) %>%
        mutate(split = str_split(value, "_", n=3)) %>%
        rowwise() %>%
        mutate(predictor = paste(split[1], split[2], sep="_")) %>%
        mutate(response = split[3]) %>%
        ungroup() %>%
        group_by(predictor, response) %>%
        summarize(frequency = n()) %>%
        group_by(predictor) %>%
        mutate(maximum = max(frequency)) %>%
        ungroup() %>%
        filter(frequency == maximum) %>% ## filter for maximum frequency of response
        select(-c(frequency, maximum)) ## remove frequency and maximum variables to save on memory

saveRDS(object = dataset.trigram.top_terms, file = "model_trigram.rds")


## Bigram
bigram <- function(ds.tm_corpus) {
	ds.tokens <- tokens(corpus(ds.tm_corpus))
	ds.bigram <- dfm(tokens_ngrams(ds.tokens, n=2))
	ds.bigram
}

bigrams.list <- lapply(datasets.list, bigram)
dataset.bigram <- dfm_compress(rbind(bigrams.list[[1]], bigrams.list[[2]], bigrams.list[[3]] ), margin="features")
dataset.bigram.trim <- dfm_trim(dataset.bigram, min_termfreq = 4, termfreq_type = "count")
dataset.bigram.terms <- convert(dataset.bigram.trim, to="tripletlist")[[2]]

dataset.bigram.top_terms <-
    tbl_df(dataset.bigram.terms) %>%
        mutate(split = str_split(value, "_", n=2)) %>%
        rowwise() %>%
        mutate(predictor = split[1]) %>%
        mutate(response = split[2]) %>%
        ungroup() %>%
        group_by(predictor, response) %>%
        summarize(frequency = n()) %>%
        group_by(predictor) %>%
        mutate(maximum = max(frequency)) %>%
        ungroup() %>%
        filter(frequency == maximum) %>% ## filter for maximum frequency of response
        select(-c(frequency, maximum)) ## remove frequency and maximum variables to save on memory

saveRDS(object = dataset.bigram.top_terms, file = "model_bigram.rds")


## Unigram
unigram <- function(ds.tm_corpus) {
    ds.tokens <- tokens(corpus(ds.tm_corpus))
    ds.unigram <- dfm(tokens_ngrams(ds.tokens, n=1))
    ds.unigram
}

unigrams.list <- lapply(datasets.list, unigram)
dataset.unigram <- dfm_compress(rbind(unigrams.list[[1]], unigrams.list[[2]], unigrams.list[[3]] ), margin="features")
dataset.unigram.trim <- dfm_trim(dataset.unigram, min_termfreq = 10, termfreq_type = "count")
dataset.unigram.terms <- convert(dataset.unigram.trim, to="tripletlist")[[2]]

dataset.unigram.top_terms <-
    tbl_df(dataset.unigram.terms) %>%
        mutate(value = as.factor(value)) %>%
        group_by(value) %>%
        summarize(frequency = n()) %>%
        arrange(desc(frequency))
## 'the' most common word

saveRDS(object = dataset.bigram.top_terms, file = "model_unigram.rds") ## will not be used in model

