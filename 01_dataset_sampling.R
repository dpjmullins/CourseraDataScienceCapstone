# Script for sub-sampling the data for test purposes

## Libraries
library(tidyverse)


## Read in the full data
twit <- read_lines(file = "final/en_US/en_US.twitter.txt", skip_empty_rows = TRUE)
blogs <- read_lines(file = "final/en_US/en_US.blogs.txt", skip_empty_rows = TRUE)
news <- read_lines(file = "final/en_US/en_US.news.txt", skip_empty_rows = TRUE)

## Sub-sample the dataset for faster testing of code
### Create both testing and validation sets
sample_size <- 0.2 ## development sample size
random_seed <- 1

### Twitter
set.seed(random_seed)
twit_i <- sample.int(n = length(twit), size = (sample_size * length(twit)) )
twit_test <- twit[twit_i]
twit_val <- twit[-twit_i]

### Blogs
set.seed(random_seed)
blogs_i <- sample.int(n = length(blogs), size = (sample_size * length(blogs)) )
blogs_test <- blogs[blogs_i]
blogs_val <- blogs[-blogs_i]

### News
set.seed(random_seed)
news_i <- sample.int(n = length(news), size = (sample_size * length(news)) )
news_test <- news[news_i]
news_val <- news[-news_i]

## Save sampled data to file
### Twitter
write_lines(x = twit_test, path = 'intermediate_data/twitter_test.txt')
write_lines(x = twit_val, path = 'intermediate_data/twitter_val.txt')
### Blogs
write_lines(x = blogs_test, path = 'intermediate_data/blogs_test.txt')
write_lines(x = blogs_val, path = 'intermediate_data/blogs_val.txt')
### News
write_lines(x = news_test, path = 'intermediate_data/news_test.txt')
write_lines(x = news_val, path = 'intermediate_data/news_val.txt')
