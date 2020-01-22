# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Homework of old aunt
# Purpose:      May you be happy
# programmer:   Zhe Liu
# Date:         21-01-2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


options(java.parameters = "-Xmx2048m",
        # encoding = "UTF-8",
        stringsAsFactors = FALSE)

##---- loading the required packages ----
suppressPackageStartupMessages({
  require(readr)
  require(openxlsx)
  require(readxl)
  require(plyr)
  require(stringi)
  require(feather)
  require(RODBC)
  require(MASS)
  require(car)
  require(data.table)
  require(tidyverse)
  require(Rwordseg)
  require(tm)
  require(tmcn)
  require(chinese.misc)
  require(ggplot2)
  require(wordcloud2)
})


##---- Readin dataset ----
train.raw <- read_csv("Input/Train_DataSet.csv")
train.label <- read_csv("Input/Train_DataSet_Label.csv", )
test.raw <- read_csv2("Input/Test_DataSet.csv")

stopword1 <- read.csv("Input/CNstopwords.csv")
stopword2 <- read.csv("Input/stop_words_zh.csv")


##---- Cleaning ----
train.set <- train.raw %>% 
  left_join(train.label, by = "id") %>% 
  unite("title_content", title, content, sep = " ") %>% 
  mutate(title_content = gsub("[[:punct:][:digit:][:blank:][:space:]a-zA-Z]", "", title_content)) %>% 
  filter(!is.na(id), !is.na(title_content), !is.na(label)) %>% 
  filter(row_number() <= 1000)

test.set <- test.raw %>% 
  separate(`id,title,content`, c("id", "title", "content"), sep = ",") %>% 
  unite("title_content", title, content, sep = " ") %>% 
  mutate(title_content = gsub("[[:punct:][:digit:][:blank:][:space:]a-zA-Z]", "", title_content)) %>% 
  filter(!is.na(id), !is.na(title_content)) %>% 
  filter(row_number() <= 1000)


##---- description ----
# label
label.count <- count(train.set, label)

train.label.bar <- ggplot(label.count, mapping = aes(x = label, y = n))+
  geom_bar(stat = "identity", colour = c("red", "yellow", "blue"), fill = c("red", "yellow", "blue"))


##---- Word segment ----
train.seg <- segmentCN(train.set$title_content, analyzer = "jiebaR") %>% 
  lapply(function(x) {
    setdiff(setdiff(x, stopword1$word), stopword2$word)
  })

test.seg <- segmentCN(test.set$title_content, analyzer = "jiebaR") %>% 
  lapply(function(x) {
    setdiff(setdiff(x, stopword1$word), stopword2$word)
  })

# train.tm <- segmentCN(train.set$title_content, analyzer = "jiebaR", returnType = "tm")
# test.tm <- segmentCN(test.set$title_content, analyzer = "jiebaR", returnType = "tm")


##---- Word cloud ----
# word frequency
word.freq <- c(unlist(train.seg), unlist(test.seg)) %>% 
  table() %>% 
  data.frame() %>% 
  select("word" = ".", "freq" = "Freq") %>% 
  arrange(-freq) %>% 
  mutate(freq_cumsum = cumsum(freq),
         freq_cumprop = freq_cumsum / sum(freq))

write.csv(word.freq[word.freq$freq >= 200, ], "Output/Wordcloud_check.csv")

# wordcloud
total.wordcloud <- wordcloud2(word.freq[word.freq$freq >= 200, ], size = 0.5)


##---- Document term matrix ----
# train
train.corpus <- lapply(train.seg, function(x) {
  paste0(x, collapse = " ")
}) %>% 
  VectorSource() %>% 
  Corpus()

# Sys.setlocale(locale = "English")
# Sys.setlocale(locale="Chinese (Simplified)_People's Republic of China.936")

train.dtm <- DocumentTermMatrix(train.corpus, 
                                control = list(weighting = weightTfIdf,
                                               wordLengths = c(1, Inf)))

train.dtm.remove <- removeSparseTerms(train.dtm, sparse = 0.95)

train.dtm.matrix <- as.matrix(train.dtm.remove)

# test
test.corpus <- lapply(test.seg, function(x) {
  paste0(x, collapse = " ")
}) %>% 
  VectorSource() %>% 
  Corpus()

test.dtm <- DocumentTermMatrix(test.corpus, 
                               control = list(weighting = weightTfIdf,
                                              wordLengths = c(1, Inf)))

test.dtm.remove <- removeSparseTerms(test.dtm, sparse = 0.95)

test.dtm.matrix <- as.matrix(test.dtm.remove)


##---- Sentiment analysis ----
# dictionary
word.dictionary.raw <- read.xlsx("Input/情感词汇本体.xlsx")

word.dictionary <- word.dictionary.raw %>% 
  distinct() %>% 
  filter(!is.na(sentiment)) %>% 
  mutate(intensity = if_else(stri_sub(sentiment, 1, 1) == "N", -intensity, intensity)) %>% 
  select(word, intensity)

# train set sentiment score
label.score <- lapply(seq_along(train.seg), function(i) {
  data.frame(id = train.set$id[i],
             label = train.set$label[i],
             word = train.seg[[i]])
}) %>% 
  rbind.fill() %>% 
  left_join(word.dictionary, by = "word") %>% 
  group_by(id, label) %>% 
  summarise(score = sum(intensity, na.rm = TRUE),
            flag = sum(!is.na(intensity))) %>% 
  ungroup() %>% 
  mutate(score = ifelse(flag == 0, NA, score)) %>% 
  filter(!is.na(score)) %>% 
  group_by(score) %>% 
  summarise(label = round(mean(label))) %>% 
  ungroup()

# test set sentiment label
test.label <- lapply(seq_along(test.seg), function(i) {
  data.frame(id = test.set$id[i],
             word = test.seg[[i]])
}) %>% 
  rbind.fill() %>% 
  left_join(word.dictionary, by = "word") %>% 
  group_by(id) %>% 
  summarise(score = sum(intensity, na.rm = TRUE),
            flag = sum(!is.na(intensity))) %>% 
  ungroup() %>% 
  mutate(score = sapply(score, function(x) {
    temp <- abs(x - label.score$score)
    label.score$score[temp == min(temp)][1]
  })) %>% 
  left_join(label.score, by = "score")


##---- Cluster analysis ----
chk <- kmeans(train.dtm.matrix, 3)




