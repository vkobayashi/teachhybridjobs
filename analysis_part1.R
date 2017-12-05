### load packages

require(tm) # text mining framework
require(topicmodels) # topic modeling
require(slam) # sparse matrix manipulation
require(text2vec) # word2vec etc.
require(data.table) #efficient and fast data manipulation
require(openNLP)

options(encoding="UTF-8")
### load data
teacher_data <- fread("teacher_skills.txt", sep="\t", header=TRUE, encoding='UTF-8')
teacher_data<-teacher_data[, title := tolower(title)]



teacher_desc<-teacher_data[grepl(pattern="docent | onderwijs",x=title),.(title,candidate_descr)]

teacher_desc[,.(.N), by= title][order(-N)][1:100]

### remove vacancies without description
teacher_desc <- teacher_desc[nchar(candidate_descr)!=0]

### create the corpus
teacher_corpus <- SimpleCorpus(VectorSource(teacher_desc$candidate_descr))

### preprocess

#remove punctuation
#teacher_corpus <- tm_map(teacher_corpus, removePunctuation)
teacher_corpus <- tm_map(teacher_corpus, content_transformer(function(x) gsub(pattern="[^[:alnum:] ]", "", x)))

#remove numbers
teacher_corpus <-tm_map(teacher_corpus, removeNumbers) 

# lowercase
teacher_corpus <- tm_map(teacher_corpus, content_transformer(tolower))

# remove stopwords
teacher_corpus <- tm_map(teacher_corpus, removeWords, stopwords(kind="nl"))

# stem document
teacher_corpus <- tm_map(teacher_corpus, stemDocument, language="nl")



teacher_corpus[[2]]$content

### Document by term matrix

teacher_dtm <- DocumentTermMatrix(teacher_corpus)

teacher_dtm <- teacher_dtm[row_sums(teacher_dtm)>0,]

### preprocess
summary(col_sums(teacher_dtm))
findFreqTerms(teacher_dtm, lowfreq=1000)

### train topic model

teacher_topics_ctm <- CTM(teacher_dtm, k=200, control=list(var=list(tol=10^-2), em=list(tol=10^-2)))



