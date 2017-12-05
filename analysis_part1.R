### load packages

require(tm) # text mining framework
require(topicmodels) # topic modeling
require(slam) # sparse matrix manipulation
require(text2vec) # word2vec etc.
require(data.table) #efficient and fast data manipulation

### load data
teacher_data <- fread("teacher_skills.txt", sep="\t", header=TRUE)
teacher_data<-teacher_data[, title := tolower(title)]



teacher_desc<-teacher_data[grepl(pattern="docent | onderwijs",x=title),.(candidate_descr), by= title][order(title)]

teacher_desc[,.(.N), by= title][order(-N)][1:100]
