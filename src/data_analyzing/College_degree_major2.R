#Data
library(readr)
#importing data
Job_info1 <- read_csv("Job_requirements/HR.csv")
Job_info2 <- read_csv("Job_requirements/文字_傳媒工作類.csv")
Job_info3 <- read_csv("Job_requirements/生產製造_品管_環衛類.csv")
Job_info4 <- read_csv("Job_requirements/行政_總務_法務類.csv")
Job_info5 <- read_csv("Job_requirements/行銷_企劃_專案管理類.csv")
Job_info6 <- read_csv("Job_requirements/客服_門市_業務_貿易類.csv")
Job_info7 <- read_csv("Job_requirements/研發相關類.csv")
Job_info8 <- read_csv("Job_requirements/軍警消_保全類.csv")
Job_info9 <- read_csv("Job_requirements/財會_金融專業類.csv")
Job_info10 <- read_csv("Job_requirements/傳播藝術_設計類.csv")
Job_info11 <- read_csv("Job_requirements/資材_物流_運輸類.csv")
Job_info12 <- read_csv("Job_requirements/資訊軟體系統類.csv")
Job_info13 <- read_csv("Job_requirements/學術_教育_輔導類.csv")
Job_info14 <- read_csv("Job_requirements/操作_技術_維修類.csv")
Job_info15 <- read_csv("Job_requirements/餐飲_旅遊_美容美髮類.csv")
Job_info16 <- read_csv("Job_requirements/營建_製圖類.csv")
Job_info17 <- read_csv("Job_requirements/醫療_保健服務類.csv")
Job <- list(Job_info1, Job_info2, Job_info3, Job_info4, Job_info5, Job_info6, Job_info7, Job_info8, 
            Job_info9, Job_info10, Job_info11, Job_info12, Job_info13, Job_info14, Job_info15, Job_info16, Job_info17)
field <- c("經營_人資類", "文字_傳媒工作類", "生產製造_品管_環衛類", "行政_總務_法務類", "行銷_企劃_專案管理類", 
           "客服_門市_業務_貿易類", "研發相關類", "軍警消_保全類", "財會_金融專業類", "傳播藝術_設計類", 
           "資材_物流_運輸類", "資訊軟體系統類", "學術_教育_輔導類", "操作_技術_維修類", "餐飲_旅遊_美容美髮類", 
           "營建_製圖類", "醫療_保健服務類")

#College degree

#major#2
library(dplyr)
get_required_majors <- function(Job_major, no_specify = FALSE)
{
  Job_major <- Job_major %>% select(科系要求)
  if (no_specify == FALSE) Job_major <- Job_major %>% filter(科系要求 != "不拘")
  required_major <- vector("character", length = length(Job_major))
  for (i in 1:length(Job_major[[1]])) {
    required_major <- c(required_major, strsplit(Job_major[[1]][i], split = "、"))
  }
  required_major <- required_major %>% unlist() %>% unname()
  return(required_major)
}
total_major_list <- sapply(Job, get_required_majors) #list of all the majors

library("RColorBrewer") #wordcloud
library("tm")
library("wordcloud")
seventeen_word_clouds <- for(i in 1:length(total_major_list)){ #17 word clouds of majors from 17 industries respectively
  docs <- Corpus(VectorSource(total_major_list[[i]]))
  inspect(docs)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}
docs <- Corpus(VectorSource(unlist(total_major_list))) #1 word cloud including all majors from all industries
inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
one_word_cloud <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                            max.words=10, random.order=FALSE, rot.per=0.35, 
                            colors=brewer.pal(8, "Dark2"), scale = c(3, 0.2))