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

#English
library(dplyr)
get_required_languages <- function(Job_language, no_specify = TRUE) #split all the languages
{ Job_language <- Job_language %>% select(語文條件)
if (no_specify == FALSE) Job_language <- Job_language %>% filter(語文條件 != "不拘")
required_language <- vector("character", length = length(Job_language))
for (i in 1:length(Job_language[[1]]))
{
  required_language <- c(required_language, sapply(strsplit(Job_language[[1]][i], split = "  ")[[1]], function(x) return(strsplit(x, split = " -- ")[[1]][1])))
}
required_language <- required_language %>% unlist() %>% unname()
return(required_language)
}
languages_vecs <- sapply(Job, get_required_languages)

English_rates <- vector(mode = "list", length = 17)
for(i in 1:length(languages_vecs)){
  total_language_vec <- languages_vecs[[i]] #a vec of all the languages and 不拘
  language_tb <- tibble(語言 = total_language_vec) #build a tibble
  English_rate <- language_tb %>%
    filter(語言 %in% c("不拘", "英文")) %>%
    count(語言) %>%
    mutate(total = sum(n)) %>%
    mutate(rate = n / total * 100) %>%
    filter(語言 == "英文") #rate of jobs that required English 
  English_rates[[i]] <- English_rate
}
English_rates <- bind_rows(English_rates)
English_rates <- English_rates %>%
  mutate(領域 = field) %>%
  arrange(desc(rate))

library(ggplot2)
ggplot(English_rates, aes(reorder(x = 領域, -rate), y = rate)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "各領域需要英文能力的職缺比例",
       x = "領域", 
       y = "比例")