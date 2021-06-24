#工作內容
library(readr)
Job_content1 <- read_csv("Job_contents/經營_人資類content.csv")
Job_content2 <- read_csv("Job_contents/文字_傳媒工作類content.csv")
Job_content3 <- read_csv("Job_contents/生產製造_品管_環衛類content.csv")
Job_content4 <- read_csv("Job_contents/行政_總務_法務類content.csv")
Job_content5 <- read_csv("Job_contents/行銷_企劃_專案管理類content.csv")
Job_content6 <- read_csv("Job_contents/客服_門市_業務_貿易類content.csv")
Job_content7 <- read_csv("Job_contents/研發相關類content.csv")
Job_content8 <- read_csv("Job_contents/軍警消_保全類content.csv")
Job_content9 <- read_csv("Job_contents/財會_金融專業類content.csv")
Job_content10 <- read_csv("Job_contents/傳播藝術_設計類content.csv")
Job_content11 <- read_csv("Job_contents/資材_物流_運輸類content.csv")
Job_content12 <- read_csv("Job_contents/資訊軟體系統類content.csv")
Job_content13 <- read_csv("Job_contents/學術_教育_輔導類content.csv")
Job_content14 <- read_csv("Job_contents/操作_技術_維修類content.csv")
Job_content15 <- read_csv("Job_contents/餐飲_旅遊_美容美髮類content.csv")
Job_content16 <- read_csv("Job_contents/營建_製圖類content.csv")
Job_content17 <- read_csv("Job_contents/醫療_保健服務類content.csv")


library(stringr)
library(ggplot2)
job_content <- function(File_path) {
  file <- read_csv(File_path)
  keyword <- c("分析", "批判性思考", "邏輯", "積極學習", "自我學習", "問題解決", "自我管理", "創意", "創造力", "創新", "領導力", "社群影響力", "適應力", "抗壓性高", "理解能力", "溝通能力", "說服", "協商", "團隊合作")
  frequent <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  for (r in 1:length(file[[1]])) {
    for (w in 1:length(keyword)) {
      if (str_detect(file[[2]][r], keyword[w])) {
        frequent[w] <- frequent[w] + 1
      }
    }
  }
  result <- data.frame(
    關鍵字 = keyword,
    出現次數 = frequent
  )
  ggplot(data = result) +
    geom_bar(mapping = aes(reorder(x = 關鍵字, -出現次數), y = 出現次數), stat = "identity") +
    labs(title = "[類別名稱]",
         x = "關鍵字") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

job_content("[類別名稱]content.csv")