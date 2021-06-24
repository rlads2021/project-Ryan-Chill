#大學學歷
library(dplyr)
get_least_degree <- function(Job_info, no_specify = TRUE)
{
  Job_degree <- Job_info %>% select(學歷要求)
  if (no_specify == FALSE) Job_degree <- Job_degree %>% filter(學歷要求 != "不拘")
  required_degree <- sapply(Job_degree[[1]], function(x){return(strsplit(x, split = "、")[1])}) %>% sapply(function(x){return(x[1] %>% str_trim())}) %>% unlist() %>% unname()
  return(required_degree)
}