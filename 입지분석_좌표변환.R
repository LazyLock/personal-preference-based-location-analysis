library(httr)
library(jsonlite) 
library(dplyr)
library(tidyverse)
center <- read_csv("서울시 대규모점포 인허가 정보.csv") #본인의 경로로 바꿔주세요
head(center)

test1 <- center[,"도로명주소"]
test_idx <- is.na(test1)
test1 <- test1[!test_idx]
test1

KAKAO_MAP_API_KEY ="d781d6719f163314cfe75bf01cbb7745"   #본인의 key를 넣어주세요
result = data.frame()

fun_change <- function() {
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json', 
             query = list(query = test1[i]),
             add_headers(Authorization = paste0("KakaoAK ", 
                                                KAKAO_MAP_API_KEY)))
  print(x=res)
  coord <- res %>% content(as = 'text') %>% fromJSON()
  str(object = coord)
  center_list <- res %>% 
    content(as='text') %>% 
    fromJSON()
  
  row_temp = cbind(center_list$documents$road_address %>% 
                     select(address_name, x, y))
  result <<- rbind(result, row_temp)
}

for(i in 1:length(test1))
{
 try(fun_change(), silent = T)
}


result

write.csv(result, file="x_y_결과.csv", fileEncoding="utf-8")