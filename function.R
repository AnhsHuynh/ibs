library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(lubridate)
library(stringr)
library(digest) #mã hóa dữ liệu
library(purrr) #xử lý nhanh hơn
#Tạo ID
creat_id <- function(data) {
  data %>% mutate(
    name = str_to_upper(str_squish(HoTen)),
    birth = as.character(NgaySinh),
    sex= str_to_lower(GioiTinh),
    key= paste0(name, birth, sex, NoiOHienTai_SauKhiSapNhap_WardId, sep="_"), #tạo key để đưa mã hóa
    hash= sapply(key,digest,algo="md5"), #tạo mã hóa tự động cho ID
    ID = hash) %>% 
    # ID= paste0("G",substr(hash,1,5))) %>%  #định dạng ID 
    select (-key, -hash, -name, -birth, -sex)
}

#Security data CHATGPT

baomat <- function(data) {
  data %>%
    mutate(
      HoTen = str_to_title(HoTen) %>%
        str_split("\\s+") %>%
        lapply(function(x) {
          if (length(x) == 1) {
            return(x)
          } else {
            initials <- paste0(substr(x[-length(x)], 1, 1), ".", collapse = " ")
            return(paste(c(initials, x[length(x)]), collapse = " "))
          }
        }) %>%
        unlist(),
      SDT = str_replace(SDT, ".*(\\d{4})$", "****\\1")
    )
}

#Lọc trùng

library(dplyr)

tinh_dot_nhapvien <- function(data, gap = 30) {
  data %>%
    # 1) chuyển NgayNhapVien về Date để bỏ phần giờ/phút nếu có
    mutate(NgayNhapVien = as.Date(NgayNhapVien)) %>%
    arrange(ID, ChanDoanChinhName, NgayNhapVien) %>%
    # 2) nhóm theo ID + ChanDoanChinhName
    group_by(ID, ChanDoanChinhName) %>%
    # 3) với mỗi nhóm, gán số episode bằng vòng lặp rõ ràng
    group_modify(~{
      d <- .x
      n <- nrow(d)
      if (n == 0) return(d)
      ep <- integer(n)
      ep[1] <- 1L
      start_date <- d$NgayNhapVien[1]
      if (n > 1) {
        for (i in 2:n) {
          # tính chênh lệch theo ngày giữa ngày hiện tại và start_date của episode đang xét
          diff_days <- as.numeric(difftime(d$NgayNhapVien[i], start_date, units = "days"))
          if (diff_days < gap) {
            ep[i] <- ep[i-1]         # cùng 1 đợt
          } else {
            ep[i] <- ep[i-1] + 1L    # bắt đầu đợt mới
            start_date <- d$NgayNhapVien[i]
          }
        }
      }
      d$episode <- ep
      d
    })
  }

    # 4) lấy dòng có NgayNhapVien nhỏ nhất trong mỗi episode (giữ nguyên cột khác)
   duplicate <- function(data){
     data %>% 
     group_by(ID, ChanDoanChinhName, episode) %>%
       slice_min(NgayNhapVien, n = 1, with_ties = FALSE) %>%
       ungroup()
     } 

   
