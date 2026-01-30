mapping <- readRDS("data/mapping_to_w38.rds")
mapping <- mapping[,-c(17,18)]

nweek <- readxl::read_xlsx("data/tuan 39.xlsx")

nweek <- nweek %>% select(HoTen,NgaySinh,GioiTinh, SDT, NoiOHienNay, 
                          NoiOHienTai_SauKhiSapNhap_WardId, NgayNhapVien, 
                          ChanDoanChinhName, LayMauXNName, NgayLayMau, 
                          DonViBaoCaoName, NgayBaoCao, PhanLoaiChanDoanName,
                          ChanDoanBenhKemTheo, NgayKhoiPhat,CMND)
nweek <- mutate(nweek,NgayNhapVien=dmy_hm(NgayNhapVien))
nweek$NgayNhapVien <- as.Date(nweek$NgayNhapVien)
nweek <-  nweek %>% mutate(week=isoweek(NgayNhapVien))

#kiểm tra data thô 
nweek %>% filter(ChanDoanChinhName=="Sốt xuất huyết Dengue") %>% group_by(week) %>% summarise(n=n())
nweek %>% filter(ChanDoanChinhName=="Tay - chân - miệng") %>% group_by(week) %>% summarise(n=n())
###

df_processed <- nweek %>%
  left_join(mapping,
            by = c("HoTen", "NgaySinh", "GioiTinh", "NoiOHienTai_SauKhiSapNhap_WardId"))

df_final <- df_processed %>%
  pivot_longer(
    cols = ends_with(c(".x", ".y")),       # 1. Chọn tất cả các cột kết thúc bằng .x hoặc .y
    names_to = c(".value", "source"),      # 2. Tách tên cột thành 2 phần
    names_pattern = "(.+)\\.(x|y)",        # 3. Quy tắc tách: (tên gốc).(x hoặc y)
    values_drop_na = TRUE                  # 4. Loại bỏ các hàng mới có giá trị là NA
  )

##add ward ID và area

ward_id <-  read_xlsx("data/ward-id.xlsx",sheet="ward_id")
df_final <- df_final %>%
  left_join(ward_id, by = "NoiOHienTai_SauKhiSapNhap_WardId")

## Gán ID
existing_cases <- df_final %>% filter(!is.na(ID))

## Tạo ID
new_cases <- df_final %>% filter(is.na(ID))
new_cases <- new_cases %>% creat_id()

df_final <- bind_rows(existing_cases, new_cases)                                                                                                                                                                                                                                                          

mapping_updated <- bind_rows(mapping, df_final)
mapping_updated <- mapping_updated %>% distinct() #lọc trùng tuyệt đối

df_clean <- tinh_dot_nhapvien(mapping_updated)
df_clean <- duplicate(df_clean)

df_clean <- df_clean %>% mutate(week=isoweek(NgayNhapVien))
df_clean <- df_clean %>% select(-source)



## cập nhật data tuần cũ và mới 
df_clean %>% 
  filter(ChanDoanChinhName == "Sốt xuất huyết Dengue") %>% 
  group_by(week) %>% 
  filter(week>=32) %>% 
  summarise(n = n())

df_clean %>% 
  filter(ChanDoanChinhName == "Tay - chân - miệng") %>% 
  group_by(week) %>% 
  filter(week>=32) %>% 
  summarise(n = n())

## lưu file dữ liệu
saveRDS(df_clean, "data/mapping_to_w39_2.rds")


##mai làm
mapping <- mapping %>%
     left_join(ward_id, by = "NoiOHienTai_SauKhiSapNhap_WardId")

mapping%>% filter(ChanDoanChinhName=="Sốt xuất huyết Dengue"& area=="KV1") %>% group_by(week) %>% summarise(n=n())
mapping%>% filter(ChanDoanChinhName=="Tay - chân - miệng"& area=="KV1") %>% group_by(week) %>% summarise(n=n())
