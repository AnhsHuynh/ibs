df <- read_xlsx("data/1.8 - 29.1 tuan 04.xlsx")

df <- df %>% select(HoTen,NgaySinh,GioiTinh, SDT, CMND, NoiOHienNay, 
                    NoiOHienTai_SauKhiSapNhap_WardId, 
                    NoiOHienTai_SauKhiSapNhap_CityId, NgheNghiep, NgayNhapVien, 
                    ChanDoanChinhName, LayMauXNName, NgayLayMau, HinhThucDieuTriName,
                    DonViBaoCaoName, NgayBaoCao, PhanLoaiChanDoanName,
                    PhanDoBenhName, ChanDoanBenhKemTheo, NgayKhoiPhat)

df <- df %>% 
  mutate(NgayNhapVien=dmy_hm(NgayNhapVien),
         NgayBaoCao=dmy_hm(NgayBaoCao))

df$NgayNhapVien <- as.Date(df$NgayNhapVien)
df$NgayBaoCao <- as.Date(df$NgayBaoCao)

df <- df %>% filter(NgayNhapVien>=as.Date("2025-08-01"))

df_clean <- process_data_fast(df, gap = 30, output_dir = "output")

merged <- readRDS("output/merged_data.rds")
dups   <- readRDS("output/duplicates_cases.rds")
diff_sdt <- readRDS("output/diff_sdt_cases.rds")
diff_ward <- readRDS("output/diff_ward_cases.rds")

df_hcm <-  merged %>%
  filter(NoiOHienTai_SauKhiSapNhap_CityId == "Thành Phố Hồ Chí Minh")

df_hcm <- mutate(df_hcm, week=isoweek(NgayNhapVien))

ward_id <- read_excel("data/ward-id.xlsx", sheet = "ward_id")

df_hcm <- df_hcm %>%
  left_join(ward_id, by = "NoiOHienTai_SauKhiSapNhap_WardId")

writexl::write_xlsx(df_hcm, "data/tuan 04.xlsx")
saveRDS(df_hcm,"data/tuan04.rds")

df_hcm %>%   
  filter(ChanDoanChinhName == "Sốt xuất huyết Dengue") %>%
  mutate(week = isoweek(NgayNhapVien)) %>% 
  group_by(week) %>%  
  summarise(n = n()) %>% (View)

df_hcm %>%   
  filter(ChanDoanChinhName == "Tay - chân - miệng") %>%
  mutate(week = isoweek(NgayNhapVien)) %>% 
  group_by(week) %>% 
  summarise(n = n()) %>% (View)

df_hcm %>%   
  filter(ChanDoanChinhName == "Sốt xuất huyết Dengue" & PhanDoBenhName== "Sốt xuất huyết Dengue nặng") %>% 
  mutate(week = isoweek(NgayNhapVien)) %>% 
  group_by(week) %>% 
  summarise(n = n()) %>% (View)


dups_hcm <- dups %>% 
  filter(NoiOHienTai_SauKhiSapNhap_CityId == "Thành Phố Hồ Chí Minh")
  
dups_hcm %>%   
  filter(ChanDoanChinhName == "Sốt xuất huyết Dengue") %>%
  mutate(week = isoweek(NgayNhapVien)) %>% 
  group_by(week) %>% 
  (View)

sxh <- df %>% filter(ChanDoanChinhName=="Sốt xuất huyết Dengue", week==2) %>%
  group_by(NoiOHienTai_SauKhiSapNhap_WardId)  


##mai làm

df_hcm%>% filter(ChanDoanChinhName=="Sốt xuất huyết Dengue"& area=="KV1") %>% group_by(week) %>% summarise(n=n()) 
df_hcm%>% filter(ChanDoanChinhName=="Tay - chân - miệng"& area=="KV1") %>% group_by(week) %>% summarise(n=n())

df_px <- df_hcm %>% 
  filter(ChanDoanChinhName == "Sốt xuất huyết Dengue") %>% 
  group_by(week, NoiOHienTai_SauKhiSapNhap_WardId) %>% 
  summarise(n = n(),.groups = "drop") %>% 
  pivot_wider(
    names_from  = week,
    values_from = n,
    values_fill = 0) 

df_quan <- df_hcm %>% 
  filter(ChanDoanChinhName == "Sốt xuất huyết Dengue") %>% 
  group_by(week, quan) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from  = week,
    values_from = n,
    values_fill = 0)
