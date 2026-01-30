# tuần 1 bắt đầu từ 30.12.2024 - 05.01.2025

df <- read_xlsx("data/1.8 - 29.10 tuan 43.xlsx")

df1 <- df %>% select(HoTen,NgaySinh,GioiTinh, SDT, NoiOHienNay, 
                     NoiOHienTai_SauKhiSapNhap_WardId, NoiOHienTai_SauKhiSapNhap_CityId,
                     NgayNhapVien, 
                     ChanDoanChinhName, LayMauXNName, NgayLayMau, 
                     DonViBaoCaoName, NgayBaoCao, PhanLoaiChanDoanName,
                     ChanDoanBenhKemTheo, NgayKhoiPhat,CMND)

# chuyển cột ngày nhập viện đúng định dạng ngày tháng năm 
df1 <- mutate(df1,NgayNhapVien=dmy_hm(NgayNhapVien))

df1$NgayNhapVien <- as.Date(df1$NgayNhapVien)

df1 <-  df1 %>% mutate(week=isoweek(NgayNhapVien))

df1 <- mutate(df1,NgayBaoCao=dmy_hm(NgayBaoCao))
df1$NgayBaoCao <- as.Date(df1$NgayBaoCao)
df1 <- df1 %>% filter(NgayBaoCao>= as.Date("2025-08-01"))

#Tạo ID
df1 <- df1 %>% creat_id() 

#Lọc trùng nếu <30 ngày thì lấy ngày nhỏ nhất, nếu >30 ngày thì giữ nguyên (coi trong function)

df1 <- tinh_dot_nhapvien(df1)

#Nếu muốn coi nhưng ca cần được lọc trùng 
#a <- df1 %>%  group_by(ID, ChanDoanChinhName, episode) %>% summarise(n(n=)) 
# df_duplicate_groups <- df1 %>%
# group_by(ID, ChanDoanChinhName, episode) %>%
# filter(n() > 1) %>%
# arrange(ID, ChanDoanChinhName, episode, NgayNhapVien) %>%
# ungroup()


#Để xem dữ liệu các ca trùng trong data gốc 
#df %>%  filter(HoTen==".....") %>% View()

df_clean <- duplicate(df1)


#Security data để xuất đưa lên risk map

df_security <- baomat(df_clean)
writexl::write_xlsx(df_security,"dữ liệu đến tuần 41.xlsx")

df_clean <- df_clean %>% filter(NoiOHienTai_SauKhiSapNhap_CityId=="Thành Phố Hồ Chí Minh")

#Thống kê số ca
df_clean %>% filter(ChanDoanChinhName=="Sốt xuất huyết Dengue") %>% group_by(week) %>% summarise(n=n()) %>% View()
df_clean %>% filter(ChanDoanChinhName=="Tay - chân - miệng") %>% group_by(week) %>% summarise(n=n()) %>% View()

