rm(list = ls())
library(tidyverse)
library(plotly)
library(vcd)
# 1 Khám phá sự biến động 1 biến --------------------------------------------

? diamonds
kimcuong <- diamonds


# 1.1 Kham pha bien phan loai -------------------------------------------------

# Kham pha bien phan loai cut
str(kimcuong$cut)
table(kimcuong$cut)
kimcuong %>%  count(cut)

p1 <- ggplot(kimcuong) +
  geom_bar(aes(x = cut), fill = "darkblue")+
  theme(legend.position = "none")+
  ylab("Tần số")+
  xlab("Loại cắt")

ggplotly(p1)
#dev.off(2)
# Nhận xét về insight: Phần lớn là kim cương chất lượng ideal
# ít thông tin

# 1.2 Kham pha bien lien tuc --------------------------------------------------
#1.2.1 Sử dụng histogram -------------------------------------------------------

# phan tich bien carat
p2 <- ggplot(kimcuong) + 
    geom_histogram(aes(carat),
                   color ="black", 
                   fill = "royalblue", 
                   binwidth = 0.5)
ggplotly(p2)

#insight: phần lớn kim cương có carat từ 0.5 - 1.0
#carat lệch phải, không chuẩn. 
####################### Kiểm tra insight dùng số liệu 
library(psych)
describe(kimcuong$carat)
# mặc định: trimmed mean: trung bình cắt, mặc định 10%
# cắt 10% giá trị min, max trong mẫu, tính trung bình trên 90% còn lại
# làm giảm độ nhạy của mean theo outliers
# Chú ý insight: skew > 0: lệch phải
# kurtosis > 0: nhọn hơn chuẩn

# Chia nhỏ độ rộng cột xem phân phối
p2 <- ggplot(kimcuong) + 
  geom_histogram(aes(carat),
                 binwidth = 0.01, 
                 color = "blue") +
  xlim(0,3)
ggplotly(p2)
# insight: nhiều đỉnh, đỉnh tại các carat chẵn 0,3; 0,7; 1,0; giảm dần về phía phải
# lí do dự đoán: Chất lượng phần lớn ideal, premium nên trọng lượng chuẩn 
# hoặc lớn hơn một chút; ít thiếu 




#1.2.2 Dùng facet_wrap ---------------------------------------------------------


p3 <- ggplot(kimcuong) + 
  geom_histogram(aes(carat, fill = cut),
                 binwidth = 0.4) +
  facet_wrap(~ cut)+
  theme(legend.position = "none")
ggplotly(p3)

# insight: carat theo loại cut cũng có phân phối lệch phải

# 1.2.3 Dùng geom_freqpoly ---------------------------------------------------
#  biểu đồ tần số dạng đường 

p4 <- ggplot(kimcuong) + 
  geom_freqpoly(aes(carat, color = cut),
                 binwidth = 0.4) 
ggplotly(p4)

# 1.2.4 Outliers - ----------------------------------------------------------
# Tìm các giá trị quan sát rất lớn, rất bé so với phần còn lai
# Thông thường, loại bỏ 

# a. Box-plot -------------------------------------------------------------
# 1. hộp: 1 cạnh nằm ở tứ phân vị thứ 1 Q1
#   1 cạnh nằm ở tứ phân vị thứ 1 Q3
# Trong hộp vẽ median
#2. Ria/râu thấp: LW = max{Q1 - 1.5IQR, gia tri min}
# Ria/râu cao: UW = min{Q3 + 1.5IQR, gia tri max}
# Ngoài boxplot: gọi là outliers. 
# IQR: interquartle range: độ dài khoảng tứ phân vị
 
p5 <- ggplot(kimcuong) + 
       geom_boxplot(aes(y = carat), 
                    outlier.colour = "red", 
                    outlier.shape = 1,
                    outlier.size = 4) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
p5

# insight: nhiều outliers nếu dùng mặc định

# b. scatter plot id, x --------------------------------------------------

names(kimcuong)
# them cot id 
dim(kimcuong)[1]
id <- c(1: dim(kimcuong)[1])
str(id)
id <- tibble(id)
head(id)
kimcuong <- bind_cols(id, kimcuong)

p6 <- ggplot(kimcuong, aes(x = id, y = carat))+
      geom_point()
p6
# insight: Một nửa số kim cương đầu tiên có carat trải đều
# nửa còn lại có carat từ 0.1 - 1.5 

# Vẽ thêm LW, UW nhìn outliers
summary(kimcuong$carat)
trongluong <- kimcuong$carat
iqr <- IQR(trongluong) # khoang tu phan vi
iqr
q1 <- fivenum(trongluong)[2] # Q1
q3 <- fivenum(trongluong)[4] # Q3
LW <- max(min(trongluong), 
          q1 - 1.5*iqr)
LW
UW <- min(max(trongluong),
          q3 + 1,5*iqr)
p7 <- p6 + geom_hline(yintercept = LW, 
                      lty = "dashed", 
                      colour = "red", 
                      lwd = 1) +
          geom_hline(yintercept = UW, 
                    lty = "dashed", 
                    colour = "red", 
                    lwd = 1)
p7

out <- which(trongluong > 3)
length(out)
# insight: quá nhiều outlier nếu dùng theo tiêu chuẩn thông thường
#nên đẩy giới hạn UW lên 3 carat -> nếu làm sạch thì loại kimcuong có carat>3
# khi đó loại 32 quan sát


# c. violin plot ----------------------------------------------------------

p8 <- ggplot(kimcuong, aes(x= 0, y = carat)) + 
  geom_violin(fill = "pink", alpha = 0.5) +
  geom_boxplot(width = 0.1)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  xlab("")
p8


# 1.2.5. unusual values --------------------------------------------------------
# giá trị tần số xuất hiện rất ít so với phần còn lại của dữ liệu

p2 <- ggplot(kimcuong) + 
  geom_histogram(aes(carat),
                 binwidth = 0.01, 
                 color = "blue") +
  ylim(c(0,25)) # loai bo cac cot co ylim >25
p2
# hoặc 
p2 <- ggplot(kimcuong) + 
  geom_histogram(aes(carat),
                 binwidth = 0.01, 
                 color = "blue") +
  coord_cartesian(ylim = c(0,10)) # giu lai  het cac cot
p2

# zoom vào carat nhỏ để tìm giá trị unusual
# insight: unusual gom mot loai carat >3, mot loai carat 
# gan cac so nguyen 

# 2. Su bien dong hai bien  -----------------------------------------------
# 2.1 Mot bien phan loai va mot bien lien tuc -----------------------------

# Kham pha bien price theo cut

# a. geom_freqpoly --------------------------------------------------------
p9 <- ggplot(data = kimcuong,
             aes(x = price, y = ..density..)) +
      geom_freqpoly(mapping = aes(color = cut),
                    lwd = 1,
                    binwidth = 500)
ggplotly(p9)
#insight: kim cuong co chat luong fair cung co the co gia rat cao
# co cac yeu to khac nhu clarity, color...
# so sanh bieu do tan suat dong thoi theo loai cut


# b. geom_boxplot ---------------------------------------------------------
# price and cut
p10 <- ggplot(data = kimcuong, 
              mapping = aes(x = cut, 
                            y = price)) +
       geom_boxplot(aes(fill = cut), show.legend = FALSE)

p10

names(kimcuong)
# insight: better quality diamonds are cheaper on average!!!
# price and color
p10 <- ggplot(data = kimcuong, 
              mapping = aes(x = color, 
                            y = price)) +
  geom_boxplot(aes(fill = color), show.legend = FALSE)
str(kimcuong$color)
p10
# insight: ti le thuan
# price and clarity
p10 <- ggplot(data = kimcuong, 
              mapping = aes(x = clarity, 
                            y = price)) +
  geom_boxplot(aes(fill = clarity), show.legend = FALSE)
str(kimcuong$clarity)
p10
# insight: ti le nghich

# 2.2 Hai bien phan loai --------------------------------------------------

library(vcd)
setwd("D:\\Hai\\De tai 2021")
credit <- read.csv("credit.csv")
 
mosaic(~ X4 + Y, 
       dat = credit, 
       shade = TRUE, 
       legend = TRUE)


chisq.test(credit$X4, credit$Y)
table(credit$X4, credit$Y)
# Tinh thu cong p - value
# P(chi^2 > chi^2_qs/ H0 dung)

pchisq(35.662, df = 3, lower.tail = FALSE)

# 2.3 Hai bien lien tuc -----------------------------

# pho bien dung scatter plot
# Kham pha bien carat va gia
ggplot(kimcuong) +
  geom_point(aes(x = carat, 
                 y = price, 
                 color = cut), 
             show.legend = FALSE,
             alpha = 0.5) +
  facet_wrap(~ cut)


ggplot(kimcuong) +
  geom_point(aes(x = depth, 
                 y = price), 
             show.legend = FALSE,
             alpha = 0.1) 
#33 The end ####
