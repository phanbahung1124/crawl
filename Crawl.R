### XU LI DU LIEU

library(dplyr)
library(tidyr)

# Doc du lieu tu tep csv
bongro_df <- read.csv('C:/Users/Admin/Downloads/bongrotv.csv', stringsAsFactors = FALSE)

# Hien thi 5 dong dau tien cua DataFrame
head(bongro_df)

# Loai bo cot khong can thiet
bongro_df <- bongro_df[, !(names(bongro_df) %in% c('channel_id', 'video_id'))]

# Hien thi kich thuoc DataFrame
cat('Number of rows =', nrow(bongro_df), '\nNumber of columns =', ncol(bongro_df), 
    '\nSize of the dataset =', nrow(bongro_df) * ncol(bongro_df), 'elements.\n')

# Chuyen doi cot 'published_date' sang dinh dang ngay thang
bongro_df$published_date <- as.POSIXct(bongro_df$published_date, format='%Y-%m-%dT%H:%M:%SZ')

# Tao cac vector trong
time <- vector("character", length = nrow(bongro_df))
date <- vector("character", length = nrow(bongro_df))
year <- vector("numeric", length = nrow(bongro_df))
month <- vector("numeric", length = nrow(bongro_df))
day <- vector("numeric", length = nrow(bongro_df))

# Lap qua hang cua DataFrame
for (i in 1:nrow(bongro_df)) {
  # Trích xuất thông tin về thời gian và ngày
  t <- format(bongro_df$published_date[i], "%H:%M:%S")
  d <- format(bongro_df$published_date[i], "%Y-%m-%d")
  
  # Trich xuat thong tin ve nam, thang, ngay
  y <- as.numeric(format(bongro_df$published_date[i], "%Y"))
  m <- as.numeric(format(bongro_df$published_date[i], "%m"))
  da <- as.numeric(format(bongro_df$published_date[i], "%d"))
  
  # Luu thong tin cac vectơ
  time[i] <- t
  date[i] <- d
  year[i] <- y
  month[i] <- m
  day[i] <- da
}

# Loai bo cot 'published_date' ban dau
bongro_df <- bongro_df %>%
  select(-published_date)

# Them cac cot moi vao DataFrame
bongro_df <- bongro_df %>%
  mutate(published_date = date,
         published_time = time,
         published_year = year,
         published_month = month,
         published_day = day)
head(bongro_df)

# Hien thi so hang, so cot va kich thuoc cua DataFrame
cat('Number of rows =', nrow(bongro_df), '\nNumber of columns =', ncol(bongro_df), 
    '\nSize of the dataset =', nrow(bongro_df) * ncol(bongro_df), 'elements.\n')

#mo ta thong ke cua DataFrame
summary(bongro_df)

# Tinh tong cho cac cot cu the
bongro_df %>%
  summarise(total_likes = sum(likes),
            total_views = sum(views),
            total_comment_count = sum(comment_count))


### TRUC QUAN HOA DU LIEU

# Khai bao thu vien
library(ggplot2)

# Cai dat cau hinh hien thi cua do thi
theme_set(
  theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_line(color = "gray", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      text = element_text(color = "black"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
)

# Du lieu
pie_data1 <- data.frame(labels = c("Reacters", "Neutral"),
                        values = c(sum(bongro_df$likes), sum(bongro_df$views) - sum(bongro_df$likes)))

pie_data2 <- data.frame(labels = c("likers", "dislikers", "commenters"),
                        values = c(sum(bongro_df$likes), sum(bongro_df$dislikes), sum(bongro_df$comment_count)))

pie_data3 <- data.frame(labels = c("comments", "non-commenters"),
                        values = c(sum(bongro_df$comment_count), sum(bongro_df$views) - sum(bongro_df$comment_count)))



# Ve do thi
p1 <- ggplot(pie_data1, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle("Viewers who react on video")

p2 <- ggplot(pie_data2, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle("Type of reacters")

p3 <- ggplot(pie_data3, aes(x = "", y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle("Viewers total comments")

# Hien thi do thi
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)


# Tao bieu do cho views, likes va comment_count
p1 <- ggplot(bongro_df, aes(x = published_month, y = views)) +
  geom_point() +
  ggtitle('Figure 1') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p2 <- ggplot(bongro_df, aes(x = published_month, y = likes)) +
  geom_point() +
  ggtitle('Figure 2') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p3 <- ggplot(bongro_df, aes(x = published_month, y = comment_count)) +
  geom_point() +
  ggtitle('Figure 3') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Hien thi bieu do trong 1 bang 1x3
grid.arrange(p1, p2, p3, ncol = 3, top = "Monthwise Statistics", widths = c(1, 1, 1.2))

# nhom du lieu theo nam va dem so luong moi nhom
year_counts <- bongro_df %>%
  group_by(published_year) %>%
  count()
print(year_counts)

# nhom du lieu theo nam va dem so luong tung nhom
year_sums <- bongro_df %>%
  group_by(published_year) %>%
  summarise_all(sum)
print(year_sums)

# Tao bieu do cho views, likes và comment_count theo nam
p1 <- ggplot(bongro_df, aes(x = published_year, y = views)) +
  geom_point() +
  ggtitle('Figure 1') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p2 <- ggplot(bongro_df, aes(x = published_year, y = likes)) +
  geom_point() +
  ggtitle('Figure 2') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p3 <- ggplot(bongro_df, aes(x = published_year, y = comment_count)) +
  geom_point() +
  ggtitle('Figure 3') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Hien thi bieu do trong 1 bang 1x3
grid.arrange(p1, p2, p3, ncol = 3, top = "Yearwise Statistics", widths = c(1, 1, 1.2))

# Sap xep DataFrame theo cot views giam dan va lay 10 hang dau tien
top_10_views <- bongro_df %>%
  arrange(desc(views)) %>%
  head(10)
print(top_10_views)

# Sap xep DataFrame theo cot views giam dan va lay 10 hang dau tien
top_10_views_asc <- bongro_df %>%
  arrange(views) %>%
  head(10)
print(top_10_views_asc)

# dat gia tri toi da cua chieu rong hien thi cho moi cot
options(width = 50)

# Sap xep DataFrame theo cot views giam dan va lay 1 hang dau tien
top_1_views <- bongro_df %>%
  arrange(desc(views)) %>%
  head(1)
print(top_1_views)

# dat gia tri toi da cua chieu rong hien thi cho moi cot
options(width = 100)

# Sap xep DataFrame theo cot views giam dan va lay 1 hang dau tien
top_1_views <- bongro_df %>%
  arrange(desc(views)) %>%
  head(1)
print(top_1_views$video_title)

# dat gia tri toi da cua chieu rong hien thi cho moi cot
options(width = 600)
# Sap xep DataFrame theo cot views giam dan va lay 1 hang dau tien
top_1_views <- bongro_df %>%
  arrange(desc(views)) %>%
  head(1)
print(top_1_views$video_description)

# dat gia tri toi da cua chieu rong hien thi cho moi cot
options(width = 50)
# Sap xep DataFrame theo cot published_date giam dan va lay 10 hang dau tien
top_10_dates <- bongro_df %>%
  arrange(desc(published_date)) %>%
  head(10)
print(top_10_dates)

# dat gia tri toi da cua chieu rong hien thi cho moi cot
options(width = 50)
# Sắp xếp DataFrame theo cột published_date tăng dần và lấy 10 hàng đầu tiên
top_10_dates_asc <- bongro_df %>%
  arrange(published_date) %>%
  head(10)
print(top_10_dates_asc)


# dat gia tri toi da cua chieu rong hien thi cho moi cot
options(width = 100)
# Sap xep DataFrame theo cot commnet_count giam dan va lay 10 hang dau tien
top_1_comments <- bongro_df %>%
  arrange(desc(comment_count)) %>%
  head(1)
print(top_1_comments)
