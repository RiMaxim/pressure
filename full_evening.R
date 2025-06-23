library(ggrepel)
library(ggplot2)
library(gtable)
library(ggthemes)
library(scales)
library(data.table)
library(tidyverse)
library(lubridate)
library(patchwork)
library(qpdf)
library(fuzzyjoin)  # для разностного соединения по времени
theme_set(theme_bw())
getwd()



# 1. Загрузить основной файл
df0 <- read.table("Desktop/presser/data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
df1 <- subset(df0, Type2 == "evening", select = c(Day,value,variable,Type))
df_main <- subset(df1, variable %in% c("Systolic", "Diastolic", "Pulse"), select = c(Day,value,variable,Type))

df_main$Day <- as.POSIXct(df_main$Day, tz = "UTC")

# 2. Отметить, где Type == 1 (для вертикальных линий)
marked_dates <- df_main$Day[df_main$Type == 1]

# 3. Загрузить второй файл с часовой активностью (например, геомагнитной)
df_h <- read.table("Desktop/presser/data2.txt", sep = "|", header = FALSE,
                   col.names = c("time", "f10", "sn", "ap", "max_kp",
                                 "h00", "h02", "h05", "h08", "h11", "h14", "h17", "h20", "h23"))

df_long <- df_h %>%
  pivot_longer(cols = starts_with("h"),
               names_to = "hour", values_to = "gm_value") %>%
  mutate(
    time = as.Date(time),
    hour = parse_number(hour),
    datetime = as.POSIXct(time) + hours(hour),
    color = case_when(
      gm_value < 4 ~ "#CAF2C2",
      gm_value < 5 ~ "#FFF8B8",
      TRUE         ~ "#FFD6C9"
    )
  )

# === Настройки ===
x_limits <- range(df_main$Day, df_long$datetime, na.rm = TRUE)
y_max <- max(df_main$value, na.rm = TRUE)
indicator_height <- y_max + 10
smile_height <- indicator_height + 4
indicator_width <- 3600

df_long_filtered <- df_long %>%
  filter(datetime >= x_limits[1], datetime <= x_limits[2])

smiles_df <- tibble(Day = as.Date(marked_dates)) %>%  
  distinct(Day) %>%                                   
  mutate(
    Day = as.POSIXct(Day) + hours(16),
    label = ":(",
    y = smile_height
  )

# Строим график
p<-ggplot() +
  geom_tile(
    data = df_long_filtered,
    aes(x = datetime, y = indicator_height, fill = color),
    width = indicator_width, height = 3
  ) +
  scale_fill_identity() +
  
  geom_point(data = df_main, aes(x = Day, y = value, group = variable, colour = variable), size = 2) +
  geom_line(data = df_main[!is.na(df_main$value), ], aes(x = Day, y = value, group = variable, colour = variable), size = 1) +
  
  geom_text(
    data = smiles_df,
    aes(x = Day, y = y, label = label),
    size = 4,
    show.legend = FALSE
  ) +
  
  scale_x_datetime(
    limits = x_limits,
    breaks = date_breaks("1 day"),
    labels = date_format("%d.%m", tz = "UTC"),
    expand = c(0.02, 0)
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Systolic" = "#7a6e65", "Diastolic" = "#eee0cb", "Pulse" = "#aab19e")
  ) +
  
  labs(title = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16, family = "sans"),
    axis.text.x = element_text(size = 16, family = "sans"),
    axis.text.y = element_text(size = 16, family = "sans"),
    plot.margin = margin(20, 20, 20, 20),
    panel.ontop = FALSE,
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  coord_cartesian(ylim = c(NA, smile_height + 1), clip = "off")
pdf("Desktop/presser/1.pdf", width = 14, height = 7)
p
dev.off()





#########################PP


# 1. Загрузить основной файл
df0 <- read.table("Desktop/presser/data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
df1 <- subset(df0, Type2 == "evening", select = c(Day,value,variable,Type))
df_main <- subset(df1, variable %in% c("PP"), select = c(Day,value,variable,Type))


df_main$Day <- as.POSIXct(df_main$Day, tz = "UTC")

# 2. Отметить, где Type == 1 (для вертикальных линий)
marked_dates <- df_main$Day[df_main$Type == 1]

# 3. Загрузить второй файл с часовой активностью (например, геомагнитной)
df_h <- read.table("Desktop/presser/data2.txt", sep = "|", header = FALSE,
                   col.names = c("time", "f10", "sn", "ap", "max_kp",
                                 "h00", "h02", "h05", "h08", "h11", "h14", "h17", "h20", "h23"))

df_long <- df_h %>%
  pivot_longer(cols = starts_with("h"),
               names_to = "hour", values_to = "gm_value") %>%
  mutate(
    time = as.Date(time),
    hour = parse_number(hour),
    datetime = as.POSIXct(time) + hours(hour),
    color = case_when(
      gm_value < 4 ~ "#CAF2C2",
      gm_value < 5 ~ "#FFF8B8",
      TRUE         ~ "#FFD6C9"
    )
  )

# === Настройки ===
x_limits <- range(df_main$Day, df_long$datetime, na.rm = TRUE)
y_max <- max(df_main$value, na.rm = TRUE)
indicator_height <- 60 + 10
smile_height <- indicator_height + 2.5
indicator_width <- 3600

df_long_filtered <- df_long %>%
  filter(datetime >= x_limits[1], datetime <= x_limits[2])

smiles_df <- tibble(Day = as.Date(marked_dates)) %>%  
  distinct(Day) %>%                                   
  mutate(
    Day = as.POSIXct(Day) + hours(16),
    label = ":(",
    y = smile_height
  )

# Строим график
p<-ggplot() +
  geom_tile(
    data = df_long_filtered,
    aes(x = datetime, y = indicator_height, fill = color),
    width = indicator_width, height = 2.5
  ) +
  scale_fill_identity() +
  
  geom_point(data = df_main, aes(x = Day, y = value, group = variable, colour = variable), size = 2) +
  geom_line(data = df_main[!is.na(df_main$value), ], aes(x = Day, y = value, group = variable, colour = variable), size = 1) +
  
  geom_hline(yintercept = 30, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1) +
  
  geom_text(
    data = smiles_df,
    aes(x = Day, y = y, label = label),
    size = 4,
    show.legend = FALSE
  ) +
  
  scale_x_datetime(
    limits = x_limits,
    breaks = date_breaks("1 day"),
    labels = date_format("%d.%m", tz = "UTC"),
    expand = c(0.02, 0)
  ) +
  scale_color_manual(
    name = NULL,
    values = c("PP" = "black")
  ) +
  
  labs(title = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16, family = "sans"),
    axis.text.x = element_text(size = 16, family = "sans"),
    axis.text.y = element_text(size = 16, family = "sans"),
    plot.margin = margin(20, 20, 20, 20),
    panel.ontop = FALSE,
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  coord_cartesian(ylim = c(NA, smile_height + 1), clip = "off")
pdf("Desktop/presser/2.pdf", width = 14, height = 7)
p
dev.off()




#########################MAP


# 1. Загрузить основной файл
df0 <- read.table("Desktop/presser/data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
df1 <- subset(df0, Type2 == "evening", select = c(Day,value,variable,Type))
df_main <- subset(df1, variable %in% c("MAP"), select = c(Day,value,variable,Type))


df_main$Day <- as.POSIXct(df_main$Day, tz = "UTC")

# 2. Отметить, где Type == 1 (для вертикальных линий)
marked_dates <- df_main$Day[df_main$Type == 1]

# 3. Загрузить второй файл с часовой активностью (например, геомагнитной)
df_h <- read.table("Desktop/presser/data2.txt", sep = "|", header = FALSE,
                   col.names = c("time", "f10", "sn", "ap", "max_kp",
                                 "h00", "h02", "h05", "h08", "h11", "h14", "h17", "h20", "h23"))

df_long <- df_h %>%
  pivot_longer(cols = starts_with("h"),
               names_to = "hour", values_to = "gm_value") %>%
  mutate(
    time = as.Date(time),
    hour = parse_number(hour),
    datetime = as.POSIXct(time) + hours(hour),
    color = case_when(
      gm_value < 4 ~ "#CAF2C2",
      gm_value < 5 ~ "#FFF8B8",
      TRUE         ~ "#FFD6C9"
    )
  )

# === Настройки ===
x_limits <- range(df_main$Day, df_long$datetime, na.rm = TRUE)
y_max <- max(df_main$value, na.rm = TRUE)
indicator_height <- 100 + 5
smile_height <- indicator_height + 2.5
indicator_width <- 3600

df_long_filtered <- df_long %>%
  filter(datetime >= x_limits[1], datetime <= x_limits[2])

smiles_df <- tibble(Day = as.Date(marked_dates)) %>%  
  distinct(Day) %>%                                   
  mutate(
    Day = as.POSIXct(Day) + hours(16),
    label = ":(",
    y = smile_height
  )

# Строим график
p<-ggplot() +
  geom_tile(
    data = df_long_filtered,
    aes(x = datetime, y = indicator_height, fill = color),
    width = indicator_width, height = 2.5
  ) +
  scale_fill_identity() +
  
  geom_point(data = df_main, aes(x = Day, y = value, group = variable, colour = variable), size = 2) +
  geom_line(data = df_main[!is.na(df_main$value), ], aes(x = Day, y = value, group = variable, colour = variable), size = 1) +
  
  geom_hline(yintercept = 70, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", size = 1) +
  
  geom_text(
    data = smiles_df,
    aes(x = Day, y = y, label = label),
    size = 4,
    show.legend = FALSE
  ) +
  
  scale_x_datetime(
    limits = x_limits,
    breaks = date_breaks("1 day"),
    labels = date_format("%d.%m", tz = "UTC"),
    expand = c(0.02, 0)
  ) +
  scale_color_manual(
    name = NULL,
    values = c("MAP" = "black")
  ) +
  
  labs(title = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16, family = "sans"),
    axis.text.x = element_text(size = 16, family = "sans"),
    axis.text.y = element_text(size = 16, family = "sans"),
    plot.margin = margin(20, 20, 20, 20),
    panel.ontop = FALSE,
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  coord_cartesian(ylim = c(NA, smile_height + 1), clip = "off")
pdf("Desktop/presser/3.pdf", width = 14, height = 7)
p
dev.off()





#########################RPP


# 1. Загрузить основной файл
df0 <- read.table("Desktop/presser/data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
df1 <- subset(df0, Type2 == "evening", select = c(Day,value,variable,Type))
df_main <- subset(df1, variable %in% c("RPP"), select = c(Day,value,variable,Type))


df_main$Day <- as.POSIXct(df_main$Day, tz = "UTC")

# 2. Отметить, где Type == 1 (для вертикальных линий)
marked_dates <- df_main$Day[df_main$Type == 1]

# 3. Загрузить второй файл с часовой активностью (например, геомагнитной)
df_h <- read.table("Desktop/presser/data2.txt", sep = "|", header = FALSE,
                   col.names = c("time", "f10", "sn", "ap", "max_kp",
                                 "h00", "h02", "h05", "h08", "h11", "h14", "h17", "h20", "h23"))

df_long <- df_h %>%
  pivot_longer(cols = starts_with("h"),
               names_to = "hour", values_to = "gm_value") %>%
  mutate(
    time = as.Date(time),
    hour = parse_number(hour),
    datetime = as.POSIXct(time) + hours(hour),
    color = case_when(
      gm_value < 4 ~ "#CAF2C2",
      gm_value < 5 ~ "#FFF8B8",
      TRUE         ~ "#FFD6C9"
    )
  )

# === Настройки ===
x_limits <- range(df_main$Day, df_long$datetime, na.rm = TRUE)
y_max <- max(df_main$value, na.rm = TRUE)
indicator_height <- 12000 + 1000
smile_height <- indicator_height + 400
indicator_width <- 3600

df_long_filtered <- df_long %>%
  filter(datetime >= x_limits[1], datetime <= x_limits[2])

smiles_df <- tibble(Day = as.Date(marked_dates)) %>%  
  distinct(Day) %>%                                   
  mutate(
    Day = as.POSIXct(Day) + hours(16),
    label = ":(",
    y = smile_height
  )

# Строим график
p<-ggplot() +
  geom_tile(
    data = df_long_filtered,
    aes(x = datetime, y = indicator_height, fill = color),
    width = indicator_width, height = 400
  ) +
  scale_fill_identity() +
  
  geom_point(data = df_main, aes(x = Day, y = value, group = variable, colour = variable), size = 2) +
  geom_line(data = df_main[!is.na(df_main$value), ], aes(x = Day, y = value, group = variable, colour = variable), size = 1) +
  
  geom_hline(yintercept = 7000, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 12000, linetype = "dashed", color = "red", size = 1) +
  
  geom_text(
    data = smiles_df,
    aes(x = Day, y = y, label = label),
    size = 4,
    show.legend = FALSE
  ) +
  
  scale_x_datetime(
    limits = x_limits,
    breaks = date_breaks("1 day"),
    labels = date_format("%d.%m", tz = "UTC"),
    expand = c(0.02, 0)
  ) +
  scale_color_manual(
    name = NULL,
    values = c("RPP" = "black")
  ) +
  
  labs(title = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16, family = "sans"),
    axis.text.x = element_text(size = 16, family = "sans"),
    axis.text.y = element_text(size = 16, family = "sans"),
    plot.margin = margin(20, 20, 20, 20),
    panel.ontop = FALSE,
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  coord_cartesian(ylim = c(NA, smile_height + 1), clip = "off")
pdf("Desktop/presser/4.pdf", width = 14, height = 7)
p
dev.off()





#########################TI


# 1. Загрузить основной файл
df0 <- read.table("Desktop/presser/data.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
df1 <- subset(df0, Type2 == "evening", select = c(Day,value,variable,Type))
df_main <- subset(df1, variable %in% c("TI"), select = c(Day,value,variable,Type))


df_main$Day <- as.POSIXct(df_main$Day, tz = "UTC")

# 2. Отметить, где Type == 1 (для вертикальных линий)
marked_dates <- df_main$Day[df_main$Type == 1]

# 3. Загрузить второй файл с часовой активностью (например, геомагнитной)
df_h <- read.table("Desktop/presser/data2.txt", sep = "|", header = FALSE,
                   col.names = c("time", "f10", "sn", "ap", "max_kp",
                                 "h00", "h02", "h05", "h08", "h11", "h14", "h17", "h20", "h23"))

df_long <- df_h %>%
  pivot_longer(cols = starts_with("h"),
               names_to = "hour", values_to = "gm_value") %>%
  mutate(
    time = as.Date(time),
    hour = parse_number(hour),
    datetime = as.POSIXct(time) + hours(hour),
    color = case_when(
      gm_value < 4 ~ "#CAF2C2",
      gm_value < 5 ~ "#FFF8B8",
      TRUE         ~ "#FFD6C9"
    )
  )

# === Настройки ===
x_limits <- range(df_main$Day, df_long$datetime, na.rm = TRUE)
y_max <- max(df_main$value, na.rm = TRUE)
indicator_height <- y_max + 10
smile_height <- indicator_height + 4
indicator_width <- 3600

df_long_filtered <- df_long %>%
  filter(datetime >= x_limits[1], datetime <= x_limits[2])

smiles_df <- tibble(Day = as.Date(marked_dates)) %>%  
  distinct(Day) %>%                                   
  mutate(
    Day = as.POSIXct(Day) + hours(16),
    label = ":(",
    y = smile_height
  )

# Строим график
p<-ggplot() +
  geom_tile(
    data = df_long_filtered,
    aes(x = datetime, y = indicator_height, fill = color),
    width = indicator_width, height = 4
  ) +
  scale_fill_identity() +
  
  geom_point(data = df_main, aes(x = Day, y = value, group = variable, colour = variable), size = 2) +
  geom_line(data = df_main[!is.na(df_main$value), ], aes(x = Day, y = value, group = variable, colour = variable), size = 1) +
  
  geom_hline(yintercept = 70, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 110, linetype = "dashed", color = "red", size = 1) +
  
  geom_text(
    data = smiles_df,
    aes(x = Day, y = y, label = label),
    size = 4,
    show.legend = FALSE
  ) +
  
  scale_x_datetime(
    limits = x_limits,
    breaks = date_breaks("1 day"),
    labels = date_format("%d.%m", tz = "UTC"),
    expand = c(0.02, 0)
  ) +
  scale_color_manual(
    name = NULL,
    values = c("TI" = "black")
  ) +
  
  labs(title = NULL, x = NULL, y = NULL) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16, family = "sans"),
    axis.text.x = element_text(size = 16, family = "sans"),
    axis.text.y = element_text(size = 16, family = "sans"),
    plot.margin = margin(20, 20, 20, 20),
    panel.ontop = FALSE,
    panel.background = element_rect(fill = "white", color = "black")
  ) +
  coord_cartesian(ylim = c(NA, smile_height + 1), clip = "off")
pdf("Desktop/presser/5.pdf", width = 14, height = 7)
p
dev.off()

##############################



# Укажите пути к вашим PDF
pdf_files <- c("Desktop/presser/1.pdf", "Desktop/presser/2.pdf", "Desktop/presser/3.pdf", "Desktop/presser/4.pdf", "Desktop/presser/5.pdf")

# Объединение в один файл
pdf_combine(input = pdf_files, output = "Desktop/presser/Report_evening.pdf")




