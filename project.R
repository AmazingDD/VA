library(tidyverse)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(gt)
library(gtExtras)
library(reactable)
library(reactablefmtr)

# plot 1: Which business struggle, which business prosper?
df <- read_csv('data/V1.csv')
df$date <- format(df$date, "%Y-%m")
# draw the 4 industry monthly spending by year-month
df <- df %>%
  group_by(category, date) %>%
  summarise(amt = sum(amount))


cat = 'Food'
start_date = '2022-04'
end_date = '2022-12'
ggplot(filter(df, category == cat & date >= start_date & date <= end_date), 
       aes(x=date, y=amt, group=1)) + 
  geom_line(color="gray50") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  labs(title = paste0("Monthly Total Expense of ", cat),
       caption = "Data source: Financial Jornal",
       x = "", y = "")

# plot 2: Food industry 
col = 'popularity' # popularity avg_wage

df <- read_csv('data/v2.csv')
df$date <- format(df$date, "%Y-%m")
res <- df %>%
  filter(date <= end_date & date >= start_date) %>%
  group_by(date) %>%
  summarise(popularity=sum(hour), avg_wage=mean(amount))

res1 <- res[,c(1, 2)] %>%
  mutate(variable = 'popularity') %>%
  rename(val=popularity)
res2 <- res[,c(1, 3)] %>%
  mutate(variable = 'avg_salary') %>%
  rename(val=avg_wage)
res <- rbind(res1, res2)

ggplot(filter(res, date >= start_date & date <= end_date), 
       aes(x=date, y=val, group=1)) + 
  geom_line(color="steelblue") +
  geom_point(shape=21, color="gray80", fill="#3d5a80", size=3) + 
  facet_grid(variable ~.,scales = "free_y") + 
  labs(title = "Monthly Trend: Food Popularity vs Salary",
       caption = "Data source: Financial Jornal & Activity Logs",
       x = "", y = "")

# plot3: Engel coef
df <- read_csv('data/v3.csv')
df$date <- format(df$date, "%Y-%m")
cands = 0:20
spark <- df %>%
  filter(date >= start_date & date <= end_date & participantId %in% cands ) %>%
  group_by(participantId) %>%
  summarise('Engel Index' = list(engel_idx), .groups = "drop") 
e_index <- df %>% 
  filter(date >= start_date & date <= end_date & participantId %in% cands ) %>%
  group_by(participantId) %>% 
  summarise("Min" = min(engel_idx, na.rm = T),
            "Max" = max(engel_idx, na.rm = T),
            "Average" = mean(engel_idx, na.rm = T)
  )
engel_index_data <- left_join(e_index, spark)
engel_index_data %>%
  gt() %>%
  gt_plt_sparkline('Engel Index') %>%
  fmt_number(columns = 2:4,
             decimals = 3)

report <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  group_by(participantId) %>%
  summarize(`Engel Index` = list(engel_idx))

reactable(
  report,
  defaultPageSize = 15,
  columns = list(participantId = colDef(maxWidth = 130),
                 `Engel Index` = colDef(
                   maxWidth = 200,
                   cell = react_sparkline(
                     report, decimals = 3,
                     highlight_points = highlight_points(min = "red", max = "blue"),
                     statline = "mean",
                     bandline = "innerquartiles",
                     bandline_color = "green"))
  )
)


# V4
df <- read_csv('data/v4.csv')

df <- df %>%
  group_by(participantId, educationLevel, mood_type, haveKids) %>%
  summarise(
    Education = mean(edu_ratio),
    Food = mean(food_ratio),
    Recreation = mean(recreation_ratio),
    Shelter = mean(shelter_ratio))

res <- rbind(
  df[,c(1, 2, 5)] %>%
    group_by(educationLevel) %>%
    summarise(ratio = mean(Education)) %>%
    mutate(type = 'Education'),
  df[,c(1, 2, 6)] %>%
    group_by(educationLevel) %>%
    summarise(ratio = mean(Food)) %>%
    mutate(type = 'Food'),
  df[,c(1, 2, 7)] %>%
    group_by(educationLevel) %>%
    summarise(ratio = mean(Recreation)) %>%
    mutate(type = 'Recreation'),
  df[,c(1, 2, 8)] %>%
    group_by(educationLevel) %>%
    summarise(ratio = mean(Shelter)) %>%
    mutate(type = 'Shelter')
)
  

ggplot(res,
       aes(x = ratio,
           xend = 0,
           y = type,
           yend = type,
           colour = type)) +
  facet_grid(educationLevel~., scales = "free_y", space = "free") +
  geom_segment() +
  geom_point(color="orange", size=3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(breaks = c("Education", "Food", "Recreation",
                                 "Shelter"),
                      values = c("#DE3533", "#0047AB", "#006644",
                                 "#10C25B")) +
  labs(x = "",
       y = "",
       title = "Living Expense Distribution for Participants",
       subtitle = paste0("With different ", 'educationLevel'),
       caption = "Data source: Financial Journal") +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "off"
  )
  
# V5
df <- read_csv('data/v5.csv')
df$date <- format(df$date, "%Y-%m")
cands <- sample(c(0:max(df$participantId)), size=60)
data <- df %>%
  filter(date >= start_date & date <= end_date & participantId %in% cands) %>%
  group_by(participantId, educationLevel) %>%
  summarise(saving_rate = mean(saving_rate)) %>%
  ungroup() %>%
  rename(individual = participantId, group = educationLevel, value = saving_rate)
data$individual <- paste0("pId ", data$individual)
data$value <- data$value * 100

empty_bar <- 2
to_add <- data.frame( matrix(NA, empty_bar * length(unique(data$group)), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(unique(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=median(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=0.8, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=0.8, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=0.8, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=0.8, size=0.3 , inherit.aes = FALSE ) +
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE)







