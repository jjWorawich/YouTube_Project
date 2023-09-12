library(tidyverse)
library(jsonlite)
library(ggbeeswarm)
library(scales)

### Load the data 

# Import CSV file without category name 
youtube <- read_csv("US_youtube_trending_data.csv")
head(youtube)

# Import category name from JSON file
category <- data.frame(fromJSON("US_category_id.json"))
head(category)

### Data preparation

# Extract the id and name 
item_id <- category$items.id
category_name <- category$items.snippet$title

new_category <- data.frame(item_id,
                           category_name)
## Gather the data 

# Change the type of primary key for joining
youtube$categoryId <- as.character(youtube$categoryId) 

full_df <- youtube %>%
  inner_join(new_category, by = c("categoryId" = "item_id" ))

colnames(full_df) <- tolower(colnames(full_df))

## Cleaning and Transformation data to be used

# Filter the data within 1 day after upload a clip
final_df <- full_df %>%
  select(video_id,
         publishedat,
         channeltitle,
         title,
         category_name,
         tags,
         view_count,
         likes,
         dislikes,
         comment_count) %>%
  group_by(video_id) %>%
  filter(view_count == min(view_count) & view_count != 0)

glimpse(final_df)

# Convert the data to date type
final_df$date <- as.Date(final_df$publishedat)
final_df$publishedat <- ymd_hms(final_df$publishedat) 
final_df$hour <- hour(final_df$publishedat)

# Remove unused columns
final_df$publishedat <- NULL
final_df$video_id <- NULL

# Preview the record that have missing values
mean(complete.cases(final_df))

final_df %>%
  filter(rowSums(is.na(.)) > 0) # Have NA in 9 rows

# Replace the missing values by "No information"
final_df$hour <- as.character(final_df$hour)

final_df <- final_df%>%
  mutate(hour = ifelse(is.na(hour), "No information", hour))

write.csv(final_df,"finalyoutube.csv",row.names = FALSE)

### Exploratory Analysis(EDA)

## Descriptive Statistics
summary(final_df$view_count)

# 1.Histogram 
hist(final_df$view_count)
mean(final_df$view_count)
median(final_df$view_count)

# 2.scatter plot 
ggplot(final_df, aes(likes, view_count, col = category_name)) + 
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  scale_x_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M")) +
  labs(title =  "Correlation between likes and view",
       x = "Likes (Millions)", y = "View Count (Millions)")

# Correlation
cor(final_df[ ,c("likes", "comment_count", "view_count")])

# 3.Beeswarm plot
ggplot(final_df, 
       aes(x = category_name, 
           y = view_count, 
           color = category_name)) +
  geom_quasirandom(alpha = 0.7,
                   size = 1.2) + 
  coord_flip()+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Spread of view in each category", 
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none")

# whisker calculate for detecting outlier
q3 <- quantile(final_df$view_count, .75)
q1 <- quantile(final_df$view_count, .25)

IQR <- q3-q1

upper_outlier <- q3 + 1.5 * IQR

print(upper_outlier)

## Explore the data by question

# 1.Which videos are the 10 most popular?
final_df %>%
  group_by(category_name) %>%
  summarise(total_view = sum(view_count)) %>%
  arrange(desc(total_view)) %>%
  head(10)

final_df %>%
  group_by(category_name) %>%
  summarise(median_view = median(view_count)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(category_name, median_view), median_view))+ 
  geom_col(fill = "skyblue")+
  coord_flip()+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M")) +
  labs(title = "The most of view in each category",
       x = "Category", y = "View")+
  theme_minimal()

# 2.When do YouTubers upload videos most often?
final_df %>%
  group_by(hour) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5)%>% 
  ggplot(aes(x = hour, y = count))+
  geom_col(fill = "wheat")+
  theme_minimal()

# 3.Which YouTube channel has the most views?
final_df %>%
  group_by(channeltitle)%>%
  summarise(view = sum(view_count))%>%
  arrange(desc(view))%>%
  head(5) %>%
  ggplot(aes(reorder(channeltitle,-view), view))+
  geom_col(fill = "salmon")+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))+
  labs(title = "Top performace youtuber",
       x = "Channel name", y = "View")+
  theme_minimal()

# 4.Which YouTube music channel is the most popular?
final_df %>%
  filter(category_name == "Music") %>%
  group_by(channeltitle)%>%
  summarise(view = sum(view_count))%>%
  arrange(desc(view))%>%
  head(5) %>%
  ggplot(aes(reorder(channeltitle,-view), view))+
  geom_col(fill = "seagreen")+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))+
  labs(title = "Popular music YouTube channels",
       x = "Channel name", y = "View")+
  theme_minimal()


# 5.Which brand of mobile phone content will people watch between the iPhone and Samsung?
phone <- final_df %>%
  mutate(type = ifelse(grepl("samsung", tags, ), "samsung", 
                       ifelse(grepl("iphone",tags), "iphone", "others" ))
  )

phone %>%
  filter(type %in% c("samsung","iphone"))%>%
  group_by(type) %>%
  summarise(total_view = sum(view_count))

phone %>% 
  filter(type %in% c("samsung","iphone")) %>%
  group_by(type) %>%
  summarise(view = median(view_count)) %>%
  ggplot(aes(type, view)) + 
  geom_col(aes(fill = type))+
  scale_fill_manual(values = c("lightpink","lightgreen"))+
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))+
  labs(title = "Iphone vs Samsung",
       x = "", y = "View")+
  theme_minimal()
