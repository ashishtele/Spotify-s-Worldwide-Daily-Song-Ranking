rm(list = ls())

load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(tree))
  suppressPackageStartupMessages(library(MASS))
  suppressPackageStartupMessages(library(mice))
  suppressPackageStartupMessages(require(xgboost))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(Matrix))
  suppressPackageStartupMessages(library(plyr))
}

load_lb()

## Data loading

features <- read.csv("E:/Study/R Projects/Common files/featuresdf.csv",
                     sep = '|', stringsAsFactors = FALSE)
## Using 'fread' to read a file 'spofity' (big file)
spotify <- fread("E:/Study/R Projects/Common files/spofity.csv",
                 sep = ',')

## Data exploration

glimpse(features)

# 'features' file has 16 variales and 100 observations
fea_columns <- names(features)
fea_class <- sapply(features, function(x) class(x))
summary(features)

# Cheking for NA's or missing

percent <- function(x, digits = 1, format = "f", ...) 
{
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

countMissing <- function(x) 
{
  ## calculate counts
  if (mode(x) == "character") emptyStrings = sum(x=="", na.rm=TRUE) else emptyStrings = 0
  if (mode(x) == "numeric") missing = sum(x=="", na.rm=TRUE) else missing = 0
  totalRows = NROW(x)
  nonMissing = totalRows - missing - emptyStrings
  
  ## present results
  cat("#           TOTAL ROWS: ", totalRows, " (", percent(totalRows/NROW(x)), ")\n", sep="")
  cat("# Missing Values (NAs): ", missing, " (", percent(missing/NROW(x)), ")\n", sep="")
  cat("  # Empty Strings (\"\"): ", emptyStrings, " (", percent(emptyStrings/NROW(x)), ")\n", sep="")
  cat("   # Non-missing Value: ", nonMissing, " (", percent(nonMissing/NROW(x)), ")\n", sep="")
  cat("    Mode & Class: ", mode(x), ", ", class(x), "\n", sep="")
}


for (i in 1:length(fea_columns))
{
  countMissing(features[i])
}

#OR
sapply(features, function(x) sum(is.na(x)))

## No missing values, clean data

# Adding new columns: Duration sec

features %>%
  mutate(duration_sec = round(duration_ms/1000)) -> features
glimpse(features)

# Exploring the top 100 song list

features %>%
  group_by(artists) %>%
  dplyr::summarise(cnt = n()) %>%
  filter(cnt>1) %>%
  arrange(-cnt)-> artists

# absolute chart: stat="identity"
ggplot(head(artists,10), aes(x=reorder(artists,cnt), y = cnt, fill = "red" )) +
  geom_bar(stat = "identity") +
  labs(title = "Top Artists in 2017", x="Artists", y="# apperances in top 100") +
  geom_text(aes(label=cnt), hjust=2, color = "white") +
  coord_flip()


# Total playing time

names(spotify)[2] <- "name"

glimpse(spotify)

spotify %>%
  filter(Region=="us") %>%
  group_by(name) %>%
  dplyr::summarise(tot_str = sum(Streams)) -> us_stream
us_stream

features %>%
  left_join(us_stream, by = "name") %>%
  dplyr::select(name, artists, duration_sec, tot_str) %>%
  mutate(tot_time = duration_sec*tot_str/60000) %>%
  arrange(-tot_time) -> top_by_play
top_by_play

top_by_play %>%
  group_by(artists) %>%
  dplyr::summarise(tot_art = sum(tot_time)) %>%
  arrange(-tot_art) %>%
  top_n(20) -> top_artist_play

# Playing time by artist
ggplot(head(top_artist_play,10), aes(x=reorder(artists,tot_art), y = tot_art, color = artists )) +
  geom_point(size=3) + 
  geom_segment(aes(x=artists,xend=artists, y=0, yend=tot_art)) +
  labs(title = "Top Artists in 2017 in US (play time)", x="Artists", y="# apperances in top 100") +
  #geom_text(aes(label=round(tot_art)), color = "red") +
  coord_flip()

# Playing time by track name
ggplot(head(top_by_play,10), aes(x=reorder(name,tot_time), y = tot_time, color = "red")) +
  geom_point(size=3) + 
  geom_segment(aes(x=name,xend=name, y=0, yend=tot_time)) +
  labs(title = "Top Tracks in 2017 in US", x="Tracks", y="track time") +
  #geom_text(aes(label=round(tot_art)), color = "red") +
  coord_flip()


# Ed Sheeran performance

spotify %>%
  filter(Region=="us" & Artist=="Ed Sheeran" & Position<100) %>%
  group_by(name) %>%
  dplyr::summarise(cnt = n()) %>%
  arrange(-cnt)-> Ed_data
Ed_data


## Variable correlation

fea_num_data <- features[,names(fea_class[!fea_class=="character"])]
corl <- cor(fea_num_data)
corrplot::corrplot(corl)
# Loudness and enery positively correlated


# Map the pitch class


features$key <- as.character(features$key)
features$key <- revalue(features$key,
                        c("0" = "C", "1" = "C???,D???", "2" = "D", "3" = "D???,E???", "4" = "E", "5" =  "F", "6" = "F???,G???","7" = "G","8" = "G???,A???","9" = "A","10" = "A???,B???","11" = "B"))

features %>%
  group_by(key) %>%
  dplyr::summarise(cnt = n()) %>%
  arrange(-cnt) -> song_key
song_key

## Key Notes
ggplot(song_key, aes(x=reorder(key,cnt),y=cnt, fill = reorder(key,cnt))) +
  geom_bar(stat = "identity") +
  labs(title = "Key notes", x="Keys", y="count") +
  geom_text(aes(label = cnt), hjust = 2) +
  coord_flip()

## Density plots of correlated variables

ggplot(features) +
  geom_density(aes(energy, fill = "energy", alpha = 0.3)) +
  geom_density(aes(valence, fill = "valence", alpha = 0.3)) +
  geom_density(aes(danceability, fill = "danceability", alpha = 0.3)) +
  ggtitle("Density plots")
## Distribution look similar

## Density plot of loudness

ggplot(features) +
  geom_density(aes(loudness, fill = "loudness", alpha = 0.3)) +
  ggtitle("Density plot of loudness")


## Loudness and speechiness

ggplot(features) +
  geom_density(aes(loudness, fill = "loudness", alpha = 0.3)) +
  geom_density(aes(log(speechiness), fill = "speechiness", alpha = 0.3)) +
  ggtitle("Density plot of loudness & Speechiness")

# log() to bring the ranges same
## Negative correlation between 'Loudness' and 'Speechiness' can be 
## confirmed by density plots


## Tempo and danceability

ggplot(features) +
  geom_density(aes(log(tempo), fill = "tempo", alpha = 0.3)) +
  geom_density(aes(log(danceability), fill = "danceability", alpha = 0.3)) +
  ggtitle("Density plot of Tempo & danceability")

## Negative correlation between 'tempo' and 'danceability' can be 
## confirmed by density plots

head(spotify)
names(spotify)

spotify %>%
  filter(Region=="global") %>%
dplyr::group_by(Artist) %>%
    dplyr::summarise(n_str = sum(Streams)) %>%
    arrange(-n_str) -> streams_global
streams_global

# Plotting global trend
ggplot(head(streams_global,15), aes(x=reorder(Artist,n_str),y=n_str,
                                              color = Artist)) +
  geom_point(size=2) +
  geom_segment(aes(x=Artist,xend = Artist, y=0, yend = n_str)) +
  labs(title = "Global Trend by Artists", x="Artist", y="Streams") +
  coord_flip()

## 'Post Malone','Kendrick Lamar' and 'Luis Fonsi' are leading the
## global trend by good margin


## Checking for 'Track trend' globally
spotify %>%
  filter(Region=="global") %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(n_str = sum(Streams)) %>%
  arrange(-n_str) -> streams_global_name
streams_global_name

ggplot(head(streams_global_name,10), aes(x=reorder(name,n_str),y=n_str,
                                    color = name)) +
  geom_point(size=2) +
  geom_segment(aes(x=name,xend = name, y=0, yend = n_str)) +
  labs(title = "Global Trend by Track name", x="Name", y="Streams") +
  coord_flip()
## 'Shape of you' is leading globally

spotify %>%
  filter(Region=="us" & Artist=="Ed Sheeran" & Position<100) %>%
  group_by(name) %>%
  dplyr::summarise(cnt = n()) %>%
  filter(cnt>20) %>%
  arrange(-cnt)-> Ed_data_1


spotify %>%
  filter(name %in% Ed_data_1$name & Position<100) %>%
  ggplot(aes(x=date(Date), y=Position, col=name)) +
  geom_point(size=3, alpha=0.8) +
  scale_y_reverse(breaks = seq(0,100,10)) +
  scale_x_date() +
  labs(title = "Song Trend", x="Date",y="Position")

head(features)
## Most important factor

features[,names(fea_class[!fea_class=="character"])] %>%
  mutate(stnd = c(1:100)) -> fea1

tr_model <- rpart(stnd~., data=fea1)
rpart.plot::rpart.plot(tr_model, box.palette = "GnBu")


