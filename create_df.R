library(tidyverse)


files <- list.files(path="./data", pattern="*_norisk.txt", full.names=TRUE, recursive=FALSE)
all_data <- NA

convert_data <- function(df, annotation) {
 # df <- bilidata
#  annotation <- filename
new_df <- NA
i = 0
  
for (column in df){
  tmp  <- NA
  day <- seq(from = 0, to = 24*(length(column)-1), by = 24) + i
  #day_i <- i+1
  #day <- day[day_i]
  tmp  <- tibble(time = day, bilirubin = column, annotation = annotation)
  #tmp <- rbind(tmp, tibble(time = 336, bilirubin = max(tmp$bilirubin, na.rm = TRUE), annotation = annotation))
  
  new_df <- rbind(new_df, tmp)
  #print(column)
  i = i + 1
}
  return(new_df)
}


for(nbr in 1:length(files)){
  bilidata <- read_delim(files[nbr], delim = " ") %>% select(-`0`)
  filename <- basename(files[nbr])
  filename <- sub(".txt", "", filename)
  converted_data <- NA
  converted_data <- convert_data(bilidata, filename)
  all_data <- rbind(all_data, converted_data)
}


all_data$bilirubin <- as.double(all_data$bilirubin)
max_vals <- all_data %>% group_by(annotation)  %>% summarise(bilirubin = max(bilirubin, na.rm = TRUE)) %>% mutate(time = 360) %>% filter(!is.na(annotation))
all_data <- rbind(max_vals, all_data)
#write_tsv(all_data, "./data/all_norisk.tsv")

ggplot(all_data, aes(x = time/24, y = bilirubin, col = annotation)) + geom_line() + ylim(0,20) + xlim(0,16)
all_data <- all_data %>% filter(annotation == "35w_norisk")
g <-
  ggplot(all_data, aes(y = bilirubin, x = time/24, col = annotation, linetype = annotation)) +
  geom_line(data = all_data %>% filter(annotation != "sample"))  + theme_bw() + xlim(0, 7) + ylim(3, 25) + xlab("age in days") + ylab("bilirubin, mg/dL") +
  geom_point(
    data = all_data %>% filter(annotation == "sample", bilirubin > 0),
    aes(y = bilirubin, x = time/24, col = annotation),
    size = 3
  ) + theme(
    text = element_text(size = 20),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank()
  ) + scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7,8,9,10,11,12,13,14,15,16),
                         limits = c(0, 16))
g
