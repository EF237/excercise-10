library(tidyverse)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv"

d <- read_csv(f, col_names= TRUE)