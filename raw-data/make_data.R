# raw-data以下のファイルをrdaファイルに保存する場合
library(tidyverse)
filename <- dir("./")
csvname <- stringr::str_c("",filename[stringr::str_detect(filename,".csv")])

alldata <- purrr::map_dfr(csvname, function(x) read_csv(x))

usethis::use_data(alldata, overwrite=TRUE)

# production modelのOMを回してそれをrdaファイルに保存する場合

