# raw-data以下のファイルをrdaファイルに保存する場合
library(tidyverse)
filename <- dir("./")
csvname <- stringr::str_c("",filename[stringr::str_detect(filename,".csv")])

alldata <- purrr::map_dfr(csvname, function(x) read_csv(x))

usethis::use_data(alldata, overwrite=TRUE)

# example1
data_example1 <- read.csv("example1.csv")
usethis::use_data(data_example1, overwrite=TRUE)

# production modelのOMを回してそれをrdaファイルに保存する場合
r_common <- 0.4
end_year <- 2020

# high contrast
setting_high_contrast <- list(
    r = r_common,
    K = 1000,
    B1_ratio = 1, # Kから個体群動態がスタートする
    sigma_i = c(0.2, 0.2), # プロセス誤差も観測誤差も比較的小さい
    sigma_p = 0.1, 
    start_year_catch = 1970, # データは50年分ある
    end_year_catch = end_year,
    F_given = c(0.05, r_common*0.8, 0.05), # 最初Fが小さく、徐々に大きくなった後に小さくなる＝コントラストが高い
    ncpue = 2, # CPUEが2つある
    start_year_cpue = c(1950, 1950),
    end_year_cpue = rep(end_year,2),
    seed=1,
    stock_name = "data_rich"
)

res_high_contrast <- list()
nsim <- 3
for(ss in 1:nsim){
  res_high_contrast[[ss]] <- setting_high_contrast %>% list_modify(seed=ss) %>%
    do.call(args=., what=prod_OM) 
}

data_high_con1 <- res_high_contrast[[1]]$obs %>% mutate(Memo=NA, CV=NA, Stock="high_con") 
usethis::use_data(data_high_con1, overwrite=TRUE)

data_high_con2 <- res_high_contrast[[2]]$obs %>% mutate(Memo=NA, CV=NA, Stock="high_con") 
usethis::use_data(data_high_con2, overwrite=TRUE)

data_high_con3 <- res_high_contrast[[3]]$obs %>% mutate(Memo=NA, CV=NA, Stock="high_con") 
usethis::use_data(data_high_con3, overwrite=TRUE)    


# no contrast

setting_no_contrast <- setting_high_contrast %>%
    list_modify(
        start_year_catch = 2000, # データすごく短い
        F_given = r_common*c(0.6,0.6,0.6), # Fずっと一定
        B1_ratio = 0.4, # Bmsyのちょっと小さいところからスタート
        F_year = c(2000,2010,end_year),
        start_year_cpue = c(2000, 2000),
        end_year_cpue = rep(end_year, 2))

res_no_contrast <- list()
nsim <- 3
for(ss in 1:nsim){
  res_no_contrast[[ss]] <- setting_no_contrast %>% list_modify(seed=ss) %>%
    do.call(args=., what=prod_OM) 
}

data_no_con1 <- res_no_contrast[[1]]$obs %>% mutate(Memo=NA, CV=NA, Stock="no_con") 
usethis::use_data(data_no_con1, overwrite=TRUE)

data_no_con2 <- res_no_contrast[[2]]$obs %>% mutate(Memo=NA, CV=NA, Stock="no_con") 
usethis::use_data(data_no_con2, overwrite=TRUE)

data_no_con3 <- res_no_contrast[[3]]$obs %>% mutate(Memo=NA, CV=NA, Stock="no_con") 
usethis::use_data(data_no_con3, overwrite=TRUE)    

