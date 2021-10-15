#'
#' @import ggplot2
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import readr
#' @import forcats
#' @import stringr
#' @import assertthat
#' @import patchwork
#' @importFrom magrittr %>%
#' @importFrom magrittr %T>%
#' @importFrom dplyr filter
#'

# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# Spictの結果のリストから推定結果をtibbleにしてつなげる関数

#' @export

get_spict_parameter <- function(res_list, id_name="id"){
  res_tibble <- res_list %>%
    purrr::map_dfr(function(x){
      if(class(x)!="try-error"){
        aa <- sumspict.parest(x) %>%
          as.data.frame() %>%
          rownames_to_column() %>%
          mutate(parameter=str_replace_all(rowname," ","")) %>%
          select(-rowname)
      }
      else{
        aa <- NULL
      }
    },.id=id_name)
  return(res_tibble)
}


#'
#' 共通データからjabba用のデータを作成する関数
#'
#' @param data_raw 共通のデータフォーマット。c("Year","Value","Stock","Label","Fleet","CV")の列名を持つ。"Label"は"Catch"または"Index"、Fleetは漁獲量データの場合は"All"、CPUEデータの場合は任意の漁業種名
#' @export

get_jabba_data <- function(data_raw){
  
  assertthat::assert_that(all(c("Year","Stock","Value","Label","Fleet","CV") %in% colnames(data_raw)))
  assertthat::assert_that(all(unique(data_raw$Label)==c("Catch","Index")))
  
  data_jabba <- list()
  data_jabba$catch <- data_raw %>%
    dplyr::filter(Label=="Catch") %>%
    select(Year, Value) %>%
    rename(catch=Value) %>%
    as.data.frame()

  data_jabba$cpue <- data_raw %>%
    dplyr::filter(Label=="Index") %>%
    select(Year, Value, Fleet) %>%
    pivot_wider(names_from=Fleet, values_from=Value) %>%
    as.data.frame()

  data_jabba$se <- data_raw %>%
    dplyr::filter(Label=="Index") %>%
    select(Year, CV, Fleet) %>%
    pivot_wider(names_from=Fleet, values_from=CV) %>%
    as.data.frame()

  data_jabba$stock <- stringr::str_c(unique(data_raw$Stock))

  return(data_jabba)

}

#'
#' 共通データからSPiCT用のデータを作成する関数
#'
#' @param data_raw 共通のデータフォーマット。c("Year","Value","Stock","Label","Fleet","CV")の列名を持つ。"Label"は"Catch"または"Index"、Fleetは漁獲量データの場合は"All"、CPUEデータの場合は任意の漁業種名
#'
#' @export 

get_spict_data <- function(data_raw){

  assertthat::assert_that(all(c("Year","Stock","Value","Label","Fleet","CV") %in% colnames(data_raw)))
  assertthat::assert_that(all(unique(data_raw$Label)==c("Catch","Index")))
  
  data_spict <- list()

  # create catch data
  obsC <- data %>% dplyr::filter(Fleet=="All") %>%
    select(Year, Value) %>%
    rename(timeC=Year, obsC=Value) %>%
    as.list()

  # create cpue data
  name_fleet <- unique(data$Fleet)
  name_fleet <- name_fleet[name_fleet!="All"]
  obsI <- list(timeI=list(), obsI=list())

  for(i in 1:length(name_fleet)){
    tmpdata <- data %>% dplyr::filter(Fleet==name_fleet[i]) %>%
      select(Year, Value, CV) 
    obsI$timeI[[i]] <- tmpdata$Year
    obsI$obsI[[i]]  <- tmpdata$Value
  }

  data_spict <- c(obsC, obsI)
  data_spict$stock <- str_c(unique(data_raw$Stock))
  
  return(data_spict)
  
}
