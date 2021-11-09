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
#' @param is_se_NA seの項目にデータが入っているが、推定には使いたくないとき、seの値をNAで置き換える
#' @export

get_jabba_data <- function(data_raw, is_se_NA = TRUE){
  
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

  if(is_se_NA==TRUE){
    data_jabba$se[,-1] <- NA
  }

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
  obsC <- data_raw %>% dplyr::filter(Fleet=="All") %>%
    select(Year, Value) %>%
    rename(timeC=Year, obsC=Value) %>%
    as.list()

  # create cpue data
  name_fleet <- unique(data_raw$Fleet)
  name_fleet <- name_fleet[name_fleet!="All"]
  obsI <- list(timeI=list(), obsI=list())

  for(i in 1:length(name_fleet)){
    tmpdata <- data_raw %>% dplyr::filter(Fleet==name_fleet[i]) %>%
      select(Year, Value, CV) 
    obsI$timeI[[i]] <- tmpdata$Year
    obsI$obsI[[i]]  <- tmpdata$Value
  }

  data_spict <- c(obsC, obsI)
  data_spict$stock <- str_c(unique(data_raw$Stock))
  
  return(data_spict)
  
}

# data cloneしたら良くなるかと思ったけど、むしろ激遅

data_clone <- function(data_jabba, n=10){
    ddata <- data_jabba$cpue
    ddata_no_year <- data_jabba$cpue %>% select(-Year)
    for(i in 1:n){
        ddata <- bind_cols(ddata, ddata_no_year)
    }
    data_jabba$cpue <- ddata

    ddata <- data_jabba$se
    ddata_no_year <- data_jabba$se %>% select(-Year)
    for(i in 1:n){
        ddata <- bind_cols(ddata, ddata_no_year)
    }
    data_jabba$se <- ddata    

    return(data_jabba)
}

#'
#' @export
#' 

get_m_from_BK <- function(BK){
  
  tmpfunc <- function(x) x^(-1/(x-1))
  objfunc <- function(x, BK){
    (tmpfunc(x)-BK)^2
  }
  res <- optimize(objfunc, c(1,2), BK)
  return(res)
}


plot_both <- function(res_spict, res_jabba){

  alldata <- get_bothres(res_spict, res_jabba)

  g1 <- alldata %>% dplyr::filter(stat%in%c("Biomass","F")) %>%
    ggplot() +
    geom_point(aes(x=year,y=est, color=model)) +
    geom_line(aes(x=year,y=est, group=model, color=model)) +
    geom_line(aes(x=year,y=ll, group=model, color=model),lty=2) +
    geom_line(aes(x=year,y=ul, group=model, color=model),lty=2) +        
    facet_wrap(.~stat,scale="free_y") +
    theme_bw()

  g2 <- alldata %>% dplyr::filter(is.na(year)) %>%
    dplyr::filter(stat%in%c("r","K","theta","sigma.proc")) %>% 
    ggplot() +
    geom_point(aes(x=model,y=est, color=model)) +
    geom_linerange(aes(x=model,ymax=ul, ymin=ll, color=model)) +
    facet_wrap(.~stat,scale="free_y") + ylim(0,NA) +
    theme_bw()  

  require(patchwork)
  return(list(g1,g2))

  ## par(mfrow=c(2,1))
  ## # plot biomass
  ## plot(res_jabba$timeseries[,"mu","B"],type="b", ylim=c(0,1100))
  ## lines(res_jabba$timeseries[,"lci","B"])
  ## lines(res_jabba$timeseries[,"uci","B"])  
  ## Best <- get.par("logB", res_spict, exp = TRUE) %>%
  ##     as.data.frame() %>%
  ##     rownames_to_column(var="year") %>%
  ##     mutate(season=as.numeric(year)-floor(as.numeric(year))) %>%
  ##     dplyr::filter(season==0)
  ## points(Best$est,col=2,type="b")
  ## lines(Best$ll,col=2)
  ## lines(Best$ul,col=2)

  ## # plot F
  ## plot(res_jabba$timeseries[,"mu","F"],type="b",ylim=c(0,0.4))
  ## lines(res_jabba$timeseries[,"lci","F"])
  ## lines(res_jabba$timeseries[,"uci","F"])    
  ## Best <- get.par("logF", res_spict, exp = TRUE) %>%
  ##     as.data.frame() %>%
  ##     rownames_to_column(var="year") %>%
  ##     mutate(season=as.numeric(year)-floor(as.numeric(year))) %>%
  ##     dplyr::filter(season==0)
  ## points(Best$est,col=2,type="b")
  ## lines(Best$ll,col=2)
  ## lines(Best$ul,col=2)
}

#' derive summary results of both models
#'
#' @export

get_bothres <- function(res_spict, res_jabba){
  res_jabba_tidy <- get_jabba_res(res_jabba)
  res_spict_tidy <- get_spict_res(res_spict)
  bind_rows(res_jabba_tidy, res_spict_tidy)
}

get_jabba_res <- function(res_jabba){
  
  get_stat_ <- function(stat_name){
    res_jabba$timeseries[,,stat_name] %>%
      as.data.frame() %>%
      rownames_to_column(var="year") 
  }  

  FBdata <- bind_rows(get_stat_("B") %>% mutate(stat="Biomass"),
                      get_stat_("F") %>% mutate(stat="F")) %>%
    rename(est=mu, ll=lci, ul=uci)

  pardata <- res_jabba$estimates %>%
    rownames_to_column(var="stat") %>%
    as_tibble() %>%    
    rename(est=mu, ll=lci, ul=uci) %>%
    mutate(stat=ifelse(stat=="m","theta",stat)) %>%
    mutate(stat=ifelse(stat=="sigma.proc","sdb",stat)) 

  bind_rows(FBdata, pardata) %>% mutate(model="jabba")
}

get_spict_res <- function(res_spict){

  get_stat_ <- function(stat_name){
    get.par(stat_name, res_spict, exp = TRUE, CI=0.95) %>%
      as.data.frame() %>%
      rownames_to_column(var="year") %>%
      mutate(season=as.numeric(year)-floor(as.numeric(year))) %>%
      dplyr::filter(season==0)  %>%
      select(-season)    
  }

  # biomass time series
  FBdata <- bind_rows(get_stat_("logB") %>% mutate(stat="Biomass"),
                      get_stat_("logF") %>% mutate(stat="F"))
  
  # important parameter
  pardata <- sumspict.parest(res_spict, CI=0.95) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(parameter=str_replace_all(rowname," ","")) %>%
    select(-rowname, -log.est) %>%
    rename(est=estimate, ll=cilow, ul=ciupp, stat=parameter) %>%
    mutate(stat=ifelse(stat=="n","theta",stat))    

  bind_rows(pardata,FBdata) %>% mutate(model="spict")

}
