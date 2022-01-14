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
  res <- optimize(objfunc, c(1,100), BK)
  return(res)
}

#'
#' @export
#' 

get_BK_from_m <- function(m){
  m^(-1/(m-1))  
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

get_bothres <- function(res_spict=NULL, res_jabba=NULL){

  if(!is.null(res_spict) && class(res_spict)!="try-error") res_spict_tidy <- get_spict_res(res_spict)
  else res_spict_tidy <- NULL
    
  if(!is.null(res_jabba) && class(res_jabba)!="try-error") res_jabba_tidy <- get_jabba_res(res_jabba)
  else res_jabba_tidy <- NULL
  
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
  FBdata <- bind_rows(get_stat_("logB") %>% mutate(stat="B"),
                      get_stat_("logF") %>% mutate(stat="F"),
                      get_stat_("logBBmsy") %>% mutate(stat="BBmsy"),
                      get_stat_("logFFmsy") %>% mutate(stat="FFmsy"))
  
  # important parameter
  pardata <- sumspict.parest(res_spict, CI=0.95) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(parameter=str_replace_all(rowname," ","")) %>%
    select(-rowname, -log.est) %>%
    rename(est=estimate, ll=cilow, ul=ciupp, stat=parameter)

  otherdata <- bind_rows(tibble(stat="convergence",est=res_spict$opt$convergence),
                         tibble(stat="number_se_nan",est=sum(is.nan(res_spict$sd))),
                         tibble(stat="number_se_infinite",est=sum(is.infinite(res_spict$sd))))

  bind_rows(pardata,FBdata,otherdata) %>% mutate(model="spict") %>% mutate(year=as.numeric(year))

}

#' @export
#' 

randam_walk <- function(init, rho, sigma, T, adjust.sigma=TRUE){
  res <- 1:T
  if(adjust.sigma==TRUE){
    rand <- rnorm(T, 0, sqrt((1-rho^2))*sigma)
  }
  else{
    rand <- rnorm(T, 0, sigma)    
    }
  res[1] <- init
  for(i in 2:T){
    res[i] <- res[i-1] * rho + rand[i]
  }
  return(tibble(year=1:T, value=res))
}


#'
#' fit.spictのwrapper
#'
#' fit.spictを一度実行してから、初期値を変えてntrials分計算しなおし、その結果から目的関数の値が最も小さい結果を示す結果とそのときのインプットを出力する関数
#'
#' @details
#' 1. デフォルトの初期値での計算の実施
#' 2. 収束しているかどうかなどの結果の出力（`res$opt$convergence`、`all(is.finite(res$sd)`、`calc.om(res)`)）
#' 3. ntrialsで指定した回数分、初期値を変えて再計算
#' 4. そのときのntrials回分のパラメータ推定結果を出力
#' 5. 目的関数の値が最も小さい結果が得られたときの初期値でもう一度計算
#' 6. もう一度計算した結果について、収束しているかどうかなどの結果を出力（`res$opt$convergence`、`all(is.finite(res$sd)`、`calc.om(res)`)）
#' 7. インプットデータと結果を返す
#' 
#' @export
#' 

fit.spict_tol <- function(inp, ntrials=30, seed=1){

  set.seed(seed)
  res<-fit.spict(inp)
  check_spictres(res) 
 
  fit<-check.ini(res,ntrials=ntrials) #本当はもっと大きい値のntrialsが必要．理想は？
  obj <- as.data.frame(fit$check.ini$resmat)$obj #初期値を変えたtrialによって推定された値
  obj[1] <- Inf
  min.obj <- which.min(obj)
  
  trial <-str_c("Trial ",min.obj-1) #ここで自分の選んだtrial noを指定
  cat("Trial ",trial," is used\n")
  init_rev <-fit$check.ini$inimat[trial,,drop=F]
  init_name <- colnames(init_rev)[-1]
  for(i in 1:length(init_name)){
    inp$ini[[init_name[i]]] <- as.numeric(init_rev[i+1])
  }
  ## rownames(b)<-NULL
  ## inp$ini$logn<-b[,"logn"]
  ## inp$ini$logK<-b[,"logK"]
  ## inp$ini$logm<-b[,"logm"]
  ## inp$ini$logq1<- b[,"logq1"]
  ## inp$ini$logq2<- b[,"logq2"]
  ## inp$ini$logsdb<- b[,"logsdb"]
  ## inp$ini$logsdf<- b[,"logsdf"]
  ## inp$ini$logsdi1<- b[,"logsdi1"]
  ## inp$ini$logsdi2<- b[,"logsdi2"]
  ## inp$ini$logsdc<- b[,"logsdc"]

  res_rev <-fit.spict(inp)
  check_spictres(res_rev)

  return(list(inp=inp, res=res_rev))
  
}

check_spictres <- function(res){
  tribble(
    ~test, ~result,
    "is_converged", ifelse(res$opt$convergence==0,TRUE,FALSE), #これが0だったら，収束しているのでOK
    "is_all_sd_finite", all(is.finite(res$sd))) %>% print() #これがTRUEだったら，推定されたパラメータの分散が全て有限であるということでOK
  try(calc.om(res)) %>% print()
  cat("objective function value",res$opt$objective,"\n")
}

#'
#' 推定パラメータの一覧を簡単に表示する関数
#'
#' @export
#'
#' 

quickplot <- function(res, fishr=NA, title_name=NA){
    maxcatch <- max(res$inp$obsC)
    dres <- get_spict_res(res)
    maxyear  <- dres$year %>% max(na.rm=T)
    est_par <- c("r","K","n","q","sdb","sdf","sdi","sdc","convergence","number_se_infinite")
    derive_par <- c("B","F","BBmsy","FFmsy")

    tmp <- dplyr::filter(dres, stat%in%derive_par & year==maxyear) %>%
        mutate(stat2=str_c(stat,maxyear)) %>% select(-stat) %>%
        rename(stat=stat2)

    level_stat <- c(est_par, str_c(derive_par,maxyear))
    dres_part <- bind_rows(dplyr::filter(dres, stat%in%est_par),tmp) %>%
        mutate(stat=factor(stat,levels=level_stat))
    
    info <- tibble(stat=factor(c(rep("r",length(fishr)),"K","K","n","n","convergence","number_se_infinite"),levels=level_stat),est=c(fishr, maxcatch*c(10,100), 1,2,0,0))

    dres_part %>% ggplot() +
        geom_pointrange(aes(x=stat, y=est, ymin=ll, ymax=ul)) +
        facet_wrap(.~stat, scale="free", ncol=8) + ylim(0,NA) +
        geom_hline(data=info, aes(yintercept=est), color=2, lty=2) +
        theme_bw(base_size=16) +
        ggtitle(title_name)

}

do_grid_search <- function(inp1,
                           shape   = c(0.7,1.01,1.19,2,3,4:10),
                           r       = c(0.02,0.03,0.05,0.1,0.15,0.20,0.25,0.30),
                           ntrials = 20){
  tmpfunc <- function(x){ res <- x$value; names(res) <- x$stat_name; return(res)}
  tmpfunc2 <- function(res){
    if(class(res)!="try-error"){
      parname <- c("r","K","q","theta","sdb","sdf","sdi","sdc")
      get_spict_res(res) %>% dplyr::filter(is.na(year), stat %in% parname) %>%
        select(est:stat) %>%
        pivot_longer(cols=c(est,ll,ul)) %>%
        mutate(stat_name=str_c(stat,name,sep="-")) %>%
        select(-stat,-name) %>% tmpfunc() %>%
        as.data.frame() %>% t()
    }
    else{
      NULL
    }
  }

  mat_par <- expand.grid(shape = shape, r = r)

  res_model12 <- purrr::map_dfr(1:nrow(mat_par),function(x){
    inp1$priors$logn <- c(log(mat_par$shape[x]),1e-3,1) 
    inp1$priors$logr <- c(log(mat_par$r    [x]),1e-3,1) 
    tmp <-try(fit.spict_tol(inp1, ntrials=ntrials)$res)
    if(class(tmp)!="try-error"){
      res <- cbind(mat_par[x,], tmpfunc2(tmp))
      res$obj <- tmp$opt$objective
      res$convergence <- tmp$opt$convergence
      res$objective   <- tmp$opt$obj
      res$unfinite_sd <- all(is.finite(tmp$sd))
      res <- as_tibble(res) %>%
        mutate(result=list(tmp))
    }
    else{
      NULL
    }
    res
  })

  res_model12$BmsyK <- get_BK_from_m(res_model12$shape)

  res_model12 <- res_model12 %>%
    mutate(model_OK = (convergence==0 & unfinite_sd==TRUE))
  min.obj <- res_model12$obj[res_model12$model_OK==TRUE] %>% min()
  res_model12 <- res_model12 %>%
    mutate(likely_model = factor(as.character((obj<min.obj+2)),levels=c("TRUE","FALSE")))
  res_model12
}

plot_grid_result <- function(res_model12, inp){
  
  g_obj <- res_model12 %>% dplyr::filter(model_OK==TRUE) %>%
    ggplot() + 
    geom_point(aes(x=BmsyK, color=factor(r), y=obj, shape=likely_model), cex=3) +
    theme_bw(base_size=16) +  ylab("Objective function value") +
    scale_shape_manual(values=c(20,3))

  g_K <- res_model12 %>% dplyr::filter(model_OK==TRUE) %>%
    ggplot() + 
    geom_point(aes(x=BmsyK, color=factor(r), y=get("K-est"), shape=likely_model), cex=3) +
    theme_bw(base_size=16) + 
    scale_shape_manual(values=c(20,3)) +
    geom_hline(yintercept=max(inp$obsC)*10)

  g_Kci <- res_model12 %>% dplyr::filter(model_OK==TRUE) %>%
    ggplot() + 
    geom_pointrange(aes(x=BmsyK, color=factor(r), y=get("K-est"),ymin=get("K-ll"),ymax=get("K-ul"),
                        shape=likely_model), cex=3) +
    theme_bw(base_size=16) +
    facet_wrap(.~r) + ylab("Estimated K with 95% CI") +
    scale_shape_manual(values=c(20,3)) +
    geom_hline(yintercept=max(inp$obsC)*10, lty=2)

  #res_model12 %>% ggplot() + 
  #    geom_pointrange(aes(x=BmsyK, color=factor(r), y=get("r-est"), ymin=get("r-ll"),ymax=get("r-ul"),
  #shape=unfinite_sd), cex=1) +
  # theme_bw(base_size=16) + 
  #  scale_shape_manual(values=c(3,20))
  library(patchwork)
  g_obj + g_Kci
}
