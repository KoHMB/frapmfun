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
  assertthat::assert_that(all(unique(data_raw$Label)%in%c("Catch","Index")))
  
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
  res <- optimize(objfunc, c(0.01,100), BK)
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
                         tibble(stat="number_se_inf",est=sum(is.infinite(res_spict$sd))))

  otherdata2 <- res_spict %>% get.par("logbkfrac",.,exp=TRUE) %>% as_tibble() %>% mutate(stat="bkfrac")

  bind_rows(pardata,otherdata2, FBdata,otherdata) %>% mutate(model="spict") %>% mutate(year=as.numeric(year))

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

fit.spict_tol <- function(inp, ntrials=30, seed=1, out="both", silent=FALSE){

  set.seed(seed)
  res<-fit.spict(inp)
  if(!silent) check_spictres(res) 

  if(res$opt$convergence!=0){
    fit<-check.ini(res,ntrials=ntrials) #本当はもっと大きい値のntrialsが必要．理想は？
    obj <- as.data.frame(fit$check.ini$resmat)$obj #初期値を変えたtrialによって推定された値
    obj[1] <- Inf
    min.obj <- which.min(obj)
  
    #trial <-str_c("Trial ",min.obj-1) #ここで自分の選んだtrial noを指定
    #cat("Trial ",trial," is used\n")
    #init_rev <-fit$check.ini$inimat[trial,,drop=F]
    #init_name <- colnames(init_rev)[-1]
    #for(i in 1:length(init_name)){
    #  inp$ini[[init_name[i]]] <- as.numeric(init_rev[i+1])
    #}
    
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

    fit$inpsens_list[[min.obj]]$do.sd.report <- TRUE
    res_rev <-fit.spict(fit$inpsens_list[[min.obj]])
    if(!silent) check_spictres(res_rev)
  }
  else{
    res_rev <- res
  }

  if(out=="both") return(list(inp=inp, res=res_rev)) else return(res_rev)
  
}

check_spictres <- function(res){
  tribble(
    ~test, ~result,
    "is_converged", ifelse(res$opt$convergence==0,TRUE,FALSE), #これが0だったら，収束しているのでOK
    "is_all_sd_finite", all(is.finite(res$sd))) %>% print() #これがTRUEだったら，推定されたパラメータの分散が全て有限であるということでOK
  #try(calc.om(res) %>% print()
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
    est_par <- c("convergence","number_se_inf","r","K","n","bkfrac","q","sdb","sdf","sdi")
    derive_par <- c("B","F","BBmsy","FFmsy")

    tmp <- dplyr::filter(dres, stat%in%derive_par & year==maxyear) %>%
        mutate(stat2=str_c(stat,maxyear)) %>% select(-stat) %>%
        rename(stat=stat2)

    level_stat <- c(est_par, str_c(derive_par,maxyear))
    dres_part <- bind_rows(dplyr::filter(dres, stat%in%est_par),tmp) %>%
        mutate(stat=factor(stat,levels=level_stat))
    
    info <- tibble(stat=factor(c(rep("r",length(fishr)),"K","K","n","n","convergence","number_se_inf"),levels=level_stat),est=c(fishr, maxcatch*c(10,100), 1,2,0,0))

    g <- dres_part %>% ggplot() +
      geom_pointrange(aes(x=stat, y=est, ymin=ll, ymax=ul)) +
      facet_wrap(.~stat, scale="free", ncol=2) + ylim(0,NA) +
      geom_hline(data=info, aes(yintercept=est), color=2, lty=2) +
      theme_bw(base_size=16) +
      ggtitle(title_name) +
      coord_flip() +
      theme(axis.text.y=element_blank())
      
    return(g)
}

#' @export
#' 

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
  res.list <- list()
  res_model12 <- purrr::map_dfr(1:nrow(mat_par),function(x){
#  for(x in 1:nrow(mat_par)){
    inp1$priors$logn <- c(log(mat_par$shape[x]),1e-3,1) 
    inp1$priors$logr <- c(log(mat_par$r    [x]),1e-3,1) 
    tmp <-try(fit.spict_tol(inp1, ntrials=ntrials, silent=TRUE)$res)
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
      res <- tibble(result=list())
    }
    res
  })

  res_model12$BmsyK <- get_BK_from_m(res_model12$shape)

  res_model12 <- res_model12 %>%
    mutate(model_OK = (convergence==0 & unfinite_sd==TRUE))
  min.obj <- res_model12$obj[res_model12$model_OK==TRUE] %>% min()
  
  res_model12 <- res_model12 %>%
    mutate(likely_model = as.numeric((obj<min.obj+2))) 
  res_model12$likely_model[res_model12$obj==min.obj] <- 2
  res_model12
}

#'
#' @export
#' 

plot_grid_result <- function(res_model12, inp, r_prior){

  min_obj <- min(res_model12$objective[res_model12$model_OK==TRUE])
  res_model12 <- res_model12 %>% dplyr::filter(model_OK==TRUE)  %>%
    mutate(likely_model=factor(likely_model)) %>%
    mutate(BmsyK=round(BmsyK,3))
  
  g_obj <- res_model12 %>% 
    ggplot() + 
    geom_point(aes(x=r, shape=factor(BmsyK), y=obj), cex=3) +
    geom_line(aes(x=r, group=factor(BmsyK), y=obj, color=BmsyK), cex=1) +      
    theme_bw(base_size=16) +  ylab("Objective function value") +
    #    scale_shape_manual(values=c(3,20,21,2)) +
    theme(legend.position="top") 

#  g_obj2 <- 
#    ggplot() +
#    geom_rect(aes(xmin=r_prior[2],xmax=r_prior[3],ymin=0.3,ymax=0.6),fill="gray") + 
#    geom_point(data=res_model12, aes(x=r, y=BmsyK, cex=obj, shape=likely_model), cex=3) +
#    theme_bw(base_size=16) +  ylab("Bmsy/K") +
#    scale_shape_manual(values=c(3,20,21)) + theme(legend.position="top")
    

  ## g_K <- res_model12 %>% dplyr::filter(model_OK==TRUE) %>%
  ##   ggplot() + 
  ##   geom_point(aes(x=BmsyK, color=factor(r), y=get("K-est"), shape=likely_model), cex=3) +
  ##   theme_bw(base_size=16) + theme(legend.position="top") +
  ##   scale_shape_manual(values=c(20,3)) +
  ##   geom_hline(yintercept=max(inp$obsC)*10)

  g_Kci <- res_model12 %>% 
    ggplot() + 
    geom_pointrange(aes(x=r, color=BmsyK, y=get("K-est"),ymin=get("K-ll"),ymax=get("K-ul"),
                        shape=likely_model)) +
    theme_bw(base_size=16) +theme(legend.position="top") +
    facet_wrap(.~BmsyK, ncol=2) + ylab("Estimated K with 95% CI") +
    scale_shape_manual(values=c(3,20,21)) + coord_cartesian(ylim=c(0,max(inp$obsC)*100)) +
    geom_hline(yintercept=max(inp$obsC)*10, lty=2,col=2)

  #res_model12 %>% ggplot() + 
  #    geom_pointrange(aes(x=BmsyK, color=factor(r), y=get("r-est"), ymin=get("r-ll"),ymax=get("r-ul"),
  #shape=unfinite_sd), cex=1) +
  # theme_bw(base_size=16) + 
  #  scale_shape_manual(values=c(3,20))
  library(patchwork)
  g_obj + g_Kci
}


#'
#' @export
#'
#'
#' 

doall <- function(data_raw, output_folder, species_name=NULL, r_prior=NULL){

  if(!file.exists(output_folder)) dir.create(output_folder, recursive=TRUE)  

  if(!is.null(species_name)){
    # Fishbaseからの推定値を確認 ----
    # species_name0 <- c("Clidoderma","asperrimum")
    species_name  <- FishLife::Search_species(Genus=species_name0[1],Species=species_name0[2])$match_taxonomy[1]
    bioinfo_org <- FishLife::Plot_taxa(species_name,mfrow=c(2,3))
    # tm (age at maturity, log), tmax (maximum age, log), Lm (legnth at maturity, log)
    tmp <- which(names(bioinfo_org[[1]]$Mean_pred)=="ln_r")
    # rの情報を事前分布として用いるために保存しておく
    r_prior <- (bioinfo_org[[1]]$Mean_pred[tmp]+(bioinfo_org[[1]]$Cov_pred[tmp,tmp])*c(1,-1.96,1.96)) %>% exp()
  
    # 既存の情報を用いて推定結果を更新することもできるみたいだが、最新のFishLifeではできないみたい
    # Ynew_ij <- matrix( c("Loo"=log(52.6),"K"=log(0.366),"Winfinity"=NA,"tmax"=22,"tm"=4,"M"=NA,"Lm"=NA,"Temperature"=NA), nrow=1)
    # bioinfo_update  <- Update_prediction(Taxon=species_name, Ynew_ij=Ynew_ij)
  }

  #----
  inp   <- get_spict_data(data_raw)
  ncpue <- length(inp$timeI)
  ## 個体群動態に関するパラメータの設定
  inp$priors$logr      <- c(log(0.5),5,0)
  inp$priors$logn      <- c(log(1.19),1e-3,0)
  inp$priors$logK      <- c(log(1000),5,0)
  inp$priors$logbkfrac <- c(log(0.8),5,0)
  inp$priors$logsdf    <- c(log(0.3),10,0)
  inp$priors$logsdc    <- c(log(1e-3),1e-3,1) # 漁獲量の誤差は考慮しない
  inp$priors$logbeta   <- c(log(1),2,0)
  inp$priors$logsdb    <- c(log(0.3),10,0)
  inp$priors$logsdi    <- c(log(0.3),10,0)
  inp$priors$logalpha  <- c(log(1),2,0)
  set_mapsdi           <- 0 # 複数のCPUEのあいだの観測誤差を同一にする（１）、しない（０）
  mapsdi <- 1:ncpue
  inp$priors$logq      <- rep(list(c(log(0.8),2,0)),ncpue)
  if(set_mapsdi==0) inp$mapsdi <- mapsdi
  inp$stabilise        <- 0 # If 1 wide uninformative priors are imposed on some parameters to stabilise optimisation (this happens inside the cpp file)
  inp$dteuler          <- 1 # 内部でどのくらい細かく時間ステップを区切って計算するか。１年の離散型のプロダクションモデルを想定するなら1とする
  inp<-check.inp(inp) # その他のもろもろのデフォルト設定がcheck.inp関数により与えられる  

  #----
  pdf(str_c(output_folder,"plotspict.ci.pdf"))
  plotspict.ci(inp)
  if(ncpue>1){
    for(i in 2:ncpue) plotspict.ci(inp, ncpue=i)
  }
  dev.off()

  #---- model0
  # fit.spict_tol ; fit.spictのwrapperで自動的に収束判定の結果を出力したりしてくれます
  res_model0 <- fit.spict_tol(inp, ntrials=20, seed=2)$res
  # パラメータのプロット
  g_model0 <- quickplot(res_model0, fishr=r_prior, title="Model0")
  ggsave(g_model0, filename=str_c(output_folder,"model0.png"))
  save(res_model0, file=str_c(output_folder,"res_model0.rda"))
  # 時系列のプロット
  #try(plot(res_model0))

  #---- grid search
  r_grid <- eval(r_grid_char)
  res_model12 <- do_grid_search(inp, shape=c(0.7,1.01,1.19,1.6,2,4), r=r_grid, ntrials=10)
  g_res12 <- plot_grid_result(res_model12, inp, r_prior)
  print(g_res12)

  ggsave(q_res12, path=str_c(output_folder,"model12.png"))
  save(res_model0, file=str_c(output_folder,"res_model12.rda"))  


  # 緩い事前分布(sigma=0.5?)つきの推定
  inp3 <- inp
  inp3$priors$logn <- c(log(1.19), 0.5, 1) 
  inp3$priors$logr <- c(log(r_prior[1]),0.5,1)

  tmp <- try(fit.spict_tol(inp3))
  if(class(tmp)!="try-error"){
    res_model3 <- tmp$res
    inp_model3 <- tmp$inp
  }

  quickplot(res_model3, fishr=r_prior, title="Model3") %>% ggsave(path=str_c(output_folder,"model3_quick.png"))
  pdf(str_c(output_folder,"model3_all.pdf"))
  plot(res_model3)
  plotspict.priors(res_model3)
  dev.off()

  #----

  if(res_model3$opt$convergence==0){
    pdf(str_c(output_folder,"model3_diag.pdf"))    
    res_resi<-calc.osa.resid(res_model3)
    plotspict.diagnostic(res_resi)

    res_retro <- retro(res_model3,nretroyear=7)
    try(plotspict.retro(res_retro)) #レトロ解析プロット
    try(plotspict.retro.fixed(res_retro)) #推定パラメータに関するレトロプロット
    try(mohns_rho(res_retro,what=c("FFmsy","BBmsy"))) #モーンズローの値
    dev.off()
  }  
    
}


#'
#' @export
#' 

make_model0 <- function(data_raw, stabilise=0){
    
    inp   <- get_spict_data(data_raw)
    ncpue <- length(inp$timeI)
    ## 個体群動態に関するパラメータの設定
    inp$priors$logr      <- c(log(0.5),5,0)
    inp$priors$logn      <- c(log(1.19),1e-3,0)
    inp$priors$logK      <- c(log(1000),5,0)
    inp$priors$logbkfrac <- c(log(0.8),5,0)
    inp$priors$logsdf    <- c(log(0.3),10,0)
    inp$priors$logsdc    <- c(log(1e-3),1e-3,1) # 漁獲量の誤差は考慮しない
    inp$priors$logbeta   <- c(log(1),2,0)
    inp$priors$logsdb    <- c(log(0.3),10,0)
    inp$priors$logsdi    <- c(log(0.3),10,0)
    inp$priors$logalpha  <- c(log(1),2,0)
    set_mapsdi           <- 0 # 複数のCPUEのあいだの観測誤差を同一にする（１）、しない（０）
    mapsdi <- 1:ncpue
    inp$priors$logq      <- rep(list(c(log(0.8),2,0)),ncpue)
    if(set_mapsdi==0) inp$mapsdi <- mapsdi
    inp$stabilise        <- stabilise # If 1 wide uninformative priors are imposed on some parameters to stabilise optimisation (this happens inside the cpp file)
    inp$dteuler          <- 1 # 内部でどのくらい細かく時間ステップを区切って計算するか。１年の離散型のプロダクションモデルを想定するなら1とする
    inp<-check.inp(inp) # その他のもろもろのデフォルト設定がcheck.inp関数により与えられる
    return(inp)
}

convert_vpa_pm_data <- function(res_vpa, stock_name="tmp"){
  years <- colnames(res_vpa$naa) %>% as.numeric()
  catch <- colSums(res_vpa$input$dat$caa[as.character(years)] *res_vpa$input$dat$waa[as.character(years)], na.rm=TRUE) %>% as.numeric()
  catch <- tibble(Stock=stock_name, Year=years, Label="Catch", Fleet="All",
                  Value=catch,CV=NA,Weight=1,Memo=NA)
  
  cpue <- colSums(res_vpa$baa[as.character(years)] *res_vpa$input$dat$waa[as.character(years)], na.rm=TRUE) %>% as.numeric()
  cpue <- tibble(Stock=stock_name, Year=years, Label="Index", Fleet="VPA",
                 Value=cpue,CV=NA,Weight=1,Memo=NA)

  data_pm <- bind_rows(catch, cpue)
  return(data_pm)
}

#----

#'
#' @export
#'
#' 

plot_barbiomass <- function(res){

  K <- get.par("logK",res,exp=TRUE)[2]
  r <- get.par("logr",res,exp=TRUE)[2]
  m <- get.par("logn",res,exp=TRUE)[2]
  Bmsy <- get.par("Bmsy",res)[2]
  calc_sp <- function(B, K, r, m){
    r/(m-1) * B * (1-(B/K)^(m-1))
  }
  
  res3 <- get_spict_res(res) %>%
    dplyr::filter(stat=="B") %>%
    select(est, year) %>%
    rename(biomass=est) %>%
    mutate(year=as.numeric(year)) %>%
    left_join(tibble(catch=res$inp$obsC, year=as.numeric(res$inp$timeC))) %>%
    mutate(sp=calc_sp(biomass,K,r,m)) %>%
    mutate(biomass0=biomass,
           biomass1=biomass+sp,
           biomass2=biomass+sp-catch)
  res3$biomass3 <- c(res3$biomass[-1],NA)
  res3 <- res3 %>% mutate(process_error=biomass3-biomass2)

  #res4 <- bind_rows(mutate(res3, year2=year,      biomass=biomass0),
  #                  mutate(res3, year2=year+0.25, biomass=biomass1),
  #                  mutate(res3, year2=year+0.5 , biomass=biomass2)) %>%
  #    arrange(year2)
  cols <- c("Suplus_production"=2, "Catch"=3, "Process_error"=4)
  res3 %>% dplyr::filter(!is.na(process_error)) %>%
    ggplot() +
    geom_area(aes(x=year,y=biomass), fill="gray")+
    geom_point(aes(x=year,y=biomass),col=1) +
    geom_segment(aes(x=year,xend=year,y=biomass,yend=biomass1),
                 col=2,arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    geom_segment(aes(x=year+0.3 ,xend=year+0.3 ,y=biomass1,yend=biomass2),
                 col=3,arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    geom_segment(aes(x=year+0.6,xend=year+0.6,y=biomass2,yend=biomass3),col=4,
                 arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    theme_bw(base_size=14) + coord_cartesian(expand=0.2) +
    geom_hline(aes(yintercept=0))
  #    geom_segment(aes(x=year+0.75 ,xend=year+0.75 ,y=0,yend=process_error),
  #                 col=4,arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1) +
  #    geom_segment(aes(x=year+0.5 ,xend=year+0.5 ,y=0,yend=-catch),
  #                 col=3,arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1) +
  #    geom_segment(aes(x=year+0.25 ,xend=year+0.25 ,y=0,yend=sp),
  #                 col=2,arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)

  cols <- c("Surplus_Production"=2, "Catch"=3, "Process_error"=4)
  gg <- res3 %>% dplyr::filter(!is.na(process_error)) %>%
    ggplot() +
    geom_area(aes(x=year,y=biomass), fill="gray")+
    scale_color_manual(name="Arrow",values=cols)+
    geom_point(aes(x=year,y=biomass),col=1) +
    geom_segment(aes(x=year,xend=year,y=biomass,yend=biomass1,color="Surplus_Production"),
                 arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    geom_segment(aes(x=year+0.3 ,xend=year+0.3 ,y=biomass1,yend=biomass2,color="Catch"),
                 arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    geom_segment(aes(x=year+0.6,xend=year+0.6,y=biomass2,yend=biomass3,color="Process_error"),
                 arrow=arrow(type="closed",length=unit(0.20,"cm")),lwd=1)+
    theme_bw(base_size=14) + coord_cartesian(expand=0.2) +
    geom_hline(aes(yintercept=0)) + theme(legend.position="top") +
    geom_hline(aes(yintercept=Bmsy),col=2,lty=2)
  gg
}

#----
size <- seq(from=100,to=3000,by=100)
number_0 <- numeric()
for(i in 1:length(size)){ x <- rmultinom(1000, size = size[i], prob = c(0.03,0.2,0.8,0.2,0.1)); number_0[i] <- sum(x==0)}


