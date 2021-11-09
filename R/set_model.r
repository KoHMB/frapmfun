set_spict <- function(filename){
    data_raw<-read.csv(filename) #データの読み込みの指定
    inp  <- get_spict_data(data_raw)
    ncpue <- length(inp$timeI)
    inp$priors$logr <- c(log(0.5),5,0)
    inp$priors$logn <- c(log(1.19),1e-3,1)
    inp$priors$logK <- c(log(1000),5,0)
    inp$priors$logbkfrac <- c(log(0.8),5,0)
    inp$priors$logsdf  <- c(log(0.3),10,0)
    inp$priors$logsdc <- c(log(1e-3),1e-3,1)
    inp$priors$logbeta <- c(log(1),2,0)
    inp$priors$logsdb <- c(log(0.3),10,0)
    inp$priors$logsdi <- c(log(0.3),10,0)
    inp$priors$logalpha <- c(log(1),2,0)
    set_mapsdi <- 0
    mapsdi <- 1:ncpue
    inp$priors$logq <- rep(list(c(log(0.8),2,0)),ncpue)
    if(set_mapsdi==1) inp$mapsdi <- mapsdi
    inp<-check.inp(inp)
    return(inp)
}

set_jabba <- function(filename){
    data_raw <- read.csv(filename)
    # 共通フォーマットからjabba用データへの変換
    data_jabba <- get_jabba_data(data_raw, is_se_NA=TRUE)

  inp <-  build_jabba(
  #********************************
  #入力リストの読み込み
  catch = data_jabba$catch, 
  cpue = data_jabba$cpue, 
  se = data_jabba$se, 
  #********************************
  # 入力オブジェクトのラベル付け
  assessment=data_jabba$stock,
  scenario = "Scenario1",
  model.type = "Pella", #FPella-Tomlinson型
  BmsyK = 0.4, #（default） 原著式2) Bmsy/K = m^(-1/(m-1))から、任意のBmsy/Kの値
  Plim = 0, #デフォルト
  r.dist = "range", #上下限付与型分布を指定
  r.prior = c(0.01,10), #下限値=0.01，上限値=10を与える
  K.dist = "range", #上下限付与型分布を指定
  K.prior = c(10,100*max(data_jabba$catch$catch)), #下限=10，上限＝漁獲量の最大観測値の100倍
  psi.dist = "lnorm", #事前分布タイプを指定(デフォルト)
  psi.prior = c(0.9,2), #mean=0.9, CV=2を与える
  sigma.proc = TRUE, #プロセス誤差の推定をオン（デフォルト）
  igamma = c(0.001,0.001), #プロセス誤差は逆ガンマ関数1/dgamma(0.001,0.001)(無情報事前分布）に従う
  sigma.est = TRUE, #観測誤差の推定をオン（デフォルト）
  fixed.obsE = 0.001, #ifelse(is.null(se),0.1,0.001),#入力データにseがない(null)場合はダミーを使用（default）
  add.catch.CV = FALSE, #漁獲量の観測誤差に関する設定（デフォルトはTRUE）
  )
    return(inp)
}
