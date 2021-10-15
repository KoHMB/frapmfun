
prod_OM <- function(
    r = 0.3,
    K = 1000,
    B1_ratio = 0.8,
    sigma_i = c(0.3, 0.3),
    sigma_p = 0.3,
    start_year_catch = 1960,
    end_year_catch = 2020,
    F_year = c(start_year_catch, median(start_year_catch:end_year_catch), end_year_catch),
    F_given = c(0.1, 0.3, 0.1),
    ncpue = 2, # ncpue1にはまだ対応していない
    start_year_cpue = c(1960, 1990),
    end_year_cpue = c(2020, 2020),
    seed=1,
    stock_name = "example"
){

   res <- tibble(Year=start_year_catch:end_year_catch,
                 Stock = stock_name,
                 Biomass=NA,
                 Catch = NA,
                 Cpue1 = NA,
                 Cpue2 = NA,
                 Proc_error= NA,
                 Obs_error1= NA,
                 Obs_error2=NA)

    nyear <- nrow(res)

    F_data <- purrr::map_dfr(2:length(F_year), function(j){
                 approx(x=c(F_year[j-1], F_year[j]),
                        y=c(F_given[j-1], F_given[j]),
                        xout=F_year[j-1]:(F_year[j]-1)) %>%
                 as_tibble()
    }) %>%
       rename(Year=x, F=y) %>%
       bind_rows(tibble(Year=tail(F_year, n=1), F=tail(F_given, n=1)))

    res <- left_join(res, F_data)

    set.seed(seed)
    res$Proc_error <- rnorm(nyear, -0.5*sigma_p^2, sigma_p)
    res$Obs_error1 <- rnorm(nyear,              0, sigma_i[1])
    res$Obs_error2 <- rnorm(nyear,              0, sigma_i[2])

    res$Biomass[1] <- K*B1_ratio
    for(i in 2:nyear){
            res$Catch[i-1] <- res$Biomass[i-1]*res$F[i-1]
            res$Biomass[i] <- (res$Biomass[i-1] + res$Biomass[i-1] * r * (1-res$Biomass[i-1]/K) - res$Catch[i-1]) * exp(res$Proc_error[i])
    }
    res$Catch[nyear] <- res$Biomass[nyear]*res$F[nyear]    

    res$Cpue1 <- res$Biomass * exp(res$Obs_error1)
    res$Cpue1[!(res$Year %in% (start_year_cpue[1]:end_year_cpue[1]))] <- NA
    
    res$Cpue2 <- res$Biomass * exp(res$Obs_error2)    
    res$Cpue2[!(res$Year %in% (start_year_cpue[2]:end_year_cpue[2]))] <- NA

    res_data <- res %>% select(Year, Catch, Cpue1, Cpue2) %>%
        pivot_longer(-Year, values_to="Value", names_to="Fleet") %>%
        mutate(Label = ifelse(Fleet=="Catch", "Catch", "Index")) %>%
        mutate(Fleet = ifelse(Fleet=="Catch", "All", Fleet)) %>%
        mutate(Weight = 1) %>%
        dplyr::filter(!is.na(Value)) %>%
        arrange(Label, Fleet, Year)

    return(list(true=res, obs=res_data))

}

plot_proddata <- function(dat_prod){
    g1 <- dat_prod$true %>% ggplot() + geom_point(aes(x=Year, y=Biomass)) +
        ggtitle("True abundance") + ylim(0,NA) + theme_bw()
    g2 <- dat_prod$obs %>% ggplot() +
        geom_point(aes(x=Year, y=Value, col=Fleet)) +
        ggtitle("Observed catch and CPUE") + theme_bw()
    (g1 + g2)
}



