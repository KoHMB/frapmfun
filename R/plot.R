# 水産庁対応のための図

if(0){
    devtools::load_all("../../frapmfun/")
    load("../raw-data/res_jabba_nigisu.rda")
    pdf("kobe_nigisu.pdf",height=4.5,width=8)
    jbplot_kobe_rev(res)
    dev.off()

    load("../raw-data/samegarei_jabba.rda")    
    pdf("kobe_samegarei.pdf",height=4.5,width=8)
    jbplot_kobe_rev(res)
    dev.off()    

}
jbplot_kobe_rev <- function (jabba, output.dir = getwd(), as.png = FALSE, add = FALSE, 
    width = 5, height = 4.5) 
{
    cat(paste0("\n", "><> jbplot_kobe() - Stock Status Plot  <><", 
        "\n"))
    mu.f = jabba$timeseries[, , "FFmsy"]
    mu.b = jabba$timeseries[, , "BBmsy"]
    f = jabba$kobe$harvest
    b = jabba$kobe$stock
    years = jabba$yr
    N = length(years)
    kernelF <- gplots::ci2d(b, f, nbins = 151, factor = 1.5, 
        ci.levels = c(0.5, 0.8, 0.75, 0.9, 0.95), show = "none", 
        col = 1, xlab = ifelse(jabba$settings$harvest.label == 
            "Fmsy", expression(paste(F/F[MSY])), expression(paste(H/H[MSY]))), 
        ylab = expression(paste(B/B[MSY])))
    Par = list(mfrow = c(1, 1), mar = c(3.5, 3.5, 1, 1), 
        mgp = c(1.5, 0.5, 0), tck = -0.02, cex = 1.2)
    if (as.png == TRUE) {
        png(file = paste0(output.dir, "/Kobe_", jabba$assessment, 
            "_", jabba$scenario, ".png"), width = width, height = height, 
            res = 200, units = "in")
    }
    if (add == FALSE) 
        par(Par)
    plot(1000, 1000, type = "b", xlim = c(0, max(1/(jabba$refpts$bmsy/jabba$refpts$k)[1], 
        mu.b[, 1]) + 0.05), ylim = c(0, max(mu.f[, 1], quantile(f, 
        0.85), 2)), lty = 3, ylab = ifelse(jabba$settings$harvest.label == 
        "Fmsy", expression(paste(F/F[MSY])), expression(paste(H/H[MSY]))), 
        xlab = expression(paste(B/B[MSY])), xaxs = "i", yaxs = "i")
    c1 <- c(-1, 100)
    c2 <- c(1, 1)
    zb2 = c(0, 1)
    zf2 = c(1, 100)
    zb1 = c(1, 100)
    zf1 = c(0, 1)
    polygon(c(zb1, rev(zb1)), c(0, 0, 1, 1), col = "olivedrab2", border = 0)
    polygon(c(zb2, rev(zb2)), c(0, 0, 1, 1), col = "khaki1", 
        border = 0)
    polygon(c(1, 100, 100, 1), c(1, 1, 100, 100), col = "khaki1", 
        border = 0)
    polygon(c(0, 1, 1, 0), c(1, 1, 100, 100), col = "indianred1", border = 0)
    polygon(kernelF$contours$"0.95", lty = 2, border = 1, col = rgb(0.9,0.85,0.85,0.3),lwd=1.5)
    polygon(kernelF$contours$"0.8", border = 1, lty = 2, col = rgb(0.7,0.7,0.7,0.3),lwd=1.5)
#    polygon(kernelF$contours$"0.5", border = NA, lty = 2, col = NA)
    points(mu.b[, 1], mu.f[, 1], pch = 16, cex = 1)
    lines(c1, c2, lty = 3, lwd = 0.7)
    lines(c2, c1, lty = 3, lwd = 0.7)
    lines(mu.b[, 1], mu.f[, 1], lty = 1, lwd = 1)
    sel.yr = c(1, round(quantile(1:N, 0.7), 0), N)
    points(mu.b[sel.yr, 1], mu.f[sel.yr, 1], col = 1, pch = c(22, 
        21, 24), bg = "white", cex = 1.9)
    Pr.green = sum(ifelse(b > 1 & f < 1, 1, 0))/length(b) * 100
    Pr.red = sum(ifelse(b < 1 & f > 1, 1, 0))/length(b) * 100
    Pr.yellow = sum(ifelse(b < 1 & f < 1, 1, 0))/length(b) * 
        100
    Pr.orange = sum(ifelse(b > 1 & f > 1, 1, 0))/length(b) * 
        100
    sel.years = c(years[sel.yr])
    ## legend("topright", c(paste(sel.years), "50% C.I.", "80% C.I.", 
    ##     "95% C.I.", paste0(round(c(Pr.red, Pr.yellow, Pr.orange, 
    ##         Pr.green), 1), "%")), lty = c(1, 1, 1, rep(-1, 8)), 
    ##     pch = c(22, 21, 24, rep(22, 8)), pt.bg = c(rep("white", 
    ##         3), "cornsilk2", "grey", "cornsilk4", "red", "yellow", 
    ##         "orange", "green"), col = 1, lwd = 1.1, cex = 0.9, 
    ##     pt.cex = c(rep(1.3, 3), rep(1.7, 3), rep(2.2, 4)), bty = "n")
    if (as.png == TRUE) {
        dev.off()
    }
}
