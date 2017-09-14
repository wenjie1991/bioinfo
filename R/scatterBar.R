#===============================================================================
#   NAME: scatterBar
#  USAGE: plot scatter plot with error bar
#  INPUT: 
#		x: a factor, define category
#		y: numeric vector, define the variable map to y axis
# OUTPUT:
#===============================================================================
scatterBar <- function(x, y, jitter = 1, xlim = NA, ylim = NA, las = 1, ...){
	library(plyr)
	library(magrittr)

	dat <- data.frame(x, y)
	## get statistics
	getStat <- function(x){        # input is a "dataframe", x means the value, y indicate the group
		if (length(x$y) == 1){
			r <- c(Mean = x$y, Sd = NA, Se=NA, Q75m50 = NA, Q50m25 =NA)
			names(r) <- c("Mean", "Sd", "Se", "Q75m50", "Q50m25")
			return(r)
		}
		Mean <- mean(x$y)
		Sd <- sd(x$y)
		Se <- Sd / sqrt(length(x$y))
		Q75m50 <- quantile(x$y, 0.75, na.rm = T) - quantile(x$y, 0.5, na.rm = T) %>% as.vector # qunaitle 75 - median
		Q50m25 <- quantile(x$y, 0.5, na.mr = T) - quantile(x$y, 0.25, na.rm = T) %>% as.vector # median - quantile 50
		r <- c( Mean, Sd, Se, Q75m50,  Q50m25)
		names(r) <- c("Mean", "Sd", "Se", "Q75m50", "Q50m25")
		return(r)
	}
	dStat <- ddply(dat, .(x), getStat)

	y <- dat$y
	x <- as.integer(dat$x)
	levelS <- levels(dat$x)
	error <- dStat$Sd
	MeX <- dStat$Mean
	if (is.na(xlim)){
		xlim <- c(0.5, max(x) + 0.5)
	}
	if (is.na(ylim)){
		ylim <- c(min(y, MeX - error, na.rm = T), max(y, MeX + error, na.rm = T))
		ylim[1] <- min(c(ylim[1] * 0.9, ylim[1] * 1.1), na.rm = T)
		ylim[2] <- max(c(ylim[2] * 0.9, ylim[2] * 1.1), na.rm = T)
	}
	if (jitter){
		p <- plot(y = y, x = jitter(x, factor = jitter), xlim = xlim, ylim = ylim, xaxt = 'n', ...)
	} else {
		p <- plot(y = y, x = x, xlim = xlim, ylim = ylim, xaxt = 'n', ...)
	}

	for (i in 1:length(levelS))
	{
		lines(x = c(i - 0.3, i + 0.3), y = c(MeX[i], MeX[i]), lwd = 3, lend = 2)
		lines(x = c(i - 0.2, i + 0.2), y = c(MeX[i] - error[i], MeX[i] - error[i]), lend = 2)
		lines(x = c(i - 0.2, i + 0.2), y = c(MeX[i] + error[i], MeX[i] + error[i]), lend = 2)
		lines(x = c(i, i), y = c(MeX[i] - error[i], MeX[i] + error[i]), lend = 2)
	}
	#     x <- boxplot(cred~group, data = dall.sub, xaxt = 'n', ylim = c(0, 25))
	axis(side = 1, at = 1:length(levelS), labels = levelS, lwd = 1, las = las)
	# text(cex = 1, x = 1:length(levelS) + 0.2, y = -,  levelS, xpd=TRUE, pos = 2, srt = 90)
}



## example
# x <- rep(1:3, 100)
# y <- x * 2 + rnorm(300)
# x <- factor(x, levels = c(1:3))
# scatterBar(x, y)
