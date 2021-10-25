###
# Rex Graphics : Common functions
###

## stop function for graphics
rexStop <- function(msg) stop(paste0("rexUserError|", msg))

## Image drawing index
imgIndex <- 1

REx_ANA_PLOT <- function(w=500,h=500,r=NA,non.gg=FALSE,res.only=FALSE) { ## modified w/ graphicsGO
  ## Check Global Option
  if(!is.na(ggGraphOpt$fig.w)) if(!res.only) w=ggGraphOpt$fig.w * ggGraphOpt$fig.res
  if(!is.na(ggGraphOpt$fig.h)) if(!res.only) h=ggGraphOpt$fig.h * ggGraphOpt$fig.res
  if(!is.na(ggGraphOpt$fig.res)) r=ggGraphOpt$fig.res

  ## save plot as temp file
  REx.plot.tempfn <<- paste(tempdir(), "\\REx_temp.png", sep="")
  png(filename=REx.plot.tempfn, width=w, height=h, res=r)
  if (non.gg == TRUE) {
	dev.control(displaylist="enable")
	REx.plot.displaylist <<- TRUE
  } else REx.plot.displaylist <<- FALSE
  rexAnaImage <<- NULL
}

REx_ANA_PLOT_OFF <- function(caption, prevent.layout=FALSE) {
	if (REx.plot.displaylist == TRUE)
		rexAnaImage <<- recordPlot()
	dev.off()
	pdfAvailable <- FALSE
	desc <- ""
	if (!prevent.layout) if (class(try(rexAnaImage, TRUE)) != 'try-error') {
		if (!is.null(rexAnaImage)) {
			assign(paste0("rexAnaImage",imgIndex), rexAnaImage, envir = .GlobalEnv)
			save(list=paste0("rexAnaImage",imgIndex), file=paste0(tempdir(), "\\rexAnaImage",imgIndex,".RData"))
			desc <- paste0(" desc='", paste0(tempdir(), "\\rexAnaImage",imgIndex,".RData"), "'")
			pdfAvailable <- TRUE
		}
	}
  load.pkg("markdown")
  ## read temp file as a binary string
  img <- paste(markdown:::.b64EncodeFile(REx.plot.tempfn))
	R2HTML::HTML(paste("<p align=left><img src='", img, "' id='rexAnaImage", imgIndex, "' draggable='true'", desc, " /><br /><font class=caption>", caption, "</font>",
                     "<!--REMOVESTART--><button onclick='window.external.ImageCopy(document.getElementById(\"rexAnaImage", imgIndex, "\").src.substring(22))'>복사</button>&nbsp;",
		"<button onclick='window.external.ImageSaveDialog(document.getElementById(\"rexAnaImage", imgIndex, "\").src.substring(22)", ifelse(pdfAvailable, paste(",", imgIndex), ""), ")'>저장</button><!--REMOVEEND-->",
                     "</p>", sep=""),file=local_output)
  imgIndex <<- imgIndex + 1
}

### Get Factor Levels
manual_color.group <- function(dataset, by){
  dat3 <- factor(dataset[,by], levels=unique(dataset[,by]))
  return(levels(dat3))
}

###
# Graphics : Plot-specific helpers
###

## grid.draw function modified for Scattermatrix
grid.draw.ggmatrix <- function(x, recording = T) print(x, gridNewPage = F)

### Get comb of group variables from XY Plot
xyplot_color.group <- function(dataset, by){
  dat3 <- dataset[by]
  for(i2 in 1:ncol(dat3)){
    dat3[,i2] <- factor(dat3[,i2], levels=unique(dat3[,i2]))
    if(sum(is.na(dat3[,i2]))>0) dat3 <- dat3[-which(is.na(dat3[,i2])),]
  }
  group <- levels(interaction(dat3[,by]))
  return(group)
}

### Get comb of var levels from Circle Plot
circleplot_color.group <- function(dataset, varname){
  x <- factor(dataset[,varname], levels=unique(dataset[,varname]))
  lev <- levels(x)
  if(sum(is.na(x))>0) lev <- c(lev, "NA")
  return(lev)
}

###
# Graphics : Global options
###

## REX Graphics Global Options
ggGraphOpt <- list(title.size=NA, axis.title.size=NA, axis.text.size=NA,
                   fig.w=NA, fig.h=NA, fig.res=NA)

REx_GraphicsGO <- function(title.size=NA, axis.title.size=NA, axis.text.size=NA,
                           fig.w=NA, fig.h=NA, fig.res=NA){
  ggGraphOpt <- list(title.size=title.size, axis.title.size=axis.title.size, axis.text.size=axis.text.size,
                     fig.w=fig.w, fig.h=fig.h, fig.res=fig.res)
  assign("ggGraphOpt", ggGraphOpt, envir=globalenv())

  return(invisible(NULL))
}

REx_GraphicsGOset <- function(ggpobj){
  if(!is.na(ggGraphOpt$title.size)) ggpobj <- ggpobj + theme(plot.title = element_text(size = ggGraphOpt$title.size))
  if(!is.na(ggGraphOpt$axis.title.size)) ggpobj <- ggpobj + theme(axis.title = element_text(size = ggGraphOpt$axis.title.size))
  if(!is.na(ggGraphOpt$axis.text.size)) ggpobj <- ggpobj + theme(axis.text = element_text(size = ggGraphOpt$axis.text.size))
  return(ggpobj)
}

###
# Graphics : Plotting functions
###

# Plot function for distribution
plotDistr <- function(x, p, discrete=FALSE, cdf=FALSE, regions = NULL,
                          col = "gray", legend = TRUE, legend.pos = "topright",
                          main, xlab, ylab, ...){
  library(ggplot2)
  dat <- data.frame(x,p)

  theme_opt <- theme_bw() +
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
    theme(plot.title = element_text(hjust = 0.5))

  ggp <- ggplot(aes(x=x, y=p), data=dat)
  if(discrete) ggp <- ggp + geom_point(size=2) + geom_segment(aes(xend=x, yend=0),lineend="butt")
  else ggp <- ggp + geom_line()
  if(cdf) ggp <- ggp + geom_hline(yintercept = 0:1, color="grey")
  else ggp <- ggp + geom_hline(yintercept = 0, color="grey")

  ggp <- ggp + theme_opt + labs(title=main, x=xlab, y=ylab)
  return(ggp)
}

# Plot function for distribution _ Modified in Rex ver 1.1
ggprob <- NULL
REx_ProbDist <- function(dist, param1, param2=NULL, param3=NULL, xmin, xmax, p=ggprob,
                         overlap=FALSE, color="#000000", fill=NULL, cdf, title=NULL,
						 title.size=NULL, axis.title.size=NULL, axis.text.size=NULL){
  load.pkg(c("ggplot2", "ggfortify"))
  
  xmaxminx <- function(xmax, xmin, discr=T){
    if(xmax-xmin<0) rexStop("최댓값은 최솟값보다 크거나 같아야 합니다.")
    x <- seq(xmin, xmax, (xmax-xmin)/10000)
    if(!discr) x <- ceiling(xmin):floor(xmax)
    return(x)
  }
  cdf2 <- ifelse(cdf,"p","d")
  cdf3 <- ifelse(cdf,"CDF","PDF")
  eval(parse(text=paste0("dname <- ",cdf2,dist)))
  
  if(!overlap) p <- NULL
  
  switch(dist,
         norm={
           mean <- param1
           sd <- param2
           if(sd<0) rexStop("표준편차는 0 이상이어야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, mean=mean, sd=sd, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Normal Distribution (",mu,"=",mean,", ",sigma^2,"=",sd^2,")"))
         },
         t={
           df <- param1
           ncp <- ifelse(is.null(param2),0,param2)
           if(df<=0) rexStop("자유도는 0보다 커야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, df=df, ncp=ncp, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of t Distribution (df=",df,", ncp=",ncp,")"))
         },
         chisq={
           df <- param1
           ncp <- ifelse(is.null(param2),0,param2)
           if(df<=0) rexStop("자유도는 0보다 커야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, df=df, ncp=ncp, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Chi-squared Distribution (df=",df,", ncp=",ncp,")"))
         },
         f={
           df1 <- param1
           df2 <- param2
           ncp <- ifelse(is.null(param3),0,param3)
           if(df1<=0|df2<=0) rexStop("자유도는 모두 0보다 커야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, df1=df1, df2=df2, ncp=ncp, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of F Distribution (df1=",df1,", df2=",df2,", ncp=",ncp,")"))
         },
         exp={
           rate <- param1
           if(rate<=0) rexStop("비율은 0보다 커야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, rate=rate, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Exponential Distribution (",lambda,"=",rate,")"))
         },
         unif={
           min <- param1
           max <- param2
           if(min>=max) rexStop("최댓값은 최솟값보다 커야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, min=min, max=max, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Uniform Distribution (min=",min,", max=",max,")"))
         },
         beta={
           shape1 <- param1
           shape2 <- param2
           if(shape1<=0|shape2<=0) rexStop("형상모수는 모두 0보다 커야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, shape1=shape1, shape2=shape2, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Beta Distribution (",alpha[1],"=",shape1,", ",alpha[2],"=",shape2,")"))
         },
         cauchy={
           location <- param1
           scale <- param2
           if(scale<=0) rexStop("척도모수는 0보다 커야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, location=location, scale=scale, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Cauchy Distribution (",mu,"=",location,", ",sigma,"=",scale,")"))
         }, 
         logis={
           location <- param1
           scale <- param2
           if(scale<=0) rexStop("척도모수는 0보다 커야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, location=location, scale=scale, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Logistic Distribution (",mu,"=",location,", ",sigma,"=",scale,")"))
         },
         lnorm={
           meanlog <- param1
           sdlog <- param2
           if(sdlog<0) rexStop("로그표준편차는 0 이상이어야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, meanlog=meanlog, sdlog=sdlog, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Log-normal Distribution (",mu,"=",meanlog,", ",sigma^2,"=",sdlog^2,")"))
         },
         gamma={
           shape <- param1
           scale <- param2
           if(shape<0) rexStop("형상모수는 0 이상이어야 합니다.")
           if(scale<=0) rexStop("척도모수는 0보다 커야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, shape=shape, scale=scale, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Gamma Distribution (",alpha,"=",shape,", ",beta,"=",scale,")"))
         },
         weibull={
           shape <- param1
           scale <- param2
           if(shape<=0|scale<=0) rexStop("형상모수와 척도모수는 0보다 커야 합니다.")
           x <- xmaxminx(xmax, xmin)
           ggp <- ggdistribution(dname, x, shape=shape, scale=scale, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Weibull Distribution (",alpha,"=",shape,", ",beta,"=",scale,")"))
         },
         binom={
           size <- param1
           prob <- param2
           if(size!=as.integer(size)|size<0) rexStop("시행횟수는 0 이상 정수만 가능합니다.")
           if(prob<0|prob>1) rexStop("성공확률은 0이상 1이하 입니다.")
           x <- xmaxminx(xmax, xmin, discr=cdf)
           ggp <- ggdistribution(dname, x, size=size, prob=prob, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Binomial Distribution (n=",size,", p=",prob,")"))
         },
         pois={
           lambda2 <- param1
           if(lambda2<0) rexStop("평균은 0이상 입니다.")
           x <- xmaxminx(xmax, xmin, discr=cdf)
           ggp <- ggdistribution(dname, x, lambda=lambda2, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Poisson Distribution (",lambda,"=",lambda2,")"))
         },
         geom={
           prob <- param1
           if(prob<=0|prob>1) rexStop("성공확률은 0초과 1이하 입니다.")
           x <- xmaxminx(xmax, xmin, discr=cdf)
           ggp <- ggdistribution(dname, x, prob=prob, colour=color, fill=fill, p=p) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Geometric Distribution (p=",prob,")"))
         },
         hyper={
           m <- param1
           n <- param2
           k <- param3
           N <- m+n
           if(m!=as.integer(m)|m<0) rexStop("모집단 내 0과 1의 개수는 0 이상 정수만 가능합니다.")
           if(n!=as.integer(n)|n<0) rexStop("모집단 내 0과 1의 개수는 0 이상 정수만 가능합니다.")
           if(k!=as.integer(k)|k>(m+n)) rexStop("표본의 개수는 0 이상이고 모집단 크기 이하인 정수만 가능합니다.")
           x <- xmaxminx(xmax, xmin, discr=cdf)
           ggp <- ggdistribution(dname, x, m=m, n=n, k=k, colour=color, fill=fill) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Hypergeometric Distribution (n=",k,"; N=",N,", K=",m,")"))
         },
         nbinom={
           size <- param1
           prob <- param2
           if(size<0) rexStop("성공목표횟수는 0보다 커야 합니다.")
           if(prob<=0|prob>1) rexStop("성공확률은 0초과 1이하 입니다.")
           x <- xmaxminx(xmax, xmin, discr=cdf)
           ggp <- ggdistribution(dname, x, size=size, prob=prob, colour=color, fill=fill) 
           if(is.null(title)) title <- substitute(paste(cdf3," of Negative binomial Distribution (r=",size,", p=",prob,")"))
         })
  
  ggp <- ggp + geom_hline(yintercept = 0, color="grey") +
    scale_y_continuous(labels=scales::format_format(scientific=F)) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
      axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size),
	  panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
    labs(y="Density", title=title)
  
  assign("ggprob", ggp, envir=globalenv())
  return(ggp)
}

## Index plot
REx_indexplot <- function(dataset,varname,type="spike", id=TRUE, idnum=2,
                          title=NULL, xlab="Observation Index", ylab=varname,
                          color="black", pch=19, lty=1, psize=1.5, pstroke=0.5, lsize=1,
                          tick_x=NULL, tick_y=NULL, grid.major=T, grid.minor=T,
                          xlim=NULL, ylim=NULL, lim.expand=TRUE,
                          title.size=NULL, axis.title.size=NULL, axis.text.size=NULL, id.size=NULL,
                          global_opt=FALSE){
  load.pkg(c("ggplot2", "ggrepel"))
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  suppressWarnings(datavar <- as.numeric(as.character(dataset[,varname])))
  if(sum(is.na(datavar))==length(datavar)) rexStop("There is no numeric values.")
  dataset <- data.frame(x=1:length(datavar), y=datavar)
  
  idn <- sort(order(datavar, decreasing = T)[1:idnum])
  datalab <- data.frame(x=idn, y=datavar[idn])
  
  themeopt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) themeopt <- themeopt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) themeopt <- themeopt + theme(panel.grid.minor = element_blank())
  
  if(!is.null(id.size)) id.size <- id.size * 0.3514598
  else id.size <- 10 * 0.3514598
  
  if(type=="spike"){
    dataset$group <- factor("Line_base")
    ggp <- ggplot() + 
      geom_segment(aes(x=x, xend=x, y=y, yend=0, size=group, color=group, linetype=group), dataset, lineend="butt", na.rm=T) + 
      geom_hline(yintercept = 0) + 
      scale_color_manual(values=color, guide="none") +
      scale_size_manual(values=lsize, guide="none") + 
      scale_linetype_manual(values=lty, guide="none") 
    if(is.null(ylim)) ylim <- range(datavar, na.rm=T)
  }
  if(type=="dot"){
    dataset$group <- factor("Point_base")
    ggp <- ggplot() + 
      geom_point(aes(x=x, y=y, size=group, color=group, shape=group), dataset, stroke=pstroke, na.rm=T) + 
      scale_color_manual(values=color, guide="none") +
      scale_size_manual(values=psize, guide="none") + 
      scale_shape_manual(values=pch, guide="none") 
  }
  
  if(id) ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y, label=x), data=datalab, hjust = 1.2, size=id.size)
  if(!is.null(tick_x)) ggp <- ggp + scale_x_continuous(breaks=as.numeric(names(tick_x)), labels = as.character(tick_x))
  if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = as.character(tick_y))
  
  ggp <- ggp + coord_cartesian(xlim=xlim, ylim=ylim, expand=lim.expand) + labs(title=title, x=xlab, y=ylab) + themeopt
  
  attr(ggp, "Rex") <- c("interactive", "REx_indexplot")
  return(ggp)
}

##### Dot plot
REx_dotplot <- function(dataset, varname, by=NULL, bin=NULL, type="stack",
                        lgd.pos="right", lgd.back=F, lgd.title=NULL, lgd.text=NULL,
                        title=NULL, xlab=varname, color="black", color.group=NULL, label=T,
                        tick_x=NULL, grid.major=F, grid.minor=F,
                        xlim=NULL, lim.expand=TRUE,
                        title.size=NULL, axis.title.size=NULL, axis.text.size=NULL, strip.text=NULL,
                        global_opt=FALSE){
  load.pkg("ggplot2")
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  suppressWarnings(datavar <- as.numeric(as.character(dataset[,varname])))
  if(sum(is.na(datavar))==length(datavar)) rexStop("There is no numeric values.")
  dataset2 <- data.frame(datavar)
  colnames(dataset2) <- "x"
  
  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  themeopt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2,
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size = strip.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) themeopt <- themeopt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) themeopt <- themeopt + theme(panel.grid.minor = element_blank())
  
  if(is.null(by)){
    dataset2$color <- factor("Dot_base")
    ggp <- ggplot()
    if(is.null(bin)) ggp <- ggp + geom_dotplot(aes(x=x, color=color, fill=color), data=dataset2, method="histodot", na.rm=T)
    else ggp <- ggp + geom_dotplot(aes(x=x, color=color, fill=color), data=dataset2, method="histodot", binwidth=diff(range(datavar, na.rm=T))/(bin-1), na.rm=T)
    ggp <- ggp + scale_y_continuous(NULL, breaks = NULL) + labs(title=title, x=xlab) + coord_cartesian(xlim=xlim, expand=lim.expand)
    if(!is.null(tick_x)) ggp <- ggp + scale_x_continuous(breaks=as.numeric(names(tick_x)), labels = as.character(tick_x))
    ggp <- ggp + 
      scale_fill_manual(values=color, aesthetics = c("fill", "colour"), guide="none")
    # scale_color_manual(value=color, guide="none") +
    # scale_fill_manual(value=color, guide="none")
  } else {
    dataset2$group <- factor(dataset[,by], levels=unique(dataset[,by]))
    if(sum(is.na(dataset2$group))>0) dataset2 <- dataset2[-which(is.na(dataset2$group)),]
    if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dataset[,by]))))
    if(!is.null(bin)) bin <- diff(range(datavar, na.rm=T))/(bin-1)
    
    if(type=="facet"){
      labelv <- NULL
      if(label){
        labelv <- paste0(by," = ",levels(dataset2$group))
        names(labelv) <- levels(dataset2$group)
      }
      if(is.null(xlim)) xlim <- range(datavar, na.rm=T)
      if(is.null(bin)) bin <- diff(range(datavar, na.rm=T))/30
    }
    
    ggp <- ggplot() +
      geom_dotplot(aes(x=x, fill=group, color=group), data=dataset2, method="histodot", stackgroups = (type=="stack"), binwidth=bin, na.rm=T) +
      scale_y_continuous(NULL, breaks = NULL)
    if(type=="facet") ggp <- ggp + facet_wrap(~group, ncol=1, scales="free", strip.position = "top", labeller = labeller(group=labelv))
    if(!is.null(tick_x)) ggp <- ggp + scale_x_continuous(breaks=as.numeric(names(tick_x)), labels = as.character(tick_x))
    ggp <- ggp + labs(title=title, x=xlab) +
      guides(color = guide_legend(by), fill = guide_legend(by)) +
      # scale_fill_manual(values=color.group) +
      # scale_colour_manual(values=color.group) +
      scale_fill_manual(values=color.group, aesthetics = c("fill", "colour")) +
      coord_cartesian(xlim=xlim, expand=lim.expand)
  }
  attr(ggp, "Rex") <- c("interactive", "REx_dotplot")
  return(ggp+themeopt)
}

##### Histogram
REx_histogram <- function(dataset, varname, by=NULL, bin=30, scale="freq", type="stack", flip=FALSE, marg=FALSE, freq.poly=F, densityplot=F,
                          stat.test="none", stat.test.pos=NULL, stat.test.size=NULL, stat.test.gl=F,
                          lgd.pos="right", lgd.back=F, lgd.title=NULL, lgd.text=NULL, 
                          title=NULL, xlab=varname, ylab=NULL, color="grey", color.group=NULL, label=T,
                          ext.hline=NULL, ext.vline=NULL,
                          tick_x=NULL, tick_y=NULL, grid.major=F, grid.minor=F,
                          marg.box=F, marg.ratio=0.2, color.marg="grey", margby=T, 
                          xlim=NULL, ylim=NULL, lim.expand=TRUE, 
                          title.size=NULL, axis.title.size=NULL, axis.text.size=NULL, strip.text=NULL,
                          global_opt=FALSE){
  load.pkg(c("ggplot2", "cowplot", "grid", "gridExtra", "ggrepel", "reshape2"))
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  # browser()
  suppressWarnings(datavar <- as.numeric(as.character(dataset[,varname])))
  if(sum(is.na(datavar))==length(datavar)) rexStop("There is no numeric values.")
  dataset2 <- data.frame(datavar)
  colnames(dataset2) <- "x"
  
  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  theme_opt <- theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2, 
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text), 
          strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size = strip.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  if(is.null(ylab)){
    if(scale=="freq") ylab <- "Frequency"
    if(scale=="percent") ylab <- "Density" ## percent < - > density in R arg
    if(scale=="density") ylab <- "Percent"
  }
  
  alpha <- ifelse(freq.poly|densityplot, 0.3, NA)
  stat.test2 <- FALSE
  
  if(is.null(by)){
    dataset2$color1 <- factor("Histogram_base")
    dataset2$color2 <- factor("Freqpoly_base")
    ggp <- ggplot()
    if(scale=="freq"){
      ggp <- ggp + geom_histogram(aes(x=x, fill=color1), data=dataset2, bins=bin, color="black", alpha=alpha, na.rm=T)
      if(freq.poly) ggp <- ggp + geom_freqpoly(aes(x=x, color=color2), data=dataset2, bins=bin, size=1, na.rm=T)
      if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
      if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)))
    }
    if(scale=="percent"){ ## density, actaully
      ggp <- ggp + geom_histogram(aes(x=x, y = ..density.., fill=color1), data=dataset2, bins=bin, color="black", alpha=alpha, na.rm=T)
      if(freq.poly) ggp <- ggp + geom_freqpoly(aes(x=x, y = ..density.., color=color2), data=dataset2, bins=bin, size=1, na.rm=T)
      if(densityplot) ggp <- ggp + stat_density(aes(x=x, color=color2), data=dataset2, alpha=0, size=1, na.rm=T)
      if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
      if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)))
    }
    if(scale=="density"){ ## percent, actaully
      ggp <- ggp + geom_histogram(aes(x=x, y = stat(width*density), fill=color1), data=dataset2, bins=bin, color="black", alpha=alpha, na.rm=T)
      if(freq.poly) ggp <- ggp + geom_freqpoly(aes(x=x, y = stat(width*density), color=color2), data=dataset2, bins=bin, size=1, na.rm=T)
      if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
      if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = scales::percent)
      else ggp <- ggp + scale_y_continuous(labels = scales::percent)
    }
    
    ggp <- ggp + 
      scale_fill_manual(values=color, guide="none") +
      scale_color_manual(values=color, guide="none")
    
  } else {
    dataset2$group <- factor(dataset[,by], levels=unique(dataset[,by]))
    if(sum(is.na(dataset2$group))>0) dataset2 <- dataset2[-which(is.na(dataset2$group)),]
    if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dataset[,by]))))
    
    if(type=="par") hist_pos <- position_dodge()
    if(type=="identity"){
      hist_pos <- type
      alpha <- ifelse(freq.poly|densityplot, 0.3, 0.5)
    }
    if(type=="stack") hist_pos <- position_stack()
    
    ggp <- ggplot()
    
    if(type=="facet"){
      binwid <- diff(range(datavar, na.rm=T))/bin
      if(is.null(xlim)) xlim <- range(datavar, na.rm=T)
      labelv <- NULL
      if(label){
        labelv <- paste0(by," = ",levels(dataset2$group))
        names(labelv) <- levels(dataset2$group)
      }	
      if(scale=="freq"){
        ggp <- ggp + geom_histogram(aes(x=x, fill=group), data=dataset2, binwidth=binwid, col="black", alpha=alpha, na.rm=T)
        if(freq.poly) ggp <- ggp + geom_freqpoly(aes(x=x, color=group), data=dataset2, binwidth=binwid, size=1, na.rm=T, show.legend = F)
        if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
        if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)))
      }
      if(scale=="percent"){ ## density, actaully
        ggp <- ggp + geom_histogram(aes(x=x, y = ..density.., fill=group), data=dataset2, binwidth=binwid, col="black", alpha=alpha, na.rm=T)
        if(freq.poly) ggp <- ggp + geom_freqpoly(aes(x=x, y = ..density.., color=group), data=dataset2, binwidth=binwid, size=1, na.rm=T, show.legend = F)
        if(densityplot) ggp <- ggp + stat_density(aes(x=x, color=group), data=dataset2, alpha=0, size=1, na.rm=T, show.legend = F)
        if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
        if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)))
      }
      if(scale=="density"){ ## percent, actaully
        ggp <- ggp + geom_histogram(aes(x=x, y = stat(width*density), fill=group), data=dataset2, binwidth=binwid, col="black", alpha=alpha, na.rm=T)
        if(freq.poly) ggp <- ggp + geom_freqpoly(aes(x=x, y = stat(width*density), color=group), data=dataset2, binwidth=binwid, size=1, na.rm=T, show.legend = F)
        if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
        if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = scales::percent)
        else ggp <- ggp + scale_y_continuous(labels = scales::percent)
      }
      ggp <- ggp + facet_wrap(~group, ncol=1, scales="free_x", labeller = labeller(group=labelv))
    } else {
      if(scale=="freq"){
        ggp <- ggp + geom_histogram(aes(x=x, fill=group), data=dataset2, bins=bin, color="black", position=hist_pos, alpha=alpha, na.rm=T)
        if(freq.poly) ggp <- ggp + geom_freqpoly(aes(x=x, color=group), data=dataset2, bins=bin, position=hist_pos, size=1, na.rm=T, show.legend = F)
        if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
        if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)))
      }
      if(scale=="percent"){ ## density, actaully
        ggp <- ggp + geom_histogram(aes(x=x, y = ..density.., fill=group), data=dataset2, bins=bin, color="black", position=hist_pos, alpha=alpha, na.rm=T)
        if(freq.poly) ggp <- ggp + geom_freqpoly(aes(x=x, y = ..density.., color=group), data=dataset2, bins=bin, position=hist_pos, size=1, na.rm=T, show.legend = F)
        if(densityplot) ggp <- ggp + stat_density(aes(x=x, color=group), data=dataset2, position=ifelse(type=="stack", "stack", "identity"), alpha=0, size=1, na.rm=T, show.legend = F)
        if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
        if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)))
      }
      if(scale=="density"){ ## percent, actaully
        ggp <- ggp + geom_histogram(aes(x=x, y = stat(width*density), fill=group), data=dataset2, bins=bin, color="black", position=hist_pos, alpha=alpha, na.rm=T)
        if(freq.poly) ggp <- ggp + geom_freqpoly(aes(x=x, y = stat(width*density), color=group), data=dataset2, bins=bin, position=hist_pos, size=1, na.rm=T, show.legend = F)
        if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
        if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = scales::percent)
        else ggp <- ggp + scale_y_continuous(labels = scales::percent)
      }
      
      if(type=="identity" & stat.test!="none"){
        if(stat.test=="param"){
          if(length(levels(dataset2$group))==2){
            stat.test.name <- "Student's t-test"
            labp <- ifelse(t.test(x~group, dataset2)$p.value<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(t.test(x~group, dataset2)$p.value,4))))
            labtest <- paste0(stat.test.name,", p",labp)
          } else {
            stat.test.name <- "ANOVA"
            labp <- ifelse(summary(aov(x~group, dataset2))[[1]]$"Pr(>F)"[1]<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(summary(aov(x~group, dataset2))[[1]]$"Pr(>F)"[1],4))))
            labtest <- paste0(stat.test.name,", p",labp)
          }
          if(stat.test.gl){
            ext.vline.dat_stat <- dcast(dataset2, group~., mean, na.rm=T, value.var = "x")
            ggp <- ggp + geom_vline(xintercept = ext.vline.dat_stat[,2], color=color.group, size=1)
          }
        } else {
          if(length(levels(dataset2$group))==2){
            stat.test.name <- "Mann-Whitney U test"
            labp <- ifelse(wilcox.test(x~group, dataset2)$p.value<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(wilcox.test(x~group, dataset2)$p.value,4))))
            labtest <- paste0(stat.test.name,", p",labp)
          } else {
            stat.test.name <- "Kruskal-Wallis rank sum test"
            labp <- ifelse(kruskal.test(x~group, dataset2)$p.value<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(kruskal.test(x~group, dataset2)$p.value,4))))
            labtest <- paste0(stat.test.name,", p",labp)
          }
        }
        if(!is.null(stat.test.size)) stat.test.size <- stat.test.size * 0.3514598
        else stat.test.size <- 10 * 0.3514598
        
        if(is.null(stat.test.pos)){
          candx <- range(dataset2$x, na.rm=T)[1] + 0.2*diff(range(dataset2$x, na.rm=T))
          candy <- max(ggplot_build(ggp)$data[[1]]$y) * 1.1
          statdatpos <- data.frame(x=candx, y=candy)
        }
        else statdatpos <- data.frame(x=stat.test.pos[1], y=stat.test.pos[2])
        
        stat.test2 <- TRUE
        # ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y), data=statdatpos, label = labtest, size=stat.test.size, inherit.aes = F, point.padding = NA, min.segment.length=10000)
      }
    }
    
    
    ggp <- ggp + guides(fill = guide_legend(by)) +
      scale_fill_manual(values=color.group) + 
      scale_color_manual(values=color.group)
  }
  
  if(!is.null(ext.hline)){
    ext.hline.dat <- list()
    for(j in 1:length(ext.hline)){
      ext.hline.dat[[j]] <- data.frame(yintercept=as.numeric(strsplit(ext.hline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.hline[[j]][2]), size=as.numeric(ext.hline[[j]][3]), color=ext.hline[[j]][4])
    }
    ext.hline.dat <- do.call(rbind, ext.hline.dat)
    ggp <- ggp + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
  } 
  if(!is.null(ext.vline)){
    ext.vline.dat <- list()
    for(j in 1:length(ext.vline)){
      ext.vline.dat[[j]] <- data.frame(xintercept=as.numeric(strsplit(ext.vline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.vline[[j]][2]), size=as.numeric(ext.vline[[j]][3]), color=ext.vline[[j]][4])
    }
    ext.vline.dat <- do.call(rbind, ext.vline.dat)
    ggp <- ggp + geom_vline(aes(xintercept = xintercept), ext.vline.dat, color=ext.vline.dat$color, linetype=ext.vline.dat$linetype, size=ext.vline.dat$size)
  } 
  
  if(!is.null(tick_x)) ggp <- ggp + scale_x_continuous(breaks=as.numeric(names(tick_x)), labels = as.character(tick_x))
  ggp <- ggp + labs(title=title, x=xlab, y=ylab)
  if(flip==TRUE) ggp <- ggp + coord_flip(xlim=xlim, ylim=ylim, expand=lim.expand)
  else ggp <- ggp + coord_cartesian(xlim=xlim, ylim=ylim, expand=lim.expand)
  
  if(stat.test2){
    xylim <- layer_scales(ggp)
    if(is.null(xlim)) xlim <- xylim$x$range$range
    if(is.null(ylim)) ylim <- xylim$y$range$range
    if(is.null(stat.test.pos)){
      if(statdatpos$x>xlim[2]) statdatpos$x <- xlim[2]
      if(statdatpos$x<xlim[1]) statdatpos$x <- xlim[1]
      if(statdatpos$y>ylim[2]) statdatpos$y <- ylim[2]
      if(statdatpos$y<ylim[1]) statdatpos$y <- ylim[1]
    } else {
      statdatpos$x <- xlim[1] + (xlim[2]-xlim[1])*statdatpos$x/100
      statdatpos$y <- ylim[1] + (ylim[2]-ylim[1])*statdatpos$y/100
    }
    ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y), data=statdatpos, label = labtest, size=stat.test.size, inherit.aes = F, point.padding = NA, min.segment.length=10000)
  }
  
  ggp <- ggp + theme_opt
  
  if(!marg.box){
    attr(ggp, "Rex") <- c("interactive", "REx_histogram")
    return(ggp)
  }
  else {
    xylim <- layer_scales(ggp)
    ggp_leg <- ggp_title <- NULL
    if(is.null(xlim)) xlim <- xylim$x$range$range
    if(lgd.pos %in% c("top", "bottom", "right", "left") & !is.null(by)){
      ggp_leg <- get_legend(ggp)
      ggp <- ggp + theme(legend.position = "none")
    }
    if(!is.null(title)){
      get_title <- function (plot) 
      {
        grobs <- plot_to_gtable(plot)$grobs
        legendIndex <- which(sapply(grobs, function(x) stringr::str_detect(x$name, "plot.title")))
        if (length(legendIndex) == 1) {
          legend <- grobs[[legendIndex]]
        }
        else {
          rexStop("Plot must contain a title")
        }
      }
      ggp_title <- get_title(ggp)
      ggp <- ggp + ggtitle(NULL)
    }
    marg.ratio <- c(1-marg.ratio, marg.ratio)
    
    if(!margby) by <- NULL
    ggp_up <- REx_boxplot(varname=varname, dataset=dataset, by=by, flip=T, marg=T,
                          ylim=xlim, lim.expand=lim.expand, lgd.pos = "none",
                          color=color.marg, color.group=color.group)
    ggp_tot <- plot_grid(ggp_up + 
                           theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                                 panel.border = element_blank(), panel.grid = element_blank()),
                         ggp, nrow=2, ncol=1, align = "v", rel_heights = rev(marg.ratio))
    
    # if(is.null(ggp_leg)){
    #   if(is.null(ggp_title)) return(ggp_tot)
    #   else grid.arrange(ggp_tot, top=ggp_title)
    # } else {
    #   if(is.null(ggp_title)) eval(parse(text=paste0("grid.arrange(ggp_tot, ",lgd.pos,"=ggp_leg)")))
    #   else{
    #     eval(parse(text=paste0("ggp_tot <- arrangeGrob(ggp_tot, ",lgd.pos,"=ggp_leg)")))
    #     grid.arrange(ggp_tot, top=ggp_title)
    #   }
    # }
    
    if(!is.null(ggp_leg)) eval(parse(text=paste0("ggp_tot <- arrangeGrob(ggp_tot, ",lgd.pos,"=ggp_leg)")))
    if(!is.null(ggp_title)) ggp_tot <- arrangeGrob(ggp_tot, top=ggp_title)
    
    attr(ggp_tot, "Rex") <- c("REx_histogram")
    return(ggp_tot)
  }
}

##### Density
REx_densityplot <- function(dataset,varname,by=NULL, kernel="gaussian", binwid="nrd0", adj=1, densthres=NULL,
                            scale="density", type="stack", rug=T, flip=FALSE, marg=FALSE,
                            lgd.pos="right", lgd.back=F, lgd.title=NULL, lgd.text=NULL,
                            title=NULL, xlab=varname, ylab=NULL, color="black", color.group=NULL, alpha=0,
                            ext.hline=NULL, ext.vline=NULL,
                            tick_x=NULL, tick_y=NULL, grid.major=T, grid.minor=F,
                            xlim=NULL, ylim=NULL, lim.expand=TRUE,
                            title.size=NULL, axis.title.size=NULL, axis.text.size=NULL,
                            global_opt=FALSE){
  load.pkg("ggplot2")

  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  if(nrow(dataset)<=1) rexStop("둘 이상의 관측치가 필요합니다.")
  suppressWarnings(datavar <- as.numeric(as.character(dataset[,varname])))
  if(sum(is.na(datavar))==length(datavar)) rexStop("There is no numeric values.")
  dataset2 <- data.frame(datavar)
  colnames(dataset2) <- varname
  if(!is.null(by)){
    dataset2$group <- factor(dataset[,by], levels=unique(dataset[,by]))
    # dataset2 <- subset(dataset2, group %in% names(table(dataset2$group))[table(dataset2$group)<=1])
    if(sum(table(na.exclude(dataset2)$group)<=1)>0) rexStop("집단변수의 각 수준 별로 둘 이상의 관측치가 필요합니다.")
    
    dataset2$group <- factor(dataset2$group, levels=unique(dataset2$group))
    if(sum(is.na(dataset2$group))>0) dataset2 <- dataset2[-which(is.na(dataset2$group)),]
    colnames(dataset2)[2] <- by
    if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(dataset2[,2])))
  } else {
    dataset2$color1 <- factor("Density_base")
    dataset2$color2 <- factor("Rug_base")
  }
  
  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  theme_opt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2,
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  if(scale=="density"){
    count <- ""
    if(is.null(ylab)){
      if(type=="fill") ylab <- "Proportion"
      else ylab <- "Density"
    }
  }
  if(scale=="count"){
    count <- ", ..count.."
    if(is.null(ylab)) ylab <- "Count"
  }
  xlim2 <- range(datavar, na.rm=T)
  xlim2a <- xlim2 + diff(xlim2)*c(-1,1)*adj
  
  if(is.null(by)){
    ggp <- ggplot() +
      stat_density(eval(parse(text=paste0("aes(x=",varname,count,", color=color1, fill=color1)"))), data=dataset2, 
                   kernel=kernel, bw=binwid, adjust=adj, position=type, alpha=alpha, na.rm=T)
    if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
    if(rug) ggp <- ggp + geom_rug(eval(parse(text=paste0("aes(x=",varname,", color=color2)"))), data=dataset2, show.legend = F)
    ggp <- ggp + 
      scale_fill_manual(values=color, guide="none") +
      scale_colour_manual(values=rep(color, ifelse(rug,2,1)), guide="none")
  } else {
    ggp <- ggplot() +
      stat_density(eval(parse(text=paste0("aes(x=",varname,count,", fill=",by,", color=",by,")"))), data=dataset2, 
                   kernel=kernel, bw=binwid, adjust=adj, position=type, alpha=alpha, na.rm=T)
    if(!marg) ggp <- ggp + geom_hline(yintercept = 0)
    if(rug) ggp <- ggp + geom_rug(eval(parse(text=paste0("aes(x=",varname,", color=",by,")"))), data=dataset2, show.legend = F)
    ggp <- ggp + 
      scale_fill_manual(values=color.group) +
      scale_colour_manual(values=color.group)
  }
  
  if(!is.null(densthres)){
    ggp2 <- ggp + xlim(xlim2a)
    if(densthres > max(ggplot_build(ggp2)$data[[1]][,"ymax"], na.rm=T)) rexStop("밀도 하한을 더 낮게 설정해야 합니다.")
    xlim2 <- range(ggplot_build(ggp2)$data[[1]][which(ggplot_build(ggp2)$data[[1]][,"ymax"] > densthres),"x"])
  }
  
  if(!is.null(ext.hline)){
    ext.hline.dat <- list()
    for(j in 1:length(ext.hline)){
      ext.hline.dat[[j]] <- data.frame(yintercept=as.numeric(strsplit(ext.hline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.hline[[j]][2]), size=as.numeric(ext.hline[[j]][3]), color=ext.hline[[j]][4])
    }
    ext.hline.dat <- do.call(rbind, ext.hline.dat)
    ggp <- ggp + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
  }
  if(!is.null(ext.vline)){
    ext.vline.dat <- list()
    for(j in 1:length(ext.vline)){
      ext.vline.dat[[j]] <- data.frame(xintercept=as.numeric(strsplit(ext.vline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.vline[[j]][2]), size=as.numeric(ext.vline[[j]][3]), color=ext.vline[[j]][4])
    }
    ext.vline.dat <- do.call(rbind, ext.vline.dat)
    ggp <- ggp + geom_vline(aes(xintercept = xintercept), ext.vline.dat, color=ext.vline.dat$color, linetype=ext.vline.dat$linetype, size=ext.vline.dat$size)
    xlim2 <- range(c(xlim2, ext.vline.dat$xintercept))
    xlim2a <- range(c(xlim2a, ext.vline.dat$xintercept))
  }
  
  if(!is.null(tick_x)) ggp <- ggp + scale_x_continuous(breaks=as.numeric(names(tick_x)), labels = as.character(tick_x),
                                                       limits = range(datavar, na.rm=T) + diff(range(datavar, na.rm=T))*c(-1,1)*adj)
  if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)))
  ggp <- ggp + labs(title=title, x=xlab, y=ylab)
  
  ggp <- ggp + xlim(xlim2a)
  if(is.null(xlim)) xlim <- xlim2
  if(flip==TRUE) ggp <- ggp + coord_flip(xlim=xlim, ylim=ylim, expand=lim.expand)
  else ggp <- ggp + coord_cartesian(xlim=xlim, ylim=ylim, expand=lim.expand)
  ggp <- ggp + theme_opt
  
  attr(ggp, "Rex") <- c("interactive", "REx_densityplot")
  return(ggp)
}

##### Box plot
REx_boxplot <- function(dataset,varname,by=NULL, flip=FALSE, dot.data="none", box_w=0.75,
                        stat.test="none", stat.test.pos=NULL, stat.test.size=NULL, 
                        stat.test.pair="none", stat.test.pair.size=NULL, stat.test.pair.adj=F, 
                        lgd.pos="none", lgd.back=F, lgd.title=NULL, lgd.text=NULL, 
                        box.mean=F, box.mean.pch=5, box.mean.psize=2.5, box.mean.pstroke=1.5, box.mean.color="red",
                        title=NULL, xlab=NULL, ylab=NULL, color="grey", color.group=NULL, ext.hline=NULL, 
                        tick_x=NULL, tick_y=NULL, grid.major=T, grid.minor=F,
                        ylim=NULL, lim.expand=TRUE, marg=F,
                        title.size=NULL, axis.title.size=NULL, axis.text.size=NULL,
                        global_opt=FALSE){
  load.pkg(c("ggplot2", "ggrepel", "ggpubr", "ggsignif"))
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  theme_opt <- theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2, 
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text), 
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  stat.test2 <- FALSE
  
  if(length(varname)==1){
    suppressWarnings(datavar <- as.numeric(as.character(dataset[,varname])))
    if(sum(is.na(datavar))==length(datavar)) rexStop("There is no numeric values.")
    dataset2 <- data.frame(datavar)
    colnames(dataset2) <- "x"
    
    if(is.null(ylab)) ylab <- varname
    
    if(is.null(by)){
      dataset2$color1 <- factor("Box_base")
      dataset2$color2 <- factor("Dot_base")
      if(box.mean) box_mean_aes <- aes(x='0', y=x, shape=shape1, size=size1, color=color3)
      
      boxbase <- ggplot() +
        stat_boxplot(aes(x='0', y=x), data=dataset2, geom="errorbar", width=box_w*0.5, na.rm=T, position=position_dodge(0.75)) + 
        geom_boxplot(aes(x='0', y=x, fill=color1), data=dataset2, width=box_w, na.rm=T) +
        labs(title=title, x=xlab, y=ylab)
      if(dot.data=="jitter") boxbase <- boxbase + geom_jitter(aes(x='0', y=x), data=dataset2, width=box_w*0.3, na.rm=T, show.legend = F)
      if(dot.data=="dot") boxbase <- boxbase + geom_dotplot(aes(x='0', y=x, fill=color2), data=dataset2, binaxis='y', stackdir='center', binwidth=diff(range(dataset2$x, na.rm=T))/50, na.rm=T, show.legend = F)
      boxbase <- boxbase + scale_fill_manual(values=rep(color, ifelse(dot.data=="dot",2,1)), guide="none")
    } else {
      dataset2$group <- factor(dataset[,by], levels=unique(dataset[,by]))
      if(sum(is.na(dataset2$group))>0) dataset2 <- dataset2[-which(is.na(dataset2$group)),]
      if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dataset[,by]))))
      if(is.null(xlab)) xlab <- by
      
      if(marg) box_aes <- aes(x="0", y=x, fill=group)
      else box_aes <- aes(x=group, y=x, fill=group)
      if(box.mean) box_mean_aes <- aes(x=group, y=x, fill=group, shape=shape1, size=size1, color=color3)
      boxbase <- ggplot() + 
        stat_boxplot(box_aes, data=dataset2, geom="errorbar", width=box_w*0.5, na.rm=T, position=position_dodge(0.75)) + 
        geom_boxplot(box_aes, data=dataset2, width=box_w, na.rm=T) +
        guides(fill = guide_legend(by)) +
        labs(title=title, x=xlab, y=ylab)
      if(dot.data=="jitter") boxbase <- boxbase + geom_point(box_aes, data=dataset2, na.rm=T, position = position_jitterdodge(dodge.width = 0.75, jitter.width = box_w*0.4), show.legend = F)
      if(dot.data=="dot") boxbase <- boxbase + geom_dotplot(box_aes, data=dataset2, binaxis='y', stackdir='center', binwidth=diff(range(dataset2$x, na.rm=T))/50, na.rm=T, position = position_dodge(0.75), show.legend = F)
      if(!is.null(color.group)) boxbase <- boxbase + scale_fill_manual(values=color.group) 
      
      if(stat.test.pair!="none"){
        # browser()
		boxbase_alt <- ggplot(dataset2, box_aes) + geom_boxplot(width=box_w, na.rm=T) ## for [stat_compare_means] function
        if(stat.test.pair=="param") lev_datg <- levels(dataset2$group)[table(dataset2$group)!=1]
        else lev_datg <- levels(dataset2$group)
        if(length(lev_datg)==1) rexStop("통계분석에 유효한 집단변수의 수준이 부족합니다.")
        boxbase_sp <- ggplot_build(boxbase_alt + stat_compare_means(comparisons=as.list(data.frame(combn(lev_datg,2), stringsAsFactors = F)), method=ifelse(stat.test.pair=="param", "t.test", "wilcox.test")))$data[[2]]
        boxbase_sp_seq <- seq(2,nrow(boxbase_sp),3)
        boxbase_sp_anno <- ggplot_build(boxbase + geom_signif(box_aes, data=dataset2, comparisons=as.list(data.frame(combn(lev_datg,2), stringsAsFactors = F)), test=ifelse(stat.test.pair=="param", "t.test", "wilcox.test"), map_signif_level = function(p) p))$data[[ifelse(dot.data=="none", 3, 4)]]
        boxbase_sp_anno <- as.numeric(as.character(boxbase_sp_anno$annotation))[boxbase_sp_seq]
        if(stat.test.pair.adj) boxbase_sp_anno <- p.adjust(boxbase_sp_anno, method="bonferroni")
        boxbase_sp_anno <- ifelse(boxbase_sp_anno<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(boxbase_sp_anno,4))))
        boxbase_sp_anno[is.na(boxbase_sp_anno)] <- "<0.0001"
        boxbase_sp_anno <- paste0("p",boxbase_sp_anno)
        
        boxbase <- boxbase + geom_signif(
		  box_aes, data=dataset2, 
          annotations = boxbase_sp_anno,
          y_position = boxbase_sp$y[boxbase_sp_seq],
          xmin=boxbase_sp$x[boxbase_sp_seq],
          xmax=boxbase_sp$xend[boxbase_sp_seq],
          textsize = ifelse(is.null(stat.test.pair.size), 8.8*0.3514598, stat.test.pair.size*0.3514598)
        )
      }
      
      if(stat.test!="none"){
        if(length(levels(dataset2$group))==1) rexStop("박스그림의 통계분석은 집단변수의 수준이 2개 이상일 때 가능합니다.")
        if(stat.test=="param"){
          if(length(levels(dataset2$group))==2){
            if(length(levels(dataset2$group)[table(dataset2$group)!=1])==1) rexStop("통계분석에 유효한 집단변수의 수준이 부족합니다.")
            stat.test.name <- "Student's t-test"
            labp <- ifelse(t.test(x~group, dataset2)$p.value<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(t.test(x~group, dataset2)$p.value,4))))
            labtest <- paste0(stat.test.name,", p",labp)
          } else {
            if(sum(table(dataset2$group)==1)==length(levels(dataset2$group))) rexStop("통계분석에 유효한 집단변수의 수준이 부족합니다.")
            stat.test.name <- "ANOVA"
            labp <- ifelse(summary(aov(x~group, dataset2))[[1]]$"Pr(>F)"[1]<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(summary(aov(x~group, dataset2))[[1]]$"Pr(>F)"[1],4))))
            labtest <- paste0(stat.test.name,", p",labp)
          }
        } else {
          if(length(levels(dataset2$group))==2){
            stat.test.name <- "Mann-Whitney U test"
            labp <- ifelse(wilcox.test(x~group, dataset2)$p.value<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(wilcox.test(x~group, dataset2)$p.value,4))))
            labtest <- paste0(stat.test.name,", p",labp)
          } else {
            stat.test.name <- "Kruskal-Wallis rank sum test"
            labp <- ifelse(kruskal.test(x~group, dataset2)$p.value<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(kruskal.test(x~group, dataset2)$p.value,4))))
            labtest <- paste0(stat.test.name,", p",labp)
          }
        }
        if(!is.null(stat.test.size)) stat.test.size <- stat.test.size * 0.3514598
        else stat.test.size <- 10 * 0.3514598
        
        if(is.null(stat.test.pos)){
          candx <- 1.2
          if(stat.test.pair=="none") candy <- max(dataset2$x, na.rm=T) * 1.1
          else candy <- max(ggplot_build(boxbase)$data[[3]]$y, na.rm=T) * 1.1
          
          statdatpos <- data.frame(x=candx, y=candy)
        }
        else statdatpos <- data.frame(x=stat.test.pos[1], y=stat.test.pos[2])
        
        stat.test2 <- TRUE
        # boxbase <- boxbase + ggrepel::geom_text_repel(aes(x=x, y=y), data=statdatpos, label = labtest, size=stat.test.size, inherit.aes = F, point.padding = NA, min.segment.length=10000)
      }
    }
  } else {
    dataset2 <- data.frame(values=as.numeric(as.matrix(dataset[,varname])),
                           name=factor(rep(varname, each=nrow(dataset)), levels=varname))
    suppressWarnings(dataset2$values <- as.numeric(as.character(dataset2$values)))
    if(sum(is.na(dataset2$values))==length(dataset2$values)) rexStop("There is no numeric values.")
    
    if(is.null(ylab)) ylab <- ""
    if(is.null(xlab)) xlab <- ""
    
    if(is.null(by)){
      dataset2$color1 <- factor("Box_base")
      dataset2$color2 <- factor("Dot_base")
      if(box.mean) box_mean_aes <- aes(x=name, y=values, shape=shape1, size=size1, color=color3)
      
      boxbase <- ggplot() +
        stat_boxplot(aes(x=name, y=values), data=dataset2, geom="errorbar", width=box_w*0.5, na.rm=T, position=position_dodge(0.75)) + 
        geom_boxplot(aes(x=name, y=values, fill=color1), data=dataset2, width=box_w, na.rm=T) +
        labs(title=title, x=xlab, y=ylab)
      if(dot.data=="jitter") boxbase <- boxbase + geom_jitter(aes(x=name, y=values), data=dataset2, width=box_w*0.3, na.rm=T, show.legend = F)
      if(dot.data=="dot") boxbase <- boxbase + geom_dotplot(aes(x=name, y=values, fill=color2), data=dataset2, binaxis='y', stackdir='center', binwidth=diff(range(dataset2$values, na.rm=T))/50, na.rm=T, show.legend = F)
      boxbase <- boxbase + scale_fill_manual(values=rep(color, ifelse(dot.data=="dot",2,1)), guide="none")
    } else {
      dataset2$group <- factor(rep(dataset[,by], length(varname)), levels=unique(dataset[,by]))
      if(sum(is.na(dataset2$group))>0) dataset2 <- dataset2[-which(is.na(dataset2$group)),]
      if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dataset[,by]))))
      if(box.mean) box_mean_aes <- aes(x=name, y=values, fill=group, shape=shape1, size=size1, color=color3)
      
      boxbase <- ggplot() +
        stat_boxplot(aes(x=name, y=values, fill=group), data=dataset2, geom="errorbar", width=box_w*0.5, na.rm=T, position=position_dodge(box_w)) + 
        geom_boxplot(aes(x=name, y=values, fill=group), data=dataset2, width=box_w, na.rm=T) +
        guides(fill = guide_legend(by)) +
        labs(title=title, x=xlab, y=ylab)
      if(dot.data=="jitter") boxbase <- boxbase + geom_point(aes(x=name, y=values, fill=group), data=dataset2, na.rm=T, position = position_jitterdodge(dodge.width = box_w, jitter.width = box_w*0.4), show.legend = F)
      if(dot.data=="dot") boxbase <- boxbase + geom_dotplot(aes(x=name, y=values, fill=group), data=dataset2, binaxis='y', stackdir='center', binwidth=diff(range(dataset2$values, na.rm=T))/50, na.rm=T, position=position_dodge(box_w), show.legend = F)
      if(!is.null(color.group)) boxbase <- boxbase + scale_fill_manual(values=color.group)
      
      if(stat.test.pair!="none"){
        # browser()
        boxbase_sp <- list()
        dataset2_tmp <- dataset2[1:(nrow(dataset2)/length(varname)),]
        if(stat.test.pair=="param") lev_datg <- levels(dataset2_tmp$group)[table(dataset2_tmp$group)!=1]
        else lev_datg <- levels(dataset2_tmp$group)
        if(length(lev_datg)==1) rexStop("통계분석에 유효한 집단변수의 수준이 부족합니다.")
        ncombn <- ncol(combn(lev_datg,2))
        for(varname_i in 1:length(varname)){
          boxbase_sptmp <- ggplot_build(REx_boxplot(varname=varname[varname_i], dataset=dataset, by=by, stat.test.pair=stat.test.pair, stat.test.pair.adj=stat.test.pair.adj))$data
          boxbase_sp[[varname_i]] <- boxbase_sptmp[[3]][(ncombn+1):(ncombn*2),]
          boxbase_sp[[varname_i]]$x <- as.numeric(as.character(factor(boxbase_sp[[varname_i]]$x, levels=which(table(dataset2_tmp$group)!=ifelse(stat.test.pair=="param", 1, 0)), 
                                                                      labels=rev(ggplot_build(boxbase)$data[[1]]$x[1:length(levels(dataset2$group))+length(levels(dataset2$group))*(varname_i-1)])[which(table(dataset2_tmp$group)!=ifelse(stat.test.pair=="param", 1, 0))])))
          boxbase_sp[[varname_i]]$xend <- as.numeric(as.character(factor(boxbase_sp[[varname_i]]$xend, levels=which(table(dataset2_tmp$group)!=ifelse(stat.test.pair=="param", 1, 0)),
                                                                         labels=rev(ggplot_build(boxbase)$data[[1]]$x[1:length(levels(dataset2$group))+length(levels(dataset2$group))*(varname_i-1)])[which(table(dataset2_tmp$group)!=ifelse(stat.test.pair=="param", 1, 0))])))
        }
        boxbase_sp_y <- ggplot_build(REx_boxplot(varname="values", dataset=dataset2, by="group", stat.test.pair=stat.test.pair))$data[[3]]$y[(ncol(combn(levels(dataset2$group),2))+1):(ncol(combn(levels(dataset2$group),2))+ncombn)]
        boxbase_sp <- do.call(rbind, boxbase_sp)
        boxbase_sp$y <- rep(boxbase_sp_y, length(varname))
        
        boxbase <- boxbase + geom_signif(
		  aes(x=name, y=values, fill=group), data=dataset2,
          annotations = boxbase_sp$annotation,
          y_position = boxbase_sp$y,
          xmin=boxbase_sp$x,
          xmax=boxbase_sp$xend,
          textsize = ifelse(is.null(stat.test.pair.size), 8.8*0.3514598, stat.test.pair.size*0.3514598)
        )
      }
    }
  }
  if(box.mean){
    dataset2$shape1 <- dataset2$size1 <- dataset2$color3 <- factor("Mean_point")
    boxbase <- boxbase + 
      stat_summary(box_mean_aes, data=dataset2, fun.y=mean, geom="point", stroke=box.mean.pstroke, na.rm=T, position=position_dodge(box_w), show.legend = F) + 
      scale_shape_manual(values=box.mean.pch, guide="none") +
      scale_size_manual(values=box.mean.psize, guide="none") + 
      scale_color_manual(values=box.mean.color, guide="none")
  }
  
  if(!is.null(ext.hline)){
    ext.hline.dat <- list()
    for(j in 1:length(ext.hline)){
      ext.hline.dat[[j]] <- data.frame(yintercept=as.numeric(strsplit(ext.hline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.hline[[j]][2]), size=as.numeric(ext.hline[[j]][3]), color=ext.hline[[j]][4])
    }
    ext.hline.dat <- do.call(rbind, ext.hline.dat)
    boxbase <- boxbase + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
  } 
  
  if(!is.null(tick_x)) boxbase <- boxbase + scale_x_discrete(breaks=as.character(names(tick_x)), labels = as.character(tick_x))
  if(!is.null(tick_y)) boxbase <- boxbase + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = as.character(tick_y))
  
  if(flip==TRUE){
    boxbase <- boxbase + coord_flip(ylim=ylim, expand=lim.expand) + theme_opt 
    if(length(varname)==1 & is.null(by)){
      boxbase <- boxbase + theme(axis.ticks.y = element_blank())
      if(!marg) boxbase <- boxbase + theme(axis.text.y = element_blank())
    }
    if(length(varname)>1) boxbase <- boxbase + theme(axis.text.y = element_text(size=rel(1.2)))
  } else {
    boxbase <- boxbase + coord_cartesian(ylim=ylim, expand=lim.expand) + theme_opt
    if(length(varname)==1 & is.null(by)){
      boxbase <- boxbase + theme(axis.ticks.x = element_blank())
      if(!marg) boxbase <- boxbase + theme(axis.text.x = element_blank())
    }
    if(length(varname)>1) boxbase <- boxbase + theme(axis.text.x = element_text(size=rel(1.2)))
  }
  
  if(stat.test2){
    xylim <- layer_scales(boxbase)
    xlim <- xylim$x$range_c$range
    if(is.null(ylim)) ylim <- xylim$y$range$range
    if(!is.null(stat.test.pos)){
      statdatpos$x <- xlim[1] + (xlim[2]-xlim[1])*statdatpos$x/100
      statdatpos$y <- ylim[1] + (ylim[2]-ylim[1])*statdatpos$y/100
    }
    boxbase <- boxbase + ggrepel::geom_text_repel(aes(x=x, y=y), data=statdatpos, label = labtest, size=stat.test.size, inherit.aes = F, point.padding = NA, min.segment.length=10000)
  }
  
  attr(boxbase, "Rex") <- c("interactive", "REx_boxplot")
  return(boxbase)
}

##### Q-Q plot
REx_qqplot <- function(dataset,varname, dist="norm", distopt=NULL, line=TRUE,
                       title=NULL, xlab=NULL, ylab=varname,
                       color.dot="black", color.line="red", pch=19, lty=1, psize=1.5, pstroke=0.5, lsize=0.5,
                       tick_x=NULL, tick_y=NULL, grid.major=T, grid.minor=F,
                       xlim=NULL, ylim=NULL, lim.expand=TRUE,
                       title.size=NULL, axis.title.size=NULL, axis.text.size=NULL,
                       global_opt=FALSE){
  
  load.pkg("ggplot2")
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  suppressWarnings(datavar <- as.numeric(as.character(dataset[,varname])))
  if(sum(is.na(datavar))==length(datavar)) rexStop("There is no numeric values.")
  dataset2 <- data.frame(datavar)
  colnames(dataset2) <- "x"
  
  theme_opt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  get_estparam <- function(vec, method){
    load.pkg("fitdistrplus")
    i=1
    if(length(unique(vec))==1) rexStop("단일 값을 갖는 변수는 모수 추정이 불가능 합니다. 직접 모수를 입력해주세요.")
    switch(method,
           norm={
             scfac <- min(abs(c(mean(vec), sd(vec))))
             if(scfac==0){
               while(TRUE){
                 scfac <- runif(1)
                 distopt <- tryCatch(fitdist(vec/scfac, "norm")$estimate*scfac, error=function(e) NULL)
                 if(is.null(distopt)){
                   i <- i+1
                   if(i>50) rexStop("모수 추정에 실패했습니다. 다른 분포를 선택하시거나, 직접 모수를 입력해주세요.")
                 }
                 else break
               }
             } else distopt <- tryCatch(fitdist(vec/scfac, "norm")$estimate*scfac, error=function(e) NULL)
             if(is.null(distopt)) rexStop("모수 추정에 실패했습니다. 다른 분포를 선택하시거나, 직접 모수를 입력해주세요.")
           },
           t={
             while(TRUE){
               j <- abs(rnorm(1, i*10, i))
               distopt <- fitdist(vec, "t", method="mge", gof="KS", start=list(df=j))$estimate
               if(is.na(distopt)){
                 i <- i+1
                 if(i>50) rexStop("모수 추정에 실패했습니다. 다른 분포를 선택하시거나, 직접 모수를 입력해주세요.")
               }
               else break
             }
           },
           chisq={
             while(TRUE){
               j <- abs(rnorm(1, i*10, i))
               distopt <- fitdist(vec, "chisq", method="mge", gof="KS", start=list(df=j))$estimate
               if(is.na(distopt)){
                 i <- i+1
                 if(i>50) rexStop("모수 추정에 실패했습니다. 다른 분포를 선택하시거나, 직접 모수를 입력해주세요.")
               }
               else break
             }
           },
           f={
             while(TRUE){
               j1 <- abs(rnorm(1, i*10, i))
               j2 <- abs(rnorm(1, i*10, i))
               distopt <- fitdist(vec, "f", method="mge", gof="KS", start=list(df1=j1, df2=j2))$estimate
               if(is.na(distopt)){
                 i <- i+1
                 if(i>50) rexStop("모수 추정에 실패했습니다. 다른 분포를 선택하시거나, 직접 모수를 입력해주세요.")
               }
               else break
             }
           },
           unif={
             distopt <- range(vec, na.rm=T)
           }
    )
    return(distopt)
  }
  
  get_qqline <- function(vec, method, distopt, xlab){
    # browser()
    y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
    n <- length(vec)
    ord <- order(vec)
    ord.x <- vec[ord]
    P <- ppoints(length(vec))
    switch(method,
           norm={
             mean1 <- distopt[1]
             sd1 <- distopt[2]
             if(sd1<0) rexStop("표준편차는 0 이상이어야 합니다.")
             x <- qnorm(c(0.25, 0.75), mean1, sd1)
             z <- qnorm(P, mean1, sd1)
             dv <- dnorm(z, mean1, sd1)
             if(is.null(xlab)) xlab <- substitute(paste("Quantiles of Normal Distribution (",mu,"=",mean1,", ",sigma^2,"=",sd1^2,")"))
           },
           t={
             df1 <- distopt
             if(df1<=0) rexStop("자유도는 0보다 커야 합니다.")
             x <- qt(c(0.25, 0.75),df1)
             z <- qt(P,df1)
             dv <- dt(z,df1)
             if(is.null(xlab)) xlab <- substitute(paste("Quantiles of t Distribution (df=",df1,")"))
           },
           chisq={
             df1 <- distopt
             if(df1<=0) rexStop("자유도는 0보다 커야 합니다.")
             x <- qchisq(c(0.25, 0.75),df1)
             z <- qchisq(P,df1)
             dv <- dchisq(z,df1)
             if(is.null(xlab)) xlab <- substitute(paste("Quantiles of Chi-squared Distribution (df=",df1,")"))
           },
           f={
             df1 <- distopt[1]
             df2 <- distopt[2]
             if(df1<=0|df2<=0) rexStop("자유도는 모두 0보다 커야 합니다.")
             x <- qf(c(0.25, 0.75),df1,df2)
             z <- qf(P,df1,df2)
             dv <- df(z,df1,df2)
             if(is.null(xlab)) xlab <- substitute(paste("Quantiles of F Distribution (df1=",df1,", df2=",df2,")"))
           },
           unif={
             min1 <- distopt[1]
             max1 <- distopt[2]
             if(min1>max1) rexStop("최댓값은 최솟값보다 크거나 같아야 합니다.")
             x <- qunif(c(0.25,0.75), min1, max1)
             z <- qunif(P, min1, max1)
             dv <- dunif(z, min1, max1)
             if(is.null(xlab)) xlab <- substitute(paste("Quantiles of Uniform Distribution (min=",min1,", max=",max1,")"))
           }
    )
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
    zz <- qnorm(1 - (1 - 0.95)/2) # confidence level = 0.95
    SE <- (slope/dv) * sqrt(P * (1 - P)/n)
    fit.value <- int + slope * z
    upper <- fit.value + zz * SE
    lower <- fit.value - zz * SE
    df <- data.frame(x=z,y=ord.x,upper=upper,lower=lower)
    return(list(intercept=int,slope=slope,df=df,xlab=xlab))
  }
  
  if(is.null(distopt)) distopt <- get_estparam(as.numeric(na.exclude(datavar)), dist)
  element_qqline <- get_qqline(as.numeric(na.exclude(datavar)), dist, distopt, xlab)
  
  dataset2 <- element_qqline$df
  dataset2$color1 <- dataset2$shape1 <- dataset2$size1 <- factor("Point_base")
  
  ggp <- ggplot() +
    geom_point(aes(x=x, y=y, color=color1, shape=shape1, size=size1), data=dataset2, stroke=pstroke)
  
  if(line==TRUE){
    dataset2$color3 <- factor("Ribbon_base")
    dataset3 <- data.frame(
      slope = element_qqline$slope,
      intercept = element_qqline$intercept
    )
    dataset3$color2 <- dataset3$linetype1 <- dataset3$size2 <- factor("Line_base")
    
    ggp <- ggp + geom_abline(aes(slope=slope, intercept=intercept, color=color2, linetype=linetype1, size=size2), data=dataset3) +
      geom_ribbon(aes(x=x, ymin = lower, ymax = upper, fill=color3), data=dataset2, alpha=0.2) +
      scale_color_manual(values=c("Point_base"=color.dot, "Line_base"=color.line), guide="none") +
      scale_shape_manual(values=pch, guide="none") +
      scale_size_manual(values=c("Point_base"=psize, "Line_base"=lsize), guide="none") +
      scale_linetype_manual(values=lty, guide="none") +
      scale_fill_manual(values=color.dot, guide="none")
  } else {
    ggp <- ggp + 
      scale_color_manual(values=color.dot, guide="none") +
      scale_shape_manual(values=pch, guide="none") +
      scale_size_manual(values=psize, guide="none")
  }
  
  if(!is.null(tick_x)) ggp <- ggp + scale_x_continuous(breaks=as.numeric(names(tick_x)))
  if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = as.character(tick_y))
  
  ggp <- ggp + coord_cartesian(xlim=xlim, ylim=ylim, expand=lim.expand) +
    labs(title=title, x=element_qqline$xlab, y=ylab) +
    theme_opt
  
  attr(ggp, "Rex") <- c("interactive", "REx_qqplot")
  return(ggp)
}

##### Scatterplot
REx_scatterplot <- function(dataset, varname1, varname2,  by=NULL, jitter.x=0, jitter.y=0,
                            lineby=FALSE, LeastSq=FALSE, Ls.spread=FALSE, Ls.spread.type="confidence", Ls.level=0.95,
                            Ls.text="none", Ls.text.pos=NULL, Ls.text.size=NULL,
                            Smooth=FALSE, Sm.span=1, Sm.spread=FALSE, Ellipse=FALSE, Index=FALSE,
                            lgd.pos="bottom", lgd.back=F, lgd.title=NULL, lgd.text=NULL,
                            ext.dot=NULL, ext.abline=NULL, ext.hline=NULL, ext.vline=NULL,
                            title=NULL, xlab=varname1, ylab=varname2,
                            pch=19, psize=1.5, pstroke=0.5, color.dot="black", color.group=NULL,
                            color.line.Ls="red", color.line.Sm="green", color.line.El="purple", color.line.Id="yellow",
                            marginal="none", marg.ratio=0.2, color.marg="grey", margby=T,
                            marg.hist.bin=30, marg.dens.adj=1,
                            tick_x=NULL, tick_y=NULL, grid.major=T, grid.minor=F,
                            xlim=NULL, ylim=NULL, lim.expand=TRUE,
                            title.size=NULL, axis.title.size=NULL, axis.text.size=NULL,
                            global_opt=FALSE){
  
  load.pkg(c("ggplot2","cowplot","grid","gridExtra","ggrepel"))
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  suppressWarnings(x <- as.numeric(as.character(dataset[,varname1])))
  suppressWarnings(y <- as.numeric(as.character(dataset[,varname2])))
  if(sum(is.na(x))==length(x)) rexStop("There is no numeric values.")
  if(sum(is.na(y))==length(y)) rexStop("There is no numeric values.")
  dat <- data.frame(x,y)
  
  jitter.x <- diff(range(x, na.rm=T))*jitter.x*0.01
  jitter.y <- diff(range(y, na.rm=T))*jitter.y*0.01
  
  if(!is.null(Ls.text.size)) Ls.text.size <- Ls.text.size * 0.3514598
  else Ls.text.size <- 10 * 0.3514598
  
  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  theme_opt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2,
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  stat.test2 <- FALSE
  
  if(is.null(by)){
    dat$color1 <- dat$size1 <- dat$shape1 <- factor("Point_base")
    dat$color2 <- dat$size2 <- dat$linetype2 <- factor("Index_base")
    dat$color3 <- dat$size3 <- dat$linetype3 <- factor("Ellipse_base")
    dat$color4 <- dat$size4 <- dat$linetype4 <- factor("Smooth_base")
    dat$color5 <- dat$size5 <- dat$linetype5 <- factor("LeastSq_base")
    
    ggp <- ggplot() + 
      geom_jitter(aes(x=x, y=y, color=color1, size=size1, shape=shape1), data=dat, width = jitter.x, height = jitter.y, stroke=pstroke, na.rm=T)
    if(Index==TRUE) ggp <- ggp + geom_path(aes(x=x, y=y, color=color2, size=size2, linetype=linetype2), data=dat, show.legend=F, na.rm=T)
    if(Ellipse==TRUE) ggp <- ggp + stat_ellipse(aes(x=x, y=y, color=color3, size=size3, linetype=linetype3), data=dat, show.legend=F, na.rm=T)
    if(Smooth==TRUE){
      if(Sm.spread) aes_Smooth <- aes(x=x, y=y, color=color4, fill=color4, size=size4, linetype=linetype4)
      else aes_Smooth <- aes(x=x, y=y, color=color4, size=size4, linetype=linetype4)
      ggp <- ggp + geom_smooth(aes_Smooth, data=dat, method="loess", span=Sm.span, se=Sm.spread, show.legend=F, na.rm=T)
    }
    if(LeastSq==TRUE){
      if(Ls.spread) aes_LeastSq <- aes(x=x, y=y, color=color5, fill=color5, size=size5, linetype=linetype5)
      else aes_LeastSq <- aes(x=x, y=y, color=color5, size=size5, linetype=linetype5)
      if(!(Ls.spread.type=="prediction" & Ls.spread)) ggp <- ggp + stat_smooth(aes_LeastSq, data=dat, method = "lm", se=Ls.spread, show.legend=F, na.rm=T, level = Ls.level)
      else {
        ggpLS <- ggp + stat_smooth(aes(x=x, y=y), data=dat, method = "lm", se=Ls.spread, color=color.line.Ls, fill=color.line.Ls, show.legend=F, na.rm=T, level = Ls.level)
        ggpLSdat <- ggplot_build(ggpLS)$data[[ifelse(Smooth,3,2)]]
        
        mod <- lm(y~x, dat)
        ggpLSpred <- predict(mod, data.frame(x=ggpLSdat$x), interval = "prediction", level = Ls.level)
        ggpLSdat$ymin <- ggpLSpred[,2]
        ggpLSdat$ymax <- ggpLSpred[,3]
        ggpLSdat$color5 <- factor("LeastSq_base")
        
        ggp <- ggp + geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax, fill=color5), data=ggpLSdat, alpha=0.2, show.legend=F, na.rm=T, inherit.aes = F) +
          stat_smooth(aes(x=x, y=y, color=color5, size=size5, linetype=linetype5), data=dat, method = "lm", se=F, show.legend=F, na.rm=T)
      }
    }
    
    ggp <- ggp + scale_color_manual(values=c("Point_base"=color.dot, "Index_base"=color.line.Id, "Ellipse_base"=color.line.El, "Smooth_base"=color.line.Sm, "LeastSq_base"=color.line.Ls), guide="none") + 
      scale_size_manual(values=c("Point_base"=psize, "Index_base"=0.5, "Ellipse_base"=0.5, "Smooth_base"=0.8, "LeastSq_base"=1), guide="none") + 
      scale_shape_manual(values=pch, guide="none") + 
      scale_linetype_manual(values=rep(1,4), guide="none") + 
      scale_fill_manual(values=c("Smooth_base"=color.line.Sm, "LeastSq_base"=color.line.Ls), guide="none")
    
    if(LeastSq==TRUE & Ls.text!="none"){
      mod <- lm(y~x, dat)
      modcor <- cor.test(~x+y, dat)
      LSdat <- ggplot_build(ggp)$data[[ifelse(Smooth,3,2)]]
      
      range_x <- range(dat$x, na.rm=T)
      range_x1 <- diff(range_x)*c(0.1,0.9) + range_x[1]
      range_x2 <- diff(range_x)*c(0.2,0.8) + range_x[1]
      range_y <- range(dat$y, na.rm=T)
      range_y1 <- diff(range_y)*c(0.2,0.8) + range_y[1]
      range_y2 <- diff(range(LSdat$y))*c(0.2,0.8) + range(LSdat$y)[1]
      
      if(mod$coefficients[2]>0){
        cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[2])), max(c(range_y2[2], range_y1[2]))))
        cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[1])), max(c(range_y2[1], range_y1[1]))))
      } else {
        cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[1])), max(c(range_y2[2], range_y1[1]))))
        cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[2])), max(c(range_y2[1], range_y1[2]))))
      }
      if(runif(1)>=0.5) cand3 <- cand1
      else cand3 <- cand2
      if(is.null(Ls.text.pos)) LSdatpos <- data.frame(x=cand3[1], y=cand3[2])
      else LSdatpos <- data.frame(x=Ls.text.pos[1], y=Ls.text.pos[2])
      
      if(Ls.text=="xy") labxy <- paste0("y=",sprintf("%.3f", mod$coefficients[1]),sprintf("%+.3f", mod$coefficients[2]),"x")
      if(Ls.text=="rp"){
        labp <- ifelse(modcor$p.value<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(modcor$p.value,4))))
        labxy <- paste0("r=",round(modcor$estimate,3),", p",labp)
      }
      
      stat.test2 <- TRUE
      # if((Ls.spread | Sm.spread) & is.null(Ls.text.pos)) ggp <- ggp + ggrepel::geom_label_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
      # else ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
      
    }
  } else {
    group <- factor(dataset[,by], levels=unique(dataset[,by]))
    dat <- cbind(dat,group)
    if(sum(is.na(dat$group))>0) dat <- dat[-which(is.na(dat$group)),]
    if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dataset[,by]))))
    
    if(length(pch)==1) pch <- rep(pch, length(levels(dat$group)))
    if(length(psize)==1) psize <- rep(psize, length(levels(dat$group)))
    if(length(pstroke)==1) pstroke <- rep(pstroke, length(levels(dat$group)))
    
    dat$stroke <- dat$group
    levels(dat$stroke) <- pstroke
    dat$stroke <- as.numeric(as.character(dat$stroke))
    
    ggp <- ggplot() +
      geom_jitter(aes(x=x, y=y, color=group, shape=group, size=group, stroke=stroke), data=dat, width = jitter.x, height = jitter.y, na.rm=T)
    if(lineby==TRUE){
      dat$group_Index <- factor(paste0("Index_",dat$group), levels=paste0("Index_",levels(dat$group)))
      dat$group_Ellipse <- factor(paste0("Ellipse_",dat$group), levels=paste0("Ellipse_",levels(dat$group)))
      dat$group_Smooth <- factor(paste0("Smooth_",dat$group), levels=paste0("Smooth_",levels(dat$group)))
      dat$group_LeastSq <- factor(paste0("LeastSq_",dat$group), levels=paste0("LeastSq_",levels(dat$group)))
      
      if(Index==TRUE) ggp <- ggp + geom_path(aes(x=x, y=y, color=group_Index, size=group_Index, linetype=group_Index), data=dat, show.legend=F, na.rm=T)
      if(Ellipse==TRUE) ggp <- ggp + stat_ellipse(aes(x=x, y=y, color=group_Ellipse, size=group_Ellipse, linetype=group_Ellipse), data=dat, show.legend=F, na.rm=T)
      if(Smooth==TRUE){
        if(Sm.spread) aes_Smooth <- aes(x=x, y=y, color=group_Smooth, fill=group_Smooth, size=group_Smooth, linetype=group_Smooth)
        else aes_Smooth <- aes(x=x, y=y, color=group_Smooth, size=group_Smooth, linetype=group_Smooth)
        ggp <- ggp + geom_smooth(aes_Smooth, data=dat, method="loess", span=Sm.span, se=Sm.spread, show.legend=F, na.rm=T)
      }
      if(LeastSq==TRUE){
        if(Ls.spread) aes_LeastSq <- aes(x=x, y=y, color=group_LeastSq, fill=group_LeastSq, size=group_LeastSq, linetype=group_LeastSq)
        else aes_LeastSq <- aes(x=x, y=y, color=group_LeastSq, size=group_LeastSq, linetype=group_LeastSq)
        if(!(Ls.spread.type=="prediction" & Ls.spread)) ggp <- ggp + stat_smooth(aes_LeastSq, data=dat, method = "lm", se=Ls.spread, show.legend=F, na.rm=T, level = Ls.level)
        else {
          ggpLS <- ggp + stat_smooth(aes(x=x, y=y, color=group, fill=group, size=NULL, shape=NULL, stroke=NULL), data=dat, method = "lm", se=Ls.spread, show.legend=F, na.rm=T, level = Ls.level)
          ggpLSdat <- ggplot_build(ggpLS)$data[[ifelse(Smooth,3,2)]]
          
          lev <- levels(group)
          for(llt in 1:length(lev)){
            ll <- lev[llt]
            datlev <- subset(dat, group==ll)
            mod <- lm(y~x, datlev)
            ggpLSpred <- predict(mod, data.frame(x=ggpLSdat$x[ggpLSdat$group==llt]), interval = "prediction", level = Ls.level)
            ggpLSdat$ymin[ggpLSdat$group==llt] <- ggpLSpred[,2]
            ggpLSdat$ymax[ggpLSdat$group==llt] <- ggpLSpred[,3]
          }
          ggpLSdat$group_LeastSq <-  factor(paste0("LeastSq_",levels(group)[ggpLSdat$group]), levels=paste0("LeastSq_",levels(dat$group)))
          
          ggp <- ggp + geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax, fill=group_LeastSq), data=ggpLSdat, alpha=0.2, show.legend=F, na.rm=T, inherit.aes = F) +
            stat_smooth(aes(x=x, y=y, color=group_LeastSq, size=group_LeastSq, linetype=group_LeastSq), data=dat, method = "lm", se=F, show.legend=F, na.rm=T)
        }
      }
      # browser()
      val_color <- rep(color.group, 5)
      val_size <- c(psize, rep(c(0.5,0.5,0.8,1), each=length(levels(group))))
      names(val_color) <- names(val_size) <- paste0(rep(c("", "Index_", "Ellipse_", "Smooth_", "LeastSq_"),each=length(levels(group))), rep(levels(group),5))
      val_fill <- rep(color.group, 2)
      names(val_fill) <- paste0(rep(c("Smooth_", "LeastSq_"),each=length(levels(group))), rep(levels(group),2))
      
      ggp <- ggp + scale_color_manual(values=val_color, breaks=as.character(levels(group)), guide=guide_legend(by, override.aes = list(stroke=pstroke))) + 
        scale_size_manual(values=val_size, breaks=as.character(levels(group)), guide=guide_legend(by)) + 
        scale_shape_manual(values=pch, guide=guide_legend(by)) + 
        scale_linetype_manual(values=rep(1,length(levels(group))*4), guide="none") + 
        scale_fill_manual(values=val_fill, guide="none")
      
      if(LeastSq==TRUE & Ls.text!="none"){
        lev <- levels(group)
        LSdatpos <- data.frame(x=NA, y=NA, lab=NA)[-1,]
        # Ls.text.pos_a <- diff(range(dat$y, na.rm=T))*0.02*Ls.text.size/3.514598
        # Ls.text.pos_a <- Ls.text.pos_a*seq((length(lev)-1)/2, (1-length(lev))/2, length.out=length(lev))
        Ls.text.pos_a <- seq((length(lev)-1)/2, (1-length(lev))/2, length.out=length(lev))
        for(llt in 1:length(lev)){
          ll <- lev[llt]
          datlev <- subset(dat, group==ll)
          mod <- lm(y~x, datlev)
          modcor <- cor.test(~x+y, datlev)
          LSdat <- subset(ggplot_build(ggp)$data[[ifelse(Smooth,3,2)]], group==llt)
          
          range_x <- range(datlev$x, na.rm=T)
          range_x1 <- diff(range_x)*c(0.1,0.9) + range_x[1]
          range_x2 <- diff(range_x)*c(0.2,0.8) + range_x[1]
          range_y <- range(datlev$y, na.rm=T)
          range_y1 <- diff(range_y)*c(0.2,0.8) + range_y[1]
          range_y2 <- diff(range(LSdat$y))*c(0.2,0.8) + range(LSdat$y)[1]
          
          if(mod$coefficients[2]>0){
            cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[2])), max(c(range_y2[2], range_y1[2]))))
            cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[1])), max(c(range_y2[1], range_y1[1]))))
          } else {
            cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[1])), max(c(range_y2[2], range_y1[1]))))
            cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[2])), max(c(range_y2[1], range_y1[2]))))
          }
          if(runif(1)>=0.5) cand3 <- cand1
          else cand3 <- cand2
          
          if(Ls.text=="xy") labxy <- paste0("y=",sprintf("%.3f", mod$coefficients[1]),sprintf("%+.3f", mod$coefficients[2]),"x")
          if(Ls.text=="rp"){
            labp <- ifelse(modcor$p.value<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(modcor$p.value,4))))
            labxy <- paste0("r=",round(modcor$estimate,3),", p",labp)
          }
          
          if(is.null(Ls.text.pos)) LSdatpos <- rbind(LSdatpos, data.frame(x=cand3[1], y=cand3[2], lab=labxy))
          else LSdatpos <- rbind(LSdatpos, data.frame(x=Ls.text.pos[1], y=Ls.text.pos[2]+Ls.text.pos_a[llt], lab=labxy))
        }
        
        stat.test2 <- TRUE
        # if((Ls.spread | Sm.spread) & is.null(Ls.text.pos)) ggp <- ggp + ggrepel::geom_label_repel(aes(x=x, y=y, label=lab), data=LSdatpos, color=color.group, size=Ls.text.size, inherit.aes = F, point.padding = NA)
        # else ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y, label=lab), data=LSdatpos, color=color.group, size=Ls.text.size, inherit.aes = F, direction = "y", min.segment.length=10000, point.padding = NA)
      }
    } else {
      dat$color2 <- dat$size2 <- dat$linetype2 <- factor("Index_base")
      dat$color3 <- dat$size3 <- dat$linetype3 <- factor("Ellipse_base")
      dat$color4 <- dat$size4 <- dat$linetype4 <- factor("Smooth_base")
      dat$color5 <- dat$size5 <- dat$linetype5 <- factor("LeastSq_base")
      
      if(Index==TRUE) ggp <- ggp + geom_path(aes(x=x, y=y, color=color2, size=size2, linetype=linetype2), data=dat, show.legend=F, na.rm=T)
      if(Ellipse==TRUE) ggp <- ggp + stat_ellipse(aes(x=x, y=y, color=color3, size=size3, linetype=linetype3), data=dat, show.legend=F, na.rm=T)
      if(Smooth==TRUE){
        if(Sm.spread) aes_Smooth <- aes(x=x, y=y, color=color4, fill=color4, size=size4, linetype=linetype4)
        else aes_Smooth <- aes(x=x, y=y, color=color4, size=size4, linetype=linetype4)
        ggp <- ggp + geom_smooth(aes_Smooth, data=dat, method="loess", span=Sm.span, se=Sm.spread, show.legend=F, na.rm=T)
      }
      if(LeastSq==TRUE){
        if(Ls.spread) aes_LeastSq <- aes(x=x, y=y, color=color5, fill=color5, size=size5, linetype=linetype5)
        else aes_LeastSq <- aes(x=x, y=y, color=color5, size=size5, linetype=linetype5)
        if(!(Ls.spread.type=="prediction" & Ls.spread)) ggp <- ggp + stat_smooth(aes_LeastSq, data=dat, method = "lm", se=Ls.spread, show.legend=F, na.rm=T, level = Ls.level)
        else {
          ggpLS <- ggp + stat_smooth(aes(x=x, y=y), data=dat, method = "lm", se=Ls.spread, color=color.line.Ls, fill=color.line.Ls, show.legend=F, na.rm=T, level = Ls.level)
          ggpLSdat <- ggplot_build(ggpLS)$data[[ifelse(Smooth,3,2)]]
          
          mod <- lm(y~x, dat)
          ggpLSpred <- predict(mod, data.frame(x=ggpLSdat$x), interval = "prediction", level = Ls.level)
          ggpLSdat$ymin <- ggpLSpred[,2]
          ggpLSdat$ymax <- ggpLSpred[,3]
          ggpLSdat$color5 <- factor("LeastSq_base")
          
          ggp <- ggp + geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax, fill=color5), data=ggpLSdat, alpha=0.2, show.legend=F, na.rm=T, inherit.aes = F) +
            stat_smooth(aes(x=x, y=y, color=color5, size=size5, linetype=linetype5), data=dat, method = "lm", se=F, show.legend=F, na.rm=T)
        }
      }
      
      val_color <- c(color.group, "Index_base"=color.line.Id, "Ellipse_base"=color.line.El, "Smooth_base"=color.line.Sm, "LeastSq_base"=color.line.Ls)
      val_size <- c(psize, "Index_base"=0.5, "Ellipse_base"=0.5, "Smooth_base"=0.8, "LeastSq_base"=1)
      names(val_color)[1:length(levels(group))] <- names(val_size)[1:length(levels(group))] <- levels(group)
      
      ggp <- ggp + scale_color_manual(values=val_color, breaks=levels(group), guide=guide_legend(by, override.aes = list(stroke=pstroke))) + 
        scale_size_manual(values=val_size, breaks=levels(group), guide=guide_legend(by)) + 
        scale_shape_manual(values=pch, guide=guide_legend(by)) + 
        scale_linetype_manual(values=rep(1,4), guide="none") + 
        scale_fill_manual(values=c("Smooth_base"=color.line.Sm, "LeastSq_base"=color.line.Ls), guide="none")
      
      if(LeastSq==TRUE & Ls.text!="none"){
        mod <- lm(y~x, dat)
        modcor <- cor.test(~x+y, dat)
        LSdat <- ggplot_build(ggp)$data[[ifelse(Smooth,3,2)]]
        
        range_x <- range(dat$x, na.rm=T)
        range_x1 <- diff(range_x)*c(0.1,0.9) + range_x[1]
        range_x2 <- diff(range_x)*c(0.2,0.8) + range_x[1]
        range_y <- range(dat$y, na.rm=T)
        range_y1 <- diff(range_y)*c(0.2,0.8) + range_y[1]
        range_y2 <- diff(range(LSdat$y))*c(0.2,0.8) + range(LSdat$y)[1]
        
        if(mod$coefficients[2]>0){
          cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[2])), max(c(range_y2[2], range_y1[2]))))
          cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[1])), max(c(range_y2[1], range_y1[1]))))
        } else {
          cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[1])), max(c(range_y2[2], range_y1[1]))))
          cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[2])), max(c(range_y2[1], range_y1[2]))))
        }
        if(runif(1)>=0.5) cand3 <- cand1
        else cand3 <- cand2
        if(is.null(Ls.text.pos)) LSdatpos <- data.frame(x=cand3[1], y=cand3[2])
        else LSdatpos <- data.frame(x=Ls.text.pos[1], y=Ls.text.pos[2])
        
        if(Ls.text=="xy") labxy <- paste0("y=",sprintf("%.3f", mod$coefficients[1]),sprintf("%+.3f", mod$coefficients[2]),"x")
        if(Ls.text=="rp"){
          labp <- ifelse(modcor$p.value<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(modcor$p.value,4))))
          labxy <- paste0("r=",round(modcor$estimate,3),", p",labp)
        }
        
        stat.test2 <- TRUE
        # if((Ls.spread | Sm.spread) & is.null(Ls.text.pos)) ggp <- ggp + ggrepel::geom_label_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
        # else ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
      }
    }
    # if(!is.null(color.group)) ggp <- ggp + scale_colour_manual(values=color.group) + scale_fill_manual(values=color.group)
  }
  if(!is.null(ext.dot)){
    ext.dot.dat <- list()
    for(j in 1:length(ext.dot)){
      ext.dot2 <- ext.dot[[j]][1]
      ext.dot.dat[[j]] <- grep("[0-9.-]+[ ]*,[ ]*[0-9.-]+",unlist(strsplit(ext.dot2,"[()]")), value=T)
      ext.dot.dat[[j]] <- do.call(rbind, strsplit(ext.dot.dat[[j]],","))
      ext.dot.dat[[j]] <- data.frame(x=as.numeric(ext.dot.dat[[j]][,1]), y=as.numeric(ext.dot.dat[[j]][,2]),
                                     shape=as.numeric(ext.dot[[j]][2]), size=as.numeric(ext.dot[[j]][3]), stroke=as.numeric(ext.dot[[j]][4]), color=ext.dot[[j]][5])
    }
    ext.dot.dat <- do.call(rbind, ext.dot.dat)
    ggp <- ggp + geom_point(mapping=aes(x=x,y=y), data=ext.dot.dat, color=ext.dot.dat$color, fill=ext.dot.dat$color, shape=ext.dot.dat$shape, size=ext.dot.dat$size, stroke=ext.dot.dat$stroke)
  }
  if(!is.null(ext.abline)){
    ext.abline.dat <- list()
    for(j in 1:length(ext.abline)){
      ext.abline2 <- ext.abline[[j]][1]
      ext.abline.dat[[j]] <- grep("[0-9.-]+[ ]*,[ ]*[0-9.-]+",unlist(strsplit(ext.abline2,"[()]")), value=T)
      ext.abline.dat[[j]] <- do.call(rbind, strsplit(ext.abline.dat[[j]],","))
      ext.abline.dat[[j]] <- data.frame(intercept=as.numeric(ext.abline.dat[[j]][,1]), slope=as.numeric(ext.abline.dat[[j]][,2]),
                                        linetype=as.numeric(ext.abline[[j]][2]), size=as.numeric(ext.abline[[j]][3]), color=ext.abline[[j]][4])
    }
    ext.abline.dat <- do.call(rbind, ext.abline.dat)
    ggp <- ggp + geom_abline(aes(intercept=intercept, slope=slope), ext.abline.dat, color=ext.abline.dat$color, linetype=ext.abline.dat$linetype, size=ext.abline.dat$size)
  }
  if(!is.null(ext.hline)){
    ext.hline.dat <- list()
    for(j in 1:length(ext.hline)){
      ext.hline.dat[[j]] <- data.frame(yintercept=as.numeric(strsplit(ext.hline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.hline[[j]][2]), size=as.numeric(ext.hline[[j]][3]), color=ext.hline[[j]][4])
    }
    ext.hline.dat <- do.call(rbind, ext.hline.dat)
    ggp <- ggp + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
  }
  if(!is.null(ext.vline)){
    ext.vline.dat <- list()
    for(j in 1:length(ext.vline)){
      ext.vline.dat[[j]] <- data.frame(xintercept=as.numeric(strsplit(ext.vline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.vline[[j]][2]), size=as.numeric(ext.vline[[j]][3]), color=ext.vline[[j]][4])
    }
    ext.vline.dat <- do.call(rbind, ext.vline.dat)
    ggp <- ggp + geom_vline(aes(xintercept = xintercept), ext.vline.dat, color=ext.vline.dat$color, linetype=ext.vline.dat$linetype, size=ext.vline.dat$size)
  }
  
  if(!is.null(tick_x)) ggp <- ggp + scale_x_continuous(breaks=as.numeric(names(tick_x)), labels = as.character(tick_x))
  if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = as.character(tick_y))
  
  ggp <- ggp + theme_opt +
    labs(title=title, x=xlab, y=ylab) +
    coord_cartesian(xlim=xlim, ylim=ylim, expand=lim.expand)
  
  # if(!is.null(by)) ggp <- ggp + guides(color = guide_legend(by, override.aes = list(shape=pch, size=psize, stroke=pstroke)),
  #                                      fill = guide_legend(by), shape=guide_legend(by), size=guide_legend(by))
  
  if(stat.test2){
    xylim <- layer_scales(ggp)
    if(is.null(xlim)) xlim <- xylim$x$range$range
    if(is.null(ylim)) ylim <- xylim$y$range$range
    if(is.null(Ls.text.pos)){
      LSdatpos$x[LSdatpos$x>xlim[2]] <- xlim[2]
      LSdatpos$x[LSdatpos$x<xlim[1]] <- xlim[1]
      LSdatpos$y[LSdatpos$y>ylim[2]] <- ylim[2]
      LSdatpos$y[LSdatpos$y<ylim[1]] <- ylim[1]
    } else {
      LSdatpos$x <- xlim[1] + (xlim[2]-xlim[1])*LSdatpos$x/100
      LSdatpos$y <- ylim[1] + (ylim[2]-ylim[1])*LSdatpos$y/100
    }
    
    if(!is.null(by) & lineby){
      if((Ls.spread | Sm.spread) & is.null(Ls.text.pos)) ggp <- ggp + ggrepel::geom_label_repel(aes(x=x, y=y, label=lab), data=LSdatpos, color=color.group, size=Ls.text.size, inherit.aes = F, point.padding = NA)
      else ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y, label=lab), data=LSdatpos, color=color.group, size=Ls.text.size, inherit.aes = F, direction = "y", min.segment.length=10000, point.padding = NA)
    } else {
      if((Ls.spread | Sm.spread) & is.null(Ls.text.pos)) ggp <- ggp + ggrepel::geom_label_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
      else ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
    }
  }
  
  if(marginal=="none"){
    attr(ggp, "Rex") <- c("interactive", "REx_scatterplot")
    return(ggp)
  }
  else {
    xylim <- layer_scales(ggp)
    ggp_leg <- ggp_title <- NULL
    if(is.null(xlim)) xlim <- xylim$x$range$range
    if(is.null(ylim)) ylim <- xylim$y$range$range
    if(lgd.pos %in% c("top", "bottom", "right", "left") & !is.null(by)){
      ggp_leg <- get_legend(ggp)
      ggp <- ggp + theme(legend.position = "none")
    }
    if(!is.null(title)){
      get_title <- function (plot)
      {
        grobs <- plot_to_gtable(plot)$grobs
        legendIndex <- which(sapply(grobs, function(x) stringr::str_detect(x$name, "plot.title")))
        if (length(legendIndex) == 1) {
          legend <- grobs[[legendIndex]]
        }
        else {
          rexStop("Plot must contain a title")
        }
      }
      ggp_title <- get_title(ggp)
      ggp <- ggp + ggtitle(NULL)
    }
    marg.ratio <- c(1-marg.ratio, marg.ratio)
    
    if(!margby) by <- NULL
    
    if(marginal=="histogram"){
      ggp_up <- REx_histogram(varname=varname1, dataset=dataset, by=by, marg=T,
                              bin=marg.hist.bin, xlim=xlim, lim.expand=lim.expand, lgd.pos = "none",
                              color=color.marg, color.group=color.group)
      ggp_rt <- REx_histogram(varname=varname2, dataset=dataset, by=by, flip=T, marg=T,
                              bin=marg.hist.bin, xlim=ylim, lim.expand=lim.expand, lgd.pos = "none",
                              color=color.marg, color.group=color.group, ylab=xlab)
    }
    if(marginal=="boxplot"){
      ggp_up <- REx_boxplot(varname=varname1, dataset=dataset, by=by, flip=T, marg=T,
                            ylim=xlim, lim.expand=lim.expand, lgd.pos = "none",
                            color=color.marg, color.group=color.group)
      ggp_rt <- REx_boxplot(varname=varname2, dataset=dataset, by=by, marg=T,
                            ylim=ylim, lim.expand=lim.expand, lgd.pos = "none",
                            color=color.marg, color.group=color.group, xlab=xlab)
    }
    if(marginal=="density"){
      ggp_up <- REx_densityplot(varname=varname1, dataset=dataset, by=by, marg=T, rug=F,
                                adj=marg.dens.adj, xlim=xlim, lim.expand=lim.expand, lgd.pos = "none",
                                color=color.marg, color.group=color.group, alpha=0.3)
      ggp_rt <- REx_densityplot(varname=varname2, dataset=dataset, by=by, flip=T, marg=T, rug=F,
                                adj=marg.dens.adj, xlim=ylim, lim.expand=lim.expand, lgd.pos = "none",
                                color=color.marg, color.group=color.group, alpha=0.3, ylab=xlab)
    }
    
    lt_part <- plot_grid(ggp_up +
                           theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                                 panel.border = element_blank(), panel.grid = element_blank(),
                                 plot.margin = unit(c(8,0,0,8), "pt")),
                         ggp + theme(plot.margin = unit(c(1,1,8,8), "pt")),
                         nrow=2, ncol=1, align = "v", rel_heights = rev(marg.ratio))
    rt_part <- plot_grid(NULL,
                         ggp_rt +
                           theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                                 axis.ticks.x = element_line(color="white"), axis.title.x = element_text(color="white", size=axis.title.size),
                                 axis.text.x = element_text(color="white", size=axis.text.size),
                                 panel.border = element_blank(), panel.grid = element_blank(),
                                 plot.margin = unit(c(1,8,8,0), "pt")),
                         nrow=2, ncol=1, align = "v", rel_heights = rev(marg.ratio))
    ggp_tot <- plot_grid(lt_part, rt_part, nrow=1, ncol=2, align = "h", rel_widths = marg.ratio)
    
    if(!is.null(ggp_leg)) eval(parse(text=paste0("ggp_tot <- arrangeGrob(ggp_tot, ",lgd.pos,"=ggp_leg)")))
    if(!is.null(ggp_title)) ggp_tot <- arrangeGrob(ggp_tot, top=ggp_title)
    
    attr(ggp_tot, "Rex") <- c("REx_scatterplot")
    return(ggp_tot)
  }
}

##### Scatter Matrix
REx_scattermatrix <- function(dataset,varname,by=NULL, jitter.x=0, jitter.y=0,
                              diagonal="none", lineby=FALSE, LeastSq=FALSE, Ls.spread=FALSE, Ls.level=0.95,
                              Smooth=FALSE, Sm.span=1, Sm.spread=FALSE, Ellipse=FALSE, Index=FALSE,
                              lgd.pos="right", lgd.back=F, lgd.title=NULL, lgd.text=NULL,
                              title=NULL, xlab=NULL, ylab=NULL,
                              color.dot="black", color.group=NULL,
                              color.line=NULL, ## tmparg for v.2.1.2
                              color.line.Ls="red", color.line.Sm="green", color.line.El="purple", color.line.Id="yellow",
                              pch=19, psize=1.5, pstroke=0.5, grid.major=F, grid.minor=F,
                              diag.hist.bin=30, diag.dens.adj=1, color.diag="grey",
                              title.size=NULL, axis.title.size=NULL, axis.text.size=NULL, strip.text=NULL,
                              global_opt=FALSE, graphics_main=TRUE){
  load.pkg(c("GGally","ggplot2"))
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  dat <- dataset[,varname]
  colnames(dat) <- paste0("X", 1:ncol(dat))
  
  for(i in 1:ncol(dat)){
    suppressWarnings(dat[,i] <- as.numeric(as.character(dat[,i])))
    if(sum(is.na(dat[,i]))==nrow(dat)) rexStop("There is no numeric values.")
  }
  n <- length(varname)
  
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  ### tmpcode for v2.1.2
  if(!is.null(color.line)) color.line.Ls <- color.line.Sm <- color.line.El <- color.line.Id <- color.line
  
  theme_opt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.background = lgd.back2, strip.text = element_text(size = strip.text),
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  point_jitter <- function(data, mapping, ...){
    verx <- as.character(mapping$x)
    very <- as.character(mapping$y)
    verx2 <- ifelse(length(verx)==1, verx, verx[2])
    very2 <- ifelse(length(very)==1, very, very[2])
    x <- data[,verx2]
    y <- data[,very2]
    jitter.x <- diff(range(x, na.rm=T))*jitter.x*0.01
    jitter.y <- diff(range(y, na.rm=T))*jitter.y*0.01
    ggplot(data = data, mapping=mapping) +
      geom_jitter(color=color.dot, shape=pch, size=psize, stroke=pstroke, width = jitter.x, height = jitter.y, na.rm=T) +
      guides(color = guide_legend(by), fill = guide_legend(by))
  }
  
  point_jitter_group <- function(data, mapping, ...){
    verx <- as.character(mapping$x)
    very <- as.character(mapping$y)
    verx2 <- ifelse(length(verx)==1, verx, verx[2])
    very2 <- ifelse(length(very)==1, very, very[2])
    x <- data[,verx2]
    y <- data[,very2]
    jitter.x <- diff(range(x, na.rm=T))*jitter.x*0.01
    jitter.y <- diff(range(y, na.rm=T))*jitter.y*0.01
    ggplot(data = data, mapping=mapping) +
      geom_jitter(width = jitter.x, height = jitter.y, na.rm=T) +
      guides(color = guide_legend(by, override.aes = list(shape=pch, size=psize, stroke=pstroke)),
             fill = guide_legend(by), shape=guide_legend(by), size=guide_legend(by))
  }
  
  qqDiag <- function(data, mapping, ...){
    get_qqline <- function(vec){
      y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
      n <- length(vec)
      ord <- order(vec)
      ord.x <- vec[ord]
      P <- ppoints(length(vec))
      x <- qnorm(c(0.25, 0.75))
      z <- qnorm(P)
      dv <- dnorm(z)
      slope <- diff(y)/diff(x)
      int <- y[1L] - slope * x[1L]
      zz <- qnorm(1 - (1 - 0.95)/2) # confidence level = 0.95
      SE <- (slope/dv) * sqrt(P * (1 - P)/n)
      fit.value <- int + slope * z
      upper <- fit.value + zz * SE
      lower <- fit.value - zz * SE
      df <- data.frame(x=z,y=ord.x,upper=upper,lower=lower)
      return(list(intercept=int,slope=slope,df=df))
    }
    verx <- as.character(mapping$x)
    verx2 <- ifelse(length(verx)==1, verx, verx[2])
    element_qqline <- get_qqline(na.exclude(data[,verx2]))
    ggplot(element_qqline$df,aes(x=x,y=y)) +
      geom_point(color=color.diag) +
      geom_abline(slope = element_qqline$slope, intercept = element_qqline$intercept) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2)
  }
  
  boxDiag <- function(data, mapping, ...){
    verx <- as.character(mapping$x)
    verx2 <- ifelse(length(verx)==1, verx, verx[2])
    box <- ggplot(data = data, mapping=aes(x="",y=data[,verx2])) +
      stat_boxplot(geom="errorbar", color="black", width=0.4, na.rm=T, position=position_dodge(0.75)) +
      geom_boxplot(fill=color.diag, color="black", size=0.5, shape=19, na.rm=T)
    return(box)
  }
  
  boxDiag_group <- function(data, mapping, ...){
    verx <- as.character(mapping$x)
    verx2 <- ifelse(length(verx)==1, verx, verx[2])
    box <- ggplot(data = data, mapping=aes(x=group, y=data[,verx2], fill=group)) +
      stat_boxplot(geom="errorbar", width=0.4, na.rm=T, position=position_dodge(0.75)) +
      geom_boxplot(size=0.5, shape=19, na.rm=T)
    if(!is.null(color.group)) box <- box + scale_fill_manual(values=color.group)
    return(box)
  }
  
  histoDiag <- function(data, mapping, ...){
    hist <- ggplot(data = data, mapping=mapping) +
      geom_histogram(color="black", fill=color.diag, bins=diag.hist.bin, size=0.5, na.rm=T)
    return(hist)
  }
  
  histoDiag_group <- function(data, mapping, ...){
    hist <- ggplot(data = data, mapping=mapping) +
      geom_histogram(color="black", bins=diag.hist.bin, size=0.5, na.rm=T)
    if(!is.null(color.group)) hist <- hist + scale_fill_manual(values=color.group)
    return(hist)
  }
  
  densDiag <- function(data, mapping, ...){
    dens <- ggplot(data = data, mapping=mapping) +
      geom_density(fill=color.diag, color=color.diag, alpha=0.3, adjust=diag.dens.adj, position="stack", size=0.5, na.rm=T)
    return(dens)
  }
  
  densDiag_group <- function(data, mapping, ...){
    dens <- ggplot(data = data, mapping=mapping) +
      geom_density(alpha=0.3, adjust=diag.dens.adj, position="stack", size=0.5, na.rm=T)
    if(!is.null(color.group)) dens <- dens + scale_fill_manual(values=color.group) + scale_colour_manual(values=color.group)
    return(dens)
  }
  
  if(is.null(by)){
    if(diagonal=="none") ggp <- ggpairs(data=dat, lower=list(continuous=point_jitter), upper=list(continuous=point_jitter), diag=NULL, columnLabels = varname)
    if(diagonal=="density") ggp <- ggpairs(data=dat, lower=list(continuous=point_jitter), upper=list(continuous=point_jitter), diag = list(continuous = densDiag), columnLabels = varname)
    if(diagonal=="histogram") ggp <- ggpairs(data=dat, lower=list(continuous=point_jitter), upper=list(continuous=point_jitter), diag = list(continuous = histoDiag), columnLabels = varname)
    if(diagonal=="qqplot") ggp <- ggpairs(data=dat, lower=list(continuous=point_jitter), upper=list(continuous=point_jitter), diag=list(continuous = qqDiag), columnLabels = varname)
    if(diagonal=="boxplot") ggp <- ggpairs(data=dat, lower=list(continuous=point_jitter), upper=list(continuous=point_jitter), diag=list(continuous = boxDiag), columnLabels = varname)
    for(i in 1:ggp$nrow){
      for(j in 1:ggp$ncol){
        if(i == j) next else {
          if(Index==TRUE) ggp[i,j] <- ggp[i,j] + geom_path(color=color.line.Id, na.rm=T)
          if(Ellipse==TRUE) ggp[i,j] <- ggp[i,j] + stat_ellipse(color=color.line.El, na.rm=T)
          if(Smooth==TRUE) ggp[i,j] <- ggp[i,j] + geom_smooth(method="loess", size=0.8, span=Sm.span, se=Sm.spread, color=color.line.Sm, na.rm=T)
          if(LeastSq==TRUE) ggp[i,j] <- ggp[i,j] + geom_smooth(method = "lm", se=Ls.spread, level = Ls.level, color=color.line.Ls, na.rm=T)
        }
      }
    }
  } else {
    group <- factor(dataset[,by], levels=unique(dataset[,by]))
    dat <- cbind(dat,group)
    if(sum(is.na(dat$group))>0) dat <- dat[-which(is.na(dat$group)),]
    if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dataset[,by]))))
    
    if(length(pch)==1) pch <- rep(pch, length(levels(dat$group)))
    if(length(psize)==1) psize <- rep(psize, length(levels(dat$group)))
    if(length(pstroke)==1) pstroke <- rep(pstroke, length(levels(dat$group)))
    
    dat$stroke <- dat$group
    levels(dat$stroke) <- pstroke
    dat$stroke <- as.numeric(as.character(dat$stroke))
    
    if(lgd.pos=="none") lgd.site <- NULL
    else lgd.site <- c(n,1)
    
    if(diagonal=="none") ggp <- ggpairs(data=dat, columns=1:n, ggplot2::aes(colour=group, fill=group, shape=group, size=group, stroke=stroke), lower=list(continuous=point_jitter_group), upper=list(continuous=point_jitter_group), diag=NULL, legend=lgd.site, columnLabels = varname)
    if(diagonal=="density") ggp <- ggpairs(data=dat, columns=1:n, ggplot2::aes(colour=group, fill=group, shape=group, size=group, stroke=stroke), lower=list(continuous=point_jitter_group), upper=list(continuous=point_jitter_group), diag = list(continuous = densDiag_group), legend=lgd.site, columnLabels = varname)
    if(diagonal=="histogram") ggp <- ggpairs(data=dat, columns=1:n, ggplot2::aes(colour=group, fill=group, shape=group, size=group, stroke=stroke), lower=list(continuous=point_jitter_group), upper=list(continuous=point_jitter_group), diag = list(continuous = histoDiag_group), legend=lgd.site, columnLabels = varname)
    if(diagonal=="qqplot") ggp <- ggpairs(data=dat, columns=1:n, ggplot2::aes(colour=group, shape=group, size=group, stroke=stroke), lower=list(continuous=point_jitter_group), upper=list(continuous=point_jitter_group), diag=list(continuous = qqDiag), legend=lgd.site, columnLabels = varname)
    if(diagonal=="boxplot") ggp <- ggpairs(data=dat, columns=1:n, ggplot2::aes(colour=group, fill=group, shape=group, size=group, stroke=stroke), lower=list(continuous=point_jitter_group), upper=list(continuous=point_jitter_group), diag=list(continuous = boxDiag_group), legend=lgd.site, columnLabels = varname)
    for(i in 1:ggp$nrow){
      for(j in 1:ggp$ncol){
        if(i == j) next else {
          if(lineby==TRUE){
            if(Index==TRUE) ggp[i,j] <- ggp[i,j] + geom_path(aes(size=NULL, shape=NULL, stroke=NULL), show.legend=FALSE, na.rm=T)
            if(Ellipse==TRUE) ggp[i,j] <- ggp[i,j] + stat_ellipse(aes(size=NULL, shape=NULL, stroke=NULL), show.legend=FALSE, na.rm=T)
            if(Smooth==TRUE) ggp[i,j] <- ggp[i,j] + geom_smooth(aes(size=NULL, shape=NULL, stroke=NULL), method="loess", span=Sm.span, se=Sm.spread, size=0.8, show.legend=FALSE, na.rm=T)
            if(LeastSq==TRUE) ggp[i,j] <- ggp[i,j] + geom_smooth(aes(size=NULL, shape=NULL, stroke=NULL), method = "lm", se=Ls.spread, level = Ls.level, show.legend=FALSE, na.rm=T)
          } else {
            if(Index==TRUE) ggp[i,j] <- ggp[i,j] + geom_path(aes(color=NULL, fill=NULL, size=NULL, shape=NULL, stroke=NULL), color=color.line.Id, show.legend=FALSE, na.rm=T)
            if(Ellipse==TRUE) ggp[i,j] <- ggp[i,j] + stat_ellipse(aes(color=NULL, fill=NULL, size=NULL, shape=NULL, stroke=NULL), color=color.line.El, show.legend=FALSE, na.rm=T)
            if(Smooth==TRUE) ggp[i,j] <- ggp[i,j] + geom_smooth(aes(color=NULL, fill=NULL, size=NULL, shape=NULL, stroke=NULL), method="loess", color=color.line.Sm, span=Sm.span, se=Sm.spread, size=0.8, show.legend=FALSE, na.rm=T)
            if(LeastSq==TRUE) ggp[i,j] <- ggp[i,j] + geom_smooth(aes(color=NULL, fill=NULL, size=NULL, shape=NULL, stroke=NULL), method = "lm", color=color.line.Ls, se=Ls.spread, level = Ls.level, show.legend=FALSE, na.rm=T)
          }
          if(!is.null(color.group)) ggp[i,j] <- ggp[i,j] + scale_fill_manual(values=color.group) + scale_colour_manual(values=color.group)
          ggp[i,j] <- ggp[i,j] + scale_shape_manual(values=pch) + scale_size_manual(values=psize)
        }
      }
    }
  }
  ggp <- ggp + labs(x=xlab, y=ylab, title=title) + theme_opt
  
  # attributes(ggp)$class <- c(attributes(ggp)$class, "grob")
  if(graphics_main) ggp <- ggmatrix_gtable(ggp)
  attr(ggp, "Rex") <- c("REx_scattermatrix")
  return(ggp)
}

##### XY condition plot
REx_xyplot <- function(dataset,varname1, varname2,by=NULL, jitter.x=0, jitter.y=0,
                       on_one_panel=TRUE, scales="fixed", direction_by=NULL,
                       title=NULL, xlab=paste0(varname1, collapse = "+"), ylab=paste0(varname2, collapse = "+"),
                       pch=19, psize=1.5, pstroke=0.5, color.dot="black", color.group=NULL,
                       lgd.pos="top", lgd.back=F, lgd.title=NULL, lgd.text=NULL,
                       grid.major=F, grid.minor=F,
                       title.size=NULL, axis.title.size=NULL, axis.text.size=NULL, strip.text=NULL,
                       global_opt=FALSE){
  load.pkg("ggplot2")
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  # browser()
  dat <- dataset[,c(varname1, varname2)]
  for(i in 1:ncol(dat)){
    suppressWarnings(dat[,i] <- as.numeric(as.character(dat[,i])))
    if(sum(is.na(dat[,i]))==nrow(dat)) rexStop("There is no numeric values.")
  }
  # n <- length(varname)
  xname <- paste0(varname1, collapse = ".")
  yname <- paste0(varname2, collapse = ".")
  if(xname==yname) xname <- paste0(xname, "_")
  
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  theme_opt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.background = lgd.back2, strip.text = element_text(size = strip.text),
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  nd2 <- 0
  dat2 <- list()
  for(j1 in 1:(length(varname1))){
    for(j2 in 1:(length(varname2))){
      nd2 <- nd2+1
      dat2[[nd2]] <- cbind(dat[,c(varname1[j1], varname2[j2])], varname1[j1], varname2[j2])
      colnames(dat2[[nd2]]) <- c("x", "y", xname, yname)
    }
  }
  dat2 <- do.call(rbind,dat2)
  
  jitter.x <- diff(range(dat2$x, na.rm=T))*jitter.x*0.01
  jitter.y <- diff(range(dat2$y, na.rm=T))*jitter.y*0.01
  
  if(is.null(by)){
    dat2$color1 <- dat2$size1 <- dat2$shape1 <- factor("Point_base")
    
    ggp <- ggplot() + 
      geom_jitter(aes(x=x, y=y, color=color1, size=size1, shape=shape1), data=dat2, stroke=pstroke, width = jitter.x, height = jitter.y, na.rm=T) +
      facet_grid(as.formula(paste0(yname, " ~ ",xname)), scales=scales) + 
      scale_color_manual(values=color.dot, guide="none") + 
      scale_size_manual(values=psize, guide="none") + 
      scale_shape_manual(values=pch, guide="none")
  } else {
    dat3 <- dataset[by]
    for(i2 in 1:ncol(dat3)) dat3[,i2] <- factor(dat3[,i2], levels=unique(dat3[,i2]))
    dat2 <- cbind(dat2, dat3)
    for(i3 in by) if(sum(is.na(dat2[,i3]))>0) dat2 <- dat2[-which(is.na(dat2[,i3])),]
    dat2$group <- interaction(dat2[,by])
    if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dat2$group))))
    
    if(length(pch)==1) pch <- rep(pch, length(levels(dat2$group)))
    if(length(psize)==1) psize <- rep(psize, length(levels(dat2$group)))
    if(length(pstroke)==1) pstroke <- rep(pstroke, length(levels(dat2$group)))
    
    dat2$stroke <- dat2$group
    levels(dat2$stroke) <- pstroke
    dat2$stroke <- as.numeric(as.character(dat2$stroke))
    
    ggp <- ggplot() +
      geom_jitter(aes(x=x, y=y, color=group, shape=group, size=group, stroke=stroke), data=dat2, width = jitter.x, height = jitter.y, na.rm=T) +
      scale_color_manual(values=color.group, guide=guide_legend(paste0(by, collapse="."), override.aes = list(stroke=pstroke))) + 
      scale_size_manual(values=psize, guide=guide_legend(paste0(by, collapse="."))) + 
      scale_shape_manual(values=pch, guide=guide_legend(paste0(by, collapse=".")))
    
    if(on_one_panel==T) ggp <- ggp + facet_grid(as.formula(paste0(yname, " ~ ",xname)), scales=scales)
    else {
      if(length(by[direction_by=="v"])!=0) {
        xname <- paste0(xname, " + ", paste0(by[direction_by=="v"], collapse=" + "))
        xsp <- rep(c(rep(0,length(levels(interaction(dat2[,by[direction_by=="v"]])))-1),0.5), length(varname1))
        xsp <- xsp[-length(xsp)]
      } else {
        xsp <- 0
      }
      if(length(by[direction_by=="h"])!=0) {
        yname <- paste0(yname, " + ", paste0(by[direction_by=="h"], collapse=" + "))
        ysp <- rep(c(rep(0,length(levels(interaction(dat2[,by[direction_by=="h"]])))-1),0.5), length(varname2))
        ysp <- ysp[-length(ysp)]
      } else {
        ysp <- 0
      }
      ggp <- ggp + facet_grid(as.formula(paste0(yname, " ~ ",xname)), scales=scales) +
        theme(panel.spacing.x=unit(xsp, "lines"), panel.spacing.y=unit(ysp, "lines"))
    }
  }
  
  ggp <- ggp + labs(x=xlab, y=ylab, title=title) + theme_opt
  attr(ggp, "Rex") <- c("interactive", "REx_xyplot")
  return(ggp)
}

##### Mean plot
REx_meanplot <- function(dataset,varname, by1, by2=NULL, type="dot",
                         errbar="se", conflevel=0.95, line=TRUE, uperr=FALSE,
                         title=NULL, xlab=by1, ylab=varname, ext.hline=NULL,
                         lgd.pos="right", lgd.back=F, lgd.title=NULL, lgd.text=NULL,
                         pch=19, lty=1, psize=1.5, pstroke=0.5, lsize=0.5,
                         color.dot="black", color.line="red", color.bar="grey", color.group=NULL,
                         tick_y=NULL, grid.major=F, grid.minor=F, ylim=NULL, lim.expand=TRUE,
                         title.size=NULL, axis.title.size=NULL, axis.text.size=NULL,
                         global_opt=FALSE){
  load.pkg("ggplot2")
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  theme_opt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2,
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  pd <- position_dodge(0.3)
  
  suppressWarnings(datavar <- as.numeric(as.character(dataset[,varname])))
  if(sum(is.na(datavar))==length(datavar)) rexStop("There is no numeric values.")
  
  databy1 <- factor(dataset[,by1], levels=unique(dataset[,by1]))
  gp1 <- levels(databy1)
  
  if(is.null(by2)){
    gpmean <- upper <- lower <- c()
    for(i in 1:length(gp1)){
      datapiece <- datavar[databy1==gp1[i]]
      gpmean[i] <- mean(datapiece, na.rm=TRUE)
      gpsd <- sd(datapiece, na.rm=TRUE)
      gpse <- gpsd/sqrt(length(datapiece))
      switch(errbar,
             se={
               upper[i] <- gpmean[i] + gpse
               lower[i] <- gpmean[i] - gpse
             },
             sd={
               upper[i] <- gpmean[i] + gpsd
               lower[i] <- gpmean[i] - gpsd
             },
             conf={
               int <- qnorm((1-conflevel)/2, lower.tail = F)
               upper[i] <- gpmean[i] + gpse*int
               lower[i] <- gpmean[i] - gpse*int
             },
             none={
               upper[i] <- gpmean[i]
               lower[i] <- gpmean[i]
             }
      )
    }
    meandat <- data.frame(x = factor(gp1, levels=gp1), y = gpmean, upper, lower)
    meandat$color1 <- meandat$size1 <- meandat$shape1 <- factor("Point_base")
    meandat$color2 <- meandat$size2 <- meandat$linetype2 <- factor("Line_base")
    meandat$color3 <- factor("Bar_base")
    meandat$color4 <- meandat$size4 <- meandat$linetype4 <- factor("Errorbar_base")
    # browser()
    ggp <- ggplot()
    if(type=="bar"){
      ggp <- ggp + geom_hline(yintercept = 0) + 
        geom_col(aes(x=x, y=y, fill=color3), data=meandat, color="black", na.rm=TRUE)
    }
    if(errbar!="none"){
      if(type=="bar" & uperr) errbar_aes <- aes(x=x, ymin = y, ymax = upper, color=color4, size=size4, linetype=linetype4)
      else errbar_aes <- aes(x=x, ymin = lower, ymax = upper, color=color4, size=size4, linetype=linetype4)
      errbar_pd <- position_dodge(width=ifelse(type=="bar",0.9,0.3))
      
      ggp <- ggp + geom_errorbar(errbar_aes, data=meandat, width = 0.4, position=errbar_pd, na.rm=TRUE, show.legend = F)
    }
    if(type=="dot"){
      ggp <- ggp + geom_point(aes(x=x, y=y, color=color1, size=size1, shape=shape1), data=meandat, stroke=pstroke, na.rm=TRUE)
      if(line==TRUE) ggp <- ggp + geom_line(aes(x=x, y=y, color=color2, group=color2, size=size2, linetype=linetype2), data=meandat, na.rm=TRUE)
    }
    
    ggp <- ggp + scale_color_manual(values=c("Point_base"=color.dot, "Line_base"=color.line, "Errorbar_base"="#000000"), guide="none") + 
      scale_size_manual(values=c("Point_base"=psize, "Line_base"=lsize, "Errorbar_base"=0.5), guide="none") + 
      scale_shape_manual(values=pch, guide="none") + 
      scale_linetype_manual(values=c("Line_base"=lty, "Errorbar_base"=1), guide="none") + 
      scale_fill_manual(values=color.bar, guide="none")
  } else {
    databy2 <- factor(dataset[,by2], levels=unique(dataset[,by2]))
    gp2 <- levels(databy2)
    gp2rep <- rep(gp2, each=length(gp1))
    gp2repl <- paste0("Line_",gp2rep)
    gp2repe <- paste0("Errorbar_",gp2rep)
    if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dataset[,by2]))))
    
    if(length(pch)==1) pch <- rep(pch, length(gp2))
    if(length(psize)==1) psize <- rep(psize, length(gp2))
    if(length(pstroke)==1) pstroke <- rep(pstroke, length(gp2))
    if(length(lty)==1) lty <- rep(lty, length(gp2))
    if(length(lsize)==1) lsize <- rep(lsize, length(gp2))
    
    gp2stroke <- rep(pstroke, each=length(gp1))
    plsize <- c(psize, lsize)
    color.group2 <- rep(color.group, 2)
    names(plsize) <- names(color.group2) <- c(gp2, paste0("Line_",gp2))
    gpmean <- upper <- lower <- c()
    for(i in 1:length(gp2)){
      for(j in 1:length(gp1)){
        ij <- (i-1)*length(gp1)+j
        datapiece <- datavar[databy1==gp1[j] & databy2==gp2[i]]
        gpmean[ij] <- mean(datapiece, na.rm=TRUE)
        gpsd <- sd(datapiece, na.rm=TRUE)
        gpse <- gpsd/sqrt(length(datapiece))
        switch(errbar,
               se={
                 upper[ij] <- gpmean[ij] + gpse
                 lower[ij] <- gpmean[ij] - gpse
               },
               sd={
                 upper[ij] <- gpmean[ij] + gpsd
                 lower[ij] <- gpmean[ij] - gpsd
               },
               conf={
                 int <- qnorm((1-conflevel)/2, lower.tail = F)
                 upper[ij] <- gpmean[ij] + gpse*int
                 lower[ij] <- gpmean[ij] - gpse*int
               },
               none={
                 upper[i] <- gpmean[i]
                 lower[i] <- gpmean[i]
               }
        )
      }
    }
    
    meandat <- data.frame(x = factor(gp1, levels=gp1), y = gpmean, group = factor(gp2rep, levels=unique(gp2rep)), stroke=gp2stroke, groupl = factor(gp2repl, levels=unique(gp2repl)), groupe = factor(gp2repe, levels=unique(gp2repe)), upper, lower)
    if(type=="dot"){
      ggp <- ggplot()
      if(errbar!="none"){
        ggp <- ggp + geom_errorbar(aes(x=x, ymin = lower, ymax = upper, color=groupe, size=groupe, linetype=groupe), data=meandat, width = 0.4, position=pd, na.rm=TRUE, show.legend = F)
        color.group3 <- color.group
        size3 <- rep(0.5, length(gp2))
        names(color.group3) <- names(size3) <- paste0("Errorbar_",gp2)
        color.group2 <- c(color.group2, color.group3)
        plsize <- c(plsize, size3)
        lty <- c(lty, rep(1, length(gp2)))
        names(lty) <- c(paste0("Line_",gp2), paste0("Errorbar_",gp2))
      }
      ggp <- ggp + geom_point(aes(x=x, y=y, color=group, shape=group, size=group, stroke=stroke), data=meandat, position=pd, na.rm=TRUE)
      if(line==TRUE) ggp <- ggp + geom_line(aes(x=x, y=y, color=groupl, group=groupl, linetype=groupl, size=groupl), data=meandat, 
                                            position=pd, na.rm=TRUE, show.legend = F, inherit.aes = F)
      
      ggp <- ggp + scale_color_manual(values=color.group2, breaks=gp2, guide=guide_legend(by2, override.aes = list(stroke=pstroke))) + 
        scale_size_manual(values=plsize, breaks=gp2, guide=guide_legend(by2)) + 
        scale_shape_manual(values=pch, guide=guide_legend(by2)) + 
        scale_linetype_manual(values=lty, guide="none") 
    }
    if(type=="bar"){
      ggp <- ggplot() + 
        geom_col(aes(x=x, y=y, fill=group), data=meandat, color="black", position=position_dodge(), na.rm=TRUE) + 
        geom_hline(yintercept = 0)
      if(errbar!="none"){
        if(uperr) errbar_aes <- aes(x=x, ymin = y, ymax = upper, color=groupe, size=groupe, linetype=groupe)
        else errbar_aes <- aes(x=x, ymin = lower, ymax = upper, color=groupe, size=groupe, linetype=groupe)
        ggp <- ggp + geom_errorbar(errbar_aes, data=meandat, width = 0.4, position=position_dodge(width=0.9), na.rm=TRUE, show.legend = F)
      }
      ggp <- ggp + scale_fill_manual(values=color.group, guide=guide_legend(by2)) + 
        scale_color_manual(values=rep("#000000", length(gp2)), guide="none") + 
        scale_size_manual(values=rep(0.5, length(gp2)), guide="none") + 
        scale_linetype_manual(values=rep(1, length(gp2)), guide="none") 
    }
  }
  
  if(!is.null(ext.hline)){
    ext.hline.dat <- list()
    for(j in 1:length(ext.hline)){
      ext.hline.dat[[j]] <- data.frame(yintercept=as.numeric(strsplit(ext.hline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.hline[[j]][2]), size=as.numeric(ext.hline[[j]][3]), color=ext.hline[[j]][4])
    }
    ext.hline.dat <- do.call(rbind, ext.hline.dat)
    ggp <- ggp + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
  }
  
  if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = as.character(tick_y))
  ggp <- ggp +
    coord_cartesian(ylim=ylim, expand=lim.expand) +
    labs(title=title, x=xlab, y=ylab) +
    theme_opt
  attr(ggp, "Rex") <- c("interactive", "REx_meanplot")
  return(ggp)
}

##### Bar plot
REx_barplot <- function(dataset,varname,by=NULL, flip=FALSE, type="stack",
                        stat.test=F, stat.test.pos=NULL, stat.test.size=NULL,
                        title=NULL, xlab=varname, ylab="Frequency", label=T, ext.hline=NULL,
                        lgd.pos="right", lgd.back=F, lgd.title=NULL, lgd.text=NULL,
                        color="grey", color.group=NULL, freq.info=F, freq.size=NULL, freq.pos.top=T,
                        tick_y=NULL, grid.major=F, grid.minor=F, ylim=NULL, lim.expand=TRUE,
                        title.size=NULL, axis.title.size=NULL, axis.text.size=NULL, strip.text=NULL,
                        global_opt=FALSE){
  load.pkg(c("ggplot2", "ggrepel"))
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  theme_opt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2,
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size = strip.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  if(!is.null(freq.size)) freq.size <- freq.size * 0.3514598
  else freq.size <- 10 * 0.3514598
  
  x <- factor(dataset[,varname], levels=unique(dataset[,varname]))
  if(flip==TRUE) x <- factor(x, levels=rev(levels(x)))
  dat <- data.frame(x)
  if(!is.null(by)) dat$group <- factor(dataset[,by], levels=unique(dataset[,by]))
  dat <- na.exclude(dat)
  
  stat.test2 <- FALSE
  
  if(is.null(by)){
    dat1 <- data.frame(x=factor(names(table(dat$x)), levels=levels(x)), y=as.integer(table(dat$x)), color1=factor("Bar_base"))
    
    ggp <- ggplot() + 
      geom_col(aes(x, y, fill=color1), data=dat1, color="black", na.rm=T) + 
      geom_hline(yintercept = 0) +
      scale_fill_manual(values=color, guide="none")
    if(freq.info){
      if(flip) ggp <- ggp + ggrepel::geom_text_repel(aes(x,y,label = y), data=dat1, size=freq.size, hjust=as.integer(freq.pos.top))
      else ggp <- ggp + ggrepel::geom_text_repel(aes(x,y,label = y), data=dat1, size=freq.size, vjust=as.integer(freq.pos.top))
    }
  } else {
    dat2 <- data.frame(x=factor(rep(rownames(table(dat)),ncol(table(dat))), levels=levels(x)),
                       y=as.integer(table(dat)), group=factor(rep(levels(dat$group), each=nrow(table(dat))), levels=levels(dat$group)))
    dat3 <- dat2[which(dat2$y!=0),]
    
    if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dataset[,by]))))
    if(type=="stack"){
      ggp <- ggplot() + 
        geom_col(aes(x, y, fill=group), data=dat3, color="black", na.rm=T) + 
        geom_hline(yintercept = 0)
      if(freq.info){
        if(flip) ggp <- ggp + ggrepel::geom_text_repel(aes(x, y, group=group, label = y), data=dat3, size=freq.size, position="stack", hjust=as.integer(freq.pos.top))
        else ggp <- ggp + ggrepel::geom_text_repel(aes(x, y, group=group, label = y), data=dat3, size=freq.size, position="stack", vjust=as.integer(freq.pos.top))
      }
    }
    if(type=="par"){
      dat2$just <- as.integer(freq.pos.top)
      dat2$just[dat2$y==0] <- 1
      ggp <- ggplot() + 
        geom_col(aes(x, y, fill=group), data=dat2, color="black", na.rm=T, position=position_dodge(width = 0.9)) + 
        geom_hline(yintercept = 0)
      if(freq.info){
        if(flip) ggp <- ggp + ggrepel::geom_text_repel(aes(x, y, group=group, label = y, hjust=just), data=dat2, size=freq.size, position=position_dodge(width = 0.9))
        else ggp <- ggp + ggrepel::geom_text_repel(aes(x, y, group=group, label = y, vjust=just), data=dat2, size=freq.size, position=position_dodge(width = 0.9))
      }
      
      if(stat.test){
        testres <- chisq.test(table(dat))
        stat.test.name <- "Pearson's chi-squared test"
        if(any(testres$expected<5)){
          testres <- tryCatch(fisher.test(table(dat)), error=function(e) NULL)
          if(is.null(testres)){
            testres <- fisher.test(table(dat), simulate.p.value = T, B=20000)
            stat.test.name <- "Fisher's exact test (MC simulated)"
          } else stat.test.name <- "Fisher's exact test"
        }
        labp <- ifelse(testres$p.value<0.0001, "<0.0001", paste0("=",sprintf("%.4f", round(testres$p.value,4))))
        labtest <- paste0(stat.test.name,", p",labp)
        
        if(!is.null(stat.test.size)) stat.test.size <- stat.test.size * 0.3514598
        else stat.test.size <- 10 * 0.3514598
        
        if(is.null(stat.test.pos)){
          candx <- 1.2
          candy <- max(ggplot_build(ggp)$data[[1]]$y) * 1.1
          statdatpos <- data.frame(x=candx, y=candy)
        }
        else statdatpos <- data.frame(x=stat.test.pos[1], y=stat.test.pos[2])
        
        stat.test2 <- TRUE
        # ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y), data=statdatpos, label = labtest, size=stat.test.size, inherit.aes = F, point.padding = NA, min.segment.length=10000)
      }
    }
    if(type=="facet"){
      labelv <- NULL
      if(label){
        labelv <- paste0(by," = ",levels(dat$group))
        names(labelv) <- levels(dat$group)
      }
      
      dat2$just <- as.integer(freq.pos.top)
      dat2$just[dat2$y==0] <- 1
      ggp <- ggplot() + 
        geom_col(aes(x, y, fill=group), data=dat2, color="black", na.rm=T) + 
        geom_hline(yintercept = 0) +
        facet_wrap(~group, ncol=1, scales="fixed", labeller = labeller(group=labelv))
      if(freq.info){
        if(flip) ggp <- ggp + ggrepel::geom_text_repel(aes(x, y, group=group, label = y, hjust=just), data=dat2, size=freq.size)
        else ggp <- ggp + ggrepel::geom_text_repel(aes(x, y, group=group, label = y, vjust=just), data=dat2, size=freq.size)
      }
      ggp <- ggp + theme(legend.position = "none")
    }
    ggp <- ggp + scale_fill_manual(values=color.group, guide=guide_legend(by))
  }
  
  if(!is.null(ext.hline)){
    ext.hline.dat <- list()
    for(j in 1:length(ext.hline)){
      ext.hline.dat[[j]] <- data.frame(yintercept=as.numeric(strsplit(ext.hline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.hline[[j]][2]), size=as.numeric(ext.hline[[j]][3]), color=ext.hline[[j]][4])
    }
    ext.hline.dat <- do.call(rbind, ext.hline.dat)
    ggp <- ggp + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
  }
  
  if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = as.character(tick_y))
  
  if(flip==TRUE) ggp <- ggp + coord_flip(ylim=ylim, expand=lim.expand)
  else ggp <- ggp + coord_cartesian(ylim=ylim, expand=lim.expand)
  
  if(stat.test2){
    xylim <- layer_scales(ggp)
    xlim <- xylim$x$range_c$range
    if(is.null(ylim)) ylim <- xylim$y$range$range
    if(is.null(stat.test.pos)){
      # if(statdatpos$x>xlim[2]) statdatpos$x <- xlim[2]
      # if(statdatpos$x<xlim[1]) statdatpos$x <- xlim[1]
      if(statdatpos$y>ylim[2]) statdatpos$y <- ylim[2]
      if(statdatpos$y<ylim[1]) statdatpos$y <- ylim[1]
    } else {
      statdatpos$x <- xlim[1] + (xlim[2]-xlim[1])*statdatpos$x/100
      statdatpos$y <- ylim[1] + (ylim[2]-ylim[1])*statdatpos$y/100
    }
    ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y), data=statdatpos, label = labtest, size=stat.test.size, inherit.aes = F, point.padding = NA, min.segment.length=10000)
  }
  
  ggp <- ggp + labs(title=title, x=xlab, y=ylab) + theme_opt
  attr(ggp, "Rex") <- c("interactive", "REx_barplot")
  return(ggp)
}

##### Circle plot
REx_circleplot <- function(dataset,varname, showCount="C", title=varname,
                           freq.size=NULL, title.size=NULL, color.group=NULL,
                           lgd.pos="right", lgd.back=F, lgd.title=NULL, lgd.text=NULL,
                           global_opt=FALSE){
  load.pkg(c("ggplot2", "ggpubr", "ggrepel"))
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  theme_opt <- theme(plot.title = element_text(hjust = 0.5, size = title.size),
                     legend.position = lgd.pos, legend.background = lgd.back2,
                     legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text))
  
  x <- factor(dataset[,varname], levels=unique(dataset[,varname]))
  dat <- data.frame(table(x))
  if(sum(is.na(x))>0) dat <- rbind(dat, data.frame(x="NA", Freq=sum(is.na(x))))
  colnames(dat)[1] <- varname
  dat$Perc <- paste0(round(dat$Freq/length(x)*100,1), "%")
  dat$Both <- paste0(dat$Freq, " (", dat$Perc, ")")
  
  lev <- levels(x)
  if(sum(is.na(x))>0) lev <- c(lev, "NA")
  if(is.null(color.group)) color.group <- gg_color_hue2(length(lev))
  
  if(showCount=="C") labs <- dat$Freq
  if(showCount=="P") labs <- dat$Perc
  if(showCount=="CP") labs <- dat$Both
  if(showCount=="N") labs <- rep("", nrow(dat))
  
  ggpubr_ver <- as.numeric(strsplit(installed.packages()["ggpubr","Version"], "\\.")[[1]])
  if(length(ggpubr_ver)<3) ggpubr_ver[3] <- 0
  if(ggpubr_ver[1]==0 & (ggpubr_ver[2]<2 | ggpubr_ver[2]==2 & ggpubr_ver[3]<3)) labs <- rev(labs)
  
  if(is.null(freq.size)) freq.size <- 10 * 0.3514598
  else freq.size <- freq.size * 0.3514598
  
  ggpie2 <- ggpie
  body(ggpie2)[[10]] <- substitute(
    p <- ggpar(p, palette = palette, ggtheme = ggtheme, font.family = font.family,
               ...) + coord_polar(theta = "y", start = pi/2) + ggtheme +
      .remove_axis()
  )
  body(ggpie2)[[11]][[3]][[3]][[3]][[2]] <- substitute(
    p <- p + ggrepel::geom_text_repel(aes_string(y = ".lab.ypos.", x=1.1, label = label), point.padding = NA,
                                      size = lab.font$size, fontface = lab.font$face, colour = "black",
                                      family = font.family) + clean_theme()
  )
  
  if (length(labs) == 1) {
    labs.safe <- ifelse(nchar(labs), gsub("^(.+)$","\"\\1\"", labs), NA)
  } else labs.safe <- labs
  ggp <- ggpie2(dat, "Freq", label=labs.safe, fill=varname, palette=color.group, lab.pos = "in", lab.font = list(size=freq.size)) +
    # guides(fill = guide_legend(varname)) + labs(title=title) + theme_opt
    scale_fill_manual(values=color.group, guide=guide_legend(varname)) + labs(title=title, x=NULL, y=NULL) + theme_opt
  
  attr(ggp, "Rex") <- c("interactive", "REx_circleplot")
  return(ggp)
}

##### Bubble chart
REx_bubbleplot <- function(dataset,varname1, varname2, varname_size,by=NULL, w_size=FALSE,
                           lineby=FALSE, LeastSq=FALSE, Ls.spread=FALSE, Ls.spread.type="confidence", Ls.level=0.95,
                           Ls.text=FALSE, Ls.text.pos=NULL, Ls.text.size=NULL,
                           Smooth=FALSE, Sm.span=1, Sm.spread=FALSE,
                           lgd.pos="bottom", lgd.back=F, lgd.title=NULL, lgd.text=NULL,
                           ext.dot=NULL, ext.abline=NULL, ext.hline=NULL, ext.vline=NULL,
                           title=NULL, xlab=varname1, ylab=varname2,
                           pch=19, pstroke=0.5, psize_min=1, psize_max=6,
                           color.dot="black", color.group=NULL, alpha=1,
                           color.line.Ls="red", color.line.Sm="green",
                           tick_x=NULL, tick_y=NULL, grid.major=T, grid.minor=F,
                           xlim=NULL, ylim=NULL, lim.expand=TRUE,
                           title.size=NULL, axis.title.size=NULL, axis.text.size=NULL,
                           global_opt=FALSE){
  load.pkg(c("ggplot2","ggrepel"))

  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }

  suppressWarnings(x <- as.numeric(as.character(dataset[,varname1])))
  suppressWarnings(y <- as.numeric(as.character(dataset[,varname2])))
  suppressWarnings(wp <- as.numeric(as.character(dataset[,varname_size])))
  if(sum(is.na(x))==length(x)) rexStop("There is no numeric values.")
  if(sum(is.na(y))==length(y)) rexStop("There is no numeric values.")
  if(sum(is.na(wp))==length(wp)) rexStop("There is no numeric values.")

  if(w_size & Reduce('&', na.exclude(wp)>0)) w <- wp
  else w <- 1

  dat <- data.frame(x,y,w,wp)

  if(!is.null(Ls.text.size)) Ls.text.size <- Ls.text.size * 0.3514598
  else Ls.text.size <- 10 * 0.3514598

  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()

  theme_opt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2,
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())

  stat.test2 <- FALSE

  if(is.null(by)){
    dat$color1 <- dat$shape1 <- factor("Point_base")
    dat$color4 <- dat$linetype4 <- factor("Smooth_base")
    dat$color5 <- dat$linetype5 <- factor("LeastSq_base")
    
    ggp <- ggplot() + 
      geom_point(mapping=aes(x=x, y=y, size=wp, color=color1, shape=shape1), data=dat, stroke=pstroke, alpha=alpha, na.rm=T)
    if(Smooth==TRUE){
      if(Sm.spread) aes_Smooth <- aes(x=x, y=y, weight=w, color=color4, fill=color4, linetype=linetype4)
      else aes_Smooth <- aes(x=x, y=y, weight=w, color=color4, linetype=linetype4)
      ggp <- ggp + geom_smooth(aes_Smooth, data=dat, method="loess", size=0.8, span=Sm.span, se=Sm.spread, show.legend=F, na.rm=T)
    }
    if(LeastSq==TRUE){
      if(Ls.spread) aes_LeastSq <- aes(x=x, y=y, weight=w, color=color5, fill=color5, linetype=linetype5)
      else aes_LeastSq <- aes(x=x, y=y, weight=w, color=color5, linetype=linetype5)
      if(!(Ls.spread.type=="prediction" & Ls.spread)) ggp <- ggp + stat_smooth(aes_LeastSq, data=dat, method = "lm", se=Ls.spread, show.legend=F, na.rm=T, level = Ls.level)
      else {
        ggpLS <- ggp + stat_smooth(mapping=aes(x=x, y=y, weight=w), data=dat, method = "lm", se=Ls.spread, color=color.line.Ls, fill=color.line.Ls, show.legend=F, na.rm=T, level = Ls.level)
        ggpLSdat <- ggplot_build(ggpLS)$data[[ifelse(Smooth,3,2)]]

        mod <- lm(y~x, dat, weights=w)
        ggpLSpred <- predict(mod, data.frame(x=ggpLSdat$x), interval = "prediction", level = Ls.level)
        ggpLSdat$ymin <- ggpLSpred[,2]
        ggpLSdat$ymax <- ggpLSpred[,3]
        ggpLSdat$color5 <- factor("LeastSq_base")

        ggp <- ggp + geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax, fill=color5), data=ggpLSdat, alpha=0.2, show.legend=F, na.rm=T, inherit.aes = F) +
          stat_smooth(aes(x=x, y=y, weight=w, color=color5, linetype=linetype5), data=dat, method = "lm", se=F, show.legend=F, na.rm=T)
      }
    }
    
    ggp <- ggp + scale_color_manual(values=c("Point_base"=color.dot, "Smooth_base"=color.line.Sm, "LeastSq_base"=color.line.Ls), guide="none") +
      scale_shape_manual(values=c("Point_base"=pch), guide="none") +
      scale_size_continuous(range=c(psize_min, psize_max), guide=guide_legend(varname_size, override.aes = list(alpha=1, shape=c("Point_base"=pch), color=c("Point_base"=color.dot)))) +
      scale_linetype_manual(values=rep(1,2), guide="none") +
      scale_fill_manual(values=c("Smooth_base"=color.line.Sm, "LeastSq_base"=color.line.Ls), guide="none")
    
    if(LeastSq==TRUE & Ls.text){
      mod <- lm(y~x, dat, weights=w)
      modcor <- cor.test(~x+y, dat)
      LSdat <- ggplot_build(ggp)$data[[ifelse(Smooth,3,2)]]

      range_x <- range(dat$x, na.rm=T)
      range_x1 <- diff(range_x)*c(0.1,0.9) + range_x[1]
      range_x2 <- diff(range_x)*c(0.2,0.8) + range_x[1]
      range_y <- range(dat$y, na.rm=T)
      range_y1 <- diff(range_y)*c(0.2,0.8) + range_y[1]
      range_y2 <- diff(range(LSdat$y))*c(0.2,0.8) + range(LSdat$y)[1]

      if(mod$coefficients[2]>0){
        cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[2])), max(c(range_y2[2], range_y1[2]))))
        cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[1])), max(c(range_y2[1], range_y1[1]))))
      } else {
        cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[1])), max(c(range_y2[2], range_y1[1]))))
        cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[2])), max(c(range_y2[1], range_y1[2]))))
      }
      if(runif(1)>=0.5) cand3 <- cand1
      else cand3 <- cand2
      if(is.null(Ls.text.pos)) LSdatpos <- data.frame(x=cand3[1], y=cand3[2])
      else LSdatpos <- data.frame(x=Ls.text.pos[1], y=Ls.text.pos[2])

      labxy <- paste0("y=",sprintf("%.3f", mod$coefficients[1]),sprintf("%+.3f", mod$coefficients[2]),"x")

      stat.test2 <- TRUE
      # if((Ls.spread | Sm.spread) & is.null(Ls.text.pos)) ggp <- ggp + ggrepel::geom_label_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
      # else ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)

    }
  } else {
    group <- factor(dataset[,by], levels=unique(dataset[,by]))
    dat <- cbind(dat,group)
    if(sum(is.na(dat$group))>0) dat <- dat[-which(is.na(dat$group)),]
    if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dataset[,by]))))

    if(length(pch)==1) pch <- rep(pch, length(levels(dat$group)))
    # if(length(psize)==1) psize <- rep(psize, length(levels(dat$group)))
    if(length(pstroke)==1) pstroke <- rep(pstroke, length(levels(dat$group)))
    names(pch) <- names(pstroke) <- levels(group)
    
    dat$stroke <- dat$group
    levels(dat$stroke) <- pstroke
    dat$stroke <- as.numeric(as.character(dat$stroke))

    ggp <- ggplot() +
      geom_point(aes(x=x, y=y, color=group, shape=group, size=wp, stroke=stroke), data=dat, alpha=alpha, na.rm=T)
    if(lineby==TRUE){
      dat$group_Smooth <- factor(paste0("Smooth_",dat$group), levels=paste0("Smooth_",levels(dat$group)))
      dat$group_LeastSq <- factor(paste0("LeastSq_",dat$group), levels=paste0("LeastSq_",levels(dat$group)))
      
      if(Smooth==TRUE){
        if(Sm.spread) aes_Smooth <- aes(x=x, y=y, weight=w, color=group_Smooth, fill=group_Smooth, linetype=group_Smooth)
        else aes_Smooth <- aes(x=x, y=y, weight=w, color=group_Smooth, linetype=group_Smooth)
        ggp <- ggp + geom_smooth(aes_Smooth, data=dat, method="loess", size=0.8, span=Sm.span, se=Sm.spread, show.legend=F, na.rm=T)
      }
      if(LeastSq==TRUE){
        if(Ls.spread) aes_LeastSq <- aes(x=x, y=y, weight=w, color=group_LeastSq, fill=group_LeastSq, linetype=group_LeastSq)
        else aes_LeastSq <- aes(x=x, y=y, weight=w, color=group_LeastSq, linetype=group_LeastSq)
        if(!(Ls.spread.type=="prediction" & Ls.spread)) ggp <- ggp + stat_smooth(aes_LeastSq, data=dat, method = "lm", se=Ls.spread, show.legend=F, na.rm=T, level = Ls.level)
        else {
          ggpLS <- ggp + stat_smooth(aes(x=x, y=y, weight=w, color=group, fill=group, size=NULL, shape=NULL, stroke=NULL), data=dat, method = "lm", se=Ls.spread, show.legend=F, na.rm=T, level = Ls.level)
          ggpLSdat <- ggplot_build(ggpLS)$data[[ifelse(Smooth,3,2)]]

          lev <- levels(group)
          for(llt in 1:length(lev)){
            ll <- lev[llt]
            datlev <- subset(dat, group==ll)
            mod <- lm(y~x, datlev, weights=w)
            ggpLSpred <- predict(mod, data.frame(x=ggpLSdat$x[ggpLSdat$group==llt]), interval = "prediction", level = Ls.level)
            ggpLSdat$ymin[ggpLSdat$group==llt] <- ggpLSpred[,2]
            ggpLSdat$ymax[ggpLSdat$group==llt] <- ggpLSpred[,3]
          }
          ggpLSdat$group_LeastSq <-  factor(paste0("LeastSq_",levels(group)[ggpLSdat$group]), levels=paste0("LeastSq_",levels(dat$group)))
          
          ggp <- ggp + geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax, fill=group_LeastSq), data=ggpLSdat, alpha=0.2, show.legend=F, na.rm=T, inherit.aes = F) +
            stat_smooth(aes(x=x, y=y, weight=w, color=group_LeastSq, linetype=group_LeastSq), data=dat, method = "lm", se=F, show.legend=F, na.rm=T)
        }
      }
      
      val_color <- rep(color.group, 3)
      names(val_color) <- paste0(rep(c("", "Smooth_", "LeastSq_"),each=length(levels(group))), rep(levels(group),3))
      val_fill <- rep(color.group, 2)
      names(val_fill) <- paste0(rep(c("Smooth_", "LeastSq_"),each=length(levels(group))), rep(levels(group),2))
      names(pch) <- levels(group)
      
      ggp <- ggp + scale_color_manual(values=val_color, breaks=as.character(levels(group)), guide=guide_legend(by, override.aes = list(alpha=1, size=(psize_min+psize_max)/2, stroke=pstroke))) + 
        scale_size_continuous(range=c(psize_min, psize_max), guide=guide_legend(varname_size, override.aes = list(alpha=1, shape=pch[1]))) +
        scale_shape_manual(values=pch, guide=guide_legend(by)) + 
        scale_linetype_manual(values=rep(1,length(levels(group))*2), guide="none") + 
        scale_fill_manual(values=val_fill, guide="none")
      
      # if(!is.null(by)) ggp <- ggp + guides(color = guide_legend(by, override.aes = list(shape=pch, stroke=pstroke, alpha=1, size=(psize_min+psize_max)/2)),
      #                                      fill = guide_legend(by), shape=guide_legend(by), size=guide_legend(varname_size, override.aes = list(alpha=1, shape=pch[1])))
      # else ggp <- ggp + guides(size=guide_legend(varname_size, override.aes = list(alpha=1, shape=pch[1])))
      
      
      if(LeastSq==TRUE & Ls.text){
        lev <- levels(group)
        LSdatpos <- data.frame(x=NA, y=NA, lab=NA)[-1,]
        # Ls.text.pos_a <- diff(range(dat$y, na.rm=T))*0.02*Ls.text.size/3.514598
        # Ls.text.pos_a <- Ls.text.pos_a*seq((length(lev)-1)/2, (1-length(lev))/2, length.out=length(lev))
        Ls.text.pos_a <- seq((length(lev)-1)/2, (1-length(lev))/2, length.out=length(lev))
        for(llt in 1:length(lev)){
          ll <- lev[llt]
          datlev <- subset(dat, group==ll)
          mod <- lm(y~x, datlev, weights=w)
          modcor <- cor.test(~x+y, datlev)
          LSdat <- subset(ggplot_build(ggp)$data[[ifelse(Smooth,3,2)]], group==llt)

          range_x <- range(datlev$x, na.rm=T)
          range_x1 <- diff(range_x)*c(0.1,0.9) + range_x[1]
          range_x2 <- diff(range_x)*c(0.2,0.8) + range_x[1]
          range_y <- range(datlev$y, na.rm=T)
          range_y1 <- diff(range_y)*c(0.2,0.8) + range_y[1]
          range_y2 <- diff(range(LSdat$y))*c(0.2,0.8) + range(LSdat$y)[1]

          if(mod$coefficients[2]>0){
            cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[2])), max(c(range_y2[2], range_y1[2]))))
            cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[1])), max(c(range_y2[1], range_y1[1]))))
          } else {
            cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[1])), max(c(range_y2[2], range_y1[1]))))
            cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[2])), max(c(range_y2[1], range_y1[2]))))
          }
          if(runif(1)>=0.5) cand3 <- cand1
          else cand3 <- cand2

          labxy <- paste0("y=",sprintf("%.3f", mod$coefficients[1]),sprintf("%+.3f", mod$coefficients[2]),"x")

          if(is.null(Ls.text.pos)) LSdatpos <- rbind(LSdatpos, data.frame(x=cand3[1], y=cand3[2], lab=labxy))
          else LSdatpos <- rbind(LSdatpos, data.frame(x=Ls.text.pos[1], y=Ls.text.pos[2]+Ls.text.pos_a[llt], lab=labxy))
        }

        stat.test2 <- TRUE
        # if((Ls.spread | Sm.spread) & is.null(Ls.text.pos)) ggp <- ggp + ggrepel::geom_label_repel(aes(x=x, y=y, label=lab), data=LSdatpos, color=color.group, size=Ls.text.size, inherit.aes = F, point.padding = NA)
        # else ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y, label=lab), data=LSdatpos, color=color.group, size=Ls.text.size, inherit.aes = F, direction = "y", min.segment.length=10000, point.padding = NA)
      }
    } else {
      dat$color4 <- dat$size4 <- dat$linetype4 <- factor("Smooth_base")
      dat$color5 <- dat$size5 <- dat$linetype5 <- factor("LeastSq_base")
      
      if(Smooth==TRUE){
        if(Sm.spread) aes_Smooth <- aes(x=x, y=y, weight=w, color=color4, fill=color4, linetype=linetype4)
        else aes_Smooth <- aes(x=x, y=y, weight=w, color=color4, linetype=linetype4)
        ggp <- ggp + geom_smooth(aes_Smooth, data=dat, method="loess", size=0.8, span=Sm.span, se=Sm.spread, show.legend=F, na.rm=T)
      }
      if(LeastSq==TRUE){
        if(Ls.spread) aes_LeastSq <- aes(x=x, y=y, weight=w, color=color5, fill=color5, linetype=linetype5)
        else aes_LeastSq <- aes(x=x, y=y, weight=w, color=color5, linetype=linetype5)
        if(!(Ls.spread.type=="prediction" & Ls.spread)) ggp <- ggp + stat_smooth(aes_LeastSq, data=dat, method = "lm", se=Ls.spread, show.legend=F, na.rm=T, level = Ls.level)
        else {
          ggpLS <- ggp + stat_smooth(aes(x=x, y=y, weight=w, color=NULL, fill=NULL, size=NULL, shape=NULL, stroke=NULL), data=dat, method = "lm", se=Ls.spread, color=color.line.Ls, fill=color.line.Ls, show.legend=F, na.rm=T, level = Ls.level)
          ggpLSdat <- ggplot_build(ggpLS)$data[[ifelse(Smooth,3,2)]]

          mod <- lm(y~x, dat, weights=w)
          ggpLSpred <- predict(mod, data.frame(x=ggpLSdat$x), interval = "prediction", level = Ls.level)
          ggpLSdat$ymin <- ggpLSpred[,2]
          ggpLSdat$ymax <- ggpLSpred[,3]
          ggpLSdat$color5 <- factor("LeastSq_base")

          ggp <- ggp + geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax, fill=color5), data=ggpLSdat, alpha=0.2, show.legend=F, na.rm=T, inherit.aes = F) +
            stat_smooth(aes(x=x, y=y, weight=w, color=color5, linetype=linetype5), data=dat, method = "lm", se=F, show.legend=F, na.rm=T)
        }
      }
      
      val_color <- c(color.group, "Smooth_base"=color.line.Sm, "LeastSq_base"=color.line.Ls)
      names(val_color)[1:length(levels(group))] <- levels(group)
      names(pch) <- levels(group)
      
      ggp <- ggp + scale_color_manual(values=val_color, breaks=levels(group), guide=guide_legend(by, override.aes = list(alpha=1, size=(psize_min+psize_max)/2, stroke=pstroke))) + 
        scale_size_continuous(range=c(psize_min, psize_max), guide=guide_legend(varname_size, override.aes = list(alpha=1, shape=pch[1]))) +
        scale_shape_manual(values=pch, guide=guide_legend(by)) + 
        scale_linetype_manual(values=rep(1,2), guide="none") + 
        scale_fill_manual(values=c("Smooth_base"=color.line.Sm, "LeastSq_base"=color.line.Ls), guide="none")
      
      if(LeastSq==TRUE & Ls.text){
        mod <- lm(y~x, dat, weights=w)
        modcor <- cor.test(~x+y, dat)
        LSdat <- ggplot_build(ggp)$data[[ifelse(Smooth,3,2)]]

        range_x <- range(dat$x, na.rm=T)
        range_x1 <- diff(range_x)*c(0.1,0.9) + range_x[1]
        range_x2 <- diff(range_x)*c(0.2,0.8) + range_x[1]
        range_y <- range(dat$y, na.rm=T)
        range_y1 <- diff(range_y)*c(0.2,0.8) + range_y[1]
        range_y2 <- diff(range(LSdat$y))*c(0.2,0.8) + range(LSdat$y)[1]

        if(mod$coefficients[2]>0){
          cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[2])), max(c(range_y2[2], range_y1[2]))))
          cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[1])), max(c(range_y2[1], range_y1[1]))))
        } else {
          cand1 <- c(runif(1,range_x1[1],range_x2[1]), runif(1,min(c(range_y2[2], range_y1[1])), max(c(range_y2[2], range_y1[1]))))
          cand2 <- c(runif(1,range_x2[2],range_x1[2]), runif(1,min(c(range_y2[1], range_y1[2])), max(c(range_y2[1], range_y1[2]))))
        }
        if(runif(1)>=0.5) cand3 <- cand1
        else cand3 <- cand2
        if(is.null(Ls.text.pos)) LSdatpos <- data.frame(x=cand3[1], y=cand3[2])
        else LSdatpos <- data.frame(x=Ls.text.pos[1], y=Ls.text.pos[2])

        labxy <- paste0("y=",sprintf("%.3f", mod$coefficients[1]),sprintf("%+.3f", mod$coefficients[2]),"x")

        stat.test2 <- TRUE
        # if((Ls.spread | Sm.spread) & is.null(Ls.text.pos)) ggp <- ggp + ggrepel::geom_label_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
        # else ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
      }
    }
    # if(!is.null(color.group)) ggp <- ggp + scale_colour_manual(values=color.group) + scale_fill_manual(values=color.group)
  }
  if(!is.null(ext.dot)){
    ext.dot.dat <- list()
    for(j in 1:length(ext.dot)){
      ext.dot2 <- ext.dot[[j]][1]
      ext.dot.dat[[j]] <- grep("[0-9.-]+[ ]*,[ ]*[0-9.-]+",unlist(strsplit(ext.dot2,"[()]")), value=T)
      ext.dot.dat[[j]] <- do.call(rbind, strsplit(ext.dot.dat[[j]],","))
      ext.dot.dat[[j]] <- data.frame(x=as.numeric(ext.dot.dat[[j]][,1]), y=as.numeric(ext.dot.dat[[j]][,2]),
                                     shape=as.numeric(ext.dot[[j]][2]), size=as.numeric(ext.dot[[j]][3]), stroke=as.numeric(ext.dot[[j]][4]), color=ext.dot[[j]][5])
    }
    ext.dot.dat <- do.call(rbind, ext.dot.dat)
    ggp <- ggp + geom_point(mapping=aes(x=x,y=y), data=ext.dot.dat, color=ext.dot.dat$color, fill=ext.dot.dat$color, shape=ext.dot.dat$shape, size=ext.dot.dat$size, stroke=ext.dot.dat$stroke)
  }
  if(!is.null(ext.abline)){
    ext.abline.dat <- list()
    for(j in 1:length(ext.abline)){
      ext.abline2 <- ext.abline[[j]][1]
      ext.abline.dat[[j]] <- grep("[0-9.-]+[ ]*,[ ]*[0-9.-]+",unlist(strsplit(ext.abline2,"[()]")), value=T)
      ext.abline.dat[[j]] <- do.call(rbind, strsplit(ext.abline.dat[[j]],","))
      ext.abline.dat[[j]] <- data.frame(intercept=as.numeric(ext.abline.dat[[j]][,1]), slope=as.numeric(ext.abline.dat[[j]][,2]),
                                        linetype=as.numeric(ext.abline[[j]][2]), size=as.numeric(ext.abline[[j]][3]), color=ext.abline[[j]][4])
    }
    ext.abline.dat <- do.call(rbind, ext.abline.dat)
    ggp <- ggp + geom_abline(aes(intercept=intercept, slope=slope), ext.abline.dat, color=ext.abline.dat$color, linetype=ext.abline.dat$linetype, size=ext.abline.dat$size)
  }
  if(!is.null(ext.hline)){
    ext.hline.dat <- list()
    for(j in 1:length(ext.hline)){
      ext.hline.dat[[j]] <- data.frame(yintercept=as.numeric(strsplit(ext.hline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.hline[[j]][2]), size=as.numeric(ext.hline[[j]][3]), color=ext.hline[[j]][4])
    }
    ext.hline.dat <- do.call(rbind, ext.hline.dat)
    ggp <- ggp + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
  }
  if(!is.null(ext.vline)){
    ext.vline.dat <- list()
    for(j in 1:length(ext.vline)){
      ext.vline.dat[[j]] <- data.frame(xintercept=as.numeric(strsplit(ext.vline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.vline[[j]][2]), size=as.numeric(ext.vline[[j]][3]), color=ext.vline[[j]][4])
    }
    ext.vline.dat <- do.call(rbind, ext.vline.dat)
    ggp <- ggp + geom_vline(aes(xintercept = xintercept), ext.vline.dat, color=ext.vline.dat$color, linetype=ext.vline.dat$linetype, size=ext.vline.dat$size)
  }

  if(!is.null(tick_x)) ggp <- ggp + scale_x_continuous(breaks=as.numeric(names(tick_x)), labels = as.character(tick_x))
  if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = as.character(tick_y))

  ggp <- ggp + 
    # scale_size_continuous(range=c(psize_min, psize_max)) +
    theme_opt +
    labs(title=title, x=xlab, y=ylab) +
    coord_cartesian(xlim=xlim, ylim=ylim, expand=lim.expand)

  # if(!is.null(by)) ggp <- ggp + guides(color = guide_legend(by, override.aes = list(shape=pch, stroke=pstroke, alpha=1, size=(psize_min+psize_max)/2)),
  #                                      fill = guide_legend(by), shape=guide_legend(by), size=guide_legend(varname_size, override.aes = list(alpha=1, shape=pch[1])))
  # else ggp <- ggp + guides(size=guide_legend(varname_size, override.aes = list(alpha=1, shape=pch[1])))

  if(stat.test2){
    xylim <- layer_scales(ggp)
    if(is.null(xlim)) xlim <- xylim$x$range$range
    if(is.null(ylim)) ylim <- xylim$y$range$range
    if(is.null(Ls.text.pos)){
      LSdatpos$x[LSdatpos$x>xlim[2]] <- xlim[2]
      LSdatpos$x[LSdatpos$x<xlim[1]] <- xlim[1]
      LSdatpos$y[LSdatpos$y>ylim[2]] <- ylim[2]
      LSdatpos$y[LSdatpos$y<ylim[1]] <- ylim[1]
    } else {
      LSdatpos$x <- xlim[1] + (xlim[2]-xlim[1])*LSdatpos$x/100
      LSdatpos$y <- ylim[1] + (ylim[2]-ylim[1])*LSdatpos$y/100
    }

    if(!is.null(by) & lineby){
      if((Ls.spread | Sm.spread) & is.null(Ls.text.pos)) ggp <- ggp + ggrepel::geom_label_repel(aes(x=x, y=y, label=lab), data=LSdatpos, color=color.group, size=Ls.text.size, inherit.aes = F, point.padding = NA)
      else ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y, label=lab), data=LSdatpos, color=color.group, size=Ls.text.size, inherit.aes = F, direction = "y", min.segment.length=10000, point.padding = NA)
    } else {
      if((Ls.spread | Sm.spread) & is.null(Ls.text.pos)) ggp <- ggp + ggrepel::geom_label_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
      else ggp <- ggp + ggrepel::geom_text_repel(aes(x=x, y=y), data=LSdatpos, label = labxy, size=Ls.text.size, inherit.aes = F, point.padding = NA)
    }
  }

  attr(ggp, "Rex") <- c("interactive", "REx_bubbleplot")
  return(ggp)
}

##### 2d-histogram
REx_histogram2d <- function(dataset,varname1, varname2, bin.x=10, bin.y=10,
                            lgd.pos="right", lgd.back=F, lgd.title=NULL, lgd.text=NULL, lgd.length1=10, lgd.length2=0.75,
                            title=NULL, xlab=varname1, ylab=varname2, color1="yellow", color2="red",
                            ext.hline=NULL, ext.vline=NULL,
                            tick_x=NULL, tick_y=NULL, grid.major=T, grid.minor=F,
                            marg.box=F, marg.ratio=0.2, color.marg="grey",
                            xlim=NULL, ylim=NULL, lim.expand=TRUE,
                            title.size=NULL, axis.title.size=NULL, axis.text.size=NULL,
                            global_opt=FALSE){
  load.pkg(c("ggplot2", "cowplot", "grid", "gridExtra", "ggrepel", "reshape2"))
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  suppressWarnings(x <- as.numeric(as.character(dataset[,varname1])))
  suppressWarnings(y <- as.numeric(as.character(dataset[,varname2])))
  if(sum(is.na(x))==length(x)) rexStop("There is no numeric values.")
  if(sum(is.na(y))==length(y)) rexStop("There is no numeric values.")
  dat <- data.frame(x,y)
  
  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  theme_opt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2,
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  ggp <- ggplot() + geom_bin2d(aes(x=x, y=y), data=dat, bins=c(bin.x, bin.y), color="black", na.rm=T)
  
  countinfo <- cumsum(table(ggplot_build(ggp)$data[[1]]$count))
  ggp <- ggp + scale_fill_gradientn(colors = colorRampPalette(c(color1, color2))(diff(range(countinfo))+1)[countinfo-countinfo[1]+1],
                                    values = scales::rescale(countinfo))
  
  if(!is.null(ext.hline)){
    ext.hline.dat <- list()
    for(j in 1:length(ext.hline)){
      ext.hline.dat[[j]] <- data.frame(yintercept=as.numeric(strsplit(ext.hline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.hline[[j]][2]), size=as.numeric(ext.hline[[j]][3]), color=ext.hline[[j]][4])
    }
    ext.hline.dat <- do.call(rbind, ext.hline.dat)
    ggp <- ggp + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
  }
  if(!is.null(ext.vline)){
    ext.vline.dat <- list()
    for(j in 1:length(ext.vline)){
      ext.vline.dat[[j]] <- data.frame(xintercept=as.numeric(strsplit(ext.vline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.vline[[j]][2]), size=as.numeric(ext.vline[[j]][3]), color=ext.vline[[j]][4])
    }
    ext.vline.dat <- do.call(rbind, ext.vline.dat)
    ggp <- ggp + geom_vline(aes(xintercept = xintercept), ext.vline.dat, color=ext.vline.dat$color, linetype=ext.vline.dat$linetype, size=ext.vline.dat$size)
  }
  
  if(!is.null(tick_x)) ggp <- ggp + scale_x_continuous(breaks=as.numeric(names(tick_x)), labels = as.character(tick_x))
  if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = as.character(tick_y))
  
  barinfo <- c(lgd.length1, lgd.length2)
  if(lgd.pos %in% c("top", "bottom")) barinfo <- c(lgd.length2, lgd.length1)
  
  ggp <- ggp + theme_opt +
    guides(fill = guide_colorbar(title="Count", frame.colour = "black", ticks.colour = "black",
                                 barwidth = barinfo[2], barheight = barinfo[1])) +
    labs(title=title, x=xlab, y=ylab) +
    coord_cartesian(xlim=xlim, ylim=ylim, expand=lim.expand)
  
  if(!marg.box){
    attr(ggp, "Rex") <- c("interactive", "REx_histogram2d")
    return(ggp)
  } else {
    xylim <- layer_scales(ggp)
    ggp_leg <- ggp_title <- NULL
    if(is.null(xlim)) xlim <- xylim$x$range$range
    if(is.null(ylim)) ylim <- xylim$y$range$range
    if(lgd.pos %in% c("top", "bottom", "right", "left")){
      ggp_leg <- get_legend(ggp)
      ggp <- ggp + theme(legend.position = "none")
    }
    
    if(!is.null(title)){
      get_title <- function (plot)
      {
        grobs <- plot_to_gtable(plot)$grobs
        legendIndex <- which(sapply(grobs, function(x) stringr::str_detect(x$name, "plot.title")))
        if (length(legendIndex) == 1) {
          legend <- grobs[[legendIndex]]
        }
        else {
          rexStop("Plot must contain a title")
        }
      }
      ggp_title <- get_title(ggp)
      ggp <- ggp + ggtitle(NULL)
    }
    
    marg.ratio <- c(1-marg.ratio, marg.ratio)
    ggp_up <- REx_boxplot(varname=varname1, dataset=dataset, flip=T, marg=T,
                          ylim=xlim, lim.expand=lim.expand, lgd.pos = "none",
                          color=color.marg)
    ggp_rt <- REx_boxplot(varname=varname2, dataset=dataset, marg=T,
                          ylim=ylim, lim.expand=lim.expand, lgd.pos = "none",
                          color=color.marg, xlab=xlab)
    
    lt_part <- plot_grid(ggp_up +
                           theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                                 panel.border = element_blank(), panel.grid = element_blank(),
                                 plot.margin = unit(c(8,0,0,8), "pt")),
                         ggp + theme(plot.margin = unit(c(1,1,8,8), "pt")),
                         nrow=2, ncol=1, align = "v", rel_heights = rev(marg.ratio))
    rt_part <- plot_grid(NULL,
                         ggp_rt +
                           theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                                 axis.ticks.x = element_line(color="white"), axis.title.x = element_text(color="white", size=axis.title.size),
                                 axis.text.x = element_text(color="white", size=axis.text.size),
                                 panel.border = element_blank(), panel.grid = element_blank(),
                                 plot.margin = unit(c(1,8,8,0), "pt")),
                         nrow=2, ncol=1, align = "v", rel_heights = rev(marg.ratio))
    ggp_tot <- plot_grid(lt_part, rt_part, nrow=1, ncol=2, align = "h", rel_widths = marg.ratio)
    
    if(!is.null(ggp_leg)) eval(parse(text=paste0("ggp_tot <- arrangeGrob(ggp_tot, ",lgd.pos,"=ggp_leg)")))
    if(!is.null(ggp_title)) ggp_tot <- arrangeGrob(ggp_tot, top=ggp_title)
    
    attr(ggp_tot, "Rex") <- c("REx_histogram2d")
    return(ggp_tot)
  }
}

##### 2d-density plot
REx_densityplot2d <- function(dataset,varname1, varname2,  by=NULL,
                              dpoint=FALSE, rug=FALSE, alpha=FALSE,
                              lgd.pos="right", lgd.back=F, lgd.title=NULL, lgd.text=NULL,
                              title=NULL, xlab=varname1, ylab=varname2,
                              pch=19, lty=1, psize=1.5, pstroke=0.5, lsize=0.5,
                              color.dot="black", color.line="red", color.group=NULL,
                              ext.hline=NULL, ext.vline=NULL,
                              tick_x=NULL, tick_y=NULL, grid.major=T, grid.minor=F,
                              marg.box=F, marg.ratio=0.2, color.marg="grey",
                              xlim=NULL, ylim=NULL, lim.expand=TRUE,
                              title.size=NULL, axis.title.size=NULL, axis.text.size=NULL,
                              global_opt=FALSE){
  load.pkg(c("ggplot2", "cowplot", "grid", "gridExtra"))
  
  if(global_opt){
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  suppressWarnings(x <- as.numeric(as.character(dataset[,varname1])))
  suppressWarnings(y <- as.numeric(as.character(dataset[,varname2])))
  if(sum(is.na(x))==length(x)) rexStop("There is no numeric values.")
  if(sum(is.na(y))==length(y)) rexStop("There is no numeric values.")
  dat <- data.frame(x,y)
  
  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  theme_opt <- theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2,
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  nrdREx <- function(x) {
    x <- as.numeric(na.exclude(x))
    r <- quantile(x, c(0.25, 0.75))
    if(r[1]==r[2]) {
      if(length(unique(x))==1) return(max(c(0.1, abs(x[1]/10))))
      if(length(unique(x))==2) r <- range(x)
      if(length(unique(x))>2) {
        r[1] <- ifelse(sum(x<r[1])>0,max(x[x<r[1]]),r[1])
        r[2] <- ifelse(sum(x>r[2])>0,min(x[x>r[2]]),r[2])
      }
    }
    h <- (r[2] - r[1])/1.34
    4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
  }
  
  if(is.null(by)){
    dat$color1 <- dat$size1 <- dat$linetype1 <- factor("Line_base")
    dat$color2 <- dat$size2 <- dat$shape2 <- factor("Point_base")
    dat$color3 <- factor("Rug_base")
    
    ggp <- ggplot()
    if(alpha){
      ggp <- ggp + stat_density2d(aes(x=x, y=y, color=color1, fill=color1, size=size1, linetype=linetype1, alpha=..level..)
                                  , data=dat, geom="polygon", show.legend = F, na.rm=T, h=sapply(dat, nrdREx)) 
    } else ggp <- ggp + stat_density2d(aes(x=x, y=y, color=color1, size=size1, linetype=linetype1), data=dat, show.legend = F, na.rm=T, h=sapply(dat, nrdREx))
    
    if(dpoint) ggp <- ggp + geom_point(aes(x=x, y=y, color=color2, shape=shape2, size=size2), data=dat, stroke=pstroke, na.rm=TRUE)
    if(rug) ggp <- ggp + geom_rug(aes(x=x, y=y, color=color3), data=dat, na.rm=TRUE)
    
    ggp <- ggp + scale_color_manual(values=c("Point_base"=color.dot, "Line_base"=color.line, "Rug_base"=color.line), guide="none", aesthetics = c("colour", "fill")) + 
      scale_size_manual(values=c("Point_base"=psize, "Line_base"=lsize), guide="none") + 
      scale_shape_manual(values=pch, guide="none") + 
      scale_linetype_manual(values=lty, guide="none") + 
      # scale_fill_manual(values=color.line, guide="none") +
      scale_alpha_continuous(range=c(0,0.3))
    
  } else {
    dat$group <- factor(dataset[,by], levels=unique(dataset[,by]))
    if(sum(is.na(dat$group))>0) dat <- dat[-which(is.na(dat$group)),]
    dat$groupl <- factor(paste0("Point_", dat$group), levels=unique(paste0("Point_", dat$group)))
    
    if(is.null(color.group)) color.group <- gg_color_hue2(length(levels(factor(dat$group))))
    
    if(length(pch)==1) pch <- rep(pch, length(levels(dat$group)))
    if(length(psize)==1) psize <- rep(psize, length(levels(dat$group)))
    if(length(pstroke)==1) pstroke <- rep(pstroke, length(levels(dat$group)))
    if(length(lty)==1) lty <- rep(lty, length(levels(dat$group)))
    if(length(lsize)==1) lsize <- rep(lsize, length(levels(dat$group)))
    
    dat$stroke <- dat$group
    levels(dat$stroke) <- pstroke
    dat$stroke <- as.numeric(as.character(dat$stroke))
    
    ggp <- ggplot(data=dat, aes(x=x, y=y, color=group))
    if(alpha) {
      ggp <- ggp + stat_density2d(aes(x=x, y=y, alpha = ..level.., fill=group, linetype=group, size=group), 
                                  data=dat, color="transparent", geom="polygon", show.legend = F, na.rm=TRUE, h=sapply(dat, nrdREx))
    } 
    ggp <- ggp + stat_density2d(aes(x=x, y=y, color=group, linetype=group, size=group), data=dat, na.rm=TRUE, h=sapply(dat, nrdREx))
    if(dpoint) ggp <- ggp + geom_point(aes(x=x, y=y, color=groupl, shape=group, size=groupl, stroke=stroke), data=dat, show.legend = F, na.rm=TRUE)
    if(rug) ggp <- ggp + geom_rug(aes(x=x, y=y, color=group), data=dat, show.legend = F)
    
    plsize <- c(lsize, psize)
    color.group2 <- rep(color.group, 2)
    names(plsize) <- names(color.group2) <- c(levels(dat$group), levels(dat$groupl))
    ggp <- ggp + scale_color_manual(values=color.group2, breaks=levels(dat$group), guide=guide_legend(by), aesthetics = c("colour", "fill")) + 
      scale_size_manual(values=plsize, breaks=levels(dat$group), guide=guide_legend(by)) + 
      scale_shape_manual(values=pch, guide="none") + 
      scale_linetype_manual(values=lty, guide=guide_legend(by)) +
      # scale_fill_manual(values=color.group, guide=guide_legend(by)) +
      scale_alpha_continuous(range=c(0,0.3))
  }
  # browser()
  xylim <- layer_scales(ggp)
  # xstep <- diff(xylim$x$range$range)/10
  # ystep <- diff(xylim$y$range$range)/10
  #
  # while(TRUE){
  #   ggptmp <- ggp + xlim(layer_scales(ggp)$x$range$range-c(xstep, 0))
  #   if(abs(layer_scales(ggp)$x$range$range[1] - layer_scales(ggptmp)$x$range$range[1]) < xstep/100) break
  #   else ggp <- ggptmp
  # }
  #
  # while(TRUE){
  #   ggptmp <- ggp + xlim(layer_scales(ggp)$x$range$range+c(0, xstep))
  #   if(abs(layer_scales(ggp)$x$range$range[2] - layer_scales(ggptmp)$x$range$range[2]) < xstep/100) break
  #   else ggp <- ggptmp
  # }
  #
  # while(TRUE){
  #   ggptmp <- ggp + ylim(layer_scales(ggp)$y$range$range-c(ystep, 0))
  #   if(abs(layer_scales(ggp)$y$range$range[1] - layer_scales(ggptmp)$y$range$range[1]) < ystep/100) break
  #   else ggp <- ggptmp
  # }
  #
  # while(TRUE){
  #   ggptmp <- ggp + ylim(layer_scales(ggp)$y$range$range+c(0, ystep))
  #   if(abs(layer_scales(ggp)$y$range$range[2] - layer_scales(ggptmp)$y$range$range[2]) < ystep/100) break
  #   else ggp <- ggptmp
  # }
  
  xlim2 <- xylim$x$range$range
  ylim2 <- xylim$y$range$range
  xlim2a <- xlim2 + c(-1,1)*diff(xlim2)
  ylim2a <- ylim2 + c(-1,1)*diff(ylim2)
  
  if(!is.null(ext.hline)){
    ext.hline.dat <- list()
    for(j in 1:length(ext.hline)){
      ext.hline.dat[[j]] <- data.frame(yintercept=as.numeric(strsplit(ext.hline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.hline[[j]][2]), size=as.numeric(ext.hline[[j]][3]), color=ext.hline[[j]][4])
    }
    ext.hline.dat <- do.call(rbind, ext.hline.dat)
    ggp <- ggp + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
    
    ylim2a <- range(c(ylim2a, ext.hline.dat$yintercept))
    ylim2 <- range(c(ylim2, ext.hline.dat$yintercept))
  }
  if(!is.null(ext.vline)){
    ext.vline.dat <- list()
    for(j in 1:length(ext.vline)){
      ext.vline.dat[[j]] <- data.frame(xintercept=as.numeric(strsplit(ext.vline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.vline[[j]][2]), size=as.numeric(ext.vline[[j]][3]), color=ext.vline[[j]][4])
    }
    ext.vline.dat <- do.call(rbind, ext.vline.dat)
    ggp <- ggp + geom_vline(aes(xintercept = xintercept), ext.vline.dat, color=ext.vline.dat$color, linetype=ext.vline.dat$linetype, size=ext.vline.dat$size)
    
    xlim2a <- range(c(xlim2a, ext.vline.dat$xintercept))
    xlim2 <- range(c(xlim2, ext.vline.dat$xintercept))
  }
  # browser()
  
  ggp <- ggp + xlim(xlim2a) + ylim(ylim2a)
  if(!is.null(tick_x)) ggp <- ggp + scale_x_continuous(breaks=as.numeric(names(tick_x)), labels = as.character(tick_x), limits = xlim2a)
  if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = as.character(tick_y), limits = ylim2a)
  
  if(is.null(xlim)) xlim <- xlim2
  if(is.null(ylim)) ylim <- ylim2
  
  ggp <- ggp +
    coord_cartesian(xlim=xlim, ylim=ylim, expand=lim.expand) +
    labs(title=title, x=xlab, y=ylab) +
    theme_opt
  
  if(!marg.box){
    attr(ggp, "Rex") <- c("interactive", "REx_densityplot2d")
    return(ggp)
  } else {
    xylim <- layer_scales(ggp)
    ggp_leg <- ggp_title <- NULL
    if(is.null(xlim)) xlim <- xylim$x$range$range
    if(is.null(ylim)) ylim <- xylim$y$range$range
    if(lgd.pos %in% c("top", "bottom", "right", "left") & !is.null(by)){
      ggp_leg <- get_legend(ggp)
      ggp <- ggp + theme(legend.position = "none")
    }
    
    if(!is.null(title)){
      get_title <- function (plot)
      {
        grobs <- plot_to_gtable(plot)$grobs
        legendIndex <- which(sapply(grobs, function(x) stringr::str_detect(x$name, "plot.title")))
        if (length(legendIndex) == 1) {
          legend <- grobs[[legendIndex]]
        }
        else {
          rexStop("Plot must contain a title")
        }
      }
      ggp_title <- get_title(ggp)
      ggp <- ggp + ggtitle(NULL)
    }
    
    marg.ratio <- c(1-marg.ratio, marg.ratio)
    ggp_up <- REx_boxplot(varname=varname1, dataset=dataset, flip=T, marg=T,
                          ylim=xlim, lim.expand=lim.expand, lgd.pos = "none",
                          color=color.marg)
    ggp_rt <- REx_boxplot(varname=varname2, dataset=dataset, marg=T,
                          ylim=ylim, lim.expand=lim.expand, lgd.pos = "none",
                          color=color.marg, xlab=xlab)
    
    lt_part <- plot_grid(ggp_up +
                           theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                                 panel.border = element_blank(), panel.grid = element_blank(),
                                 plot.margin = unit(c(8,0,0,8), "pt")),
                         ggp + theme(plot.margin = unit(c(1,1,8,8), "pt")),
                         nrow=2, ncol=1, align = "v", rel_heights = rev(marg.ratio))
    rt_part <- plot_grid(NULL,
                         ggp_rt +
                           theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                                 axis.ticks.x = element_line(color="white"), axis.title.x = element_text(color="white", size=axis.title.size),
                                 axis.text.x = element_text(color="white", size=axis.text.size),
                                 panel.border = element_blank(), panel.grid = element_blank(),
                                 plot.margin = unit(c(1,8,8,0), "pt")),
                         nrow=2, ncol=1, align = "v", rel_heights = rev(marg.ratio))
    ggp_tot <- plot_grid(lt_part, rt_part, nrow=1, ncol=2, align = "h", rel_widths = marg.ratio)
    
    if(!is.null(ggp_leg)) eval(parse(text=paste0("ggp_tot <- arrangeGrob(ggp_tot, ",lgd.pos,"=ggp_leg)")))
    if(!is.null(ggp_title)) ggp_tot <- arrangeGrob(ggp_tot, top=ggp_title)
    
    attr(ggp_tot, "Rex") <- c("REx_densityplot2d")
    return(ggp_tot)
  }
}

##### Trend plot
REx_trendplot <- function(dataset, var_x, var_y, var_col=NULL, var_line=NULL, var_area=NULL, var_smooth=NULL,
                          lgd.pos="right", lgd.back=F, lgd.title=NULL, lgd.text=NULL, lgd.name="Variables",
                          title=NULL, xlab=var_x, ylab="Values", color.group=NULL, color.group_sm=NULL,
                          ylim=NULL, lim.expand=TRUE, tick_y=NULL, grid.major=F, grid.minor=F, ext.hline=NULL,
                          type_col="stack", alpha=0.3,
                          lty=1, lsize=1, lsize_sm=1.5,
                          pch=19, psize=1.5, pstroke=0.5,
                          title.size=NULL, axis.title.size=NULL, axis.text.size=NULL,
                          global_opt=FALSE){
  load.pkg(c("ggplot2", "reshape2"))
  
  if(global_opt){
    # if(!is.na(ggGraphOpt$color)) color <- ggGraphOpt$color
    if(!is.na(ggGraphOpt$title.size)) title.size <- ggGraphOpt$title.size
    if(!is.na(ggGraphOpt$axis.title.size)) axis.title.size <- ggGraphOpt$axis.title.size
    if(!is.na(ggGraphOpt$axis.text.size)) axis.text.size <- ggGraphOpt$axis.text.size
  }
  
  if(is.numeric(lgd.pos)){
    lgd.just <- lgd.pos <- lgd.pos/100
    if(!lgd.back) lgd.just <- 1.02*lgd.just-0.01
  }
  else lgd.just <- NULL
  if(lgd.back) lgd.back2 <- NULL
  else lgd.back2 <- element_rect()
  
  gl <- guide_legend(lgd.name)
  
  theme_opt <- theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = title.size),
          legend.position = lgd.pos, legend.justification = lgd.just, legend.background = lgd.back2, 
          legend.title = element_text(size = lgd.title), legend.text = element_text(size = lgd.text),
          axis.text = element_text(size = axis.text.size), axis.title = element_text(size = axis.title.size))
  if(!(grid.major)) theme_opt <- theme_opt + theme(panel.grid.major = element_blank())
  if(!(grid.minor)) theme_opt <- theme_opt + theme(panel.grid.minor = element_blank())
  
  if(sum(duplicated(dataset[,var_x])) > 0) rexStop("X-변수에 중복된 값이 있습니다.")
  
  if(length(pch)==1) pch <- rep(pch, length(var_y))
  if(length(psize)==1) psize <- rep(psize, length(var_y))
  if(length(pstroke)==1) pstroke <- rep(pstroke, length(var_y))
  if(length(lty)==1) lty <- rep(lty, length(var_y))
  if(length(lsize)==1) lsize <- rep(lsize, length(var_y))
  if(length(lsize_sm)==1) lsize_sm <- rep(lsize_sm, length(var_y))
  if(is.null(color.group)) color.group <- gg_color_hue2(length(var_y))
  color.group2 <- color.group
  color.group2[var_y %in% var_line] <- "transparent"
  
  dataset <- dataset[,c(var_x, var_y)]
  dat1 <- melt(dataset, id.vars=var_x)
  dat1[,var_x] <- factor(dat1[,var_x], levels=dataset[,var_x])
  dat1$variable <- factor(dat1$variable, levels = var_y)
  
  suppressWarnings(dat1$value <- as.numeric(as.character(dat1$value)))
  if(sum(is.na(dat1$value))==length(dat1$value)) rexStop("There is no numeric values.")
  
  dat2 <- dat1
  colnames(dat2)[1] <- "VarX"
  v_area_fill <- v_point_color <- v_smooth_color <- character(0)
  v_point_size <- v_smooth_size <- v_smooth_linetype <- numeric(0)
  
  if(!is.null(var_col)){
    dat2$value_col <- 0
    dat2$value_col[dat2$variable %in% var_col] <- dat2$value[dat2$variable %in% var_col]
    dat2_col <- dat2[which(dat2$variable %in% var_col),]
    dat2_col$variable <- factor(as.character(dat2_col$variable), levels=unique(dat2_col$variable))
  }
  if(!is.null(var_line)){
    dat2$value_line <- NA
    dat2$value_line[dat2$variable %in% var_line] <- dat2$value[dat2$variable %in% var_line]
    dat2$group_Point <- factor(paste0("Point_",dat2$variable), levels=paste0("Point_",var_y))
    
    v_point_color <- color.group
    v_point_size <- psize
    names(v_point_color) <- names(v_point_size) <- paste0("Point_",var_y)
    v_point_color[pch==32|!var_y %in% var_line] <- "transparent"
    v_point_size[pch==32|!var_y %in% var_line] <- 0
    
    if(!is.null(var_smooth)){
      dat2$value_smooth <- NA
      dat2$value_smooth[dat2$variable %in% var_smooth] <- dat2$value[dat2$variable %in% var_smooth]
      if(is.null(color.group_sm)) color.group_sm <- color.group
      dat2$group_Smooth <- factor(paste0("Smooth_",dat2$variable), levels=paste0("Smooth_",var_y))
      
      v_smooth_color <- color.group_sm
      v_smooth_size <- lsize_sm
      v_smooth_linetype <- rep(1, length(var_y))
      names(v_smooth_color) <- names(v_smooth_size) <- names(v_smooth_linetype) <- paste0("Smooth_",var_y)
      v_smooth_color[!var_y %in% var_smooth] <- "transparent"
      v_smooth_size[!var_y %in% var_smooth] <- 0
      v_smooth_linetype[!var_y %in% var_smooth] <- 0
      
    }
    if(!is.null(var_area)){
      dat2$value_area <- NA
      dat2$value_area[dat2$variable %in% var_area] <- dat2$value[dat2$variable %in% var_area]
      dat2$group_Area <- factor(paste0("Area_",dat2$variable), levels=paste0("Area_",var_y))
      
      v_area_fill <- color.group
      names(v_area_fill) <- paste0("Area_",var_y)
      v_area_fill[!var_y %in% var_area] <- "transparent"
    }
  }
  
  ## fill : column (if not >>> transparent)
  ## color : line, point, smooth (lgd>>line)
  ## size : point, line, smooth (lgd>>line)
  ## shape : point
  ## linetype : line, smooth (lgd>>line)
  
  ggp <- ggplot() 
  if(!is.null(var_col)){
    if(type_col=="stack") ggp <- ggp + geom_col(aes(x=VarX, y=value_col, fill=variable), data=dat2, position = "stack", color="transparent")
    if(type_col=="par") ggp <- ggp + geom_col(aes(x=VarX, y=0, fill=variable), data=dat2, position = "stack", color="transparent") + 
        geom_col(aes(x=VarX, y=value_col, fill=variable), data=dat2_col, position="dodge", color="transparent", show.legend = F, inherit.aes = F)
  }
  if(!is.null(var_line)){
    if(!is.null(var_area)) ggp <- ggp + geom_ribbon(aes(x=VarX, ymax=value_area, ymin=0, fill=group_Area, group=group_Area), data=dat2, alpha=alpha, color="transparent", show.legend = F)
    ggp <- ggp + 
      geom_line(aes(x=VarX, y=value_line, color=variable, size=variable, linetype=variable, group=variable), data=dat2) +
      geom_point(aes(x=VarX, y=value_line, color=group_Point, size=group_Point, shape=variable), data=dat2, stroke=rep(pstroke, each=nrow(dataset)), show.legend = F) 
    if(!is.null(var_smooth)) ggp <- ggp + geom_smooth(aes(x=VarX, y=value_smooth, color=group_Smooth, size=group_Smooth, linetype=group_Smooth, group=group_Smooth), data=dat2, method="auto", se=F, show.legend = F, na.rm = T)
  } 
  
  v_fill <- color.group2
  v_color <- color.group
  v_size <- lsize
  v_linetype <- lty
  names(v_fill) <- names(v_color) <- names(v_size) <- names(v_linetype) <- var_y
  v_color[!var_y %in% var_line] <- "transparent"
  v_size[!var_y %in% var_line] <- 0
  v_linetype[!var_y %in% var_line] <- 0
  pch[!var_y %in% var_line] <- 32
  v_fill <- c(v_fill, v_area_fill)
  v_color <- c(v_color, v_point_color, v_smooth_color)
  v_size <- c(v_size, v_smooth_size, v_point_size)
  v_linetype <- c(v_linetype, v_smooth_linetype)
  
  ggp <- ggp + geom_hline(yintercept = 0) + 
    scale_color_manual(values = v_color, breaks=var_y, guide=gl) + 
    scale_fill_manual(values = v_fill, breaks=var_y, guide=gl) + 
    scale_shape_manual(values = pch, guide=gl) + 
    scale_size_manual(values = v_size, breaks=var_y, guide=gl) +
    scale_linetype_manual(values = v_linetype, breaks=var_y, guide=gl)
  
  if(!is.null(ext.hline)){
    ext.hline.dat <- list()
    for(j in 1:length(ext.hline)){
      ext.hline.dat[[j]] <- data.frame(yintercept=as.numeric(strsplit(ext.hline[[j]][1],",")[[1]]),
                                       linetype=as.numeric(ext.hline[[j]][2]), size=as.numeric(ext.hline[[j]][3]), color=ext.hline[[j]][4])
    }
    ext.hline.dat <- do.call(rbind, ext.hline.dat)
    ggp <- ggp + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
  }
  
  if(!is.null(tick_y)) ggp <- ggp + scale_y_continuous(breaks=as.numeric(names(tick_y)), labels = as.character(tick_y))
  
  ggp <- ggp + theme_opt + 
    labs(title=title, x=xlab, y=ylab) +
    coord_cartesian(ylim=ylim, expand=lim.expand)
  
  attr(ggp, "Rex") <- c("interactive", "REx_trendplot")
  return(ggp)
}


###### tmpcode >> nomogram for lm, glm

REx_nomogram <- function(mod, newdat, compact=T,
                         text.size.y = 10, text.size.pinfo = 7.5, text.size.x = 9,
                         Points="Points", Total_Points="Total Points", Linear_Predictor="Linear Predictor"){
  
  ## mod: model object from lm function
  ## newdat: new data (data.frame class)
  ## compact: if true, variable axis with selected level in new data (dummy variables) will be represented.
  ## text.size: text size of y axis text (.y), information of points etc. (.pinfo), x axis text (.x)
  ## Points,...: can change y axis text by options
  
  library(ggplot2)
  library(ggrepel)
  library(stringr)
  
  text.size.pinfo <- text.size.pinfo * 0.3514598
  text.size.x <- text.size.x * 0.3514598
  
  predict2 <- function(mm, nn) suppressWarnings(predict(mm, nn))
  pretty2 <- function(xx, n){
    i=2
    while(TRUE){
      res <- pretty(xx, n=i)
      if(length(res) >= 8 | i == 100) break
      i <- i+1
    }
    return(res)
  }
  format2 <- function(xx){
    if(is.factor(xx)) return(format(xx, trim=T, justify="none"))
    else return(formatC(xx, format="fg"))
  }
  
  ## new data
  newdat <- data.frame(newdat[,colnames(mod$model[-1])])
  colnames(newdat) <- colnames(mod$model[-1])
  for(i in which(colnames(newdat) %in% names(mod$xlevels))) newdat[,i] <- as.character(newdat[,i])
  
  ## prediction test
  if(sum(is.na(mod$coefficients))>0) warning("[Rex] Model from a rank-deficient fit: prediction may be misleading")
  test <- tryCatch(predict2(mod, newdat), error=function(e) e$message)
  if(is.character(test)) stop(paste0("[Rex] Inadequate newdata: ", test))
  
  combdat <- rbind(mod$model[-1], newdat)
  factor_info <- mod$xlevels
  modmat <- as.data.frame(attr(mod$terms, "factors"))
  
  ## conti. variables
  conti_varn <- rownames(modmat)[-1][!(rownames(modmat)[-1] %in% names(factor_info))]
  conti_varn_list <- list()
  if(length(conti_varn)>1){
    conti_varn2 <- t(unique(t(modmat[conti_varn,])))
    conti_varn2 <- conti_varn2[,colSums(conti_varn2)>0]
    for(i in 1:ncol(conti_varn2)){
      modmat_tmp <- modmat[apply(modmat[conti_varn,] == conti_varn2[,i], 2, function(x) as.logical(prod(x)))]
      conti_varn_list[[i]] <- rownames(modmat)[rowSums(modmat_tmp)>0]
    }
  }
  if(length(conti_varn)==1) conti_varn_list[[1]] <- rownames(modmat)[rowSums(modmat[which(modmat[conti_varn,]>=1)])>0]
  if(length(conti_varn)==0) conti_varn_list <- NULL
  
  ## dummy variables
  if(length(factor_info)==0) dummy_varn_list <- NULL
  else {
    dummy_varn <- names(factor_info)
    modmat_tmp <- modmat[!rownames(modmat) %in% dummy_varn,]
    if(is.data.frame(modmat_tmp)) modmat_tmp <- modmat[dummy_varn, colSums(modmat_tmp)==0]
    else modmat_tmp <- modmat[dummy_varn, sum(modmat_tmp)==0]
    if(is.integer(modmat_tmp)) {
      modmat_tmp <- data.frame(modmat_tmp)
      rownames(modmat_tmp) <- colnames(modmat_tmp) <- dummy_varn
    }
    modmat_tmp[modmat_tmp>=1] <- 1

    dummy_varn_list <- list()
    modmat_tmp_cs <- colSums(modmat_tmp)
    suppressWarnings(modmat_tmp_cs_max <- max(modmat_tmp_cs)) 
    if(modmat_tmp_cs_max<=0) dummy_varn_list <- NULL
    else {
      if(modmat_tmp_cs_max==1){
        modmat_tmp_csn <- names(modmat_tmp_cs)[modmat_tmp_cs==1]
        for(i in 1:length(modmat_tmp_csn)) dummy_varn_list[[i]] <- modmat_tmp_csn[i]
      } else {
        modmat_tmp2 <- modmat_tmp[modmat_tmp_cs>1]
        while(max(rowSums(modmat_tmp2))>1){
          targ <- which(modmat_tmp2[which(rowSums(modmat_tmp2)>1)[1],]==1)
          targ2 <- data.frame(as.integer(as.logical(rowSums(modmat_tmp2[,targ]))))
          colnames(targ2) <- paste0(rownames(modmat_tmp2)[targ2==1], collapse = ":")
          modmat_tmp2 <- cbind(targ2, modmat_tmp2)
          modmat_tmp2 <- modmat_tmp2[-(targ+1)]
        }
        
        for(i in 1:ncol(modmat_tmp2)) dummy_varn_list[[i]] <- rownames(modmat_tmp)[modmat_tmp2[,i]==1]
        targ <- rownames(modmat_tmp)[rowSums(modmat_tmp2)==0]
        if(length(targ)>0) for(i in 1:length(targ)) dummy_varn_list[[i+ncol(modmat_tmp2)]] <- targ[i]
      }
    }
  }

  ## variable list, coefficients, data breaks
  modmatfunc <- function(modmatrix, dat, pret=T){
    depvar_list <- c()
    res_coef <- c()
    res <- list()
    
    modmat_tmp_func <- function(type=2, dv){
      modmat_tmp <- expand.grid(factor_info[dv[dv %in% names(factor_info)]])
      for(kn in rownames(modmatrix)[-1]){
        if(!kn %in% colnames(modmat_tmp)){
          if(kn %in% names(factor_info)) modmat_tmp[,kn] <- factor_info[[kn]][1]
          else modmat_tmp[,kn] <- 0
        }
      }
      if(type==1) return(modmat_tmp[rownames(modmatrix)[-1]])
      if(type==2) return(modmat_tmp[dv])
    }

    if(!is.null(dummy_varn_list)){
      for(k in 1:length(dummy_varn_list)){
        dvlk <- dummy_varn_list[[k]]
        
        ## depvar_list
        depvar_tmp <- list()
        fac1 <- names(which.max(sapply(factor_info[dvlk], length))[1])
        for(ktmp in 1:length(dvlk)){
          if(dvlk[ktmp] == fac1) depvar_tmp[[ktmp]] <- dvlk[ktmp]
          else depvar_tmp[[ktmp]] <- paste0(dvlk[ktmp], factor_info[[dvlk[ktmp]]])
        }
        depvar_tmp <- apply(expand.grid(depvar_tmp), 1, paste0, collapse=":")
        depvar_list <- c(depvar_list, depvar_tmp)
        
        ## res_coef
        coef_tmp <- rep(1, length(depvar_tmp))
        names(coef_tmp) <- depvar_tmp
        res_coef <- c(res_coef, coef_tmp)
        
        ## res
        if(length(dvlk)==1){
          coef_tmp <- predict2(mod, modmat_tmp_func(type=1, dv=dvlk)) - mod$coefficients[1]
          names(coef_tmp) <- factor_info[[dvlk]]
          
          sdat <- rep(NA, nrow(dat))
          for(fn in factor_info[[dvlk]]) sdat[dat[,dvlk]==fn] <- coef_tmp[which(factor_info[[dvlk]]==fn)]
          res[[dvlk]] <- sdat
          if(pret) res[[dvlk]] <- coef_tmp
        } else {
          fac_comb <- modmat_tmp_func(type=1, dv=dvlk[dvlk != fac1])
          for(i in 1:nrow(fac_comb)){
            suppressWarnings(fac_comb2 <- data.frame(x=factor_info[[fac1]], fac_comb[i,which(colnames(fac_comb) != fac1)]))
            colnames(fac_comb2) <- c(fac1, colnames(fac_comb)[which(colnames(fac_comb) != fac1)])
            
            coef_tmp <- predict2(mod, fac_comb2) - mod$coefficients[1]
            names(coef_tmp) <- factor_info[[fac1]]
            
            sdat <- dat
            for(j in which(colnames(fac_comb) %in% dvlk[dvlk != fac1])) if(nrow(sdat)>0) sdat <- sdat[which(sdat[,colnames(fac_comb)[j]]==fac_comb[i,j]),]
            if(nrow(sdat)>0){
              sdat2 <- rep(NA, nrow(sdat))
              for(fn in factor_info[[fac1]]) sdat2[sdat[,fac1]==fn] <- coef_tmp[which(factor_info[[fac1]]==fn)]
              res[[depvar_tmp[i]]] <- sdat2
            } else res[[depvar_tmp[i]]] <- integer(0)
            if(pret) res[[depvar_tmp[i]]] <- coef_tmp
          }
        }
      }
    }
    
    if(!is.null(conti_varn_list)){
      for(k in 1:length(conti_varn_list)){
        cvlk <- conti_varn_list[[k]]
        
        if(sum(cvlk %in% names(factor_info))==0){
          depvar_tmp <- paste0(cvlk, collapse = ":")
          depvar_list <- c(depvar_list, depvar_tmp)
          res_coef <- c(res_coef, mod$coefficients[depvar_tmp])
          res[[depvar_tmp]] <- as.numeric(apply(dat[cvlk],1,prod))
          if(pret) res[[depvar_tmp]] <- pretty2(res[[depvar_tmp]], n=10)
        } else {
          ## depvar_list
          depvar_tmp2 <- depvar_tmp <- list()
          for(ktmp in 1:length(cvlk)){
            if(cvlk[ktmp] %in% names(factor_info)) depvar_tmp[[ktmp]] <- paste0(cvlk[ktmp], factor_info[[cvlk[ktmp]]])
            else {
              depvar_tmp[[ktmp]] <- cvlk[ktmp]
              depvar_tmp2[[cvlk[ktmp]]] <- 0:1
            }
          }
          depvar_tmp <- apply(expand.grid(depvar_tmp), 1, paste0, collapse=":")
          depvar_list <- c(depvar_list, depvar_tmp)

          ## res_coef
          coef_tmp <- predict2(mod, modmat_tmp_func(type=1, dv=cvlk)) ### all cont variable=0
          depvar_tmp2 <- expand.grid(depvar_tmp2)
          pm1 <- (-1)^ncol(depvar_tmp2)
          coef_tmp <- coef_tmp * pm1
          
          for(i in 1:ncol(depvar_tmp2)){
            pm1 <- pm1*(-1)
            depvar_tmp2i <- depvar_tmp2[which(rowSums(depvar_tmp2)==i),]
            if(ncol(depvar_tmp2)==1){
              modmat_tmp_i <- modmat_tmp_func(type=1, dv=cvlk)
              modmat_tmp_i[,colnames(depvar_tmp2)] <- depvar_tmp2i
              coef_tmp <- rbind(coef_tmp, predict2(mod, modmat_tmp_i)*pm1)
            } else {
              for(ii in 1:nrow(depvar_tmp2i)){
                modmat_tmp_i <- modmat_tmp_func(type=1, dv=cvlk)
                for(ij in colnames(depvar_tmp2)) modmat_tmp_i[,ij] <- depvar_tmp2i[ii,ij]
                coef_tmp <- rbind(coef_tmp, predict2(mod, modmat_tmp_i)*pm1)
              }
            }
          }
          
          coef_tmp <- colSums(coef_tmp)
          names(coef_tmp) <- depvar_tmp
          res_coef <- c(res_coef, coef_tmp)
          
          ## res
          fac_comb <- modmat_tmp_func(dv=cvlk)
          for(i in 1:nrow(fac_comb)){
            sdat <- dat
            for(j in which(colnames(fac_comb) %in% names(factor_info))) if(nrow(sdat)>0) sdat <- sdat[which(sdat[,colnames(fac_comb)[j]]==fac_comb[i,j]),]
            res[[depvar_tmp[i]]] <- as.numeric(apply(sdat[colnames(depvar_tmp2)],1,prod))
            if(pret) res[[depvar_tmp[i]]] <- pretty2(res[[depvar_tmp[i]]], n=10)
          }
        }
      }
    }
    
    for(res_n in names(res)[sapply(res, length)==0]) res[[res_n]] <- NA
    return(list(depvar_list, res_coef, res))
  }
  newdat2 <- data.frame(modmatfunc(modmat, newdat, pret=F)[[3]])
  pretbreak <- modmatfunc(modmat, combdat, pret=T)

  ## scale calculation
  modscalefunc <- function(pretbreakdat){

    modcoef <- pretbreakdat[[2]]
    pretbreakdat1 <- pretbreakdat[[3]]
    res <- c()
    
    if(compact) pbd_name <- which(!sapply(newdat2,is.na))
    else pbd_name <- 1:length(pretbreakdat1)
    
    for(k in pbd_name){
      k <- names(pretbreakdat1)[k]
      res <- c(res, range(pretbreakdat1[[k]])*modcoef[k])
    }
    
    return(c(diff(range(res)),-(min(res)))) ### total effect size, point zero shifting
  }
  basedat <- modscalefunc(pretbreak)
  
  ## scaling for nomogram
  graphdatfunc <- function(pretbreakdat){
    modcoef <- pretbreakdat[[2]]
    pretbreakdat1 <- pretbreakdat[[3]]
    if(compact){
      modcoef <- modcoef[!sapply(newdat2,is.na)]
      pretbreakdat1 <- pretbreakdat1[!sapply(newdat2,is.na)]
    }
    res2 <- list()

    for(i in 1:length(pretbreakdat1)){
      iname <- names(pretbreakdat1)[i]
      pretdat <- pretbreakdat1[[iname]]
      coeftmp <- modcoef[iname]
      dpoint <- (basedat[2]+pretdat*coeftmp)/basedat[1]*100
      
      if(is.null(names(pretdat))){
        res2[[i]] <- data.frame(x=dpoint,
                                y=-i,
                                label=pretdat)
      } else {
        res2[[i]] <- data.frame(x=dpoint,
                                y=-i,
                                label2=pretdat,
                                label=names(pretdat))
      }
    }
    return(res2)
  }
  coefdat <- graphdatfunc(pretbreak)
  
  ## newdat for nomogram
  newdatfunc <- function(i){
    if(compact) newdat2 <- newdat2[!sapply(newdat2,is.na)]
    newd <- newdat2[,i]
    if(is.na(newd)) return(NULL)
    else {
      coefd <- coefdat[[i]]
      if(ncol(coefd)==4){
        targ <- coefd$x[coefd$label2==newd]
        newd <- as.character(coefd$label[coefd$label2==newd])
      }
      else targ <- predict2(lm(x~label, coefd), data.frame(label=newd))
      
      return(data.frame(x=targ, y=-i, xend=targ, yend=0, label=newd, stringsAsFactors = F))
    }
  }
  newdat3 <- do.call(rbind,lapply(1:length(coefdat), newdatfunc))

  ## point, total point, linear predictor axis
  totaldatfunc <- function(model){
    points_dat <- data.frame(x=seq(0,100,10), y=0, label=seq(0,100,10))
    totalpoints_dat <- data.frame(x=NA, y=-(length(coefdat)+1), 
                                  label=pretty2(c(0, 100, sum(sapply(coefdat[-newdat3$y], function(x) max(x$x)))), n=10))
    totalpoints_dat$x <- totalpoints_dat$label/max(totalpoints_dat$label)*100

    locshift <- sum(newdat3$x)*basedat[1]/100 - predict2(mod, newdat)

    dep_dat <- mod$model
    if(compact & length(factor_info)>0 & !is.null(conti_varn_list)) for(fn in names(factor_info)) dep_dat <- dep_dat[which(dep_dat[,fn]==as.character(newdat[,fn])),]
    linearpred_dat <- data.frame(x=NA, y=-(length(coefdat)+2),
                                 label=pretty2(c(predict2(mod)[rownames(dep_dat)], predict2(mod, newdat)), n=10))
    linearpred_dat$x <- ((linearpred_dat$label+locshift)/basedat[1]*100)/max(totalpoints_dat$label)*100
    
    return(list(points_dat, totalpoints_dat, linearpred_dat))
  }
  graphdat <- totaldatfunc(mod)
  
  ## ggplot function for each axis data
  geom_nomogram <- function(dat, ggp){
    dat1 <- data.frame(x=min(dat$x), xend=max(dat$x), y=dat$y[1])
    dat$yend <- dat$y-0.05
    
    res <- ggp + geom_segment(mapping=aes(x=x,xend=xend,y=y,yend=y), data=dat1) + 
      geom_segment(mapping=aes(x=x,xend=x,y=y,yend=yend), data=dat) + 
      geom_text(mapping=aes(x=x,y=yend,label=format2(label)), data=dat, vjust=1, nudge_y = -0.03, size=text.size.x, check_overlap = T)
    
    return(res)
  }
  
  ggp <- ggplot()
  for(i in 1:length(coefdat)) ggp <- geom_nomogram(coefdat[[i]], ggp)
  for(i in 1:length(graphdat)) ggp <- geom_nomogram(graphdat[[i]], ggp)
  
  ## ggplot function for newdat
  geom_nomogram2 <- function(dat, ggp){
    cumx <- cumsum(dat$x)/rev(graphdat[[2]]$label)[1]*100
    dat1 <- data.frame(x=c(0, cumx[-length(cumx)]), xend=cumx, y=graphdat[[2]][1,2])
    dat2 <- data.frame(x=cumx[length(cumx)], yend=graphdat[[2]][1,2], y=graphdat[[3]][1,2],
                       label=predict2(mod, newdat))
    
    res <- ggp + 
      geom_segment(mapping=aes(x=x,xend=xend,y=y,yend=y), data=dat1, color=gg_color_hue2(length(cumx)), size=1.5) +
      
      geom_point(mapping=aes(x=x, y=0), data=dat, shape=4, stroke=1.5, color=gg_color_hue2(length(cumx))) +
      geom_segment(mapping=aes(x=x,xend=xend,y=y,yend=yend), data=dat, color=gg_color_hue2(length(cumx)), linetype=2) +
      geom_text(mapping=aes(x=x, y=y, label=format(label, trim = T)), data=dat, color=gg_color_hue2(length(cumx)), size=text.size.pinfo, vjust=0, hjust=0, nudge_x = 0.15, nudge_y = 0.03) +
      geom_text_repel(mapping=aes(x=x, y=0, label=round(x,1)), data=dat, color=gg_color_hue2(length(cumx)), size=text.size.pinfo, vjust=0, hjust=0, nudge_x = 0.5, nudge_y = 0.02, ylim=c(0,NA)) +
      
      geom_segment(mapping=aes(x=x,xend=x,y=y,yend=yend), data=dat2, linetype=2, color="black") +
      geom_text_repel(mapping=aes(x=x, y=y, label=format2(label)), data=dat2, color="black", size=text.size.pinfo, vjust=0, hjust=0, nudge_x = 0.5, nudge_y = 0.02, ylim=c(dat2$y[1],NA)) +
      geom_point(mapping=aes(x=x, y=y), data=dat2, shape=4, stroke=1.5)
    
    return(res)
  }
  
  ### Using Analysis modules...
  gg_color_hue2 <- function(n) {
    hues = seq(6.330885, 6.330885+360, length = n + 1)
    hcl(h = hues, l = 51.19696, c = 144.4467)[1:n]
  }
  
  ggp <- geom_nomogram2(newdat3, ggp)
  if(compact) lab_y <- c(Points, names(pretbreak[[3]])[!sapply(newdat2,is.na)], Total_Points, Linear_Predictor)
  else lab_y <- c(Points, names(pretbreak[[3]]), Total_Points, Linear_Predictor)
  ggp <- ggp + scale_y_continuous(breaks=rev(-(1:length(lab_y))+1), labels=rev(lab_y)) + 
    theme_minimal() + theme(axis.title = element_blank(), panel.grid = element_blank(), 
                            axis.text.x = element_blank(), axis.text.y = element_text(size=text.size.y))
  
  return(ggp)
}


REx_nomogram_glm <- function(mod, newdat, compact=T, 
						     text.size.y = 10, text.size.pinfo = 7.5, text.size.x = 9,
                             Points="Points", Total_Points="Total Points", Linear_Predictor="Linear Predictor", Response_Scale="Response Scale"){
  
  ## mod: model object from glm function
  ## newdat: new data (data.frame class)
  ## compact: if true, variable axis with selected level in new data (dummy variables) will be represented.
  ## text.size: text size of y axis text (.y), information of points etc. (.pinfo), x axis text (.x)
  ## Points,...: can change y axis text by options

  library(ggplot2)
  library(ggrepel)
  library(stringr)
  
  text.size.pinfo <- text.size.pinfo * 0.3514598
  text.size.x <- text.size.x * 0.3514598
  
  pretty2 <- function(xx, n){
    i=2
    while(TRUE){
      res <- pretty(xx, n=i)
      if(length(res) >= 8 | i == 100) break
      i <- i+1
    }
    return(res)
  }
  format2 <- function(xx){
    if(is.factor(xx)) return(format(xx, trim=T, justify="left"))
    else return(formatC(xx, format="fg"))
  }

  ## new data
  if(is.null(mod$offset)){
    newdat_full <- newdat <- data.frame(newdat[,colnames(mod$model[-1])])
    colnames(newdat_full) <- colnames(newdat) <- colnames(mod$model[-1])
  } else {
    cn_tmp <- colnames(mod$model[-1])
    cn_offset <- colnames(mod$model)[str_detect(colnames(mod$model), "offset\\(")]
    cn_offset <- str_split(cn_offset, "[\\(\\) +]+")[[1]]
    cn_offset <- cn_offset[-c(1, length(cn_offset))]
    cn_rest <- colnames(mod$model)[!str_detect(colnames(mod$model), "offset\\(")][-1]
    
    newdat_full <- data.frame(newdat[,c(cn_rest, cn_offset)])
    colnames(newdat_full) <- c(cn_rest, cn_offset)
    newdat <- data.frame(newdat[,cn_rest])
    colnames(newdat) <- cn_rest
  }
  
  for(i in which(colnames(newdat) %in% names(mod$xlevels))) newdat[,i] <- as.character(newdat[,i])
  
  predict2 <- function(mm, nn, type=NULL){
    if(is.null(mod$offset)) suppressWarnings(predict(mm, nn, type=type))
    else {
      ntmp <- as.data.frame(t(rep(1, length(cn_offset))))
      colnames(ntmp) <- cn_offset
      suppressWarnings(predict(mm, cbind(nn, ntmp), type=type))
    }
  }
  
  ## prediction test
  if(sum(is.na(mod$coefficients))>0) warning("[Rex] Model from a rank-deficient fit: prediction may be misleading")
  test <- tryCatch(predict2(mod, newdat_full), error=function(e) e$message)
  if(is.character(test)) stop(paste0("[Rex] Inadequate newdata: ", test))
  
  combdat <- rbind(mod$model[colnames(newdat)], newdat)
  factor_info <- mod$xlevels
  modmat <- attr(mod$terms, "factors")
  modmat <- as.data.frame(modmat[which(!str_detect(rownames(modmat), "offset\\(")),])
  colnames(modmat) <- colnames(attr(mod$terms, "factors"))

  ## conti. variables
  conti_varn <- rownames(modmat)[-1][!(rownames(modmat)[-1] %in% names(factor_info))]
  conti_varn_list <- list()
  if(length(conti_varn)>1){
    conti_varn2 <- t(unique(t(modmat[conti_varn,])))
    conti_varn2 <- conti_varn2[,colSums(conti_varn2)>0]
    for(i in 1:ncol(conti_varn2)){
      modmat_tmp <- modmat[apply(modmat[conti_varn,] == conti_varn2[,i], 2, function(x) as.logical(prod(x)))]
      conti_varn_list[[i]] <- rownames(modmat)[rowSums(modmat_tmp)>0]
    }
  }
  if(length(conti_varn)==1) conti_varn_list[[1]] <- rownames(modmat)[rowSums(modmat[which(modmat[conti_varn,]>=1)])>0]
  if(length(conti_varn)==0) conti_varn_list <- NULL
  
  ## dummy variables
  if(length(factor_info)==0) dummy_varn_list <- NULL
  else {
    dummy_varn <- names(factor_info)
    modmat_tmp <- modmat[!rownames(modmat) %in% dummy_varn,]
    if(is.data.frame(modmat_tmp)) modmat_tmp <- modmat[dummy_varn, colSums(modmat_tmp)==0]
    else modmat_tmp <- modmat[dummy_varn, sum(modmat_tmp)==0]
    if(is.integer(modmat_tmp)) {
      modmat_tmp <- data.frame(modmat_tmp)
      rownames(modmat_tmp) <- colnames(modmat_tmp) <- dummy_varn
    }
    modmat_tmp[modmat_tmp>=1] <- 1

    dummy_varn_list <- list()
    modmat_tmp_cs <- colSums(modmat_tmp)
    suppressWarnings(modmat_tmp_cs_max <- max(modmat_tmp_cs)) 
    if(modmat_tmp_cs_max<=0) dummy_varn_list <- NULL
    else {
      if(modmat_tmp_cs_max==1){
        modmat_tmp_csn <- names(modmat_tmp_cs)[modmat_tmp_cs==1]
        for(i in 1:length(modmat_tmp_csn)) dummy_varn_list[[i]] <- modmat_tmp_csn[i]
      } else {
        modmat_tmp2 <- modmat_tmp[modmat_tmp_cs>1]
        while(max(rowSums(modmat_tmp2))>1){
          targ <- which(modmat_tmp2[which(rowSums(modmat_tmp2)>1)[1],]==1)
          targ2 <- data.frame(as.integer(as.logical(rowSums(modmat_tmp2[,targ]))))
          colnames(targ2) <- paste0(rownames(modmat_tmp2)[targ2==1], collapse = ":")
          modmat_tmp2 <- cbind(targ2, modmat_tmp2)
          modmat_tmp2 <- modmat_tmp2[-(targ+1)]
        }
        
        for(i in 1:ncol(modmat_tmp2)) dummy_varn_list[[i]] <- rownames(modmat_tmp)[modmat_tmp2[,i]==1]
        targ <- rownames(modmat_tmp)[rowSums(modmat_tmp2)==0]
        if(length(targ)>0) for(i in 1:length(targ)) dummy_varn_list[[i+ncol(modmat_tmp2)]] <- targ[i]
      }
    }
  }

  ## variable list, coefficients, data breaks
  modmatfunc <- function(modmatrix, dat, pret=T){
    depvar_list <- c()
    res_coef <- c()
    res <- list()
    
    modmat_tmp_func <- function(type=2, dv){
      modmat_tmp <- expand.grid(factor_info[dv[dv %in% names(factor_info)]])
      for(kn in rownames(modmatrix)[-1]){
        if(!kn %in% colnames(modmat_tmp)){
          if(kn %in% names(factor_info)) modmat_tmp[,kn] <- factor_info[[kn]][1]
          else modmat_tmp[,kn] <- 0
        }
      }
      if(type==1) return(modmat_tmp[rownames(modmatrix)[-1]])
      if(type==2) return(modmat_tmp[dv])
    }

    if(!is.null(dummy_varn_list)){
      for(k in 1:length(dummy_varn_list)){
        dvlk <- dummy_varn_list[[k]]
        
        ## depvar_list
        depvar_tmp <- list()
        fac1 <- names(which.max(sapply(factor_info[dvlk], length))[1])
        for(ktmp in 1:length(dvlk)){
          if(dvlk[ktmp] == fac1) depvar_tmp[[ktmp]] <- dvlk[ktmp]
          else depvar_tmp[[ktmp]] <- paste0(dvlk[ktmp], factor_info[[dvlk[ktmp]]])
        }
        depvar_tmp <- apply(expand.grid(depvar_tmp), 1, paste0, collapse=":")
        depvar_list <- c(depvar_list, depvar_tmp)
        
        ## res_coef
        coef_tmp <- rep(1, length(depvar_tmp))
        names(coef_tmp) <- depvar_tmp
        res_coef <- c(res_coef, coef_tmp)
        
        ## res
        if(length(dvlk)==1){
          coef_tmp <- predict2(mod, modmat_tmp_func(type=1, dv=dvlk)) - mod$coefficients[1]
          names(coef_tmp) <- factor_info[[dvlk]]
          
          sdat <- rep(NA, nrow(dat))
          for(fn in factor_info[[dvlk]]) sdat[dat[,dvlk]==fn] <- coef_tmp[which(factor_info[[dvlk]]==fn)]
          res[[dvlk]] <- sdat
          if(pret) res[[dvlk]] <- coef_tmp
        } else {
          fac_comb <- modmat_tmp_func(type=1, dv=dvlk[dvlk != fac1])
          for(i in 1:nrow(fac_comb)){
            suppressWarnings(fac_comb2 <- data.frame(x=factor_info[[fac1]], fac_comb[i,which(colnames(fac_comb) != fac1)]))
            colnames(fac_comb2) <- c(fac1, colnames(fac_comb)[which(colnames(fac_comb) != fac1)])
            
            coef_tmp <- predict2(mod, fac_comb2) - mod$coefficients[1]
            names(coef_tmp) <- factor_info[[fac1]]
            
            sdat <- dat
            for(j in which(colnames(fac_comb) %in% dvlk[dvlk != fac1])) if(nrow(sdat)>0) sdat <- sdat[which(sdat[,colnames(fac_comb)[j]]==fac_comb[i,j]),]
            if(nrow(sdat)>0){
              sdat2 <- rep(NA, nrow(sdat))
              for(fn in factor_info[[fac1]]) sdat2[sdat[,fac1]==fn] <- coef_tmp[which(factor_info[[fac1]]==fn)]
              res[[depvar_tmp[i]]] <- sdat2
            } else res[[depvar_tmp[i]]] <- integer(0)
            if(pret) res[[depvar_tmp[i]]] <- coef_tmp
          }
        }
      }
    }
    
    if(!is.null(conti_varn_list)){
      for(k in 1:length(conti_varn_list)){
        cvlk <- conti_varn_list[[k]]
        
        if(sum(cvlk %in% names(factor_info))==0){
          depvar_tmp <- paste0(cvlk, collapse = ":")
          depvar_list <- c(depvar_list, depvar_tmp)
          res_coef <- c(res_coef, mod$coefficients[depvar_tmp])
          res[[depvar_tmp]] <- as.numeric(apply(dat[cvlk],1,prod))
          if(pret) res[[depvar_tmp]] <- pretty2(res[[depvar_tmp]], n=10)
        } else {
          ## depvar_list
          depvar_tmp2 <- depvar_tmp <- list()
          for(ktmp in 1:length(cvlk)){
            if(cvlk[ktmp] %in% names(factor_info)) depvar_tmp[[ktmp]] <- paste0(cvlk[ktmp], factor_info[[cvlk[ktmp]]])
            else {
              depvar_tmp[[ktmp]] <- cvlk[ktmp]
              depvar_tmp2[[cvlk[ktmp]]] <- 0:1
            }
          }
          depvar_tmp <- apply(expand.grid(depvar_tmp), 1, paste0, collapse=":")
          depvar_list <- c(depvar_list, depvar_tmp)

          ## res_coef
          coef_tmp <- predict2(mod, modmat_tmp_func(type=1, dv=cvlk)) ### all cont variable=0
          depvar_tmp2 <- expand.grid(depvar_tmp2)
          pm1 <- (-1)^ncol(depvar_tmp2)
          coef_tmp <- coef_tmp * pm1
          
          for(i in 1:ncol(depvar_tmp2)){
            pm1 <- pm1*(-1)
            depvar_tmp2i <- depvar_tmp2[which(rowSums(depvar_tmp2)==i),]
            if(ncol(depvar_tmp2)==1){
              modmat_tmp_i <- modmat_tmp_func(type=1, dv=cvlk)
              modmat_tmp_i[,colnames(depvar_tmp2)] <- depvar_tmp2i
              coef_tmp <- rbind(coef_tmp, predict2(mod, modmat_tmp_i)*pm1)
            } else {
              for(ii in 1:nrow(depvar_tmp2i)){
                modmat_tmp_i <- modmat_tmp_func(type=1, dv=cvlk)
                for(ij in colnames(depvar_tmp2)) modmat_tmp_i[,ij] <- depvar_tmp2i[ii,ij]
                coef_tmp <- rbind(coef_tmp, predict2(mod, modmat_tmp_i)*pm1)
              }
            }
          }
          
          coef_tmp <- colSums(coef_tmp)
          names(coef_tmp) <- depvar_tmp
          res_coef <- c(res_coef, coef_tmp)
          
          ## res
          fac_comb <- modmat_tmp_func(dv=cvlk)
          for(i in 1:nrow(fac_comb)){
            sdat <- dat
            for(j in which(colnames(fac_comb) %in% names(factor_info))) if(nrow(sdat)>0) sdat <- sdat[which(sdat[,colnames(fac_comb)[j]]==fac_comb[i,j]),]
            res[[depvar_tmp[i]]] <- as.numeric(apply(sdat[colnames(depvar_tmp2)],1,prod))
            if(pret) res[[depvar_tmp[i]]] <- pretty2(res[[depvar_tmp[i]]], n=10)
          }
        }
      }
    }
    
    for(res_n in names(res)[sapply(res, length)==0]) res[[res_n]] <- NA
    return(list(depvar_list, res_coef, res))
  }
  newdat2 <- data.frame(modmatfunc(modmat, newdat, pret=F)[[3]])
  pretbreak <- modmatfunc(modmat, combdat, pret=T)

  ## scale calculation
  modscalefunc <- function(pretbreakdat){

    modcoef <- pretbreakdat[[2]]
    pretbreakdat1 <- pretbreakdat[[3]]
    res <- c()
    
    if(compact) pbd_name <- which(!sapply(newdat2,is.na))
    else pbd_name <- 1:length(pretbreakdat1)
    
    for(k in pbd_name){
      k <- names(pretbreakdat1)[k]
      res <- c(res, range(pretbreakdat1[[k]])*modcoef[k])
    }
    
    return(c(diff(range(res)),-(min(res)))) ### total effect size, point zero shifting
  }
  basedat <- modscalefunc(pretbreak)
  
  ## scaling for nomogram
  graphdatfunc <- function(pretbreakdat){
    modcoef <- pretbreakdat[[2]]
    pretbreakdat1 <- pretbreakdat[[3]]
    if(compact){
      modcoef <- modcoef[!sapply(newdat2,is.na)]
      pretbreakdat1 <- pretbreakdat1[!sapply(newdat2,is.na)]
    }
    res2 <- list()

    for(i in 1:length(pretbreakdat1)){
      iname <- names(pretbreakdat1)[i]
      pretdat <- pretbreakdat1[[iname]]
      coeftmp <- modcoef[iname]
      dpoint <- (basedat[2]+pretdat*coeftmp)/basedat[1]*100
      
      if(is.null(names(pretdat))){
        res2[[i]] <- data.frame(x=dpoint,
                                y=-i,
                                label=pretdat)
      } else {
        res2[[i]] <- data.frame(x=dpoint,
                                y=-i,
                                label2=pretdat,
                                label=names(pretdat))
      }
    }
    return(res2)
  }
  coefdat <- graphdatfunc(pretbreak)

  ## newdat for nomogram
  newdatfunc <- function(i){
    if(compact) newdat2 <- newdat2[!sapply(newdat2,is.na)]
    newd <- newdat2[,i]
    if(is.na(newd)) return(NULL)
    else {
      coefd <- coefdat[[i]]
      if(ncol(coefd)==4){
        targ <- coefd$x[coefd$label2==newd]
        newd <- as.character(coefd$label[coefd$label2==newd])
      }
      else targ <- predict2(lm(x~label, coefd), data.frame(label=newd))
      
      return(data.frame(x=targ, y=-i, xend=targ, yend=0, label=newd, stringsAsFactors = F))
    }
  }
  newdat3 <- do.call(rbind,lapply(1:length(coefdat), newdatfunc))

  ## point, total point, linear predictor axis
  totaldatfunc <- function(model){
    points_dat <- data.frame(x=seq(0,100,10), y=0, label=seq(0,100,10))
    totalpoints_dat <- data.frame(x=NA, y=-(length(coefdat)+1), 
                                  label=pretty2(c(0, 100, sum(sapply(coefdat[-newdat3$y], function(x) max(x$x)))), n=10))
    totalpoints_dat$x <- totalpoints_dat$label/max(totalpoints_dat$label)*100

    locshift <- sum(newdat3$x)*basedat[1]/100 - predict2(mod, newdat)

    dep_dat <- mod$model
    if(compact & length(factor_info)>0 & !is.null(conti_varn_list)) for(fn in names(factor_info)) dep_dat <- dep_dat[which(dep_dat[,fn]==as.character(newdat[,fn])),]
    linearpred_dat <- data.frame(x=NA, y=-(length(coefdat)+2),
                                 label=pretty2(c(predict2(mod, mod$model[colnames(newdat)])[rownames(dep_dat)], predict2(mod, newdat)), n=10))
    linearpred_dat$x <- ((linearpred_dat$label+locshift)/basedat[1]*100)/max(totalpoints_dat$label)*100
    
    pretty_binomial <- function(x){
      if(min(x)>=0.1) res_i <- floor(min(x)*10)/10
      else {
        res_i <- 0.1
        while(res_i[1]>min(x)) res_i <- c(res_i[1]/10, res_i)
      }
      
      if(max(x)<=0.9) res_f <- ceiling(max(x)*10)/10
      else {
        res_f <- 0.9
        while(res_f[1]<max(x)) res_f <- c(1-(1-res_f[1])/10, res_f)
      }
      
      res <- sort(unique(c(res_i, res_f, seq(rev(res_i)[1], rev(res_f)[1], 0.1))))
      return(list(res,
                  str_trim(formatC(res, format="fg", digits=nchar(as.character(rev(res)[1]))-2))))
    }

    if(mod$family$family=="binomial"){
      fitted_dat <- data.frame(x=NA, y=-(length(coefdat)+3),
                               label2=pretty_binomial(mod$family$linkinv(c(predict2(mod, mod$model[colnames(newdat)])[rownames(dep_dat)], predict2(mod, newdat))))[[1]],
                               label=pretty_binomial(mod$family$linkinv(c(predict2(mod, mod$model[colnames(newdat)])[rownames(dep_dat)], predict2(mod, newdat))))[[2]],
                               stringsAsFactors = F)
      fitted_dat <- fitted_dat[which(!fitted_dat$label %in% c("0", "1")),]
    } else {
      fitted_dat <- data.frame(x=NA, y=-(length(coefdat)+3),
                               label2=pretty2(mod$family$linkinv(c(predict2(mod, mod$model[colnames(newdat)])[rownames(dep_dat)], predict2(mod, newdat))), n=10),
                               label=pretty2(mod$family$linkinv(c(predict2(mod, mod$model[colnames(newdat)])[rownames(dep_dat)], predict2(mod, newdat))), n=10),
                               stringsAsFactors = F)
    }
    fitted_dat$x <- ((mod$family$linkfun(fitted_dat$label2)+locshift)/basedat[1]*100)/max(totalpoints_dat$label)*100
    fitted_dat <- fitted_dat[which(!is.infinite(fitted_dat$x)),]
    return(list(points_dat, totalpoints_dat, linearpred_dat, fitted_dat))
  }
  graphdat <- totaldatfunc(mod)

  ## ggplot function for each axis data
  geom_nomogram <- function(dat, ggp){
    dat1 <- data.frame(x=min(dat$x), xend=max(dat$x), y=dat$y[1])
    dat$yend <- dat$y-0.05
    
    res <- ggp + geom_segment(mapping=aes(x=x,xend=xend,y=y,yend=y), data=dat1) + 
      geom_segment(mapping=aes(x=x,xend=x,y=y,yend=yend), data=dat) + 
      geom_text(mapping=aes(x=x,y=yend,label=format2(label)), data=dat, vjust=1, nudge_y = -0.03, size=text.size.x, check_overlap = T)
    
    return(res)
  }
  
  ggp <- ggplot()
  for(i in 1:length(coefdat)) ggp <- geom_nomogram(coefdat[[i]], ggp)
  for(i in 1:length(graphdat)) ggp <- geom_nomogram(graphdat[[i]], ggp)
  
  ## ggplot function for newdat
  geom_nomogram2 <- function(dat, ggp){
    cumx <- cumsum(dat$x)/rev(graphdat[[2]]$label)[1]*100
    dat1 <- data.frame(x=c(0, cumx[-length(cumx)]), xend=cumx, y=graphdat[[2]][1,2])
    dat2 <- data.frame(x=cumx[length(cumx)], yend=graphdat[[2]][1,2], y=c(graphdat[[3]][1,2], graphdat[[4]][1,2]), 
                       label=c(predict2(mod, newdat), predict2(mod, newdat, type="response")))

    res <- ggp + 
      geom_segment(mapping=aes(x=x,xend=xend,y=y,yend=y), data=dat1, color=gg_color_hue2(length(cumx)), size=1.5) +
      
      geom_point(mapping=aes(x=x, y=0), data=dat, shape=4, stroke=1.5, color=gg_color_hue2(length(cumx))) +
      geom_segment(mapping=aes(x=x,xend=xend,y=y,yend=yend), data=dat, color=gg_color_hue2(length(cumx)), linetype=2) +
      geom_text(mapping=aes(x=x, y=y, label=format(label, trim = T)), data=dat, color=gg_color_hue2(length(cumx)), size=text.size.pinfo, vjust=0, hjust=0, nudge_x = 0.15, nudge_y = 0.03) +
      geom_text_repel(mapping=aes(x=x, y=0, label=round(x,1)), data=dat, color=gg_color_hue2(length(cumx)), size=text.size.pinfo, vjust=0, hjust=0, nudge_x = 0.5, nudge_y = 0.02, ylim=c(0,NA)) +
      
      geom_segment(mapping=aes(x=x,xend=x,y=y,yend=yend), data=dat2[2,], linetype=2, color="black") +
      geom_text_repel(mapping=aes(x=x, y=y, label=format2(label)), data=dat2[1,], color="black", size=text.size.pinfo, vjust=0, hjust=0, nudge_x = 0.5, nudge_y = 0.02, ylim=c(dat2$y[1],NA)) +
      geom_text_repel(mapping=aes(x=x, y=y, label=format2(label)), data=dat2[2,], color="black", size=text.size.pinfo, vjust=0, hjust=0, nudge_x = 0.5, nudge_y = 0.02, ylim=c(dat2$y[2],NA)) +
      geom_point(mapping=aes(x=x, y=y), data=dat2, shape=4, stroke=1.5)
    
    return(res)
  }
  
  ### Using Analysis modules...
  gg_color_hue2 <- function(n) {
    hues = seq(6.330885, 6.330885+360, length = n + 1)
    hcl(h = hues, l = 51.19696, c = 144.4467)[1:n]
  }
  
  ggp <- geom_nomogram2(newdat3, ggp)
  if(compact) lab_y <- c(Points, names(pretbreak[[3]])[!sapply(newdat2,is.na)], Total_Points, Linear_Predictor, Response_Scale)
  else lab_y <- c(Points, names(pretbreak[[3]]), Total_Points, Linear_Predictor, Response_Scale)
  ggp <- ggp + scale_y_continuous(breaks=rev(-(1:length(lab_y))+1), labels=rev(lab_y)) + 
    theme_minimal() + theme(axis.title = element_blank(), panel.grid = element_blank(), 
                            axis.text.x = element_blank(), axis.text.y = element_text(size=text.size.y))
  
  return(ggp)
}


##### Interactive plot >>

REx_ggplot_extract <- function(ggp){
  ## check interactive plot available
  if(!"interactive" %in% attr(ggp, "Rex")) return(NULL)
  
  load.pkg("ggplot2")
  # browser()
  ## scales extract
  scale_name <- c("colour", "fill", "shape", "linetype", "size")
  nscale2 <- sapply(ggp$scales$scales, function(x) ifelse(x$aesthetics[1] %in% scale_name, x$aesthetics[1], NA)) ## check scale_x or scale_y
  
  nlayer <- length(ggp$layers)
  res <- list()
  for(i in 1:nlayer){
    dat1 <- ggplot_build(ggp)$data[[i]]
    scale_pos <- which(names(dat1) %in% nscale2)
    ii <- 1
    while(TRUE){
      if(ii %in% scale_pos) ii <- ii+1
      else break
    }
    # browser()
    if(ii>1){
      for(i3 in 1:(ii-1)){
        scale_class <- class(ggp$scales$scales[[which(nscale2 %in% colnames(dat1)[i3])]])
        if(scale_class[1]=="ScaleContinuous"){
          ## for REx_bubbleplot
          if(colnames(dat1)[i3]=="size") res[["size"]] <- c("Point_Min.size"=min(dat1$size), "Point_Max.size"=max(dat1$size))
          ## for REx_histogram2d
          if(colnames(dat1)[i3]=="fill"){
            count_range <- range(dat1$count)
            res[["fill"]] <- c("Color_Min.count"=dat1$fill[which(dat1$count==count_range[1])[1]], 
                               "Color_Max.count"=dat1$fill[which(dat1$count==count_range[2])[1]])
          }
        } else {
          tmp <- unique(dat1[,c(i3, which(colnames(dat1)=="group"))])
          if("REx_densityplot2d" %in% attr(ggp, "Rex") & !is.integer(tmp$group)){
            tmp$group <- as.integer(do.call(rbind, strsplit(as.character(tmp$group), "-"))[,1])
            tmp <- unique(tmp)
          }
          tmp2 <- tmp[,1]
          names(tmp2) <- levels(ggp$layers[[i]]$data[,as.character(ggp$layers[[i]]$mapping[[colnames(tmp)[1]]])[2]])[tmp[,2]]
          tmp2 <- tmp2[!is.na(names(tmp2))]
          for(j in 1:length(tmp2)) if(!names(tmp2)[j] %in% names(res[[colnames(dat1)[i3]]])) res[[colnames(dat1)[i3]]] <- c(res[[colnames(dat1)[i3]]],tmp2[j])
        }
        # if(!names(tmp2) %in% names(res[[colnames(dat1)[i3]]])) res[[colnames(dat1)[i3]]] <- c(res[[colnames(dat1)[i3]]],tmp2)
        # res[[colnames(dat1)[i3]]] <- unique(c(res[[colnames(dat1)[i3]]],tmp2))
      }
    }
  }
  # browser()
  if("REx_trendplot" %in% attr(ggp, "Rex")){
    res_names <- names(res)
    for(rn in res_names){
      res[[rn]] <- res[[rn]][!is.na(res[[rn]])]
      if(rn=="shape") res[[rn]] <- res[[rn]][res[[rn]]!=32]
      if(rn %in% c("colour", "fill")) res[[rn]] <- res[[rn]][res[[rn]]!="transparent"]
      if(rn %in% c("size","linetype")) res[[rn]] <- res[[rn]][res[[rn]]!=0]
    }
  }
  
  ## other information extract
  res$title <- ggp$labels$title
  res$title.size <- ggp$theme$plot.title$size
  if(class(res$title.size)=="rel") res$title.size <- as.numeric(res$title.size) * ggp$theme$text$size
  res$xlab <- ggp$labels$x
  res$ylab <- ggp$labels$y
  res$axis.title.size <- ggp$theme$axis.title$size
  
  res$grid.major <- ifelse(is.null(ggp$theme$panel.grid.major), "TRUE", "FALSE")
  res$grid.minor <- ifelse(class(ggp$theme$panel.grid.minor)[1]=="element_line", "TRUE", "FALSE")
  
  res$lgd.pos <- ggp$theme$legend.position
  if(is.numeric(res$lgd.pos)) res$lgd.pos <- res$lgd.pos*100
  
  ## no legend rules added (scale_*_manual(..., guide="none"))
  lgd.info <- sapply(ggp$scales$scales, function(x) x$guide)
  if(sum(lgd.info=="none")==length(lgd.info)) res$lgd.pos <- NULL
  
  ## inactive option rules (can add rules later...)
  if("REx_dotplot" %in% attr(ggp, "Rex")) res$inactive <- c("ylab")
  if("REx_circleplot" %in% attr(ggp, "Rex")) res$inactive <- c("xlab", "ylab", "grid.major", "grid.minor")
  
  return(res)
}

REx_ggplot_interactive <- function(ggp, ia_name=NULL, ia_level=NULL, ia_value=NULL){
  load.pkg("ggplot2")
  
  ## scales 
  scale_name <- c("colour", "fill", "shape", "linetype", "size")
  nscale2 <- sapply(ggp$scales$scales, function(x) ifelse(x$aesthetics[1] %in% scale_name, x$aesthetics[1], NA))
  if(ia_name=="color") ia_name=="colour"
  
  if(ia_name %in% scale_name){
    ggp_info <- REx_ggplot_extract(ggp)
    new_scale <- ggp_info[[ia_name]]
    if(ia_level %in% names(new_scale)) new_scale[ia_level] <- ia_value
    name_i <- which(nscale2 == ia_name)
    
    if(ia_name=="size" & "REx_bubbleplot" %in% attributes(ggp)$Rex) ggp <- ggp + scale_size_continuous(range=as.numeric(new_scale), guide=ggp$scales$scales[[name_i]]$guide)
    else if(ia_name=="fill" & "REx_histogram2d" %in% attributes(ggp)$Rex){
      countinfo <- cumsum(table(ggplot_build(ggp)$data[[1]]$count))
      ggp <- ggp + scale_fill_gradientn(colors = colorRampPalette(as.character(new_scale))(diff(range(countinfo))+1)[countinfo-countinfo[1]+1],
                                        values = scales::rescale(countinfo))
    } else {
      ggp <- ggp + ggplot2:::manual_scale(values = new_scale, 
                                          breaks=ggp$scales$scales[[name_i]]$breaks, guide=ggp$scales$scales[[name_i]]$guide, 
                                          aesthetic=ggp$scales$scales[[name_i]]$aesthetics) ## especially for colour-fill set
    }
    nscale2 <- sapply(ggp$scales$scales, function(x) ifelse(x$aesthetics[1] %in% scale_name, x$aesthetics[1], NA))
    
    if("REx_bubbleplot" %in% attributes(ggp)$Rex){
      if(ia_name=="shape"){
        size_guide <- ggp$scales$scales[[which(nscale2 == "size")]]$guide
        if(ia_level %in% names(size_guide$override.aes$shape)){
          size_guide$override.aes$shape[[ia_level]] <- ia_value
          ggp <- ggp + scale_size_continuous(range=as.numeric(REx_ggplot_extract(ggp)$size), guide=size_guide)
          nscale2 <- sapply(ggp$scales$scales, function(x) ifelse(x$aesthetics[1] %in% scale_name, x$aesthetics[1], NA))
        }
        colour_scale <- ggp$scales$scales[[which(nscale2 == "colour")]]
        if(ia_level %in% names(colour_scale$guide$override.aes$shape)){
          colour_scale$guide$override.aes$shape[[ia_level]] <- ia_value
          ggp <- ggp + scale_color_manual(values = REx_ggplot_extract(ggp)$colour, breaks=colour_scale$breaks,
                                          guide=colour_scale$guide)
          nscale2 <- sapply(ggp$scales$scales, function(x) ifelse(x$aesthetics[1] %in% scale_name, x$aesthetics[1], NA))
          
        }
      }
      if(ia_name=="size"){
        colour_scale <- ggp$scales$scales[[which(nscale2 == "colour")]]
        if("size" %in% names(colour_scale$guide$override.aes)){
          colour_scale$guide$override.aes$size <- mean(as.numeric(REx_ggplot_extract(ggp)$size))
          ggp <- ggp + scale_color_manual(values = REx_ggplot_extract(ggp)$colour, breaks=colour_scale$breaks,
                                          guide=colour_scale$guide)
          nscale2 <- sapply(ggp$scales$scales, function(x) ifelse(x$aesthetics[1] %in% scale_name, x$aesthetics[1], NA))
        }
      }
    }
  }

  
  ## other information
  if(ia_name=="title") ggp <- ggp + labs(title=ia_value)
  if(ia_name=="xlab") ggp <- ggp + labs(x=ia_value)
  if(ia_name=="ylab") ggp <- ggp + labs(y=ia_value)
  if(ia_name=="title.size") ggp <- ggp + theme(plot.title = element_text(size=ia_value))
  if(ia_name=="axis.title.size") ggp <- ggp + theme(axis.title = element_text(size = ia_value))
  if(ia_name=="axis.text.size") ggp <- ggp + theme(axis.text = element_text(size = ia_value))
  
  if(ia_name %in% c("grid.major", "grid.minor")){
    if(ia_value) ia_grid <- element_line()
    else ia_grid <- element_blank()
    # ia_grid <- ifelse(ia_value, element_line(), element_blank())
    if(ia_name=="grid.major") ggp <- ggp + theme(panel.grid.major = ia_grid)
    if(ia_name=="grid.minor") ggp <- ggp + theme(panel.grid.minor = ia_grid)
  }
  
  if(ia_name=="lgd.pos"){
    if(is.numeric(ia_value)){
      lgd.just <- ia_value <- ia_value/100
      if(!is.null(ggp$theme$legend.background)) lgd.just <- 1.02*lgd.just-0.01
    }
    else lgd.just <- NULL
    if("REx_histogram2d" %in% attributes(ggp)$Rex){
      lgd.pos.orig <- ifelse(ggp$theme$legend.position %in% c("top", "bottom"), 1, 0)
      lgd.pos.new <- ifelse(ia_value[1] %in% c("top", "bottom"), 1, 0)
      if(lgd.pos.orig != lgd.pos.new){
        barinfo <- c(ggp$guides$fill$barwidth, ggp$guides$fill$barheight)
        ggp$guides$fill$barwidth <- unit(barinfo[2], "line")
        ggp$guides$fill$barheight <- unit(barinfo[1], "line")
      }
    }
    ggp <- ggp + theme(legend.position = ia_value, legend.justification = lgd.just)
  }
  
  ## text added
  if(ia_name=="ext.text"){
    ext.text2 <- ia_value[2]
    ext.text.dat <- grep("[0-9.-]+[ ]*,[ ]*[0-9.-]+",unlist(strsplit(ext.text2,"[()]")), value=T)
    ext.text.dat <- do.call(rbind, strsplit(ext.text.dat,","))
    ext.text.dat <- data.frame(x=as.numeric(ext.text.dat[,1]), y=as.numeric(ext.text.dat[,2]), 
                               label=ia_value[1], size=as.numeric(ia_value[3]), color=ia_value[4])
    ggp <- ggp + geom_text(mapping=aes(x=x,y=y,label=label), data=ext.text.dat, color=ext.text.dat$color, size=ext.text.dat$size * 0.3514598)
  }
  
  ## point/line added
  if(ia_name=="ext.dot"){
    ext.dot2 <- ia_value[1]
    ext.dot.dat <- grep("[0-9.-]+[ ]*,[ ]*[0-9.-]+",unlist(strsplit(ext.dot2,"[()]")), value=T)
    ext.dot.dat <- do.call(rbind, strsplit(ext.dot.dat,","))
    ext.dot.dat <- data.frame(x=as.numeric(ext.dot.dat[,1]), y=as.numeric(ext.dot.dat[,2]), 
                              shape=as.numeric(ia_value[2]), size=as.numeric(ia_value[3]), stroke=as.numeric(ia_value[4]), color=ia_value[5])
    ggp <- ggp + geom_point(mapping=aes(x=x,y=y), data=ext.dot.dat, color=ext.dot.dat$color, fill=ext.dot.dat$color, shape=ext.dot.dat$shape, size=ext.dot.dat$size, stroke=ext.dot.dat$stroke)
  } 
  if(ia_name=="ext.abline"){
    ext.abline2 <- ia_value[1]
    ext.abline.dat <- grep("[0-9.-]+[ ]*,[ ]*[0-9.-]+",unlist(strsplit(ext.abline2,"[()]")), value=T)
    ext.abline.dat <- do.call(rbind, strsplit(ext.abline.dat,","))
    ext.abline.dat <- data.frame(intercept=as.numeric(ext.abline.dat[,1]), slope=as.numeric(ext.abline.dat[,2]),
                                 linetype=as.numeric(ia_value[2]), size=as.numeric(ia_value[3]), color=ia_value[4])
    ggp <- ggp + geom_abline(aes(intercept=intercept, slope=slope), ext.abline.dat, color=ext.abline.dat$color, linetype=ext.abline.dat$linetype, size=ext.abline.dat$size)
  }
  if(ia_name=="ext.hline"){
    ext.hline.dat <- data.frame(yintercept=as.numeric(strsplit(ia_value[1],",")[[1]]),
                                linetype=as.numeric(ia_value[2]), size=as.numeric(ia_value[3]), color=ia_value[4])
    ggp <- ggp + geom_hline(aes(yintercept = yintercept), ext.hline.dat, color=ext.hline.dat$color, linetype=ext.hline.dat$linetype, size=ext.hline.dat$size)
  } 
  if(ia_name=="ext.vline"){
    ext.vline.dat <- data.frame(xintercept=as.numeric(strsplit(ia_value[1],",")[[1]]),
                                linetype=as.numeric(ia_value[2]), size=as.numeric(ia_value[3]), color=ia_value[4])
    ggp <- ggp + geom_vline(aes(xintercept = xintercept), ext.vline.dat, color=ext.vline.dat$color, linetype=ext.vline.dat$linetype, size=ext.vline.dat$size)
  } 
  
  return(ggp)
}

##### Read image files into Rex... ing

REx_readimg <- function(path){
  load.pkg(c("ggplot2", "cowplot", "magick"))
  ggp <- tryCatch(ggdraw() + draw_image(image_read(path)), error=function(e) NULL)
  if(is.null(ggp)) rexStop("Can't read image file.")
  
  attr(ggp, "Rex") <- c("REx_readimg")
  return(ggp)
}

