###
# Rex Common functions # pkg : "utils","VGAM","survival","rms","caret","R2HTML" (6)
###

options(encoding="utf-8")
options("scipen"=999,"digits"=4)

local.install.packages <- function(pkgs, repos, type='win.binary') {
  rversion = paste(R.version$major, strsplit(R.version$minor, '.', fixed=TRUE)[[1]][1], sep='.')
  local.contriburl = contrib.url(paste('file:///', repos, sep=''), type=type)
  local.pkgs = available.packages(local.contriburl)
  
  dir = '/src/contrib/'
  extension = '.tar.gz'
  if(type == 'mac.binary.leopard' | type == 'mac.binary') {
    dir = paste('/bin/macosx/leopard/contrib/', rversion, '/', sep='')
    extension = '.tgz'
  } else if(type == 'win.binary') {
    dir = paste('/bin/windows/contrib/', rversion, '/', sep='')
    extension = '.zip'
  }
  installed = installed.packages()
  #package.dependencies(local.pkgs[local.pkgs[,'Package'] %in% pkgs,])
  
  d = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  toinstall = utils:::getDependencies(pkgs, d, local.pkgs)
  ininstall2 = utils:::getDependencies(toinstall, d, local.pkgs)
  while(length(toinstall) != length(ininstall2)) {
    toinstall = ininstall2
    ininstall2 = utils:::getDependencies(toinstall, d, local.pkgs)
  }
  
  for(i in 1:length(toinstall)) {
    toinstall[i] = paste(repos, dir,
                         toinstall[i], '_',
                         local.pkgs[local.pkgs[,'Package'] == toinstall[i],'Version'],
                         extension,
                         sep='')
  }
  print('Install the following packages:')
  print(toinstall)
  install.packages(toinstall, repos=NULL, contrib.url=local.contriburl, available=local.pkgs, type=type)
}

setup.lib.dir <- function() 
{
  lib <- .libPaths()[1L]
  ok <- dir.exists(lib) & (file.access(lib, 2) == 0L)
  if (length(lib) > 1 && any(!ok)) {
    ok <- FALSE
  } else {
    if (length(lib) == 1L) {
      ok <- dir.exists(lib)
      if (ok) {
        fn <- file.path(lib, paste0("_test_dir_", Sys.getpid()))
        unlink(fn, recursive = TRUE)
        res <- try(dir.create(fn, showWarnings = FALSE))
        if (inherits(res, "try-error") || !res) 
          ok <- FALSE
        else unlink(fn, recursive = TRUE)
      }
    }
    if (length(lib) == 1L && !ok) {
      userdir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"), 
                                 .Platform$path.sep))[1L]
      lib <- userdir
      if (!file.exists(userdir)) {
        if (dir.create(userdir, recursive = TRUE)) {
          .libPaths(c(userdir, .libPaths()))
          ok <- TRUE
        } else {
          ok <- FALSE
        }
      }
    }
  }
  ifelse(ok,"TRUE","FALSE")
}

inst.pkg <- function(pkg, repos="http://healthstat.snu.ac.kr/CRAN") {
  if (!suppressWarnings(require(pkg, quietly=TRUE, character.only=TRUE))) {
    ret <- try(local.install.packages(pkg, repos=repos), T)
    if (class(ret) == 'try-error') 2
    else 1
  }
  0
}

if (0) load.pkg <- function(pkgs) {
  for (i in pkgs) {
    ret <- inst.pkg(i)
    if (ret != 0) {
      if (ret == 2) rexStop(paste0("패키지 [", i, "] 설치에 실패했습니다. Rex 개발팀에게 문의해 주세요!"))
      else if (ret == 1) {
        if (class(require(i, quietly=TRUE, character.only=TRUE)) == 'try-error')
          rexStop(paste0("패키지 [", i, "] 가 설치되었지만 로드에 실패했습니다. Rex 개발팀에게 문의해 주세요!"))
      }
    }
  }
}

load.pkg <- function(pkgs) {
  for (i in pkgs) {
    if (class(try(library(i, character.only=TRUE))) == 'try-error')
      stop(paste0("패키지 [", i, "] 가 없어 실행할 수 없습니다. 패키지를 설치해 주세요."))
  }
}

# Function for formatting numbers
Digits <- function(x) {
  x1 <- x
  if(!is.vector(x)){
    # all NA
    allNA <- apply(x,2,function(gg) all(is.na(gg)))
    x <- x[,!allNA,drop=F]
  }
  
  if(length(x)!=0){
    x[abs(x)>=0.0001&!is.na(x)] <- round(x[abs(x)>=0.0001&!is.na(x)],4)
    w.xx <- abs(x)<0.0001&!is.na(x)&x!=0; xx = x[w.xx]
    w.xxx <- abs(x)>=1e15&!is.na(x); xxx = x[w.xxx]
    if(length(xx)>0){
      x[w.xx] <- paste0(gsub('e','x10<sup>',format(xx,scientific=T,digits=4)),'</sup>')
    }
    if(length(xxx)>0){
      x[w.xxx] <- paste0(gsub('e\\+','x10<sup>',format(xxx,scientific=T,digits=4)),'</sup>')
    }
    x[x==0] <- '0'
    if(!is.vector(x)) {
      x1[,!allNA] <- x
    } else {
      x1 <- x
    }
  }
  x1[is.na(x1)] <- ""
  return(x1)
}

# Package List # 수정 (210527, modified packages 출력)
indiv.pkg.info <- function(pkg.info) {
  funs <- strsplit(pkg.info[[2]],",\\s*")
  pkgs_leng<-unlist(lapply(funs,length))
  pkgs<-rep(pkg.info[[3]],pkgs_leng)
  pkgs_indx<-paste((cumsum(pkgs_leng)-pkgs_leng+1),":",cumsum(pkgs_leng),sep='')
  funs_link<-paste("'<a href=\"https://www.rdocumentation.org/packages/",pkgs,"/topics/",unlist(funs),"\" target='_new'>",unlist(funs),"</a>'",sep='')
  #f.fun<-lapply(lapply(pkg.info[[3]],function(x)grep(x,funs_link,value=T)),paste0,collapse=', ')	
  f.funs<-lapply(lapply(pkgs_indx,function(x)funs_link[eval(parse(text=x))]),paste0,collapse=', ')
  f.pkgs<-paste0("'<a href=\"https://www.rdocumentation.org/packages/",pkg.info[[3]], "\" target='_new'>", pkg.info[[3]],"</a>'",collape='')
  if(length(pkg.info)==4) f.modif <- "(modified for Rex)" else f.modif <- NULL 
  paste("<li> ",pkg.info[[1]]," : ",paste0(f.funs," of R package ",f.pkgs,collapse=', '), f.modif)
}
used.pkg <- function(pkg.list) paste(c(sapply(pkg.list,indiv.pkg.info),"<li> All results other than those mentioned above were written with basic functions of R."),collapse=" <br> ")

# Check integer
check.integer <- function(x) x == round(x)

# Append list obejct
list.append <- function (.data, ...){ if(is.list(.data)){ c(.data, list(...)) }else{ c(.data, ..., recursive = FALSE) } }

# Global option - digits
dgest  <- 2    # 추정치 소수점
dgstat <- 3    # 통계량 소수점
dgpval <- 4    # p-value 소수점

# pvalf - dgpval에 따라 <0.01, <0.001 출력되도록 세팅
fmp <- function(pval){
  
  if(is.nan(pval)) {
    out <- NaN
  } else {
    if(!is.numeric(pval)){ out <- NA
    }else if(pval < 10^(-dgpval)){ out <- paste0("<0.", paste0(rep(0, dgpval-1), collapse=""), "1")
    }else{ out <- format(round(pval, digits=dgpval), nsmall=dgpval)}
  }
  return(out)
  
}

# format 함수 수정
fm <- function(var, fmvar){ if(is.numeric(var)){ return( format(round(var, digits=fmvar), nsmall=fmvar) )}else{ return(var)} }

try({
  rexM <<- TRUE
  lockBinding("rexM", globalenv())
}, TRUE)

# htmlTable
rex.align <- "left"
rex.cspan.rgroup <- 1
rex.css.cell <- "padding-right: 1.5em ; padding-top: .3em; padding-bottom: .3em;"
rex.css.table <- "font-family: Times"

# Data Structure # used_pkg : basic pkg
DStr <- function(dat=NULL,
                 vars=NULL,
                 anal_type=1) {
  
  #. 함수 변수 설명
  #. dat		: 데이터 셋
  #. vars		: 입력된 모든 변수 인자
  #. anal_type	: 분석종류 구성, 1=(총 관측값 수, 사용된 변수 수), 2=(총 관측값 수, 사용된 관측값 수, 사용된 변수 수), 3=요약데이터 시 주석
  if(!is.null(dat)){
    res_DS <- matrix(NA,3,2)
    res_DS[,1] <- c("No. of total observations", "No. of used observations", "No. of used variable(s)")
    res_DS[1,2] <- nrow(dat)
    res_DS[2,2] <- nrow(dat[complete.cases(dat[,vars]),,drop=FALSE])
    res_DS[3,2] <- length(vars)
  }else{
    fnt_msg <- "<li> Information : Because input data is 'Summary dataset', 'Data Structure' was not provided."
  }
  
  if(anal_type==1) (res <- res_DS[c(1,3),])
  if(anal_type==2) (res <- res_DS)
  if(anal_type==3) (res <- fnt_msg)
  
  return(res)
}


# Variable List
VList <- function(category, name, x, digits=dgest, nsmall=dgest){
  
  # category : 카테고리명. 입력된 문자값.
  # name : 변수명. 입력된 문자값.
  # x : 실제 분석할 변수
  # digits : 반올림 숫자
  # nsmall : 출력 숫자
  
  n.sample <- length(x)
  n.valid <- sum(!is.na(x))
  n.miss <- sum(is.na(x))
  p.valid <- paste0("(", format(n.valid/n.sample*100, digits=digits, nsmall=nsmall), "%)")
  p.miss <- paste0("(", format(n.miss/n.sample*100, digits=digits, nsmall=nsmall), "%)")
  
  VL <- matrix(c(category, name, n.sample, n.valid, p.valid, n.miss, p.miss), byrow=T, nrow=1)
  colnames(VL) <- c("Category", "Variable", "N", "N.valid", "(%.valid)", "N.miss", "(%.miss)")
  
  return(VL)
}


# Variable selection
stepAIC.wj <- function(object,dataset,dep_var=NULL,type=c('lm','wlm','binom','multinom','poisson','coxph','clogit','negabino'),
                       noint,direct,keep_var,hr,vars,WTS=NULL,link=NULL,res_type=NULL,dist=NULL,baseline=NULL,
                       tmp.parallel=NULL,offset=NULL,time1=NULL,event=NULL,
                       CI=FALSE, confint.level=0.95, VIF=FALSE,VIP=FALSE,odds=FALSE,exp_estim=FALSE,printlevel=NULL,norm.parallel=FALSE,ord.parallel=FALSE,expestim=FALSE,cond_var=NULL,ANOVA=NULL,ss=NULL) {
  
  R2HTML::HTML(R2HTML::as.title("Variable Selection"),HR=hr,file=local_output)
  
  # Process of VS
  R2HTML::HTML(R2HTML::as.title("Process of Variable Selection"),HR=hr+1,file=local_output,append=TRUE)
  cap <- "<div style='text-align:left'><small>* Models that failed to fit are excluded from the results. <br>* AICs are omitted for the models where log-likelihood is not calculated.</small>" # 변경 : caption 수정
  
  #browser()
  if(direct=="forward"){
    predictor <- ifelse(is.null(keep_var),1,paste(keep_var,collapse='+'))
    if(noint){
      if(is.null(keep_var)){
        getIM <- function(v,cond_var){
          form.temp <- paste(dep_var,'~',v,'-1')
          if(type=='lm') fit.temp <- lm(as.formula(form.temp),data=dataset)
          if(type=='wlm') fit.temp <- lm(as.formula(form.temp),data=dataset,weight=WTS)
          if(type=='binom') {
            command_str <- paste0("fit.temp <- glm(",form.temp,",data=dataset,family=binomial(",link,"))")
            eval(parse(text=command_str))
          }
          if(type=='multinom'){
            if (res_type == "nominal") {
              fit.temp <- try(suppressWarnings(VGAM::vglm(as.formula(form.temp), data=dataset, family=dist(refLevel = baseline, parallel=tmp.parallel))))
            } else {
              command_str	<- paste0("try(fit.temp <- VGAM::vglm(",form.temp,", data=dataset, family=dist(link=", link, ", parallel=tmp.parallel)))") ;
              suppressWarnings(eval(parse(text=command_str)))
            }
          }
          if(type=='poisson'){
            if(is.null(offset)) command_str <- paste0("fit.temp <- try(glm(",form.temp,",data=dataset,family=dist(",link,")),silent=TRUE)")
            if(!is.null(offset)) command_str <- paste0("fit.temp <- try(glm(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,family=dist(",link,")),silent=TRUE)")
            eval(parse(text=command_str))
          }
          if(type=='negabino'){
            if(is.null(offset)) command_str <- paste0("fit.temp <- try(glm.nb(",form.temp,",data=dataset,link=",link,"),silent=TRUE)")
            if(!is.null(offset)) command_str <- paste0("fit.temp <- try(glm.nb(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,link=",link,"),silent=TRUE")
            eval(parse(text=command_str))
          }					
          if(type=='coxph') {
            form.temp <- paste0("survobj ~ ",v)
            fm <- formula(form.temp)
            fit.temp <- survival::coxph(fm, data=dataset)
          }
          if(type=='clogit') {
            form.temp <- paste0(dep_var,'_ ~ ',v,'+ strata(',cond_var,')')
            fm <- formula(form.temp)
            fit.temp <- survival::clogit(fm,method='exact',data=dataset)
            form.temp <- paste0(dep_var,'~ ',v,'+ strata(',cond_var,')')
          }
          return(data.frame(Model=form.temp,AIC=AIC(fit.temp)))
        }
        comp <- do.call(rbind,lapply(vars,getIM,cond_var=cond_var))
        comp[,1] <- as.character(comp[,1])
        form.temp <- comp[comp[,2]==min(comp[,2]),1]
        if(type=='lm') fit.lower <- lm(as.formula(form.temp),data=dataset)
        if(type=='wlm') fit.lower <- lm(as.formula(form.temp),data=dataset,weight=WTS)
        if(type=='binom') {
          command_str <- paste0("fit.lower <- glm(",form.temp,",data=dataset,family=binomial(",link,"))")
          eval(parse(text=command_str))
        }
        if(type=='multinom'){
          if (res_type == "nominal") {
            fit.lower <- try(suppressWarnings(VGAM::vglm(as.formula(form.temp), data=dataset, family=dist(refLevel = baseline, parallel=tmp.parallel))))
          } else {
            command_str	<- paste0("try(fit.lower <- VGAM::vglm(",form.temp,", data=dataset, family=dist(link=", link, ", parallel=tmp.parallel)))") ;
            suppressWarnings(eval(parse(text=command_str)))
          }
        }
        if(type=='poisson'){
          if(is.null(offset)) command_str <- paste0("fit.lower <- try(glm(",form.temp,",data=dataset,family=dist(",link,")),silent=TRUE)")
          if(!is.null(offset)) command_str <- paste0("fit.lower <- try(glm(",paste(form.temp,paste0("+ offset(",paste(offset,collapse=" +"),")")),",data=dataset,family=dist(",link,")),silent=TRUE)")
          eval(parse(text=command_str))
        }
        if(type=='negabino'){
          if(is.null(offset)) command_str <- paste0("fit.lower <- try(glm.nb(",form.temp,",data=dataset,link=",link,"),silent=TRUE)")
          if(!is.null(offset)) command_str <- paste0("fit.lower <- try(glm.nb(",paste(form.temp,paste0("+ offset(",paste(offset,collapse=" +"),")")),",data=dataset,link=",link,"),silent=TRUE)")
          eval(parse(text=command_str))
        }				
        if(type=='coxph') {
          fm <- formula(form.temp)
          fit.lower <- survival::coxph(fm, data=dataset)
        }
        if(type=='clogit') {
          fm <- formula(gsub('~','_~',form.temp))
          fit.lower <- survival::clogit(fm,method='exact', data=dataset)
        }
        predictor <- paste(vars[comp[,2]==min(comp[,2])],'-1')
        comp[,1] <- gsub('-1','(Intercept is not included)',comp[,1])
        comp <- comp[order(comp[,2]),]
        comp[,-1] <- Digits(comp[,-1])
        if(type=='coxph'){
          comp[,1] <- gsub("Surv.*\\)","Hazard ratio",comp[,1])
          comp[,1] <- paste0(gsub("~ ","~ exp(",comp[,1]),")")
        }
        R2HTML::HTML(R2HTML::as.title('Step 0 : Initial model (Smaller AIC values are better)'),HR=hr+2,file=local_output,append=TRUE)
        R2HTML::HTML(comp,file=local_output,align="left",row.names=F,digits=15)
      } else {
        if(type!='clogit') predictor <- paste(predictor,'-1')
        form.temp <- paste0(dep_var,'~',predictor)
        
        if(type=='lm') fit.lower <- lm(as.formula(form.temp),data=dataset)
        if(type=='wlm') fit.lower <- lm(as.formula(form.temp),data=dataset,weight=WTS)
        if(type=='binom') {
          command_str <- paste0("fit.lower <- glm(",form.temp,",data=dataset,family=binomial(",link,"))")
          eval(parse(text=command_str))
        }
        if(type=='multinom'){
          if (res_type == "nominal") {
            fit.lower <- try(suppressWarnings(VGAM::vglm(as.formula(form.temp), data=dataset, family=dist(refLevel = baseline, parallel=tmp.parallel))))
          } else {
            command_str	<- paste0("try(fit.lower <- VGAM::vglm(",form.temp,", data=dataset, family=dist(link=", link, ", parallel=tmp.parallel)))") ;
            suppressWarnings(eval(parse(text=command_str)))
          }
        }
        if(type=='poisson'){
          if(is.null(offset)) command_str <- paste0("fit.lower <- try(glm(",form.temp,",data=dataset,family=dist(",link,")),silent=TRUE)")
          if(!is.null(offset)) command_str <- paste0("fit.lower <- try(glm(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,family=dist(",link,")),silent=TRUE)")
          eval(parse(text=command_str))
        }
        if(type=='negabino'){
          if(is.null(offset)) command_str <- paste0("fit.lower <- try(glm.nb(",form.temp,",data=dataset,link=",link,"),silent=TRUE)")
          if(!is.null(offset)) command_str <- paste0("fit.lower <- try(glm.nb(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,link=",link,"),silent=TRUE)")
          eval(parse(text=command_str))
        }				
        if(type=='coxph') {
          form.temp <- paste0("survobj ~ ",predictor)
          fm <- formula(form.temp)
          fit.lower <- survival::coxph(fm, data=dataset)
        }
        if(type=='clogit') {
          form.temp <- paste0(dep_var,'_ ~ ',predictor,'+ strata(',cond_var,')')
          fm <- formula(form.temp)
          fit.lower <- survival::clogit(fm,method='exact', data=dataset)
        }				
      }
    } else {
      form.temp <- paste0(dep_var,'~',predictor)
      
      if(type=='lm') fit.lower <- lm(as.formula(form.temp),data=dataset)
      if(type=='wlm') fit.lower <- lm(as.formula(form.temp),data=dataset,weight=WTS)
      if(type=='binom') {
        command_str <- paste0("fit.lower <- glm(",form.temp,",data=dataset,family=binomial(",link,"))")
        eval(parse(text=command_str))
      }
      if(type=='multinom'){
        if (res_type == "nominal") {
          fit.lower <- try(suppressWarnings(VGAM::vglm(as.formula(form.temp), data=dataset, family=dist(refLevel = baseline, parallel=tmp.parallel))))
        } else {
          command_str	<- paste0("try(fit.lower <- VGAM::vglm(",form.temp,", data=dataset, family=dist(link=", link, ", parallel=tmp.parallel)))") ;
          suppressWarnings(eval(parse(text=command_str)))
        }
      }
      if(type=='poisson'){
        if(is.null(offset)) command_str <- paste0("fit.lower <- try(glm(",form.temp,",data=dataset,family=dist(",link,")),silent=TRUE)")
        if(!is.null(offset)) command_str <- paste0("fit.lower <- try(glm(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,family=dist(",link,")),silent=TRUE)")
        eval(parse(text=command_str))
      }
      if(type=='negabino'){
        if(is.null(offset)) command_str <- paste0("fit.lower <- try(glm.nb(",form.temp,",data=dataset,link=",link,"),silent=TRUE)")
        if(!is.null(offset)) command_str <- paste0("fit.lower <- try(glm.nb(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,link=",link,"),silent=TRUE)")
        eval(parse(text=command_str))
      }			
    }
    
    AICtable <- list()
    currAIC <- currModel <- c()
    j=0
    while(1){
      j=j+1
      add.vars <- vars[!vars%in%unlist(strsplit(as.character(predictor),"\\+| "))]
      AIC.table <- data.frame(term='none',DF=as.numeric(NA),AIC=AIC(fit.lower))
      currAIC <- c(currAIC,AIC(fit.lower))
      if(type=='coxph'){
        currModel <- c(currModel,paste0('Hazard ratio ~ exp(',ifelse(predictor==1,'(Intercept)',gsub(' -1','',predictor)),')'))
        
      } else {
        if(type=='clogit') { predictor<-gsub('-1','',predictor);currModel <- c(currModel,paste(dep_var,'~',ifelse(predictor==1,'(Intercept)',predictor))) }
        if(type!='clogit') currModel <- c(currModel,paste(dep_var,'~',ifelse(predictor==1,'(Intercept)',predictor)))
      }
      jjj=1
      all.fit <- list(fit.lower)
      if(length(add.vars)!=0) {
        for(jj in add.vars){
          new.predictor <- paste(predictor,'+',jj)
          form.temp <- paste(dep_var,'~',new.predictor)
          
          if(type=='lm') fit.upper <- try(lm(as.formula(form.temp),data=dataset),silent=T)
          if(type=='wlm') fit.upper <- try(lm(as.formula(form.temp),data=dataset,weight=WTS),silent=T)
          if(type=='binom') {
            command_str <- paste0("fit.upper <- try(glm(",form.temp,",data=dataset,family=binomial(",link,")),silent=T)")
            eval(parse(text=command_str))
          }
          if(type=='multinom'){
            if (res_type == "nominal") {
              fit.upper <- try(suppressWarnings(VGAM::vglm(as.formula(form.temp), data=dataset, family=dist(refLevel = baseline, parallel=tmp.parallel))))
            } else {
              command_str	<- paste0("fit.upper <- try(VGAM::vglm(",form.temp,", data=dataset, family=dist(link=", link, ", parallel=tmp.parallel)),silent=T)") ;
              suppressWarnings(eval(parse(text=command_str)))
            }
          }
          if(type=='poisson'){
            if(is.null(offset)) command_str <- paste0("fit.upper <- try(glm(",form.temp,",data=dataset,family=dist(",link,")),silent=T)")
            if(!is.null(offset)) command_str <- paste0("fit.upper <- try(glm(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,family=dist(",link,")),silent=T)")
            eval(parse(text=command_str))
          }
          if(type=='negabino'){
            if(is.null(offset)) command_str <- paste0("fit.upper <- try(glm.nb(",form.temp,",data=dataset,link=",link,"),silent=T)")
            if(!is.null(offset)) command_str <- paste0("fit.upper <- try(glm.nb(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,link=",link,"),silent=T)")
            eval(parse(text=command_str))
          }					
          if(type=='coxph') {
            form.temp <- paste0("survobj ~ ",gsub("-1","",new.predictor))
            fm <- formula(form.temp)
            fit.upper <- try(survival::coxph(fm, data=dataset),silent=T)
          }
          if(type=='clogit') {
            form.temp <- paste0(dep_var,'_ ~ ',new.predictor,'+ strata(',cond_var,')')
            fm <- formula(form.temp)
            fit.upper <- try(survival::clogit(fm, method='exact', data=dataset),silent=T)
          }					
          
          if(!'try-error'%in%class(fit.upper)){
            DF <- ifelse(type%in%c('lm','wlm','binom','poisson','negabino'),fit.lower$df.residual-fit.upper$df.residual,ifelse(type=='multinom',fit.lower@df.residual-fit.upper@df.residual,length(which(!is.na(fit.upper$coef)))-length(which(!is.na(fit.lower$coef)))))
            #if(type=='clogit') DF <- anova(fit.upper,fit.lower)$Df[2]
            tmp.AIC.table <- data.frame(term=paste('+',jj),DF=DF,AIC=AIC(fit.upper))
            AIC.table <- rbind(AIC.table,tmp.AIC.table)
            jjj=jjj+1
            all.fit[[jjj]] <- fit.upper
          }
        }
      }
      Best <- temp.best <- which(AIC.table$AIC==min(AIC.table$AIC,na.rm=TRUE))
      if(length(Best)>1) {
        if(1%in%Best) {
          Best <- 1
        } else {
          temp.df <- AIC.table[Best,2]
          min.df <- Best[temp.df==min(temp.df,na.rm=T)]
          if(length(min.df)==1){
            Best <- min.df
          } else {
            Best <- sample(Best,1)
          }
        }
        temp.best <- c(Best,temp.best[!temp.best==Best])
      }
      fit.lower <- all.fit[[Best]]
      AIC.table <- AIC.table[order(AIC.table$AIC),]
      AIC.table[,-1] <- Digits(AIC.table[,-1])
      new.rn <- row.names(AIC.table)
      new.rn[1:length(temp.best)] <- temp.best
      AIC.table <- AIC.table[new.rn,]
      AICtable[[j]] <- AIC.table
      if(AIC.table[1,1]=='none') {
        break
      } else {
        if(is.null(keep_var)&j==1) {
          predictor <- add.vars[as.numeric(row.names(AIC.table)[1])-1]
        } else {
          predictor <- paste(predictor,'+',add.vars[as.numeric(row.names(AIC.table)[1])-1])
        }
      }
    }
    if(noint & !type %in% c('clogit','coxph')) {currModel <- paste(gsub(' -1','',currModel),'(Intercept is not included)')}
    for(j in seq(length(currAIC))){
      Title <- ifelse(j==length(currAIC),paste0("Step ",j," (Final)"),paste0("Step ",j))
      R2HTML::HTML(R2HTML::as.title(Title),HR=hr+2,file=local_output,append=TRUE)
      R2HTML::HTML(paste0("AIC=",Digits(currAIC[j])),file=local_output,align="left")
      R2HTML::HTML(currModel[j],file=local_output,align="left")
      if(j==length(currAIC)) {
        R2HTML::HTML(AICtable[[j]],file=local_output,align="left",row.names=F,digits=15,caption=cap) ## tab_idx_2
      } else {
        R2HTML::HTML(AICtable[[j]],file=local_output,align="left",row.names=F,digits=15) ## tab_idx_2
      }
    }
  } else if (direct=='backward') {
    tmp.vars <- vars
    fit.upper <- object
    AICtable <- list()
    currAIC <- currModel <- c()
    j=0
    while(1){
      j=j+1
      rm.vars <- tmp.vars[!tmp.vars%in%keep_var]
      if(noint & length(tmp.vars)==1) rm.vars <- character(0)
      AIC.table <- data.frame(term='none',DF=as.numeric(NA),AIC=AIC(fit.upper))
      currAIC <- c(currAIC,AIC(fit.upper))
      if(type=='coxph'){
        currModel <- c(currModel,paste0('Hazard ratio ~ exp(',paste0(paste(tmp.vars,collapse='+')),')'))
        
      } else {
        if(type=='clogit') { currModel <- c(currModel,paste(dep_var,'~',paste(tmp.vars,collapse='+'))) }
        else { currModel <- c(currModel,paste(dep_var,'~',paste(tmp.vars,collapse='+'),ifelse(noint,'-1','')))}
      }
      
      jjj=1
      all.fit <- list(fit.upper)
      if(length(rm.vars)!=0){
        for(jj in rm.vars){
          new.predictor <- ifelse(length(tmp.vars[!tmp.vars%in%jj])==0,1,paste(tmp.vars[!tmp.vars%in%jj],collapse='+'))
          new.predictor <- paste(new.predictor,ifelse(noint,'-1',''))
          form.temp <- paste(dep_var,'~',new.predictor)
          
          if(type=='lm') fit.lower <- lm(as.formula(form.temp),data=dataset)
          if(type=='wlm') fit.lower <- lm(as.formula(form.temp),data=dataset,weight=WTS)
          if(type=='binom') {
            command_str <- paste0("fit.lower <- glm(",form.temp,",data=dataset,family=binomial(",link,"))")
            eval(parse(text=command_str))
          }
          if(type=='multinom'){
            if (res_type == "nominal") {
              fit.lower <- try(suppressWarnings(VGAM::vglm(as.formula(form.temp), data=dataset, family=dist(refLevel = baseline, parallel=tmp.parallel))))
            } else {
              command_str	<- paste0("try(fit.lower <- VGAM::vglm(",form.temp,", data=dataset, family=dist(link=", link, ", parallel=tmp.parallel)))") ;
              suppressWarnings(eval(parse(text=command_str)))
            }
          }
          if(type=='poisson'){
            if(is.null(offset)) command_str <- paste0("fit.lower <- try(glm(",form.temp,",data=dataset,family=dist(",link,")),silent=TRUE)")
            if(!is.null(offset)) command_str <- paste0("fit.lower <- try(glm(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,family=dist(",link,")),silent=TRUE)")
            eval(parse(text=command_str))
          }
          if(type=='negabino'){
            if(is.null(offset)) command_str <- paste0("fit.lower <- try(glm.nb(",form.temp,",data=dataset,link=",link,"),silent=TRUE)")
            if(!is.null(offset)) command_str <- paste0("fit.lower <- try(glm.nb(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,link=",link,"),silent=TRUE)")
            eval(parse(text=command_str))
          }					
          if(type=='coxph') {
            form.temp <- paste0("survobj ~ ",gsub("-1","",new.predictor))
            fm <- formula(form.temp)
            fit.lower <- survival::coxph(fm, data=dataset)
          }
          if(type=='clogit') {
            form.temp <- paste0(dep_var,'_ ~ ',gsub("-1","",new.predictor),'+ strata(',cond_var,')')
            fm <- formula(form.temp)
            fit.lower <- survival::clogit(fm, method='exact', data=dataset)
          }
          
          if(!'try-error'%in%class(fit.lower)){
            DF <- ifelse(type%in%c('lm','wlm','binom','poisson','negabino'),fit.lower$df.residual-fit.upper$df.residual,ifelse(type=='multinom',fit.lower@df.residual-fit.upper@df.residual,length(which(!is.na(fit.upper$coef)))-length(which(!is.na(fit.lower$coef)))))
            #if(type=='clogit') DF <- anova(fit.upper,fit.lower)$Df[2]
            tmp.AIC.table <- data.frame(term=paste('-',jj),DF=DF,AIC=AIC(fit.lower))
            AIC.table <- rbind(AIC.table,tmp.AIC.table)
            jjj=jjj+1
            all.fit[[jjj]] <- fit.lower
          }
        }
      }
      Best <- temp.best <- which(AIC.table$AIC==min(AIC.table$AIC,na.rm=TRUE))
      if(length(Best)>1) {
        if(1%in%Best) {
          Best <- 1
        } else {
          temp.df <- AIC.table[Best,2]
          min.df <- Best[temp.df==min(temp.df,na.rm=T)]
          if(length(min.df)==1){
            Best <- min.df
          } else {
            Best <- sample(Best,1)
          }
        }
        temp.best <- c(Best,temp.best[!temp.best==Best])
      }
      fit.upper <- all.fit[[Best]]
      AIC.table <- AIC.table[order(AIC.table$AIC),]
      AIC.table[,-1] <- Digits(AIC.table[,-1])
      new.rn <- row.names(AIC.table)
      new.rn[1:length(temp.best)] <- temp.best
      AIC.table <- AIC.table[new.rn,]
      AICtable[[j]] <- AIC.table
      if(AIC.table[1,1]=='none') {
        break
      } else {
        tmp.vars <- tmp.vars[!tmp.vars%in%rm.vars[as.numeric(row.names(AIC.table)[1])-1]]
        if(length(tmp.vars)==0){
          if(!noint){
            j=j+1
            currAIC <- c(currAIC,AIC(fit.upper))
            currModel <- c(currModel,paste(dep_var,'~ (Intercept)'))
            AIC.table <- data.frame(term='none',DF=as.numeric(NA),AIC=AIC(fit.upper))
            AIC.table[,-1] <- Digits(AIC.table[,-1])
            AICtable[[j]] <- AIC.table
          }
          break
        }
      }
    }
    if(noint & !type %in% c('clogit','coxph')) {currModel <- paste(gsub(' -1','',currModel),'(Intercept is not included)')}
    for(j in seq(length(currAIC))){
      Title <- ifelse(j==length(currAIC),paste0("Step ",j," (Final)"),paste0("Step ",j))
      R2HTML::HTML(R2HTML::as.title(Title),HR=hr+2,file=local_output,append=TRUE)
      R2HTML::HTML(paste0("AIC=",Digits(currAIC[j])),file=local_output,align="left")
      R2HTML::HTML(currModel[j],file=local_output,align="left")
      if(j==length(currAIC)) {
        R2HTML::HTML(AICtable[[j]],file=local_output,align="left",row.names=F,digits=15,caption=cap) ## tab_idx_2
      } else {
        R2HTML::HTML(AICtable[[j]],file=local_output,align="left",row.names=F,digits=15) ## tab_idx_2
      }
    }
  } else {
    tmp.vars <- vars
    fit.model <- object
    AICtable <- list()
    currAIC <- currModel <- c()
    j=0
    while(1){
      j=j+1
      rm.vars <- tmp.vars[!tmp.vars%in%keep_var]
      if(noint & length(tmp.vars)==1) rm.vars <- character(0)
      add.vars <- vars[!vars%in%tmp.vars]
      AIC.table <- data.frame(term='none',DF=as.numeric(NA),AIC=AIC(fit.model))
      currAIC <- c(currAIC,AIC(fit.model))
      if(type=='coxph'){
        currModel <- c(currModel,paste0('Hazard ratio ~ exp(',ifelse(length(tmp.vars)==0,'(Intercept)',paste0(paste(tmp.vars,collapse='+'))),')'))
        
      } else {
        if(type=='clogit') { currModel <- c(currModel,paste(dep_var,'~',ifelse(length(tmp.vars)==0,'(Intercept)',paste0(paste(tmp.vars,collapse='+'))))) }
        if(type!='clogit') { currModel <- c(currModel,paste(dep_var,'~',ifelse(length(tmp.vars)==0,'(Intercept)',paste0(paste(tmp.vars,collapse='+'),ifelse(noint,' -1',''))))) }
      }
      
      jjj=1
      all.fit <- list(fit.model)
      if(length(rm.vars)!=0){
        for(jj in rm.vars){
          new.predictor <- ifelse(length(tmp.vars[!tmp.vars%in%jj])==0,1,paste0(paste(tmp.vars[!tmp.vars%in%jj],collapse='+'),ifelse(noint,' -1','')))
          form.temp <- paste(dep_var,'~',new.predictor)
          
          if(type=='lm') fit.lower <- try(lm(as.formula(form.temp),data=dataset),silent=T)
          if(type=='wlm') fit.lower <- try(lm(as.formula(form.temp),data=dataset,weight=WTS),silent=T)
          if(type=='binom') {
            command_str <- paste0("fit.lower <- try(glm(",form.temp,",data=dataset,family=binomial(",link,")),silent=T)")
            eval(parse(text=command_str))
          }
          if(type=='multinom'){
            if (res_type == "nominal") {
              fit.lower <- try(suppressWarnings(VGAM::vglm(as.formula(form.temp), data=dataset, family=dist(refLevel = baseline, parallel=tmp.parallel))))
            } else {
              command_str	<- paste0("try(fit.lower <- VGAM::vglm(",form.temp,", data=dataset, family=dist(link=", link, ", parallel=tmp.parallel)))") ;
              suppressWarnings(eval(parse(text=command_str)))
            }
          }
          if(type=='poisson'){
            if(is.null(offset)) command_str <- paste0("fit.lower <- try(glm(",form.temp,",data=dataset,family=dist(",link,")),silent=T)")
            if(!is.null(offset)) command_str <- paste0("fit.lower <- try(glm(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,family=dist(",link,")),silent=T)")
            eval(parse(text=command_str))
          }
          if(type=='negabino'){
            if(is.null(offset)) command_str <- paste0("fit.lower <- try(glm.nb(",form.temp,",data=dataset,link=",link,"),silent=T)")
            if(!is.null(offset)) command_str <- paste0("fit.lower <- try(glm.nb(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,link=",link,"),silent=T)")
            eval(parse(text=command_str))
          }					
          if(type=='coxph') {
            form.temp <- paste0("survobj ~ ",gsub("-1","",new.predictor))
            fm <- formula(form.temp)
            fit.lower <- try(survival::coxph(fm, data=dataset),silent=T)
          }
          if(type=='clogit') {
            form.temp <- paste0(dep_var,'_ ~ ',gsub("-1","",new.predictor),'+ strata(',cond_var,')')
            fm <- formula(form.temp)
            fit.lower <- try(survival::clogit(fm, method='exact', data=dataset),silent=T)
          }
          
          if(!'try-error'%in%class(fit.lower)){
            DF <- ifelse(type%in%c('lm','wlm','binom','poisson','negabino'),fit.lower$df.residual-fit.model$df.residual,ifelse(type=='multinom',fit.lower@df.residual-fit.model@df.residual,length(which(!is.na(fit.model$coef)))-length(which(!is.na(fit.lower$coef)))))
            tmp.AIC.table <- data.frame(term=paste('-',jj),DF=DF,AIC=AIC(fit.lower))
            AIC.table <- rbind(AIC.table,tmp.AIC.table)
            jjj=jjj+1
            all.fit[[jjj]] <- fit.lower
          }
        }
      }
      if(length(add.vars)!=0) {
        for(jj in add.vars){
          new.predictor <- ifelse(length(tmp.vars)==0,jj,paste(paste(tmp.vars,collapse='+'),'+',jj,ifelse(noint,' -1','')))
          form.temp <- paste(dep_var,'~',new.predictor)
          
          if(type=='lm') fit.upper <- try(lm(as.formula(form.temp),data=dataset),silent=T)
          if(type=='wlm') fit.upper <- try(lm(as.formula(form.temp),data=dataset,weight=WTS),silent=T)
          if(type=='binom') {
            command_str <- paste0("fit.upper <- try(glm(",form.temp,",data=dataset,family=binomial(",link,")),silent=T)")
            eval(parse(text=command_str))
          }
          if(type=='multinom'){
            if (res_type == "nominal") {
              fit.upper <- try(suppressWarnings(VGAM::vglm(as.formula(form.temp), data=dataset, family=dist(refLevel = baseline, parallel=tmp.parallel))))
            } else {
              command_str	<- paste0("try(fit.upper <- VGAM::vglm(",form.temp,", data=dataset, family=dist(link=", link, ", parallel=tmp.parallel)))") ;
              suppressWarnings(eval(parse(text=command_str)))
            }
          }
          if(type=='poisson'){
            if(is.null(offset)) command_str <- paste0("fit.upper <- try(glm(",form.temp,",data=dataset,family=dist(",link,")),silent=T)")
            if(!is.null(offset)) command_str <- paste0("fit.upper <- try(glm(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,family=dist(",link,")),silent=T)")
            eval(parse(text=command_str))
          }
          if(type=='negabino'){
            if(is.null(offset)) command_str <- paste0("fit.upper <- try(glm.nb(",form.temp,",data=dataset,link=",link,"),silent=T)")
            if(!is.null(offset)) command_str <- paste0("fit.upper <- try(glm.nb(",paste(form.temp,paste0("+ offset(", paste(offset, collapse=" + "),")")),",data=dataset,link=",link,"),silent=T)")
            eval(parse(text=command_str))
          }					
          if(type=='coxph') {
            form.temp <- paste0("survobj ~ ",gsub("-1","",new.predictor))
            fm <- formula(form.temp)
            fit.upper <- try(survival::coxph(fm, data=dataset),silent=T)
          }
          if(type=='clogit') {
            form.temp <- paste0(dep_var,'_ ~ ',gsub("-1","",new.predictor),'+ strata(',cond_var,')')
            fm <- formula(form.temp)
            fit.upper <- try(survival::clogit(fm, method='exact', data=dataset),silent=T)
          }
          
          if(!'try-error'%in%class(fit.upper)){
            DF <- ifelse(type%in%c('lm','wlm','binom','poisson','negabino'),fit.model$df.residual-fit.upper$df.residual,ifelse(type=='multinom',fit.model@df.residual-fit.upper@df.residual,length(which(!is.na(fit.upper$coef)))-length(which(!is.na(fit.model$coef)))))
            tmp.AIC.table <- data.frame(term=paste('+',jj),DF=DF,AIC=AIC(fit.upper))
            AIC.table <- rbind(AIC.table,tmp.AIC.table)
            jjj=jjj+1
            all.fit[[jjj]] <- fit.upper
          }
        }
      }
      Best <- temp.best <- which(AIC.table$AIC==min(AIC.table$AIC,na.rm=TRUE))
      if(length(Best)>1) {
        if(1%in%Best) {
          Best <- 1
        } else {
          temp.df <- AIC.table[Best,2]
          min.df <- Best[temp.df==min(temp.df,na.rm=T)]
          if(length(min.df)==1){
            Best <- min.df
          } else {
            Best <- sample(Best,1)
          }
        }
        temp.best <- c(Best,temp.best[!temp.best==Best])
      }
      fit.model <- all.fit[[Best]]
      AIC.table <- AIC.table[order(AIC.table$AIC),]
      AIC.table[,-1] <- Digits(AIC.table[,-1])
      new.rn <- row.names(AIC.table)
      new.rn[1:length(temp.best)] <- temp.best
      AIC.table <- AIC.table[new.rn,]
      AICtable[[j]] <- AIC.table
      if(AIC.table[1,1]=='none') {
        break
      } else {
        tmp.res <- strsplit(as.character(AIC.table[1,1]),' ')[[1]]
        if(tmp.res[1]=='-'){
          tmp.vars <- tmp.vars[!tmp.vars%in%tmp.res[2]]
        } else {
          tmp.vars <- c(tmp.vars,tmp.res[2])
        }
      }
    }
    if(noint & !type %in% c('clogit','coxph')) {currModel <- paste(gsub(' -1','',currModel),'(Intercept is not included)')}
    for(j in seq(length(currAIC))){
      Title <- ifelse(j==length(currAIC),paste0("Step ",j," (Final)"),paste0("Step ",j))
      R2HTML::HTML(R2HTML::as.title(Title),HR=hr+2,file=local_output,append=TRUE)
      R2HTML::HTML(paste0("AIC=",Digits(currAIC[j])),file=local_output,align="left")
      R2HTML::HTML(currModel[j],file=local_output,align="left")
      if(j==length(currAIC)) {
        R2HTML::HTML(AICtable[[j]],file=local_output,align="left",row.names=F,digits=15,caption=cap) ## tab_idx_2
      } else {
        R2HTML::HTML(AICtable[[j]],file=local_output,align="left",row.names=F,digits=15) ## tab_idx_2
      }
    }
  }
  
  # Result of VS
  R2HTML::HTML(R2HTML::as.title("Result of Variable Selection"),HR=hr+1,file=local_output)
  resVS <- data.frame(Model=currModel[c(1,length(currModel))])
  row.names(resVS) <- c('Initial Model','Final Model')
  R2HTML::HTML(resVS,file=local_output,align="left") ## tab_idx_4
  
  R2HTML::HTML(R2HTML::as.title("Coefficient Estimates of the Final Model"),HR=hr+1,file=local_output)
  final.predictor <- gsub('\\(Intercept\\)','1',gsub('\\(Intercept is not included\\)','-1',gsub("^.+ ~ ","",currModel[length(currModel)])))
  form.final <- paste(dep_var,'~',final.predictor)
  fin_vars_temp <- unique(unlist(strsplit(final.predictor,'\\+|:')))
  fin_vars <- fin_vars_temp[which(fin_vars_temp!=1)]
  fin_vars2_temp <- unique(unlist(strsplit(final.predictor,'\\+')))
  fin_vars2 <- fin_vars2_temp[which(fin_vars2_temp!=1)]
  if(type=='lm') {
    fit.final <- lm(as.formula(form.final),data=dataset)
    CE <- summary(fit.final)$coef; colnames(CE)[2:3] <- c('SE', 'T-value') # 변경: Std.Error -> SE, t value -> T-value
    if(CI){
      tmp <- merge(CE,confint(fit.final, level=confint.level),by="row.names",all=TRUE,sort=F)
      rownames(tmp) <- tmp[,1]
      CE <- tmp[,-1]
      colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Estimate<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
    }
    if(VIF)	{
      if(final.predictor==1){
        warn.vif <- '<li> Warning : VIF is not supported for intercept-only model.'
      } else {
        VIF.1 <- rms::vif(fit.final)
        if(!noint) {
          VIF.1 <- c(NA,VIF.1)
          names(VIF.1)[1] <- "(Intercept)"
        }
        
        tmp <- merge(CE,VIF.1,by='row.names',all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        colnames(tmp)[ncol(tmp)] <- "VIF"
        CE <- tmp[,-1]
      }
    }
    if(VIP){
      if(final.predictor!=1){
        vip <- caret::varImp(fit.final)
        if(!noint) {
          vip <- rbind(NA,vip)
          rownames(vip)[1] <- "(Intercept)"
        }
        tmp <- merge(CE,vip,by='row.names',all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        colnames(tmp)[ncol(tmp)] <- "VIP"
        CE <- tmp[,-1]
      } else {
        warn.VIP <- "<li> Warning : Variable importance table is not supported for intercept only model."
      }
    }
    if(CI) R2HTML::HTML(Digits(CE),file=local_output,align="left",digits=15,caption="<div style='text-align:left'><small>* Wald confidence intervals were calculated.</small>") ## tab_idx_4 # 변경: caption 수정
    if(!CI) R2HTML::HTML(Digits(CE),file=local_output,align="left",digits=15) ## tab_idx_4
    if(exists('warn.VIP')) R2HTML::HTML(warn.VIP,file=local_output)
    if(exists('warn.vif')) R2HTML::HTML(warn.vif,file=local_output)
    # Anova table and R-squared
    if(ANOVA){
      R2HTML::HTML(R2HTML::as.title("Analysis-of-Variance Table"),HR=3,file=local_output)
      if(length(fin_vars)==0){
        warn.AT1 <- "<li> Warning : ANOVA table is not supported for intercept only model."
        R2HTML::HTML(warn.AT1,file=local_output)
      } else if(noint){
        warn.AT2 <- "<li> Warning : ANOVA table is not supported for the model without intercept."
        R2HTML::HTML(warn.AT2,file=local_output)
      } else {
        R2HTML::HTML(R2HTML::as.title("Model Effect (Goodness of Fit Test)"),HR=4,file=local_output)
        Anova <- as.matrix(anova(lm(formula(paste(dep_var,'~1',sep='')),x=TRUE,data=dataset),fit.final))
        A1 <- rbind(Anova[2,4:3], Anova[2:1,c(2,1)])
        MS <- c(A1[1:2,1]/A1[1:2,2],NA)
        A2 <- data.frame(A1,MS=MS,Fvalue=c(Anova[2,5],NA,NA),Pvalue=c(Anova[2,6],NA,NA),R2=c(summary(fit.final)$r.squared,NA,NA),adj.R2=c(summary(fit.final)$adj.r.squared,NA,NA))
        colnames(A2) <- c('SS','DF','MS','F-value','p-value','R<sup>2</sup>','adj.R<sup>2</sup>') # 변경: P-value -> p-value, R2 -> R<sup>2</sup>
        rownames(A2) <- c('Regression','Residual','Total')
        R2HTML::HTML(Digits(A2),file=local_output,align="left",digits=15) ## tab_idx_4
        
        R2HTML::HTML(R2HTML::as.title(paste0("Variable Effect with Type ",ss," SS")),HR=4,file=local_output)
        warn.desc <- ifelse(ss=='I',"<div style='text-align:left'><small>* In type I test, terms are added sequentially (first to last).</small>",
                            ifelse(ss=='II',"<div style='text-align:left'><small>* In type II test, each row is the testing result for each main effect after the other main effect.</small>",
                                   "<div style='text-align:left'><small>* In type III test, each row is the testing result for each effect after the other effect.</small>")) # 변경 : cation 수정
        
        if(ss=='I'){
          AT <- try(anova(fit.final,test="Chisq"),s=T)
          rowNames <- rownames(AT)
          if(class(AT)[1]!='try-error'){
            AT <- data.frame(AT)
            AT <- AT[,c(2,1,3,4,5)]
            rownames(AT) <- rowNames
            colnames(AT) <- c('SS','DF','MS','F-value','p-value') # 변경: P-value -> p-value
            R2HTML::HTML(Digits(AT),file=local_output,align="left",digits=15,
                         caption=warn.desc) ## tab_idx_4 # 변경 : caption 수정
          } else {
            warn.AT3 <- "<li> Error : Fail to fit the Model."
            R2HTML::HTML(warn.AT3,file=local_output)
          }
        }
        
        if(ss %in% c('II','III')){
          # II type : ignore interaction term
          # III type : calculate SS including interaction term
          RN <- final.predictor
          if(ss=='II' & length(grep(':',fin_vars2))>0) {
            RN <- fin_vars2[-grep(':',fin_vars2)]
            warn.AT4 <- '<li> Warning : Test for interaction effects is not provided in type II test. Use type III test for interaction effects.'
          }
          # warn.AT5 <- '<li> Note : If there is indeed no interaction, then type II is statistically more powerful than type III.'
          if(length(RN)>0){ ##20210813
            AT.form.full <- paste(dep_var,'~',paste(RN,collapse=' + '))
            full.fit <- lm(formula(AT.form.full),data=dataset)
            
            if(ss=='III') options(contrasts=Type3.contr)
            res <- try(car::Anova(lm(formula(AT.form.full),data=dataset),type=ss), silent = T)
            if(ss=='III') options(contrasts=Orig.contr)
            
            if(class(res)[1]!='try-error'){
              res <- data.frame(res)
              MS <- res[,1]/res[,2]
              AT <- data.frame(res[,c(1:2)],MS,res[,c(3,4)])
              colnames(AT) <- c('SS','DF','MS','F-value','p-value') # 변경: P-value -> p-value
              
              R2HTML::HTML(Digits(AT[-nrow(AT),]),file=local_output,align="left",digits=15) ## tab_idx_4
              R2HTML::HTML(warn.desc,file=local_output)
              if(exists('warn.AT4')) R2HTML::HTML(warn.AT4,file=local_output)
            } else {
              warn.AT6 <- "<li> Error : Fail to fit the Model."
              R2HTML::HTML(warn.AT6,file=local_output)
            }
          } else if(exists('warn.AT4')) R2HTML::HTML(warn.AT4,file=local_output)
        }
      }
    }		
  }
  if(type=='wlm') {
    fit.final <- lm(as.formula(form.final),data=dataset,weight=WTS)
    
    CE <- summary(fit.final)$coef; colnames(CE)[2:3] <- c('SE', 'T-value') # 변경: Std.Error -> SE, t value -> T-value
    if(CI){
      tmp <- merge(CE,confint(fit.final, level=confint.level),by="row.names",all=TRUE,sort=F)
      rownames(tmp) <- tmp[,1]
      CE <- tmp[,-1]
      colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Estimate<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
    }
    if(VIF)	{
      if(final.predictor==1){
        warn.vif <- '<li> Warning : VIF is not supported for intercept-only model.'
      } else {
        VIF.1 <- rms::vif(fit.final)
        if(!noint) {
          VIF.1 <- c(NA,VIF.1)
          names(VIF.1)[1] <- "(Intercept)"
        }
        
        tmp <- merge(CE,VIF.1,by='row.names',all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        colnames(tmp)[ncol(tmp)] <- "VIF"
        CE <- tmp[,-1]
      }
    }
    if(VIP){
      if(final.predictor!=1){
        vip <- caret::varImp(fit.final)
        if(!noint) {
          vip <- rbind(NA,vip)
          rownames(vip)[1] <- "(Intercept)"
        }
        
        tmp <- merge(CE,vip,by='row.names',all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        colnames(tmp)[ncol(tmp)] <- "VIP"
        CE <- tmp[,-1]
      } else {
        warn.VIP <- "<li> Warning : Variable importance table is not supported for intercept only model."
      }
    }
    if(CI) R2HTML::HTML(Digits(CE),file=local_output,align="left",digits=15,caption="<div style='text-align:left'><small>* Wald confidence intervals were calculated.</small>") ## tab_idx_4 # 변경: caption 수정
    if(!CI) R2HTML::HTML(Digits(CE),file=local_output,align="left",digits=15) ## tab_idx_4
    if(exists('warn.VIP')) R2HTML::HTML(warn.VIP,file=local_output)
    if(exists('warn.vif')) R2HTML::HTML(warn.vif,file=local_output)
    # Anova table and R-squared
    if(ANOVA){
      R2HTML::HTML(R2HTML::as.title("Analysis-of-Variance Table"),HR=3,file=local_output)
      if(length(fin_vars)==0){
        warn.AT1 <- "<li> Warning : ANOVA table is not supported for intercept only model."
        R2HTML::HTML(warn.AT1,file=local_output)
      } else if(noint){
        warn.AT2 <- "<li> Warning : ANOVA table is not supported for the model without intercept."
        R2HTML::HTML(warn.AT2,file=local_output)
      } else {
        R2HTML::HTML(R2HTML::as.title("Model Effect (Goodness of Fit Test)"),HR=4,file=local_output)
        Anova <- as.matrix(anova(lm(formula(paste(dep_var,'~1',sep='')),x=TRUE,data=dataset,weight=WTS),fit.final))
        A1 <- rbind(Anova[2,4:3], Anova[2:1,c(2,1)])
        MS <- c(A1[1:2,1]/A1[1:2,2],NA)
        A2 <- data.frame(A1,MS=MS,Fvalue=c(Anova[2,5],NA,NA),Pvalue=c(Anova[2,6],NA,NA),R2=c(summary(fit.final)$r.squared,NA,NA),adj.R2=c(summary(fit.final)$adj.r.squared,NA,NA))
        colnames(A2) <- c('SS','DF','MS','F-value','p-value','R<sup>2</sup>','adj.R<sup>2</sup>') # 변경: P-value -> p-value, R2 -> R<sup>2</sup>
        rownames(A2) <- c('Regression','Residual','Total')
        R2HTML::HTML(Digits(A2),file=local_output,align="left",digits=15) ## tab_idx_4
        
        R2HTML::HTML(R2HTML::as.title(paste0("Variable Effect with Type ",ss," SS")),HR=4,file=local_output)
        warn.desc <- ifelse(ss=='I',"<div style='text-align:left'><small>* In type I test, terms are added sequentially (first to last).</small>",
                            ifelse(ss=='II',"<div style='text-align:left'><small>* In type II test, each row is the testing result for each main effect after the other main effect.</small>",
                                   "<div style='text-align:left'><small>* In type III test, each row is the testing result for each effect after the other effect.</small>"))
        
        if(ss=='I'){
          AT <- try(anova(fit.final,test="Chisq"),s=T)
          rowNames <- rownames(AT)
          if(class(AT)[1]!='try-error'){
            AT <- data.frame(AT)
            AT <- AT[,c(2,1,3,4,5)]
            rownames(AT) <- rowNames
            colnames(AT) <- c('SS','DF','MS','F-value','p-value') # 변경: P-value -> p-value
            R2HTML::HTML(Digits(AT),file=local_output,align="left",digits=15,
                         caption=warn.desc) ## tab_idx_4 # 변경 : caption 수정
          } else {
            warn.AT3 <- "<li> Error : Fail to fit the Model."
            R2HTML::HTML(warn.AT3,file=local_output)
          }
        }
        
        if(ss %in% c('II','III')){
          # II type : ignore interaction term
          # III type : calculate SS including interaction term
          RN <- final.predictor
          if(ss=='II' & length(grep(':',fin_vars2))>0) {
            RN <- fin_vars2[-grep(':',fin_vars2)]
            warn.AT4 <- '<li> Warning : Test for interaction effects is not provided in type II test. Use type III test for interaction effects.'
          }
          # warn.AT5 <- '<li> Note : If there is indeed no interaction, then type II is statistically more powerful than type III.'
          if(length(RN)>0){
            AT.form.full <- paste(dep_var,'~',paste(RN,collapse=' + '))
            full.fit <- lm(formula(AT.form.full),data=dataset)
            
            if(ss=='III') options(contrasts=Type3.contr)
            res <- try(car::Anova(lm(formula(AT.form.full),data=dataset),type=ss), silent = T)
            if(ss=='III') options(contrasts=Orig.contr)
            
            if(class(res)[1]!='try-error'){
              res <- data.frame(res)
              MS <- res[,1]/res[,2]
              AT <- data.frame(res[,c(1:2)],MS,res[,c(3,4)])
              colnames(AT) <- c('SS','DF','MS','F-value','p-value') # 변경: P-value -> p-value
              
              R2HTML::HTML(Digits(AT[-nrow(AT),]),file=local_output,align="left",digits=15) ## tab_idx_4
              R2HTML::HTML(warn.desc,file=local_output)
              if(exists('warn.AT4')) R2HTML::HTML(warn.AT4,file=local_output)
            } else {
              warn.AT6 <- "<li> Error : Fail to fit the Model."
              R2HTML::HTML(warn.AT6,file=local_output)
            }
          } else if(exists('warn.AT4')) R2HTML::HTML(warn.AT4,file=local_output)
        }
      }
    }
  }
  if(type=='binom') {
    command_str <- paste0("fit.final <- glm(",form.final,",data=dataset,family=binomial(",link,"))")
    eval(parse(text=command_str))
    
    CE <- as.data.frame(summary(fit.final)$coef)
    colnames(CE) <- c('Estimate','SE','Z-value','p-value') # 변경: P-value -> p-value
    if(CI){
      #CE_conf <- confint(fit.final, level=confint.level)
      CE_conf <- confint.default(fit.final, level=confint.level)
      CE_conf_1 <- cbind.data.frame(matrix(CE_conf,ncol=2))
      #row.names(CE_conf_1) <- row.names(CE)
      row.names(CE_conf_1) <- names(fit.final$coef)
      tmp <- merge(CE,CE_conf_1,by="row.names",all=TRUE,sort=F)
      rownames(tmp) <- tmp[,1]
      CE <- tmp[,-1]
      colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Estimate<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
    }
    
    if(odds) {
      ORs <- exp(CE[,1,drop=F])
      colnames(ORs) <- 'exp(Estimate)'
      if(link=='logit') colnames(ORs) <- 'Odds ratio'
      CE <- cbind(CE,ORs)
      CE <- CE[,c(1,ncol(CE),2:(ncol(CE)-1))]
      if(CI){
        tmp <- merge(CE,exp(CE_conf_1),by="row.names",all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        CE <- tmp[,-1]
        colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for exp(Estimate)<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
        if(link=='logit') colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Odds ratio<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
      }
    }
    
    if(VIF)	{
      if(final.predictor==1){
        warn.vif <- '<li> Warning : VIF is not supported for intercept-only model.'
      } else {
        VIF.1 <- rms::vif(fit.final)
        if(!noint) {
          VIF.1 <- c(NA,VIF.1)
          names(VIF.1)[1] <- "(Intercept)"
        }
        
        tmp <- merge(CE,VIF.1,by='row.names',all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        colnames(tmp)[ncol(tmp)] <- "VIF"
        CE <- tmp[,-1]
      }
    }
    if(VIP){
      if(final.predictor!=1){
        vip <- varImp(fit.final)
        if(!noint) {
          vip <- rbind(NA,vip)
          rownames(vip)[1] <- "(Intercept)"
        }
        
        tmp <- merge(CE,vip,by='row.names',all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        colnames(tmp)[ncol(tmp)] <- "VIP"
        CE <- tmp[,-1]
      } else {
        warn.msg9 <- "<li> Warning : Variable importance table is not supported for intercept only model."
      }
    }
    if(CI) R2HTML::HTML(Digits(CE),file=local_output,align="left",digits=15,caption="<div style='text-align:left'><small>* Wald confidence intervals were calculated.</small>") ## tab_idx_4 # 변경: caption 수정
    if(!CI) R2HTML::HTML(Digits(CE),file=local_output,align="left",digits=15) ## tab_idx_4
    if(exists('warn.msg9')) R2HTML::HTML(warn.msg9,file=local_output)
    if(exists('warn.vif')) R2HTML::HTML(warn.vif,file=local_output)
    if(ANOVA){
      R2HTML::HTML(R2HTML::as.title("Analysis-of-Deviance Table"),HR=3,file=local_output)
      if(length(fin_vars)==0) { warn.msg8 <- "<li> Warning : ANOVA table is not supported for intercept only model."
      R2HTML::HTML(warn.msg8,file=local_output)
      } else if(noint ){
        warn.AT1 <- "<li> Warning : ANOVA table is not supported for the model without intercept."
        R2HTML::HTML(warn.AT1,file=local_output)
      } else {
        R2HTML::HTML(R2HTML::as.title("Model Effect (Goodness of Fit Test)"),HR=4,file=local_output)
        command_str<- paste0("null.fit<- try(glm(formula(gsub('~.+','~ 1',form.final)), data=dataset, family=binomial(",link,")))")
        eval(parse(text=command_str)) ;
        model.fit <- data.frame(anova(null.fit,fit.final))
        model.fit <- model.fit[c(2,1),c(2,1,4,3)]
        model.fit <- cbind(model.fit,c(pchisq(model.fit[1,3],model.fit[1,4],lower.tail=F),NA))
        rownames(model.fit) <- c('Proposed model','Null model')
        colnames(model.fit) <- c('Deviance','DF(Deviacne)','χ<sup>2</sup>','DF(χ<sup>2</sup>)','p-value') # 변경: Chisq -> χ<sup>2</sup>, P-value -> p-value
        anova.msg <- "<div style='text-align:left'><small>* 'Null model' means the model including only intercept and 'Proposed model' means the model including all explanatory variables including interaction effect.</small>"
        R2HTML::HTML(Digits(model.fit),file=local_output,align="left",digits=15,
                     caption=anova.msg) ## tab_idx_4 # 변경 : caption 수정
        
        R2HTML::HTML(R2HTML::as.title(paste0("Variable Effect with Type ",ss," SS")),HR=4,file=local_output)
        warn.desc <- ifelse(ss=='I',"<div style='text-align:left'><small>* In type I test, 'Null model' means the model including only intercept. Terms are added sequentially (first to last).</small>",
                            ifelse(ss=='II',"<div style='text-align:left'><small>* In type II test, 'Proposed model' means the model including all explanatory variables except interaction effect. The other rows are the testing results for each main effect after the other main effect.</small>",
                                   "<div style='text-align:left'><small>* In type III test, 'Proposed model' means the model including all explanatory variables including interaction effect. The other rows are the testing results for each effect after the other effect.</small>")) # 변경 : caption 수정
        
        if(ss=='I'){
          AT <- try(anova(fit.final,test="Chisq"),s=T)
          rowNames <- rownames(AT)
          if(class(AT)[1]!='try-error'){
            AT <- data.frame(AT)
            AT <- AT[,c(4,3,2,1,5)]
            rownames(AT) <- rowNames; rownames(AT)[1] <- 'Null model'
            colnames(AT) <- c('Deviance','DF(Deviacne)','χ<sup>2</sup>','DF(χ<sup>2</sup>)','p-value') # 변경: Chisq -> χ<sup>2</sup>, P-value -> p-value
            R2HTML::HTML(Digits(AT),file=local_output,align="left",digits=15,
                         caption=warn.desc) ## tab_idx_4 # 변경 : caption 수정
          } else {
            warn.msg6 <- "<li> Error : Fail to fit the Model."
            R2HTML::HTML(warn.msg6,file=local_output)
          }
        }
        
        if(ss %in% c('II','III')){
          # II type : ignore interaction term
          # III type : calculate SS including interaction term
          RN <- final.predictor
          if(ss=='II' & length(grep(':',fin_vars2))>0) {
            RN <- fin_vars2[-grep(':',fin_vars2)]
            warn.AT2 <- '<li> Warning : Test for interaction effects is not provided in type II test. Use type III test for interaction effects.'
          }
          # warn.AT3 <- '<li> Note : If there is indeed no interaction, then type II is statistically more powerful than type III.'
          if(length(RN)>0){
            AT.form.full <- paste(dep_var,'~',paste(RN,collapse=' + '))
            command_str<- paste0("full.fit<- try(glm(formula(AT.form.full), data=dataset, family=binomial(",link,")))")
            
            eval(parse(text=command_str))
            if(ss=='III') options(contrasts=Type3.contr)
            res <- try(car::Anova(glm(formula(AT.form.full), data=dataset, family=binomial(link)),type=ss,test="LR"))
            if(ss=='III') options(contrasts=Orig.contr)
            
            if(class(res)[1]!='try-error'){
              dev <- deviance(full.fit)
              dev.df <- df.residual(full.fit)
              devs <- dev+res[,1]
              dev.dfs <- dev.df + res[,2]
              AT <- data.frame(c(dev,devs),c(dev.df,dev.dfs),c(NA,res[,1]),c(NA,res[,2]),c(NA,res[,3]))
              colnames(AT) <- c('Deviance','DF(Deviacne)','χ<sup>2</sup>','DF(χ<sup>2</sup>)','p-value') # 변경: chisq -> χ<sup>2</sup>, P-value -> p-value
              rownames(AT) <- c('Proposed model',rownames(res))
              
              R2HTML::HTML(Digits(AT),file=local_output,align="left",digits=15) ## tab_idx_4
              R2HTML::HTML(warn.desc,file=local_output)
              if(exists('warn.AT2')) R2HTML::HTML(warn.AT2,file=local_output)
            } else {
              warn.msg7 <- "<li> Error : Fail to fit the Model."
              R2HTML::HTML(warn.msg7,file=local_output)
            }
          } else if(exists('warn.AT2')) R2HTML::HTML(warn.AT2,file=local_output)
        }
      }
    }
  }
  if(type=='multinom'){
    if (res_type == "nominal") {
      fit.final <- try(suppressWarnings(VGAM::vglm(as.formula(form.final), data=dataset, family=dist(refLevel = baseline, parallel=tmp.parallel))))
    } else {
      command_str	<- paste0("try(fit.final <- VGAM::vglm(",form.final,", data=dataset, family=dist(link=", link, ", parallel=tmp.parallel)))") ;
      suppressWarnings(eval(parse(text=command_str)))
    }
    
    CE <- data.frame(coef(summary(fit.final))) ;
    colnames(CE) <- c("Estimate","SE","Z-value","p-value") # 변경: Std.Error -> SE, P-value -> p-value
    if(CI){
      tmp <- merge(CE,confint(fit.final, level=confint.level),by="row.names",all=TRUE,sort=F)
      rownames(tmp) <- tmp[,1]
      CE <- tmp[,-1]
      colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Estimate<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
    }
    if(exp_estim){
      CE <- cbind(CE,'Odds ratio'=exp(CE[,1]))
      CE <- CE[,c(1,ncol(CE),2:(ncol(CE)-1))]
      if(CI) {
        tmp <- merge(CE,exp(confint(fit.final, level=confint.level)),by="row.names",all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        CE <- tmp[,-1]
        if(res_type=='ordinal') { colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Odds ratio<sup>*</sup>') } # 변경: UCI / LCI 통일 + 들여쓰기 수정
        if(res_type=='nominal') { colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for exp(Estimate)<sup>*</sup>') } # 변경: UCI / LCI 통일 + 들여쓰기 수정
      }
    }
    
    if(res_type=='nominal'){
      tt <- sapply(printlevel, function(y) paste0("log(P(",dep_var,"=",y,")/P(",dep_var,"=",baseline,"))"))
    } else {
      tt <- sapply(seq(length(printlevel)), function(y) paste0(link, "(",printlevel[y],")"))
    }
    if(any(norm.parallel, ord.parallel)){
      for(ii in 1:length(printlevel)) {
        R2HTML::HTML(R2HTML::as.title(tt[ii]), HR=hr+2, file=local_output)
        tmp.CE <- CE[c(ii,(1+length(printlevel)):nrow(CE)),]
        rownames(tmp.CE) <- gsub(paste0(":",ii,"$"),"",rownames(tmp.CE))
        if(CI) R2HTML::HTML(Digits(tmp.CE), file=local_output, na="", align="left", digits=15,caption="<div style='text-align:left'><small>* Wald confidence intervals were calculated.</small>") ## tab_idx_4 # 변경: caption 수정
        if(!CI) R2HTML::HTML(Digits(tmp.CE), file=local_output, na="", align="left", digits=15) ## tab_idx_4
      }
    } else {
      for(ii in 1:length(printlevel)) {
        R2HTML::HTML(R2HTML::as.title(tt[ii]), HR=hr+2, file=local_output)
        tmp.CE <- CE[seq(from=ii,to=nrow(CE),by=length(printlevel)),]
        rownames(tmp.CE) <- gsub(paste0(":",ii,"$"),"",rownames(tmp.CE))
        if(CI) R2HTML::HTML(Digits(tmp.CE), file=local_output, na="", align="left", digits=15,caption="<div style='text-align:left'><small>* Wald confidence intervals were calculated.</small>") ## tab_idx_4 # 변경: caption 수정
        if(!CI) R2HTML::HTML(Digits(tmp.CE), file=local_output, na="", align="left", digits=15) ## tab_idx_4
      }
    }
    # ANOVA Table
    if (ANOVA) {
      R2HTML::HTML(R2HTML::as.title(paste0("ANOVA Table with Type ",ss," SS")),HR=3,file=local_output)
      if (length(fin_vars)==0){
        R2HTML::HTML("Testing is not available when the model contains only an intercept.", file=local_output, na="")
      } else {
        if(ss%in% c(2,'II')) res<-try(car::Anova(fit.final,test="F",vcov.=vcovvlm,type='II'))
        if(ss%in% c(3,'III')){
          options(contrasts=Type3.contr)
          if (res_type == "nominal") {
            fit.final.type3 <- try(suppressWarnings(VGAM::vglm(as.formula(form.final), data=dataset, family=dist(refLevel = baseline, parallel=tmp.parallel))))
          } else {
            command_str	<- paste0("try(fit.final.type3 <- VGAM::vglm(",form.final,", data=dataset, family=dist(link=", link, ", parallel=tmp.parallel)))") ;
            suppressWarnings(eval(parse(text=command_str)))
          }
          res<-try(car::Anova(fit.final.type3,test="F",vcov.=vcovvlm,type='III'))
          options(contrasts=Orig.contr)
        }
        table_tmp	<- data.frame(res) ;
        colnames(table_tmp)	<- c("DF", "F-value", "p-value") ; # 변경: F -> F-value, P-value -> p-value
        R2HTML::HTML(Digits(table_tmp), file=local_output, align="left",digits=15) ; ## tab_idx_4
      }
    }
  }
  if(type %in% c('poisson','negabino')){
    if(type=='poisson') {
      if(!is.null(offset)) command_str <- paste0("fit.final <- try(glm(",form.final,"+offset(",paste(offset, collapse=" + "),"),data=dataset,family=dist(",link,")),silent=TRUE)")
      if(is.null(offset)) command_str <- paste0("fit.final <- try(glm(",form.final,",data=dataset,family=dist(",link,")),silent=TRUE)")
    }
    if(type=='negabino') {
      if(!is.null(offset)) command_str <- paste0("fit.final <- try(glm.nb(",form.final,"+offset(",paste(offset, collapse=" + "),"),data=dataset,link=",link,"),silent=TRUE)")
      if(is.null(offset)) command_str <- paste0("fit.final <- try(glm.nb(",form.final,",data=dataset,link=",link,"),silent=TRUE)")
    }		
    eval(parse(text=command_str))
    
    CE <- data.frame(coef(summary(fit.final))) ;
    colnames(CE) <- c("Estimate","SE","Z-value","p-value") # 변경: Std.Error -> SE, P-value -> p-value
    if(CI) {
      tmp <- merge(CE,confint.default(fit.final, level=confint.level),by="row.names",all=TRUE,sort=F)
      rownames(tmp) <- tmp[,1]
      CE <- tmp[,-1]
      colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Estimate<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
    }
    if(exp_estim){
      CE <- cbind(CE,'Odds ratio'=exp(CE[,1]))
      CE <- CE[,c(1,ncol(CE),2:(ncol(CE)-1))]
      if(CI) {
        tmp <- merge(CE,exp(confint.default(fit.final, level=confint.level)),by="row.names",all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        CE <- tmp[,-1]
        colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for exp(Estimate)<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
      }
    }
    if(VIF)	{
      if(final.predictor==1){
        warn.vif <- '<li> Warning : VIF is not supported for intercept-only model.'
      } else {
        VIF.1 <- rms::vif(fit.final)
        if(!noint) {
          VIF.1 <- c(NA,VIF.1)
          names(VIF.1)[1] <- "(Intercept)"
        }
        
        tmp <- merge(CE,VIF.1,by='row.names',all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        colnames(tmp)[ncol(tmp)] <- "VIF"
        CE <- tmp[,-1]
      }
    }
    if(CI) R2HTML::HTML(Digits(CE),file=local_output,align="left",digits=15,caption="<div style='text-align:left'><small>* Wald confidence intervals were calculated.</small>") ## tab_idx_4 # 변경: caption 수정
    if(!CI) R2HTML::HTML(Digits(CE),file=local_output,align="left",digits=15) ## tab_idx_4
    if(exists('warn.vif')) R2HTML::HTML(warn.vif,file=local_output)
    if(ANOVA){
      R2HTML::HTML(R2HTML::as.title("Analysis-of-Deviance Table"),HR=3,file=local_output)
      if(length(fin_vars)==0){
        warn.AT0 <- "<li> Warning : ANOVA table is not supported for intercept only model."
        R2HTML::HTML(warn.AT0,file=local_output)
      } else if(noint){
        warn.AT1 <- "<li> Warning : ANOVA table is not supported for the model without intercept."
        R2HTML::HTML(warn.AT1,file=local_output)
      } else {
        R2HTML::HTML(R2HTML::as.title("Model Effect (Goodness of Fit Test)"),HR=4,file=local_output)
        form.null <- paste0(gsub('~.+','~ 1',form.final),ifelse(class(offset)=='character',paste0('+offset(',offset,')'),''))
        if(type=='poisson') command_str<- paste0("null.fit<- try(glm(formula(",form.null,"), data=dataset, family=",'poisson',"(",link,")))")
        if(type=='negabino') command_str<- paste0("null.fit<- try(glm.nb(formula(",form.null,"), data=dataset, link=",link,"))")
        eval(parse(text=command_str)) ;
        model.fit <- data.frame(anova(null.fit,fit.final))
        if(type=='poisson') model.fit <- model.fit[c(2,1),c(2,1,4,3)]
        if(type=='negabino') model.fit <- model.fit[c(2,1),c(4,3,7,6)]
        model.fit <- cbind(model.fit,c(pchisq(model.fit[1,3],model.fit[1,4],lower.tail=F),NA))
        rownames(model.fit) <- c('Proposed model','Null model')
        colnames(model.fit) <- c('Deviance','DF(Deviacne)','χ<sup>2</sup>','DF(χ<sup>2</sup>)','p-value') # 변경: chisq -> χ<sup>2</sup>, P-value -> p-value
        anova.msg <- "<div style='text-align:left'><small>* 'Null model' means the model including only intercept and 'Proposed model' means the model including all explanatory variables including interaction effect.</small>"
        R2HTML::HTML(Digits(model.fit),file=local_output,align="left",digits=15,
                     caption=anova.msg) ## tab_idx_4 # 변경 : caption 수정
        
        R2HTML::HTML(R2HTML::as.title(paste0("Variable Effect with Type ",ss," SS")),HR=4,file=local_output)
        warn.desc <- ifelse(ss=='I',"<div style='text-align:left'><small>* In type I test, 'Null model' means the model including only intercept. Terms are added sequentially (first to last).</small>",
                            ifelse(ss=='II',"<div style='text-align:left'><small>* In type II test, 'Proposed model' means the model including all explanatory variables except interaction effect. The other rows are the testing results for each main effect after the other main effect.</small>",
                                   "<div style='text-align:left'><small>* In type III test, 'Proposed model' means the model including all explanatory variables including interaction effect. The other rows are the testing results for each effect after the other effect.</small>")) # 변경 : caption 수정
        
        if(ss=='I'){
          AT <- try(anova(fit.final,test="Chisq"),s=T)
          rowNames <- rownames(AT)
          if(class(AT)[1]!='try-error'){
            AT <- data.frame(AT)
            AT <- AT[,c(4,3,2,1,5)]
            rownames(AT) <- rowNames; rownames(AT)[1] <- 'Null model'
            colnames(AT) <- c('Deviance','DF(Deviacne)','χ<sup>2</sup>','DF(χ<sup>2</sup>)','p-value') # 변경: Chisq -> χ<sup>2</sup>, P-value -> p-value
            R2HTML::HTML(Digits(AT),file=local_output,align="left",digits=15,
                         caption=warn.desc) ## tab_idx_4 # 변경 : caption 수정
          } else {
            warn.msg6 <- "<li> Error : Fail to fit the Model."
            R2HTML::HTML(warn.msg6,file=local_output)
          }
        }
        
        if(ss %in% c('II','III')){
          # II type : ignore interaction term
          # III type : calculate SS including interaction term
          RN <- final.predictor
          if(ss=='II' & length(grep(':',fin_vars2))>0) {
            RN <- fin_vars2[-grep(':',fin_vars2)]
            warn.AT2 <- '<li> Warning : Test for interaction effects is not provided in type II test. Use type III test for interaction effects.'
          }
          # warn.AT3 <- '<li> Note : If there is indeed no interaction, then type II is statistically more powerful than type III.'
          if(length(RN)>0){
            AT.form.full <- paste(dep_var,'~',paste(RN,collapse=' + '),ifelse(class(offset)=='character',paste0('+ offset(',offset,')'),''))
            if(type=='poisson') command_str<- paste0("full.fit<- try(glm(formula(AT.form.full), data=dataset, family=",'poisson',"(",link,")),silent=TRUE)")
            if(type=='negabino') command_str<- paste0("full.fit<- try(glm.nb(formula(AT.form.full), data=dataset, link=",link,"),silent=TRUE)")
            eval(parse(text=command_str))
            if(ss=='II') res <- try(car::Anova(full.fit,type='II',test="LR"))
            if(ss=='III'){
              options(contrasts=Type3.contr)
              AT.form.full <- paste(dep_var,'~',paste(RN,collapse=' + '),ifelse(class(offset)=='character',paste0('+ offset(',offset,')'),''))
              if(type=='poisson') command_str<- paste0("full.fit.type3 <- try(glm(formula(AT.form.full), data=dataset, family=",'poisson',"(",link,")),silent=TRUE)")
              if(type=='negabino') command_str<- paste0("full.fit.type3 <- try(glm.nb(formula(AT.form.full), data=dataset, link=",link,"),silent=TRUE)")
              eval(parse(text=command_str))
              res <- try(car::Anova(full.fit.type3,type='III',test="LR"))
              options(contrasts=Orig.contr)
            }

            if(class(res)[1]!='try-error'){
              dev <- deviance(full.fit)
              dev.df <- df.residual(full.fit)
              devs <- dev+res[,1]
              dev.dfs <- dev.df + res[,2]
              AT <- data.frame(c(dev,devs),c(dev.df,dev.dfs),c(NA,res[,1]),c(NA,res[,2]),c(NA,res[,3]))
              colnames(AT) <- c('Deviance','DF(Deviacne)','χ<sup>2</sup>','DF(χ<sup>2</sup>)','p-value') # 변경: Chisq -> χ<sup>2</sup>, P-value -> p-value
              rownames(AT) <- c('Proposed model',rownames(res))
              
              R2HTML::HTML(Digits(AT),file=local_output,align="left",digits=15) ## tab_idx_4
              R2HTML::HTML(warn.desc,file=local_output)
              if(exists('warn.AT2')) R2HTML::HTML(warn.AT2,file=local_output)
            } else {
              warn.AT4 <- "<li> Error : Fail to fit the Model."
              R2HTML::HTML(warn.AT4,file=local_output)
            }
          } else if(exists('warn.AT2')) R2HTML::HTML(warn.AT2,file=local_output)
        }
      }
    }
  }
  if(type=='coxph') {
    final.predictor <- gsub("exp\\(|\\)","",final.predictor)
    form.final <- paste0("survobj ~ ",gsub("-1","",final.predictor))
    fm <- formula(form.final)
    fit.final <- survival::coxph(fm, data=dataset)
    
    CE <- summary(fit.final)$coef
    colnames(CE) <- c('Estimate','Hazard ratio','SE(Estimate)','Z-value','p-value') # 변경: P-value -> p-value
    if(!expestim) CE <- CE[,-2,drop=F]
    if(CI){
      tmp <- merge(CE,confint(fit.final, level=confint.level),by="row.names",all=TRUE,sort=F)
      rownames(tmp) <- tmp[,1]
      CE <- tmp[,-1]
      colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Estimate<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
      if(expestim){
        CE <- cbind(CE,exp(CE[,(ncol(CE)-1):ncol(CE)]))
        colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Hazard ratio<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
      }
    }
    
    if(CI) R2HTML::HTML(Digits(CE), file=local_output, align="left",digits=15,caption="<div style='text-align:left'><small>* Wald confidence intervals were calculated.</small>") ## tab_idx_4 # 변경: caption 수정
    if(!CI) R2HTML::HTML(Digits(CE), file=local_output, align="left",digits=15)
  }
  if(type=='clogit') {
    cond_method <- 'exact'
    form.final <- paste0(dep_var,'_ ~ ',gsub("-1","",final.predictor),'+ strata(',cond_var,')')
    fm <- formula(form.final)
    fit.final <- survival::clogit(fm, method='exact', data=dataset)
    
    if(VIF) { fm_temp <- formula(paste0('cbind(',dep_var,'_,',cond_var,') ~ ',gsub("-1","",final.predictor)))
    fit.final_temp <- try(mclogit(fm_temp, data=dataset),silent=T)
    }
    
    CE <- summary(fit.final)$coef
    colnames(CE) <- c('Estimate','Odds ratio','SE(Estimate)','Z-value','p-value') # 변경: P-value -> p-value
    if(!odds) CE <- CE[,-2,drop=F]
    if(CI){
      tmp <- merge(CE,confint(fit.final, level=confint.level),by="row.names",all=TRUE,sort=F)
      rownames(tmp) <- tmp[,1]
      CE <- tmp[,-1]
      colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Estimate<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
      if(odds){
        CE <- cbind(CE,exp(CE[,(ncol(CE)-1):ncol(CE)]))
        colnames(CE)[(ncol(CE)-1):ncol(CE)] <- paste0(confint.level*100,'% ', c('LCI', 'UCI'), '<br>for Odds ratio<sup>*</sup>') # 변경: UCI / LCI 통일 + 들여쓰기 수정
      }
    }
    if(VIF) {
      if(class(fit.final_temp)!='try-error')	{
        VIF.1 <- rms::vif(fit.final_temp)
        tmp <- merge(CE,VIF.1,by='row.names',all=TRUE,sort=F)
        rownames(tmp) <- tmp[,1]
        colnames(tmp)[ncol(tmp)] <- "VIF"
        CE <- tmp[rownames(CE),-1]		
      }
    }
    if(CI) R2HTML::HTML(Digits(CE), file=local_output, align="left",digits=15,caption="<div style='text-align:left'><small>* Wald confidence intervals were calculated.</small>") ## tab_idx_4 # 변경: caption 수정
    if(!CI) R2HTML::HTML(Digits(CE), file=local_output, align="left",digits=15) ## tab_idx_4
    if(ANOVA){
      R2HTML::HTML(R2HTML::as.title("Analysis-of-Deviance Table"),HR=3,file=local_output)
      R2HTML::HTML(R2HTML::as.title("Model Effect (Goodness of Fit Test)"),HR=4,file=local_output)
      model.fit <- data.frame(matrix(NA,ncol=4,nrow=2))
      model.fit[,1] <- -2*fit.final$loglik[2:1]
      model.fit[1,2] <- model.fit[2,1]-model.fit[1,1]
      model.fit[1,3] <- summary(fit.final)$logtest['df']
      model.fit[,4] <- c(pchisq(model.fit[1,2],model.fit[1,3],lower.tail=F),NA)
      rownames(model.fit) <- c('Proposed model','Null model')
      colnames(model.fit) <- c('Deviance','Chisq','DF(Chisq)','p-value') # 변경: P-value -> p-value
      anova.msg <- "<div style='text-align:left'><small>* 'Null model' means the model including only intercept and 'Proposed model' means the model including all explanatory variables including interaction effect.</small>"
      R2HTML::HTML(Digits(model.fit),file=local_output,align="left",digits=15,
                   caption=anova.msg) ## tab_idx_4 # 변경 : caption 수정
      
      R2HTML::HTML(R2HTML::as.title(paste0("Variable Effect with Type ",ss," SS")),HR=4,file=local_output)
      warn.desc <- ifelse(ss=='I',"<div style='text-align:left'><small>* In type I test, 'Null model' means the model including only intercept. Terms are added sequentially (first to last).</small>",
                          ifelse(ss=='II',"<div style='text-align:left'><small>* In type II test, 'Proposed model' means the model including all explanatory variables except interaction effect. The other rows are the testing results for each main effect after the other main effect.</small>",
                                 "<div style='text-align:left'><small>* In type III test, 'Proposed model' means the model including all explanatory variables including interaction effect. The other rows are the testing results for each effect after the other effect.</small>")) # 변경 : caption 수정
      
      if(ss=='I'){
        AT <- try(anova(fit.final,test="Chisq"),s=T)
        if(class(AT)[1]!='try-error'){
          AT <- data.frame(AT)
          AT[,1] <- -2* AT[,1]
          rownames(AT)[1] <- 'Null model'
          colnames(AT) <- c('Deviance','Chisq','DF(Chisq)','p-value') # 변경: P-value -> p-value
          R2HTML::HTML(Digits(AT),file=local_output,align="left",digits=15,
                       caption=warn.desc) ## tab_idx_4 # 변경 : caption 수정
        } else {
          warn.msg6 <- "<li> Error : Fail to fit the Model."
          R2HTML::HTML(warn.msg6,file=local_output)
        }
      }
      
      if(ss %in% c('II','III')){
        # II type : ignore interaction term
        # III type : calculate SS including interaction term
        RN <- final.predictor
        if(ss=='II' & length(grep(':',fin_vars2))>0) {
          RN <- fin_vars[-grep(':',fin_vars2)]
          warn.AT2 <- '<li> Warning : Test for interaction effects is not provided in type II test. Use type III test for interaction effects.'
        }
        # warn.AT3 <- '<li> Note : If there is indeed no interaction, then type II is statistically more powerful than type III.'
        if(length(RN)>0){
          AT.form.full <- paste(dep_var,'_~',paste(RN,collapse=' + '),'+strata(',cond_var,')',sep='')
          command_str<- paste0("full.fit<- try(clogit(formula(AT.form.full), data=dataset, method='",cond_method,"'),silent=T)")
          
          eval(parse(text=command_str))
          AT <- try(data.frame(drop1(full.fit,.~.,test='Chisq')),silent=T)
          if(class(AT)[1]!='try-error'){
            AT <- AT[-grep('strata\\(',rownames(AT)),c(2,3,1,4)]
            AT[1,1] <- -2* full.fit$loglik[2]
            for(i in 2:nrow(AT)) AT[i,1] <- AT[1,1] + AT[i,2]
            colnames(AT) <- c('Deviance','Chisq','DF(Chisq)','p-value') # 변경: P-value -> p-value
            rownames(AT)[1] <- 'Proposed model'
            R2HTML::HTML(Digits(AT),file=local_output,align="left",digits=15) ## tab_idx_4
            R2HTML::HTML(warn.desc,file=local_output)
            if(exists('warn.AT2')) R2HTML::HTML(warn.AT2,file=local_output)
          } else {
            warn.msg7 <- "<li> Error : Fail to fit the Model."
            R2HTML::HTML(warn.msg7,file=local_output)
          }
        } else if(exists('warn.AT2')) R2HTML::HTML(warn.AT2,file=local_output)
      }
    }
  }	
}

### Using Analysis modules...
gg_color_hue2 <- function(n) {
  hues = seq(6.330885, 6.330885+360, length = n + 1)
  hcl(h = hues, l = 51.19696, c = 144.4467)[1:n]
}

######## 기술통계량 소수점 ########
Dgist <- function(x,dg,ns) {
  if(is.vector(x)) {
    for(i in 1:length(x)) {
      if(is.na(x[i])) (x[i] <- "") else (x[i] <- format(round(as.numeric(x[i]),digits=dg),scientific=FALSE,digits=dg,nsmall=ns))
    }
  } else {
    for(j in 1:ncol(x)) {
      for(i in 1:nrow(x)) {
        if(is.na(x[i,j])) (x[i,j] <- "") else (x[i,j] <- format(round(as.numeric(x[i,j]),digits=dg),scientific=FALSE,digits=dg,nsmall=ns))
      }
    }
  }	
  res <- x
  
  return(res)
}

## 요약표함수
sum_tab <- function(dataset, quan_var=NULL, qual_var=NULL, group_var=NULL, append=T){
  
  R2HTML::HTML(R2HTML::as.title("Summary Table"),HR=3,file=local_output,append=append)
  
  if(is.null(group_var)){ dataset$rex_total = rep("Total", nrow(dataset)); group_var="rex_total" }
  digits.mean = dgest
  digits.perc = dgest
  digits.pval = dgpval
  
  # Function ----------------------------------------------------------------
  
  quan_tab = function(dataset, quan_var, group_var){
    
    x = dataset[,group_var]
    y = dataset[,quan_var]
    
    tot.mean = round(mean(y, na.rm=T), digits=digits.mean)
    tot.sd = round(sd(y, na.rm=T), digits=digits.mean)
    tot.median = round(median(y, na.rm=T), digits=digits.mean)
    tot.q1 = round(summary(y)[2], digits=digits.mean)
    tot.q3 = round(summary(y)[5], digits=digits.mean)
    tot.min = round(summary(y)[1], digits=digits.mean)
    tot.max = round(summary(y)[6], digits=digits.mean)
    
    tab.mean = tab.sd = tab.median = tab.q1 = tab.q3 = tab.min = tab.max = tab.p1 = tab.p2 = tab.p3 = tab.method = NA
    
    tab.mean = round(tapply(y, x, mean, na.rm=T), digits=digits.mean)
    tab.sd   = round(tapply(y, x, sd, na.rm=T), digits=digits.mean)
    tab.median = round(tapply(y, x, median, na.rm=T), digits=digits.mean)
    tab.q1 = round(tapply(y, x, function(x) summary(x)[2]), digits=digits.mean)
    tab.q3 = round(tapply(y, x, function(x) summary(x)[5]), digits=digits.mean)
    tab.min = round(tapply(y, x, function(x) summary(x)[1]), digits=digits.mean)
    tab.max = round(tapply(y, x, function(x) summary(x)[6]), digits=digits.mean)
    
    tab.p1 = tryCatch({round(tapply(y, x, function(x) shapiro.test(x)$p.value), digits=digits.pval)},
                      error = function(e){if(sum(table(x)<3)==0){ tryCatch({round(tapply(y, x, function(x) ks.test(x=x, y="pnorm")$p.value), digits=digits.pval)},
                                                                           error = function(e){NA})}}) # NA then error; NULL then group size less than 3
    if(is.null(tab.p1)) tab.p1 = NA
    
    # group variable statistic
    tab.p2 = tryCatch({round(oneway.test(y~factor(x))$p.value, digits=digits.pval)}, error = function(e){NA})
    tab.p3 = tryCatch({if(length(table(x))==2){ round(wilcox.test(y~factor(x))$p.value, digits=digits.pval)}else{round(kruskal.test(y~factor(x))$p.value, digits=digits.pval)}},
                      error = function(e){ NA })
    
    tab.method = ifelse(sum(is.na(tab.p1))!=0, NA, ifelse(sum(tab.p1<0.05)==0, 1, 2)) # 1:para / 2:nonpara
    
    for(ii in 1:length(tab.p1)) if(!is.na(tab.p1[ii])) if(tab.p1[ii]<0.001){ tab.p1[ii] = "<0.001"}else if(tab.p1[ii]>0.99) tab.p1[ii] = ">0.99"
    for(ii in 1:length(tab.p2)) if(!is.na(tab.p2[ii])) if(tab.p2[ii]<0.001){ tab.p2[ii] = "<0.001"}else if(tab.p2[ii]>0.99) tab.p2[ii] = ">0.99"
    for(ii in 1:length(tab.p3)) if(!is.na(tab.p3[ii])) if(tab.p3[ii]<0.001){ tab.p3[ii] = "<0.001"}else if(tab.p3[ii]>0.99) tab.p3[ii] = ">0.99"
    
    return(list(tmean=tot.mean, tsd=tot.sd, tmedian=tot.median, tq1=tot.q1, tq3=tot.q3, tmin=tot.min, tmax=tot.max,
                mean=tab.mean, sd=tab.sd, median=tab.median, q1=tab.q1, q3=tab.q3, min=tab.min, max=tab.max,
                p1=tab.p1, p2=tab.p2, p3=tab.p3, method=tab.method))
    
  }
  
  qual_tab = function(dataset, qual_var, group_var){
    
    x = dataset[,group_var]
    y = dataset[,qual_var]
    
    tot.n = table(y)
    tot.perc = round(prop.table(table(y))*100, digits=digits.perc)
    tot.nperc = paste0(tot.n, " (", tot.perc, "%)")
    
    tab.n = tab.perc = tab.nperc = tab.p1 = tab.p2 = NA
    
    tab.n = table(y, x)
    tab.perc = round(prop.table(table(y,x), 2)*100, digits=digits.perc)
    tab.nperc = c(); for(ii in 1:ncol(tab.n)) tab.nperc = cbind(tab.nperc, paste0(tab.n[,ii], " (", tab.perc[,ii], "%)"))
    
    tab.p1 = tryCatch({round(chisq.test(table(y,x))$p.value, digits=digits.pval)}, error = function(e){NA})
    tab.p2 = tryCatch({round(fisher.test(table(y,x))$p.value, digits=digits.pval)}, error = function(e){NA})
    chisq.warn = tryCatch({chisq.test(table(y,x))}, warning=function(w){TRUE}, error=function(e){NA})
    if(class(chisq.warn)=="htest") chisq.warn = FALSE
    
    if(!is.na(tab.p1)) if(tab.p1<0.001){ tab.p1 = "<0.001"}else if(tab.p1>0.99) tab.p1 = ">0.99"
    if(!is.na(tab.p2)) if(tab.p2<0.001){ tab.p2 = "<0.001"}else if(tab.p2>0.99) tab.p2 = ">0.99"
    
    return(list(tn=tot.n, tperc=tot.perc, tnperc=tot.nperc, n=tab.n, perc=tab.perc, nperc=tab.nperc, p1=tab.p1, p2=tab.p2, chisq.warn=chisq.warn))
    
  }
  
  # Final table -------------------------------------------------------------
  stable1 = stable2 = c()
  
  for(i in quan_var){
    yy = i
    xx = quan_tab(dataset, yy, group_var)
    nn = length(table(dataset[,group_var]))
    
    # name
    re0 = yy
    
    # parametric
    re1 = tryCatch({paste0(xx$tmean, " ± ", xx$tsd)}, error=function(e){NA})
    re2 = tryCatch({paste0(xx$mean, " ± ", xx$sd)}, error=function(e){rep(NA, nn)})
    re3 = tryCatch({xx$p2}, error=function(e){NA})
    
    re_para = c(re1, re2, re3)
    
    # non-parametric
    re1 = tryCatch({paste0(xx$tmedian, " (", xx$tq1, ", ", xx$tq3, ")")}, error=function(e){NA})
    re2 = tryCatch({paste0(xx$median, " (", xx$q1, ", ", xx$q3, ")")}, error=function(e){rep(NA, nn)})
    re3 = tryCatch({xx$p3}, error=function(e){NA})
    
    re_nonpara = c(re1, re2, re3)
    
    # range
    re1 = tryCatch({paste0(xx$tmin, " to ", xx$tmax)}, error=function(e){NA})
    re2 = tryCatch({paste0(xx$min, " to ", xx$max)}, error=function(e){NA})
    re_range = c(re1, re2)
    
    # normality test
    re1 = tryCatch({paste0(names(xx$p1), ", ", xx$p1, collapse=" / ")}, error=function(e){NA})
    re_normal = re1
    
    
    # final
    if(is.na(xx$method)){ re_final = re_nonpara }else if(xx$method==1){ re_final = re_para }else{ re_final = re_nonpara}
    re_final = c(re0, re_final)
    
    # final in detail
    re_final2 = c(re_para, re_nonpara, re_normal, re_range)
    re_final2 = c(re0, re_final2)
    
    stable1 = rbind(stable1, re_final)
    stable2 = rbind(stable2, re_final2)
  }
  
  
  
  for(i in qual_var){
    yy = i
    xx = qual_tab(dataset, yy, group_var)
    nn = length(table(dataset[,group_var]))
    ny = length(table(dataset[,yy]))
    
    # name
    re0 = c(yy, paste0("   - ", names(table(dataset[,yy]))))
    
    # chisq.test
    re1 = c("", xx$tnperc)
    re2 = rbind("", xx$nperc)
    re3 = c("", xx$p1, rep("", ny-1))
    re_chisq = cbind(re1, re2, re3)
    
    # fisher.test
    re1 = c("", xx$tnperc)
    re2 = rbind("", xx$nperc)
    re3 = c("", xx$p2, rep("", ny-1))
    re_fisher = cbind(re1, re2, re3)
    
    # chisq.warning
    re_notice = c(ifelse(is.na(xx$chisq.warn), "Fisher's test", ifelse(!xx$chisq.warn, "Chi-squared test", "Fisher's test")), rep("", ny))
    
    # final
    if(is.na(xx$chisq.warn)){ re_final = re_chisq
    }else{ if(!xx$chisq.warn){ re_final = re_chisq
    }else{ re_final = re_fisher }
    }
    
    re_final = cbind(re0, re_final)
    
    # final in detail
    re_final2 = cbind(re_chisq, re_fisher, re_notice, matrix("", nrow=ny+1, ncol=nn+1))
    re_final2 = cbind(re0, re_final2)
    
    stable1 = rbind(stable1, re_final)
    stable2 = rbind(stable2, re_final2)
    
  }
  
  
  # Title -------------------------------------------------------------------
  
  tot = table(dataset[,group_var])
  
  re1 = paste0("Total<br>(N=", sum(tot), ")")
  re2 = paste0(names(tot), "<br>(N=", tot, ")")
  re3 = paste0("Range<br>(", names(tot), ")")
  
  colnames(stable1) = c("", re1, re2, "p-value") # 변경: p-value # colnames, total, not total, p-value
  colnames(stable2) = c("", paste0(re1, "<br>mean±SD (or n(%))"), paste0(re2, "<br>mean±SD (or n(%))"), "p-value<sup>1</sup>", # 변경: p-value
                        paste0(re1, "<br>median(IQR) (or n(%))"), paste0(re2, "<br>median(IQR) (or n(%))"), "p-value<sup>2</sup>", # 변경: p-value
                        "Normality test <br> chi-squared or Fisher", "Range<br>(Total)",re3)
  
  rownames(stable1) = rownames(stable2) = stable1[,1]
  
  if(group_var=="rex_total"){
    if(is.null(qual_var)){
      if(length(quan_var)==1){
        stable1 = matrix(stable1[,c(1,2)], nrow=1); colnames(stable1) = c("", re1)
        stable2 = matrix(stable2[c(1,2,5,8,9)], nrow=1);
        rownames(stable2) = quan_var
        colnames(stable2) = c("", paste0("Total<br>(N=", sum(tot), ")<br>mean±SD"), paste0("Total<br>(N=", sum(tot), ")<br>median(IQR)"), "Normality test", "Range<br>(Total)")
      } else {
        stable1 = as.matrix(stable1[,1:2]); colnames(stable1) = c("", re1)
        stable2 = stable2[,c(1,2,5,8,9)];
        colnames(stable2)[4] = "Normality test"
      }
    } else if(is.null(quan_var)) {
      stable1 <- as.matrix(stable1[,c(1,2)])
      stable2 <- stable2[,c(1,2,8)]
      colnames(stable2)[3] <- "Chi-squared or Fisher"
    } else {
      stable1 <- as.matrix(stable1[,1:2]); colnames(stable1) = c("", re1)
      stable2 <- stable2[,c(1,2,5,8,9)]
      if(!is.null(qual_var)){ stable2[(length(quan_var)+2):nrow(stable2),4] = "" }
    }
  } else {
    if(is.null(qual_var)){
      colnames(stable2)[8+2*nn-2] <- "Normality test"
    } else if(is.null(quan_var)){
      colnames(stable2)[8+2*nn-2] <- "Chi-squared or Fisher"
    }
  }
  
  #  if(group_var=="rex_total"){
  #    if(length(quan_var)==1 & is.null(qual_var)){
  #      stable1 = matrix(stable1[,c(1,2)], nrow=1); colnames(stable1) = c("", re1)
  #      stable2 = matrix(stable2[c(1,2,5,8,9)], nrow=1);
  #      rownames(stable2) = quan_var
  #      colnames(stable2) = c("", paste0("Total<br>(N=", sum(tot), ")<br>mean±SD"), paste0("Total<br>(N=", sum(tot), ")<br>median(IQR)"), "Normality test", "Range<br>(Total)")
  #    } else{
  #      stable1 = as.matrix(stable1[,1:2]); colnames(stable1) = c("", re1)
  #      stable2 = stable2[,c(1,2,5,8,9)];
  #      if(!is.null(qual_var)){  stable2[(length(quan_var)+1):nrow(stable2),4] = ""; }
  #      colnames(stable2)[4] = "Normality test"
  #    }
  #  }
  
  R2HTML::HTML(R2HTML::as.title(ifelse(group_var=="rex_total", "Summary table in total", paste0("Summary table (group: ", group_var, ")"))), HR=4, file=local_output)
  rownames(stable1) = NULL
  # R2HTML::HTML(stable1, align="left", row.names=F, file=local_output) ## tab_idx_4
  
  if(!is.null(qual_var)) cap.msg <- "<div style='text-align:left'><small>* This table was summarized as appropriate according to the presence of warning from chi-squared test.</small>"
  if(!is.null(quan_var)) cap.msg <- "<div style='text-align:left'><small>* This table was summarized as appropriate according to normality test.</small>"
  if(!is.null(qual_var) & !is.null(quan_var)) cap.msg <- "<div style='text-align:left'><small>* This table was summarized as appropriate according to normality test and the presence of warning from chi-squared test.</small>"
  R2HTML::HTML(stable1, align="left", caption=cap.msg, file=local_output)
  
  cap <- ""; cap2 <- ""
  if(group_var!="rex_total"){
    if(!is.null(quan_var) & nn==2) cap <- "<small>* (Continuous variable) p-value<sup>1</sup>=Two sample t-test, p-value<sup>2</sup>=Wilcoxon rank sum test (Mann-Whitney U test)</small>" # 변경: p-value, caption 수정
    if(!is.null(quan_var) & nn>2) cap <- "<small>* (Continuous variable) p-value<sup>1</sup>=One-way ANOVA, p-value<sup>2</sup>=Kruskal-Wallis test</small>" # 변경: p-value, caption 수정
    if(!is.null(qual_var)) cap2 <- "<small>* (Categorical variable) p-value<sup>1</sup>=Chi-squared test, p-value<sup>2</sup>=Fisher's exact test</small>" # 변경: p-value, caption 수정
  }
  
  if(!(is.null(quan_var) & is.null(qual_var) & group_var=="rex_total")){
    R2HTML::HTML(R2HTML::as.title(ifelse(group_var=="rex_total", "Summary table in detail", paste0("Summary table in detail (group: ", group_var, ")"))), HR=4, file=local_output)
    rownames(stable2) = NULL; caption <- ifelse(!is.null(quan_var), paste0(cap, "<br>", cap2), cap2)
    if(is.null(quan_var)){ R2HTML::HTML(stable2, align="left", row.names=F, file=local_output, caption=paste0("<div style='text-align:left'>",caption)) } ## tab_idx_4
    if(!is.null(quan_var)){ R2HTML::HTML(stable2, align="left", row.names=F, file=local_output, ## tab_idx_4
                                         caption=paste0("<div style='text-align:left'><small>* Normality test was performed by Shapiro-Wilk test (3 ≤ sample size < 5000) or Kolmogorov-Smirnov test (sample size ≥ 5000).</small>","<br>",caption)) }
  } # 변경 : 메세지 수정(caption)
}

## Contrasts options
Orig.contr <- c(unordered="contr.treatment", ordered="contr.poly")
Type3.contr <- c(unordered="contr.sum", ordered="contr.poly")