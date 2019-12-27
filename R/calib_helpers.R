#'@import adagio
#'@importFrom utils write.csv
calib_GLM <- function(var, ub, lb, init.val, obs, method, glmcmd,
                      metric, target.fit, target.iter, nml_file, path, scaling, verbose){
  path <<- path
  
  if (method == 'CMA-ES'){
    glmOPT <- pureCMAES(par = init.val, fun = glmFUN, lower = rep(0,length(init.val)), 
                        upper = rep(10,length(init.val)), 
                        sigma = 0.5, 
                        stopfitness = target.fit, 
                        stopeval = target.iter, 
                        glmcmd = glmcmd, nml_file = nml_file, var = var,
                        scaling = scaling, metric = metric, verbose = verbose)
  } else if (method == 'Nelder-Mead'){
    glmOPT <- neldermeadb(fn = glmFUN, init.val, lower = rep(0,length(init.val)), 
                        upper = rep(10,length(init.val)), 
                        adapt = TRUE,
                        tol = 1e-10,
                        maxfeval = target.iter, glmcmd = glmcmd, nml_file = nml_file, var = var,
                        scaling = scaling, metric = metric, verbose = verbose )
  }
  
  glmFUN(p = glmOPT$xmin,nml_file = nml_file,glmcmd = glmcmd, var = var,scaling,metric,verbose)
  # glmFUN(glmOPT$xmin, glmcmd, scaling, metric, verbose)
  calib <- read.csv(paste0(path,'/calib_results_',metric,'_',var,'.csv'))
  eval(parse(text = paste0('best_par <- calib[which.min(calib$',metric,'),]')))
  write.csv(best_par, paste0(path,'/calib_par_',var,'.csv'), row.names = F, quote = F)
  best_par <- read.csv(paste0(path,'/calib_par_',var,'.csv'))
  
  #Input best parameter set
  nml <- read_nml(nml_file = nml_file)
  check_duplicates <- c()
  for (i in 2:(ncol(best_par)-2)){
    string1 <- colnames(best_par)[i]
    for (j in (i+1):(ncol(best_par)-1)){
      string2 <- colnames(best_par)[j]
      if (substr(string1,1,floor(nchar(string1)*9/10)) == substr(string2,1,floor(nchar(string1)*9/10))){
        check_duplicates <- append(check_duplicates, i)
        check_duplicates <- append(check_duplicates, j)
      }
    }
  }
  checked <- 2:(ncol(best_par)-1)
  for (i in 1:length(check_duplicates)){
    checked <- checked[!checked == check_duplicates[i]]
  }
  
  for(i in checked){
    nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
  }
  
  if (!is.null(check_duplicates)){
    check_duplicates <- matrix(check_duplicates,ncol=2, byrow = TRUE)
    find_dupl_groups <- list()
    it <- 1
    for (ii in 1:nrow(check_duplicates)){
      if (ii == 1){
        find_dupl_groups[[it]] <- (check_duplicates[ii,])
      } else {
        if (ii > 1){
          if (any(check_duplicates[ii,] %in% find_dupl_groups[[it]])){
            place <- !(check_duplicates[ii,] %in% find_dupl_groups[[it]])
            find_dupl_groups[[it]]<-append(find_dupl_groups[[it]], check_duplicates[ii, which(place == TRUE)])
          } else {
            it <- it+1
            find_dupl_groups[[it]] <- (check_duplicates[ii,])
          } 
        }
      } }
    
    
    for (i in 1:length(find_dupl_groups)){
      nml <- set_nml(nml,
                     gsub('[.]','%',colnames(best_par))[find_dupl_groups[[i]][1]],
                     as.numeric(best_par[find_dupl_groups[[i]]]))
      #print(gsub('[.]','%',colnames(best_par))[find_dupl_groups[[i]][1]])
      #print(as.numeric(best_par[unlist(find_dupl_groups[[i]])]))
    }
  } else {
    for(i in 2:(ncol(best_par)-1)){
      nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
    }
  }
  
  write_nml(nml, file = nml_file)
  
  #Run GLM
  run_glmcmd(glmcmd, path, verbose)
  
  g1 <- diag.plots(mod2obs(paste0(path,'/output/output.nc'), obs, reference = 'surface', var), obs)

  ggsave(filename = paste0(path,'/diagnostics_',method,'_',var,'.png'), plot = g1, 
         dpi = 300,width = 384,height = 216, units = 'mm')
  
  return()
}


glmFUN <- function(p, glmcmd, nml_file, var, scaling, metric, verbose){
  #Catch non-numeric arguments
  if(!is.numeric(p)){
    p = values.optim
  }
  if (scaling == TRUE){
    p <- wrapper_scales(p, lb, ub)
  }
  
  eg_nml <- read_nml(nml_file)
  
  for(i in 1:length(pars[!duplicated(pars)])){
    if (any(pars[!duplicated(pars)][i] == pars[duplicated(pars)])){
      eg_nml <- set_nml(eg_nml, pars[!duplicated(pars)][i], 
                        p[which(pars[!duplicated(pars)][i] == pars)])
    } else {
      eg_nml <- set_nml(eg_nml,pars[!duplicated(pars)][i],p[!duplicated(pars)][i])
    }
  }
  
  write_path <- nml_file
  write_nml(eg_nml, file = write_path)
  
  error <- try(run_glmcmd(glmcmd, path, verbose))
  while (error != 0){
    error >- try(run_glmcmd(glmcmd, path, verbose))
  }
  
  mod <- mod2obs(mod_nc = paste0(path,'/output/output.nc'), obs = obs, reference = 'surface',var = var)

  fit = get_rmse(mod,obs)
  
  #Create a data frame to output each calibration attempt
  dat = data.frame(matrix(NA, ncol = (length(pars)+2), nrow = 1, dimnames = list(c(1), c('DateTime', pars, metric))))
  dat[1,] = c(format(Sys.time()),p,fit)
  
  #Opens and writes a csv file with datetime, parameters,and fitness
  
    if(!file.exists(paste0(path,'/calib_results_',metric,'_',var,'.csv'))){
      write.csv(dat,paste0(path,'/calib_results_',metric,'_',var,'.csv'), row.names = F, quote = F)
    }else{
      df = read.csv(paste0(path,'/calib_results_',metric,'_',var,'.csv'))
      df = rbind.data.frame(dat, df)
      write.csv(df,paste0(path,'/calib_results_',metric,'_',var,'.csv'), row.names = F, quote = F)
    }

  print(paste(metric, round(fit,3)))
  return(fit)
}

run_glmcmd <- function(glmcmd, path, verbose){
  if (is.null(glmcmd)){
    run_glm(path, verbose = verbose)
  } else{
    system(glmcmd,ignore.stdout=TRUE)
  }
}


wrapper_scales <- function(x, lb, ub){
  y <-  lb+(ub-lb)/(10)*(x)
  return(y)
}

get_rmse <- function(mods, obs){
  id1 <- !is.na(obs[,3]) 
  obs <- obs[id1,3]
  mods <- mods[id1,3]
  id2 <- !is.na(mods) 
  obs <- obs[id2]
  mods <- mods[id2]
  sum_up <- sum((mods-obs)^2)
  rmse <- sqrt(sum_up/length(obs))
  return(rmse)
}

#'@importFrom reshape2 melt
mod2obs <- function(mod_nc, obs, reference = 'surface', var){
  deps = unique(obs[,2])
  #tim = unique(obs[,1])
  mod <- glmtools::get_var(file = mod_nc,var_name = var,reference = reference, z_out = deps)
  mod <- match.tstep(obs, mod) #From gotm_functions.R
  mod <- reshape2::melt(mod, id.vars = 1)
  mod[,2] <- as.character(mod[,2])
  mod[,2] <- as.numeric(gsub(paste(var,"_",sep=''),'',mod[,2]))
  colnames(mod) <- c('DateTime', 'Depth', var)
  mod <- mod[order(mod$DateTime, mod$Depth),]
  if(nrow(mod) != nrow(obs)){
    mod <- merge(obs, mod, by = c(1,2), all.x = T)
    #mod <- merge(obs, mod, by = c(1,2), all = T)
    mod <- mod[order(mod$DateTime, mod$Depth),]
    mod <- mod[,c(1,2,4)]
    colnames(mod) <- c('DateTime', 'Depth', var)
  }
  return(mod)
}

match.tstep <- function(df1, df2){
  if(df1[1,1] == df1[2,1]){
    df = data.frame(DateTime = unique(df1[,1]))
    df = merge(df, df2, by = 1)
    return(df)
  }else{
    # tim1 = df1[,1]
    # tim2 = df2[,1]
    # ind = c()
    # pb = txtProgressBar(min = 0, max = length(tim1), style = 3)
    # for(i in 1:length(tim1)){
    #   ind = append(ind, which(tim2 == tim1[i]))
    #   setTxtProgressBar(pb, i)
    # }
    # close(pb)
    # df2.mat = df2[ind,]
    # return(df2.mat)
    df = df2[(df2[,1] %in% df1[,1]),]
    return(df)
  }
}

#'@import graphics
diag.plots <- function(mod, obs, ggplot = T){
  stats = sum_stat(mod, obs, depth = T)
  if(max(mod[,2]) >= 0){ #Makes depths negative
    mod[,2] <- -mod[,2]
  }
  if(ggplot == F){
    dif = mod[,3] - obs[,3]
    par(mfrow=c(2,3))
    
    xfit <- seq(min(dif, na.rm = T), max(dif, na.rm = T), length=40)
    yfit_density <- dnorm(xfit, mean=mean(0, na.rm = T), sd=sd(dif, na.rm = T))
    
    # frequency
    h_freq <- hist(dif, breaks=50, col="blue", xlab="Model - Obs (C)", main='Histogram of residuals',probability = F, xlim = c(min(na.rm =T,  dif),max(na.rm =T,  dif)))
    yfit_freq <- yfit_density*diff(h_freq$mids[1:2])*length(dif)
    lines(xfit, yfit_freq, col="red",lty =2, lwd=2)
    mn <- round(mean(dif, na.rm =T),2)
    abline(v = mn,lty =2,lwd =2, col = 'green')
    std.dev <- round(sd(dif, na.rm =T),2)
    eqn <- bquote(Mean == .(mn) * "," ~~ S.D. == .(std.dev))
    Corner_text(eqn)
    
    plot(mod[,3], dif, cex = 0.5, pch ='.', main = 'Residuals vs. Modelled',
         ylab = 'Residuals', xlab = 'Modelled values')
    abline( h =0, col =2, lty =2)
    
    plot(mod[,1], dif, ylab = 'Time', xlab = 'Residuals', main = 'Residuals vs. Time', pch = '.')
    abline(h =0, lty =2, col =2)
    
    if(min(mod[,2]) >= 0){
      mod[,2] = -mod[,2]
    }
    plot(dif, mod[,2], ylim = range(mod[,2]), ylab = 'Depth (m)', xlab = 'Residuals', main = 'Residuals vs. Depth', pch = '.')
    abline(v =0, lty =2, col =2)
    
    plot(mod[,3], obs[,3], pch ='.', main = 'Obs vs. Mod', ylab = 'Obs',
         xlab ='Mod', ylim = range(mod[,3], obs[,3], na.rm =T), xlim = range(mod[,3], obs[,3], na.rm =T))
    abline(0,1, col =2, lty =2)
    eqn <- bquote(Pear_R == .(round(stats$Pearson_r,2)) * "," ~~ var.obs == .(round(stats$Variance_obs,2)) *
                    "," ~~ var.mod == .(round(stats$Variance_mod,2)) *  "," ~~ NSE == .(round(stats$NSE,2)))
    eqn2 <- bquote(cov == .(round(stats$Covariance,2)) * "," ~~ bias == .(round(stats$Bias,2)) *
                     "," ~~ MAE == .(round(stats$MAE,2)) * "," ~~ RMSE == .(round(stats$RMSE,2)))
    Corner_text(eqn)
    Corner_text(eqn2,location = 'bottomright')
    
    qqnorm(dif)
    abline(0,1, lty =2, col =2)
} else {
    #ggplot2 version - put all variables in one dataframe
    mod$res <- mod[,3] - obs[,3]
    deps <- unique(mod[,2])
    deps <- deps[order(deps)]
    if(length(deps) < 10){
      lgd.sz = 4
    }else{
      lgd.sz =2
    }
    mod$fdepth <- factor(mod[,2], levels = as.character(deps))
    mod$obs <- obs[,3]
    
    mean.res = round(mean(mod$res, na.rm =T),2)
    med.res = round(median(mod$res, na.rm = T),2)
    std.dev = round(sd(mod$res, na.rm =T), 2)
    n = nrow(mod[!is.na(mod$res),])
    bw = 0.2
    min.res = min(mod$res, na.rm =T)
    max.res = max(mod$res, na.rm =T)
    
    # Create text to be added to plots
    grob1 <- grid::grobTree(grid::textGrob(paste0("Mean = ", mean.res,'; S.D = ', std.dev), x=0.5,  y=0.95, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    grob2 <- grid::grobTree(grid::textGrob(paste0("Pear_R = ", round(stats$Pearson_r,2),'; v.obs = ', round(stats$Variance_obs,2),'; v.mod = ', round(stats$Variance_mod,2),'; NSE = ',round(stats$NSE,2)), x=0.05,  y=0.95, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    grob3 <- grid::grobTree(grid::textGrob(paste0("cov = ", round(stats$Covariance,2),'; bias = ', round(stats$Bias,2),'; MAE = ', round(stats$MAE,2),'; RMSE = ',round(stats$RMSE,2)), x=0.05,  y=0.05, hjust=0,
                                           gp=grid::gpar(col="black", fontsize=10)))
    
    #Plots
    p1 <-ggplot(mod, aes(x = res)) + 
      geom_histogram(fill = "lightblue4", colour = 'black', breaks = seq(min.res, max.res, bw)) + 
      stat_function( 
        fun = function(x, mean, sd, n, bw){ 
          dnorm(x = x, mean = mean, sd = sd) * n * bw
        }, 
        args = c(mean = 0, sd = std.dev, n = n, bw = bw), colour = 'red', linetype = 'dashed', size = 1.2) + 
      scale_x_continuous("Model - Obs (C)")+
      scale_y_continuous("Frequency")+
      ggtitle('Histogram of residuals')+
      coord_cartesian(xlim = c(min(mod$res, na.rm = T),max(mod$res,na.rm =T)))+
      geom_vline(xintercept = med.res, colour = 'green', linetype = 'dashed', size = 1.2)+
      theme_bw()
    p1 <- p1 + annotation_custom(grob1)
    
    p2 <- ggplot(mod, aes_string(names(mod)[3], 'res', colour = 'fdepth'))+
      geom_point(size = 0.1)+
      xlab('Modelled values')+
      ylab('Residuals')+
      ggtitle('Residuals vs. Modelled')+
      scale_color_discrete(name = 'Depths', guide = F)+
      #guides(colour = guide_legend(override.aes = list(size=5)))+
      geom_hline(yintercept = 0, size = 1, linetype = 'dashed')+
      theme_bw()
    
    p3 <- ggplot(mod, aes_string(names(mod)[1], 'res', colour = 'fdepth'))+
      geom_point(size = 0.1)+
      xlab('Time')+
      ylab('Residuals')+
      ggtitle('Residuals vs. Time')+
      #scale_color_gradientn(colors = rev(my.cols), name = 'Depths')+
      scale_color_discrete(name = 'Depths', guide = F)+
      #guides(colour = guide_legend(override.aes = list(size=lgd.sz)))+
      geom_hline(yintercept = 0, size = 1, linetype = 'dashed')+
      #xlim(min(names(mod)[1]),max(names(mod)[1])) +
      theme_bw()#+
    #theme(legend.text=element_text(size= (lgd.sz*2.5)))
    
    p4 <- ggplot(mod, aes_string('res', names(mod)[2], colour = 'fdepth'))+
      geom_point(size = 0.1)+
      ylab('Depth')+
      xlab('Residuals')+
      ggtitle('Residuals vs. Depth')+
      scale_color_discrete(name = 'Depths', guide = F)+
      geom_vline(xintercept = 0, linetype = 'dashed', size = 1)+
      #guides(colour = guide_legend(override.aes = list(size=5)))+
      theme_bw()
    
    
    p5 <- ggplot(mod,aes_string(names(mod)[3], 'obs', colour = 'fdepth'))+
      geom_point(size = 0.1)+
      ylab('Obs')+
      xlab('Modelled')+
      ggtitle('Obs vs. Mod')+
      scale_color_discrete(name = 'Depths', guide = F)+
      coord_cartesian(xlim = range(mod[,3], obs[,3], na.rm =T), ylim = range(mod[,3], obs[,3], na.rm =T))+
      geom_abline(slope = 1, intercept = 0, colour = 'black', linetype = 'dashed', size =1)+
      #guides(colour = guide_legend(override.aes = list(size=5)))+
      theme_bw()
    p5 <- p5 + annotation_custom(grob2) + annotation_custom(grob3)   
    
    p6 <- ggplot(mod, aes(sample = res))+
      stat_qq()+
      geom_abline(slope = 1, intercept = 0, size =1, linetype = 'dashed')+
      xlab('Sample Quantiles')+
      ylab('Theoretical Quantiles')+
      ggtitle('Normal Q-Q Plot')+
      theme_bw()
    
    # g <- gridExtra::arrangeGrob(p1,p2,p3,p4,p5,p6, nrow = 2)
    # gridExtra::grid.arrange(g)
    
    g = patchwork::wrap_plots(p1,p2,p3,p4,p5,p6, nrow = 2)
    print(g)
    
    return(g)
  }
}

get_nse <- function(x, y){
  id1 <- !is.na(y) 
  obs <- y[id1]
  mods <- x[id1]
  id2 <- !is.na(mods) 
  obs <- obs[id2]
  mods <- mods[id2]
  sum_up <- sum((mods-obs)^2)
  sum_bottom <- sum((obs-mean(obs))^2)
  nse <- 1- sum_up/sum_bottom
  return(nse)
}

# gotmtools.R
sum_stat <- function(mod, obs, depth =F,na.rm =T, depth.range =NULL){
  if(depth == T){
    if(!is.null(depth.range)){
      obs = obs[(obs[,2] <= depth.range[1] & obs[,2] >= depth.range[2]),]
      mod = mod[(mod[,2] <= depth.range[1] & mod[,2] >= depth.range[2]),]
    }
    dif = mod[,3]- obs[,3]
    pear_r = cor.test(obs[,3], mod[,3], method = 'pearson')
    var_obs = mean(((obs[,3]-mean(obs[,3], na.rm = na.rm))^2), na.rm = na.rm)
    var_mod = mean(((mod[,3]-mean(mod[,3], na.rm = na.rm))^2), na.rm = na.rm)
    SD_obs = sd(obs[,3], na.rm = na.rm)
    SD_mod = sd(mod[,3], na.rm = na.rm)
    cov = mean((obs[,3]-mean(obs[,3], na.rm = na.rm))*(mod[,3]-mean(mod[,3], na.rm = na.rm)), na.rm = na.rm)
    cor = cov/sqrt(var_obs*var_mod)
    bias = mean(dif, na.rm = na.rm)
    mae = mean(abs(dif), na.rm = na.rm)
    rmse = sqrt(mean(dif^2, na.rm = na.rm))
    nse = get_nse(mod[,3], obs[,3])
    summary_stats = data.frame(Pearson_r = pear_r$estimate,Variance_obs = var_obs,
                               Variance_mod = var_mod, SD_obs = SD_obs, SD_mod = SD_mod,
                               Covariance = cov, #Correlation =cor,
                               Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, row.names = c())
    return(summary_stats)
  }else{
    dif = mod- obs
    pear_r = cor.test(obs, mod, method = 'pearson')
    var_obs = mean(((obs-mean(obs, na.rm = na.rm))^2), na.rm = na.rm)
    var_mod = mean(((mod-mean(mod, na.rm = na.rm))^2), na.rm = na.rm)
    SD_obs = sd(obs, na.rm = na.rm)
    SD_mod = sd(mod, na.rm = na.rm)
    cov = mean((obs-mean(obs, na.rm = na.rm))*(mod-mean(mod, na.rm = na.rm)), na.rm = na.rm)
    cor = cov/sqrt(var_obs*var_mod)
    bias = mean(dif, na.rm = na.rm)
    mae = mean(abs(dif), na.rm = na.rm)
    rmse = sqrt(mean(dif^2, na.rm = na.rm))
    nse = get_nse(mod, obs)
    summary_stats = data.frame(Pearson_r = pear_r$estimate,Variance_obs = var_obs,
                               Variance_mod = var_mod, SD_obs = SD_obs, SD_mod = SD_mod,
                               Covariance = cov, #Correlation =cor,
                               Bias = bias, MAE = mae, RMSE = rmse, NSE = nse, row.names = c())
    return(summary_stats)
  }
  
}

checkHourFormat <- function(timest, hourst){
  if (format(strptime(timest,'%Y-%m-%d %H:%M:%S'),'%H:%M:%S') != hourst){
    newTimest <- paste(gsub( " .*$", "", timest), hourst)
  } else {
    newTimest <- timest
  }
  return(as.POSIXct(newTimest))
}