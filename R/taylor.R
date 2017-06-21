#' Taylor Diagram
#'
#' Create Taylor diagram from time-series data.
#'
#' @param obs Numeric; Standard deviation of observational dataset.
#' @param sim Numeric; List of vectors of standard deviations and correlations of each dataset, in that order.
#' @param dataNames Character; Names of datasets in order of c(obs, sim). If NULL, names are defined as 'obs', 'sim1',
#' 'sim2', etc.  Defaults to NULL.
#' @param dataColors Character; Specify colors for each dataset.  If NULL, colors are assigned automatically. Defaults
#' to NULL.
#' @export
#' @return Taylor diagram
#' @examples
#' taylor()



taylor <- function(allData, dataNames = NULL, dataColors = NULL){
    
  if (is.null(dim(allData))){
    stop('Taylor plot requires at least one simulation dataset')
  }else{
    listData <- split(allData, rep(1:ncol(allData), each = nrow(allData)))
    obs <- listData[[1]]
    sim <- listData[2:length(listData)]
  }
  
  # calc sd for each sim versus obs
  sigma_obs <- sd(obs, na.rm = T)
  sigma_sim <- unlist(lapply(sim, sd, na.rm = T))
    
  # get max sd to use for sigma major
  sigma_max <- max(c(sigma_obs, sigma_sim))
    
  # calc rmse between sim and obs
  rmseFun <- function(simVec, obsVec){
    return(sqrt( mean( (simVec - obsVec)^2, na.rm = TRUE) ))
  }
  rmse_all <- unlist(lapply(sim, rmseFun, obsVec = obs))
    
  # get max rmse for rms_major
  rmse_max <- max(rmse_all)
  
  # calculate correlations between sim and obs
  corFun <- function(simVec, obsVec){
    return(cor(x = simVec,
               y = obsVec,
               use = "pairwise.complete.obs"))
  }
  cor_all <- suppressWarnings(unlist(lapply(sim, corFun, obsVec = obs)))
    
  # assign sigma_reference as std of obs
    
  # Based off of:
  # https://stackoverflow.com/questions/24999338/r-taylor-diagram-plotting
  
  ####################################
  # SET UP PLOTTING PARAMETERS
  ####################################
  
  # - Correlation lines and tick marks
  # (major w/ lines to center, minor w/ tick marks only)
  correlation_major <- c(seq(-1,1,0.1),-0.95,0.95)
  correlation_minor <- c(seq(-1,-0.95,0.01),seq(-0.9,0.9,0.05),seq(0.95,1,0.01))
  
  # - Standard deviation lines and tick marks
  # (major w/ lines to center, minor w/ nothing now but can be changed)
    #sigma_major <- seq(15,60,15)  #lines
  sigma_major <- seq((ceiling(sigma_max)/4), ceiling(sigma_max), (ceiling(sigma_max)/4))
    #sigma_reference <- 5
  sigma_reference <- sigma_obs
  #sigma_minor <- seq(0.5,4,0.5)
  
  # - Centered RMS difference lines
  # N.B.: Centered RMS difference = centered RMS diff btwn the fields with any
  # diff in the means first removed, so basically sqrt((anom_mod-anom_obs)^2)
    #rms_major <- seq(15,90,15)
  rms_major <- seq(ceiling(rmse_max)/6, ceiling(rmse_max), ceiling(rmse_max)/6)
  
  # - Color schemes for the lines
  correlation_color <- 'black'
  sigma_color <- 'blue'
  rms_color <- 'green'
  
  # - Line types
  correlation_type <- 1
  sigma_type <- 1
  rms_type <- 1
  
  # - Plot parameters
  #par(pty='s')
  #par(mar=c(3,3,3,3)+0.1)
  
  ####################################
  # CREATE PLOT
  ####################################
  
  # - Set up plot spacing based on the sigma_major limits
  plot(NA,NA
       ,xlim=c(-max(sigma_major),max(sigma_major))
       ,ylim=c(0,max(sigma_major))
       ,xaxt='n'
       ,yaxt='n'
       ,xlab=''
       ,ylab=''
       ,bty='n')
  
  #------------------------------
  # Sigma related lines, tick marks, and labels
  #------------------------------
  # - Add sigma semicircles
  for(i in 1:length(sigma_major)){
    lines(x   = sigma_major[i]*cos(seq(0,pi,pi/1000)),
          y   = sigma_major[i]*sin(seq(0,pi,pi/1000)),
          col = sigma_color,
          lty = sigma_type,
          lwd = 1
          )
    }
  
  # - Add horizontal axis
  lines(x   = c(-max(sigma_major),max(sigma_major)),
        y   = c(0,0),
        col = sigma_color,
        lty = sigma_type,
        lwd = 1)
  
  # - Add sigma labels
  par(xpd=TRUE)
  text(x      = c(-sigma_major,0,sigma_major),
       y      = -sigma_reference/20,
       labels = as.character(c(-sigma_major,0,sigma_major)),
       col    = sigma_color,
       cex    = 0.7)
  par(xpd=FALSE)
  
  # - Add sigma title
    # text(x      = 0,
    #      y      = (-sigma_reference/7)-1.0,
    #      labels = "Standard Deviation",
    #      col    = sigma_color,
    #      cex    = 1)
  mtext(text = 'Standard Deviation',
        side = 1,
        col = sigma_color,
        line = 0.4)
  
  #------------------------------
  # Correlation related lines, tick marks, and labels
  #------------------------------
  # - Add correlation lines
  for(i in 1:length(correlation_major)){
    lines(c(0,1.02*max(sigma_major)*cos(acos(correlation_major[i])))
          ,c(0,1.02*max(sigma_major)*sin(acos(correlation_major[i])))
          ,lwd=2
          ,lty=correlation_type
          ,col=correlation_color
    )
  }
  
  # - Add correlation minor ticks
  for(i in 1:length(correlation_minor)){
    lines(max(sigma_major)*cos(acos(correlation_minor[i]))*c(1,1.01)
          ,max(sigma_major)*sin(acos(correlation_minor[i]))*c(1,1.01)
          ,lwd=2
          ,lty=correlation_type
          ,col=correlation_color
    )
  }
  
  # - Add correlation labels
  par(xpd=TRUE)
  text(x      = 1.06*max(sigma_major)*cos(acos(correlation_major)),
       y      = 1.06*max(sigma_major)*sin(acos(correlation_major)),
       labels = as.character(correlation_major),
       col    = correlation_color,
       cex    = 0.75)
  par(xpd=FALSE)
  
  # - Add correlation title
    # text(x      = 0,
    #      y      = (max(sigma_major)+sigma_reference/10)+0,
    #      labels = "Correlation",
    #      col    = correlation_color,
    #      cex    = 1)
  mtext(text = 'Correlation',
        side = 3,
        col = correlation_color,
        line = 0.6)
  
  #------------------------------
  # RMS difference-related lines, tick marks, and labels
  #------------------------------
  # - Add rms semicircles
  for(i in 1:length(rms_major)){
    inds <- which((rms_major[i]*cos(seq(0,pi,pi/1000))+sigma_reference)^2 + (rms_major[i]*sin(seq(0,pi,pi/1000)))^2 < max(sigma_major)^2)
    lines(rms_major[i]*cos(seq(0,pi,pi/1000))[inds]+sigma_reference
          ,rms_major[i]*sin(seq(0,pi,pi/1000))[inds]
          ,col=rms_color
          ,lty=rms_type
          ,lwd=1
    )
  }
  
  # - Add labels for the rms lines
  text(x      = -rms_major*cos(pi*rms_major/(sigma_reference*20))+sigma_reference,
       y      = rms_major*sin(pi*rms_major/(sigma_reference*20)),
       labels = as.character(round(rms_major, digits = 2)),
       col    = (rms_color),
       cex    = 0.7,
       font = 2,
       adj    = 1)
  
  # - Add title
      # text(x      = 0,
      #      y      = (-sigma_reference/4)-0,
      #      labels = 'Centered RMS Difference',
      #      col    = rms_color,
      #      cex    = 1,
      #      adj    = 0.5,
      #      font   = 2)
  mtext(text = 'Centered RMSE Difference',
        side = 1,
        font = 2,
        line = 1.4,
        col = rms_color)
  
  #------------------------------
  # Add model/data points
  #------------------------------
  if (is.null(dataColors)){
    allcolor <- topo.colors(length(sim) + 1)
  }else{
    allcolor = dataColors
  }
  
  
  # - Add observed/"truth" point
  # points(allsigma[1]
  #        ,0
  #        ,pch=19
  #        ,col=allcolor[1]
  #        ,cex=1.5)
  points(x   = sigma_obs,
         y   = 0,
         pch = 19,
         col = allcolor[1],
         cex = 1.5)
  
  # - Add other points
  # points(allsigma[-1]*cos(acos(allcc[-1]))
  #        ,allsigma[-1]*sin(acos(allcc[-1]))
  #        ,col=allcolor[-1]
  #        ,pch=19
  #        ,cex=1.5)
  
  for (i in 1:length(sigma_sim)){
    points(x   = sigma_sim[i],
           y   = cor_all[i],
           col = allcolor[i+1],
           pch = 19,
           cex = 1.5)
  }
  
  # - Plot legend
  par(xpd=TRUE)
  if (is.null(dataNames)){
    legNames <- c('obs', paste0('sim', seq(1:length(sim))))
  }else{
    legNames <- dataNames
  }
  
  legend(x      = 0,
         y      = -sigma_reference/4,
         legend = legNames,
         pc     = 19,
         col    = allcolor,
         ncol   = 3,
         bty    = 'n',
         xjust  = 0.5)
  
}

# obs <- 1:10
# sim1 <- c(1,3,3,5,6,8,7,9,10,11)
# sim2 <- c(1,5,3,5,2,8,7,9,10,15)
# allData <- cbind(obs, sim1, sim2)

# taylor(allData = allData)
