#' Downscale HUC-10 means to HUCs 2-8
#'
#' This functions downscales HUC-10 values to the HUC 2-8 levels.
#' Accepts outputs from the METsteps::aggregateRasterToPolygons (when MET.HUC10==T) and the ________ functions.
#' Currently requires the following packages: _______.
#' @param HUC10monthly Results directly from the METsteps::aggregateRasterToPolygons function.
#' @export
#' @return List of tables of HUC 2-8 means in 'zoo' format.  Time-step x HUC-10.
#' @examples
#' agg_Raster_to_HUC10

downscale_HUC10 <- function(HUC10monthly){
  require(raster)
  require(rgdal)
  require(sp)
  require(rgeos)
  require(progress)
  require(scales)
  require(zoo)
  require(fields)
  require(pbapply)
  require(lubridate)
  require(maptools)
  #---- Import HUC10
  base <- METsteps::HUC10_Shape
  
  #---- Create HUCX.info objects
  HUC2.info <- METsteps::HUC_IDs$HUC2.info
  HUC4.info <- METsteps::HUC_IDs$HUC4.info
  HUC6.info <- METsteps::HUC_IDs$HUC6.info
  HUC8.info <- METsteps::HUC_IDs$HUC8.info
  HUC10.info <- METsteps::HUC_IDs$HUC10.info

  #---- Data
  HUC10.data <- HUC10monthly$HUC10matrix
  step <- HUC10monthly$info['timeStep']
  
  #---- Time range
  timeRange <- as.numeric(gsub('X', '', colnames(HUC10.data)))
  
  #---- Convert to HUCS 2-8
  HUCs.all <- c(2,4,6,8,10)
  for (i in 1:length(HUCs.all)){
    cur.HUC <- HUCs.all[i]; cur.HUC
  
    if (cur.HUC == 10){
      writeLines(paste0('Downscaling to HUC', cur.HUC, ':'))
      start.t <- proc.time()
      HUC.ts <- stats::ts(data      = t(HUC10.data),
                          start     = lubridate::year(zoo::yearmon(timeRange[1])),
                          frequency = 12)
      HUC.zoo <- zoo::as.zoo(HUC.ts)
      colnames(HUC.zoo) <- paste('Series',
                                 1:ncol(HUC.zoo))
      colnames(HUC.zoo) <- HUC10.info
      assign(paste0('HUC',
                    cur.HUC,
                    '.zoo'),
             HUC.zoo)
      end.t <- proc.time()
      rt <- as.character(round(x      = (end.t - start.t)[3],
                               digits = 0))
      if (nchar(rt) < 2) {
        rt <- paste0('0',
                     rt)
        }
      writeLines(paste0('   |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed = ', rt, 's'))
      next
    } else {
      # Subset HUC10 names to current HUC
      HUC10.Hnew <- as.numeric(substring(text  = HUC10.info,
                                         first = 1,
                                         last  = cur.HUC))
      cur.percs <- METsteps::HUCX_to_HUC10_percData[[paste0('HUC',
                                                            cur.HUC,
                                                            '_to_HUC10_percs')]]
      
      # Apply conversion function to each row of HUC10 values
      convHUC <- function(foo){   #Input is HUCX.info[,1]
        # Sum and weight HUC-10 values to lower HUC level.
        #
        # Args:
        #   foo: HUC ID.
        #
        # Returns:
        #   Numeric vector of downscaled HUC10 values for region.
        
        # Identify rows of interest (ROI) for current HUC region in HUC2
        cur.percs.iso <- cur.percs[cur.percs[,1] == as.numeric(as.character(foo)), ]
        
        # Define weights
        total.percs <- sum(cur.percs.iso$PERCENTAGE)
        if (total.percs > 100){
          leftover <- total.percs - 100
          leftover <- leftover/nrow(cur.percs.iso)
          weights  <- (cur.percs.iso$PERCENTAGE-leftover)/100
        }
        if (total.percs < 100){   #If total percentages not equal to 100, distribute remaining percentage among hucs equally
          leftover <- 100-total.percs
          leftover <- leftover/(nrow(cur.percs.iso))
          weights  <- (cur.percs.iso$PERCENTAGE+leftover)/100
        } else{
          weights <- cur.percs.iso$PERCENTAGE/100
        }
        
        cur.HUCS <- cur.percs.iso$HUC10
        
        # Extract values for HUC10's of cur.HUCs
        HUC10.data.iso <- HUC10.data[(as.numeric(rownames(HUC10.data)) %in% cur.HUCS), ]
        rNames <- rownames(HUC10.data)[(as.numeric(rownames(HUC10.data)) %in% cur.HUCS)]
        
        # Error handling:  Make sure orders are the same as in cur.HUCS
        if (!(test_match_order(as.numeric(rNames), cur.HUCS))) stop('Orders incorrect')
        
        # Apply weights to data
        weighted.data <- HUC10.data.iso * weights
        
        # Return weighted average
        if (is.null(dim(weighted.data)) == FALSE){
          return(
            as.numeric(colSums(weighted.data, na.rm = T)))
        }else{
          return(
            as.numeric(weighted.data))
        }
      }
      
      # Collect current HUC data
      cur.info <- get(paste0('HUC',
                             cur.HUC,
                             '.info'))
      writeLines(paste0('Downscaling to HUC',
                        cur.HUC,
                        ':'))
      
      # lapply convHUC function
      out <- pbapply::pblapply(X   = cur.info,
                               FUN = convHUC)
      
      # Reformat data from list to matrix format
      HUC.conv <- matrix(data  = unlist(out),
                         byrow = T,
                         nrow  = length(get(paste0('HUC',cur.HUC,'.info'))),
                         ncol  = ncol(HUC10.data))
      
      # Convert matrix to timeseries zoo object
      HUC.ts <- stats::ts(data      = t(HUC.conv),
                          start     = lubridate::year(zoo::yearmon(timeRange[1])),
                          frequency = 12)
      HUC.zoo = zoo::as.zoo(HUC.ts)
      
      # Add HUC IDs as column names
      colnames(HUC.zoo) <- cur.info
      
      # Assign result to object
      assign(paste0('HUC',
                    cur.HUC,
                    '.zoo'),
             HUC.zoo)
    }
  }
  
  # Output return
  return(list(HUC2.zoo = HUC2.zoo,
              HUC4.zoo = HUC4.zoo,
              HUC6.zoo = HUC6.zoo,
              HUC8.zoo = HUC8.zoo,
              HUC10.zoo = HUC10.zoo,
              # Add information
              info = HUC10monthly$info
         ))
}





