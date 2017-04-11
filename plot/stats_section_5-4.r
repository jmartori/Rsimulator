source("R/utils.r")
combine <- function(data, what=c(dc,dnc,dc_er)){
  mm <- NULL
  for (i in 1:dim(data)[2]) {
    
    if (what == "dc_er"){
      mm <- rbind(mm, data[,i][["dc_er"]]$data)
    } else{
      mm <- rbind(mm, data[,i][[what]])
    }
  }
  return (mm)
}

run <- function(k=1){
  pbs_df <- data.frame(row.names = c("mean", "median", "sd","99.9th", "P2P mean", "P2P median", "P2P sd"))
  pbs <- new.env()
  
  fn <- sprintf("data/pbs_%d_iter_25_c1_10-1-01_rate_50.rdata", k)
  
  load(fn, envir = pbs)
  
  dc <- combine(pbs$r_data.c10, what="dc")
  dnc <- combine(pbs$r_data.c10, what="dnc")
  dc_er.c10 <- combine(pbs$r_data.c10, what="dc_er")
  dc_er.c1 <- combine(pbs$r_data.c1, what="dc_er")
  dc_er.c01 <- combine(pbs$r_data.c01, what="dc_er")
  
  dt <- get_operation_delivery_time(dnc)
  lt <- get_operation_latency_time(dnc)
  lt <- lt[lt > 0]
  dnc.stats <- c( mean(dt), median(dt), sd(dt), quantile(dt, probs=0.999), mean(lt), median(lt), sd(lt))
  
  dt <- get_operation_delivery_time(dc)
  dc.stats <- c( mean(dt), median(dt), sd(dt), quantile(dt, probs=0.999), mean(lt), median(lt), sd(lt))
    
  dt <- get_operation_delivery_time(dc_er.c10)
  dc.c10.stats <- c( mean(dt), median(dt), sd(dt), quantile(dt, probs=0.999), mean(lt), median(lt), sd(lt))
  
  dt <- get_operation_delivery_time(dc_er.c1)
  dc.c1.stats <- c( mean(dt), median(dt), sd(dt), quantile(dt, probs=0.999), mean(lt), median(lt), sd(lt))
  
  dt <- get_operation_delivery_time(dc_er.c01)
  dc.c01.stats <- c( mean(dt), median(dt), sd(dt), quantile(dt, probs=0.999), mean(lt), median(lt), sd(lt))
  
  
  pbs_df["dc"] <- dc.stats
  pbs_df["dnc"] <- dnc.stats
  pbs_df["dc.c10"] <- dc.c10.stats
  pbs_df["dc.c1"] <- dc.c1.stats
  pbs_df["dc.c01"] <- dc.c01.stats
  
  
  cn <- colnames(pbs_df)
  cn <- paste(k, cn,sep="_")
  colnames(pbs_df) <- cn
  
  return (pbs_df)
}

if (exists("pbs_df") ==FALSE){
  pbs_df <- run(1)
  pbs_df <- cbind(pbs_df, run(2))
  pbs_df <- cbind(pbs_df, run(3))
  pbs_df <- cbind(pbs_df, run(4))
  # Now df is yours to use!
} else {
  warning("df variable already in use. skipped creating it. you can rm(df) and run again to load it.")
}