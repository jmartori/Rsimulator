# using the ml_data and ml_data_log from latency.r that are stored in a rdata file.
#
# We check a bit more the relation of FP with RT and WT.
# 


v_filename=c("data/pbs_2_c1.rdata")


for (fn in v_filename)
{
  if (file.exists(fn)){
    load(fn)
    if (! exists("ml_data") && exists("ml_dc_er_log") && exists("l_config") ){
      warning(sprintf("Key Variables are missing...\nAre you sure you loaded the right rdata file?\nSkipping ...", fn))
      break  
    }
  } else {
    warning(sprintf("File %s doesn't exist...\nSkipping ...", fn))
    break
  }
  
  
  
  
}