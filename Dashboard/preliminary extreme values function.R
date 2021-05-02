#this file creates a preliminary version of the extreme values function
#ultimately used in the dashboard

extval_info <- function(d, var_name, min_range_val, max_range_val){
  var = enquo(var_name)
  
  #getting table of unique donor IDs, accpted y/n, and var min/max
  extreme.table = d %>% 
    group_by(DONOR_ID) %>% 
    dplyr::summarize(Accepted = Accepted[1], 
                     min = min(!!var, na.rm=TRUE), 
                     max = max(!!var, na.rm=TRUE) ) #%>% 
  #mutate_at(c(min, max), function(x) ifelse(is.finite(x), x, NA))
  
  #getting maximum variable value for each donor for accepted and not accepted
  max_values_acc = extreme.table$max[which(extreme.table$Accepted == "Yes")]
  max_values_nonacc = extreme.table$max[which(extreme.table$Accepted == "No")]
  #getting minimum variable value for each donor for accepted and not accepted
  min_values_acc = extreme.table$min[which(extreme.table$Accepted == "Yes")]
  min_values_nonacc = extreme.table$min[which(extreme.table$Accepted == "No")]
  
  #boxplot of max variable value for variable for both accepted and not accepted
  boxplot(max_values_acc, max_values_nonacc, names=c("Accepted", "Not Accepted"), main=paste0("Max ", var))
  abline(h = min_range_val, col = "blue")
  abline(h = max_range_val, col = "blue")
  
  #boxplot of min variable value for variable for both accepted and not accepted
  boxplot(min_values_acc, min_values_nonacc, names=c("Accepted", "Not Accepted"), main=paste0("Min ", var))
  abline(h = min_range_val, col = "blue")
  abline(h = max_range_val, col = "blue")
  
  #getting percent of accepted donors with max within normal range
  max_values_acc = as.data.frame(max_values_acc)
  names(max_values_acc)[1] <- "values"
  total_num_max_acc = nrow(max_values_acc)
  
  if(!is.null(max_range_val)){
    max_values_acc = as.data.frame(max_values_acc[which(max_values_acc$values <= max_range_val),])
    names(max_values_acc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    max_values_acc = as.data.frame(max_values_acc[which(max_values_acc$values >= min_range_val),])
  }
  num_max_acc = nrow(max_values_acc)
  perc_max_acc = num_max_acc / total_num_max_acc * 100
  perc_max_acc = paste(round(perc_max_acc, digits=2), "%")
  
  #getting percent of accepted donors with min within normal range
  min_values_acc = as.data.frame(min_values_acc)
  names(min_values_acc)[1] <- "values"
  
  total_num_min_acc = nrow(min_values_acc)
  if(!is.null(max_range_val)){
    min_values_acc = as.data.frame(min_values_acc[which(min_values_acc$values <= max_range_val),])
    names(min_values_acc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    min_values_acc = as.data.frame(min_values_acc[which(min_values_acc$values >= min_range_val),])
  }
  num_min_acc = nrow(min_values_acc)
  perc_min_acc = num_min_acc / total_num_min_acc * 100
  perc_min_acc = paste(round(perc_min_acc, digits=2), "%")
  
  #getting percent of not accepted donors with max within normal range
  max_values_nonacc = as.data.frame(max_values_nonacc)
  names(max_values_nonacc)[1] <- "values"
  
  total_num_max_nonacc = nrow(max_values_nonacc)
  if(!is.null(max_range_val)){
    max_values_nonacc = as.data.frame(max_values_nonacc[which(max_values_nonacc$values <= max_range_val),])
    names(max_values_nonacc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    max_values_nonacc = as.data.frame(max_values_nonacc[which(max_values_nonacc$values >= min_range_val),])
  }
  num_max_nonacc = nrow(max_values_nonacc)
  perc_max_nonacc = num_max_nonacc / total_num_max_nonacc * 100
  perc_max_nonacc = paste(round(perc_max_nonacc, digits=2), "%")
  
  #getting percent of not accepted donors with min within normal range
  min_values_nonacc = as.data.frame(min_values_nonacc)
  names(min_values_nonacc)[1] <- "values"
  
  total_num_min_nonacc = nrow(min_values_nonacc)
  if(!is.null(max_range_val)){
    min_values_nonacc = as.data.frame(min_values_nonacc[which(min_values_nonacc$values <= max_range_val),])
    names(min_values_nonacc)[1] <- "values"
  }
  if(!is.null(min_range_val)){
    min_values_nonacc = as.data.frame(min_values_nonacc[which(min_values_nonacc$values >= min_range_val),])
  }
  num_min_nonacc = nrow(min_values_nonacc)
  perc_min_nonacc = num_min_nonacc / total_num_min_nonacc * 100
  perc_min_nonacc = paste(round(perc_min_nonacc, digits=2), "%")
  
  #getting average max and min values for accepted and for non accepted
  mean_max_info = data.frame("Average Max Value", round(mean(as.numeric(max_values_acc[[1]])), digits=2), 
                             round(mean(as.numeric(max_values_nonacc[[1]])), digits=2))
  mean_min_info = data.frame("Average Min Value", round(mean(as.numeric(min_values_acc[[1]])), digits=2), 
                             round(mean(as.numeric(min_values_nonacc[[1]])), digits=2))
  
  #arranging stats in table  
  range_max_info = data.frame("% of Donors with Max within Range", perc_max_acc, perc_max_nonacc)
  range_min_info = data.frame("% of Donors with Min within Range", perc_min_acc, perc_min_nonacc)
  names(mean_max_info) = c("", "Accepted Donors", "Not Accepted Donors")
  names(mean_min_info) = c("", "Accepted Donors", "Not Accepted Donors")
  names(range_max_info) = c("", "Accepted Donors", "Not Accepted Donors")
  names(range_min_info) = c("", "Accepted Donors", "Not Accepted Donors")
  var_info = rbind(mean_max_info, mean_min_info, range_max_info, range_min_info)
  print(var_info)
}

extval_info(data.after.BD, ABG_PH, 7.35, 7.45)
extval_info(data.after.BD, PAO2, 60, NULL)
extval_info(data.after.BD, PEEP, 5, 8)
