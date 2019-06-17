# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  TM_new
# Purpose:      Calculation of TM_new
# programmer:   Zhe Liu
# Date:         17-06-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

calculation <- function(receive) {
  
  if (length(fromJSON(receive)) != 0) {
    
    receive <<- receive
    
    send_data <- tryCatch({
      
      dat <- preprocess(receive = receive)
      result <- get_result(dat = dat, curves = curves, weightages = weightages)
      
      report <- get_report(result = result, dat = dat)
      rep_ability <- get_rep_ability(result = result)
      action_kpi <- get_action_kpi(rep_ability = rep_ability, dat = dat)
      
      send_data <- postprocess()
    }, error = function(e) {
      
      send_data <- list("error" = list("code" = 500,
                                       "msg" = "Calculation error. "))
    })
    
    send <- toJSON(list(records = list(list(value = as.list(send_data)))), auto_unbox = TRUE)
    sendResultMessage(paste0(options()$uri, "/topics"), "RReturnResult", send)
    print(prettify(send))
  }
  
  print("null")
}











