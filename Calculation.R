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
      
      application <- fromJSON(receive, simplifyDataFrame = TRUE)[["value"]][["header"]][["application"]]
      
      if (application == "ucb") {
        dat <- preprocess(receive = receive)
        
        result <- get_result(input_data = dat$input_data, p_data1 = dat$p_data1, p_data4 = dat$p_data4, 
                             current_phase = dat$phase, curves = curves, weightages = weightages)
        
        report <- get_report(result = result, competitor_data = dat$competitor_data)
        assessment <- get_assessment(result = result, p_data2 = dat$p_data2, scenarios = dat$scenarios)
        
        send_data <- postprocess(report = report, assessment = assessment, dat = dat)
        
      } else if (application == "tmist") {
        dat <- preprocess_tm(receive = receive)
        
        result <- get_result_tm(cal_data = dat$cal_data, manager_data = dat$manager_data, 
                                p_customer_relationship = p_customer_relationship, 
                                curves = curves, weightages = weightages)
        
        representative_info <- get_representative_info_tm(result = result, p_action_kpi = dat$p_action_kpi)
        sales_report <- get_sales_report_tm(result = result, competition_data = dat$competition_data)
        assessment <- get_assessment_tm(result = result, representative_ability = representative_info$representative_ability, 
                                        p_representative_ability = dat$p_representative_ability, standard_time = standard_time, 
                                        level_data = level_data)
        
        send_data <- postprocess_tm(headers = dat$headers, scenario = dat$scenario, sales_report = sales_report, 
                                    representative_info = representative_info, assessment = assessment)
      }
      
      
    }, error = function(e) {
      
      send_data <- list("error" = list("code" = 500,
                                       "msg" = "Calculation error. "))
    })
    
    send <- toJSON(list(records = list(list(value = as.list(send_data)))),auto_unbox = TRUE)
    sendResultMessage(paste0(options()$uri, "/topics"), options()$sendTopics, send)
  }
}











