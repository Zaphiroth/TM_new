# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  TM_new
# Purpose:      Functions of TM_new
# programmer:   Zhe Liu & Peng Qian
# Date:         17-06-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##------------------------------------------------------------------------------
##--                 Curves computation
##------------------------------------------------------------------------------

curve_func <- function(curve, curves, input) {
  
  curve_data <- curves[[curve]]
  
  if (input < min(curve_data$x))
    return(curve_data[which.min(curve_data$x), 2])
  
  if (input > max(curve_data$x))
    return(curve_data[which.max(curve_data$x), 2])
  
  left <- curve_data[which.min(abs(input - curve_data$x)), ]
  tmp <- curve_data[-which.min(abs(input - curve_data$x)), ]
  right <- tmp[which.min(abs(input - tmp$x)), ]
  
  y <- ifelse(left$x <= right$x,
              (1 - (input - left$x) / (right$x - left$x)) * left$y + (1 - (right$x - input) / (right$x - left$x)) * right$y, 
              (1 - (input - right$x) / (left$x - right$x)) * right$y + (1 - (left$x - input) / (left$x - right$x)) * left$y)
  
  return(y)
}


##------------------------------------------------------------------------------
##--                 Data preprocess
##------------------------------------------------------------------------------

preprocess <- function(receive) {}



##------------------------------------------------------------------------------
##--                 Calculation function
##------------------------------------------------------------------------------

get_result <- function(dat, curves, weightages) {
  
  dat <- dat %>% 
    mutate(budget = budget / total_budget * 100)
  
  # general ability
  dat01 <- dat %>% 
    mutate(work_motivation = p_work_motivation + (10 - p_work_motivation) * 0.15 * (performance_review + career_development_guide),
           territory_management_ability = p_territory_management_ability + (10 - p_territory_management_ability) * 0.3 * territory_management_training,
           sales_skills = p_sales_skills + (10 - p_sales_skills) * 0.3 * sales_skills_training,
           product_knowledge = p_product_knowledge + (10 - p_product_knowledge) * 0.3 * product_knowledge_training,
           behavior_efficiency_factor = sapply(one_on_one_coaching, function(x) {curve_func("curve09", curves, x)}),
           behavior_efficiency = p_behavior_efficiency + (10 - p_behavior_efficiency) * behavior_efficiency_factor,
           general_ability = (territory_management_ability * weightages[["weightage02"]]$territory_management_ability + 
                                sales_skills * weightages[["weightage02"]]$sales_skills + 
                                product_knowledge * weightages[["weightage02"]]$product_knowledge + 
                                behavior_efficiency * weightages[["weightage02"]]$behavior_efficiency + 
                                work_motivation * weightages[["weightage02"]]$work_motivation) * 10)
  
  # rep ability efficiency
  dat02 <- dat01 %>% 
    mutate(quota_restriction_factor = ifelse(quota / p_sales < 0.5 | quota / p_sales > 2, 
                                             0.8, 
                                             ifelse(quota / p_sales >= 0.5 & quota / p_sales <= 2, 
                                                    1, 
                                                    0)),
           quota_restriction_factor = sapply(quota_restriction_factor, function(x) {curve_func("curve14", curves, x)}),
           rep_ability_efficiency = general_ability * weightages[["weightage03"]]$general_ability + 
             call_time_factor * weightages[["weightage03"]]$call_time_factor + 
             quota_restriction_factor * weightages[["weightage03"]]$quota_restriction_factor)
  
  # field work factor
  dat03 <- dat02 %>% 
    mutate(field_work_factor = sapply(field_work, function(x) {curve_func("curve16", curves, x)}))
  
  # deployment quality
  dat04 <- dat03 %>% 
    mutate(business_strategy_planning_factor = sapply(business_strategy_planning, function(x) {curve_func("curve18", curves, x)}),
           admin_work_factor = sapply(admin_work, function(x) {curve_func("curve19", curves, x)}),
           employee_kpi_and_compliance_check_factor = sapply(employee_kpi_and_compliance_check, function(x) {curve_func("curve20", curves, x)}),
           team_meeting_factor = sapply(team_meeting, function(x) {curve_func("curve21", curves, x)}),
           kol_management_factor = sapply(kol_management, function(x) {curve_func("curve22", curves, x)}),
           deployment_quality = business_strategy_planning_factor * weightages[["weightage04"]]$business_strategy_planning_factor + 
             admin_work_factor * weightages[["weightage04"]]$admin_work_factor + 
             employee_kpi_and_compliance_check_factor * weightages[["weightage04"]]$employee_kpi_and_compliance_check_factor + 
             team_meeting_factor * weightages[["weightage04"]]$team_meeting_factor + 
             kol_management_factor * weightages[["weightage04"]]$kol_management_factor)
  
  # sales performance
  dat05 <- dat04 %>% 
    mutate(sales_performance = rep_ability_efficiency * weightages[["weightage05"]]$rep_ability_efficiency + 
             field_work_factor * weightages[["weightage05"]]$field_work_factor + 
             deployment_quality * weightages[["weightage05"]]$deployment_quality)
  
  # customer relationship
  dat06 <- dat05 %>% 
    mutate(budget_factor = ifelse(hosp_size == 1, 
                                  sapply(budget, function(x) {curve_func("curve02", curves, x)}), 
                                  ifelse(hosp_size == 2, 
                                         sapply(budget, function(x) {curve_func("curve03", curves, x)}), 
                                         ifelse(hosp_size == 3, 
                                                sapply(budget, function(x) {curve_func("curve04", curves, x)}), 
                                                0))),
           meeting_attendance_factor = ifelse(hosp_size == 1, 
                                              sapply(meeting_attendance, function(x) {curve_func("curve05", curves, x)}), 
                                              ifelse(hosp_size == 2, 
                                                     sapply(meeting_attendance, function(x) {curve_func("curve06", curves, x)}), 
                                                     ifelse(hosp_size == 3, 
                                                            sapply(meeting_attendance, function(x) {curve_func("curve07", curves, x)}), 
                                                            0))),
           customer_relationship_factor = budget_factor * weightages[["weightage06"]]$budget_factor + 
             meeting_attendance_factor * weightages[["weightage06"]]$meeting_attendance_factor,
           customer_relationship = p_customer_relationship + (100 - p_customer_relationship) * customer_relationship_factor)
  
  # current oa
  dat07 <- dat06 %>% 
    mutate(current_oa = sales_performance * weightages[["weightage07"]]$sales_performance + 
             customer_relationship * weightages[["weightage07"]]$customer_relationship)
  
  # offer attractiveness
  dat08 <- dat07 %>% 
    mutate(offer_attractiveness = ifelse(life_cycle == "导入期", 
                                         current_oa * weightages[["weightage10"]]$current_oa + 
                                           p_offer_attractiveness * weightages[["weightage10"]]$p_offer_attractiveness, 
                                         ifelse(life_cycle == "成熟期", 
                                                current_oa * weightages[["weightage11"]]$current_oa + 
                                                  p_offer_attractiveness * weightages[["weightage11"]]$p_offer_attractiveness, 
                                                0)))
  
  # market share, sales
  dat09 <- dat08 %>% 
    mutate(potential = p_potential,
           market_share = sapply(offer_attractiveness, function(x) {curve_func("curve28", curves, x)}),
           market_share = round(market_share / 100, 2),
           sales = round(potential * market_share / 4, 2),
           quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)))
  
  return(dat09)
}

##------------------------------------------------------------------------------
##--                 Update representative information
##------------------------------------------------------------------------------

get_rep_ability <- function(result) {
  
  rep_ability <- results %>% 
    group_by(rep_id, product_knowledge, sales_skills, territory_management_ability, work_motivation, behavior_efficiency) %>% 
    summarise(potential = sum(potential),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    ungroup() %>% 
    mutate(work_motivation = ifelse(sales / quota >= 0.9 & sales / quota <= 1.2, 
                                    work_motivation + (10 - work_motivation) * 0.2, 
                                    ifelse(sales / quota < 0.9 | sales / quota > 1.2, 
                                           work_motivation, 
                                           0))) %>%
    mutate(product_knowledge = round(product_knowledge, 1),
           sales_skills = round(sales_skills, 1),
           territory_management_ability = round(territory_management_ability, 1),
           work_motivation = round(work_motivation, 1),
           behavior_efficiency = round(behavior_efficiency, 1)) %>% 
    select(`rep_id`, `product_knowledge`, `sales_skills`, `territory_management_ability`, `work_motivation`, `behavior_efficiency`)
  colnames(rep_ability) <- c("representative-id", "product-knowledge", "sales-ability", "regional-management-ability", "job-enthusiasm", "behavior-validity")
  
  return(rep_ability)
}

get_action_kpi <- function(rep_ability, dat) {
  
  action_kpi <- p_action_kpi %>% 
    left_join(rep_ability, by = c("representative-id")) %>% 
    mutate(class1 = ifelse(`behavior-validity` >= 0 & `behavior-validity` < 3, 
                           1, 
                           ifelse(`behavior-validity` >= 3 & `behavior-validity` < 6, 
                                  2, 
                                  ifelse(`behavior-validity` >= 6 & `behavior-validity` < 8, 
                                         3, 
                                         ifelse(`behavior-validity` >= 8 & `behavior-validity` <= 10, 
                                                4, 
                                                0))))) %>% 
    mutate(`target-coverage` = ifelse(class1 == 1, 
                                      sapply(`target-coverage`, function(x) {x - sample(5:10, 1)}), 
                                      ifelse(class1 == 2, 
                                             sapply(`target-coverage`, function(x) {x - sample(0:5, 1)}), 
                                             ifelse(class1 == 3, 
                                                    sapply(`target-coverage`, function(x) {x + sample(0:5, 1)}), 
                                                    ifelse(class1 == 4, 
                                                           sapply(`target-coverage`, function(x) {x + sample(5:10, 1)}), 
                                                           0))))) %>% 
    mutate(class2 = ifelse(`job-enthusiasm` >= 0 & `job-enthusiasm` < 3, 
                           1, 
                           ifelse(`job-enthusiasm` >= 3 & `job-enthusiasm`< 6, 
                                  2, 
                                  ifelse(`job-enthusiasm` >= 6 & `job-enthusiasm` < 8, 
                                         3, 
                                         ifelse(`job-enthusiasm` >= 8 & `job-enthusiasm` < 10, 
                                                4, 
                                                0))))) %>% 
    mutate(`high-level-frequency` =  ifelse(class1 == 1, 
                                            sapply(`high-level-frequency`, function(x) {sample(13:14, 1)}), 
                                            ifelse(class1 == 2, 
                                                   sapply(`high-level-frequency`, function(x) {sample(14:15, 1)}), 
                                                   ifelse(class1 == 3, 
                                                          sapply(`high-level-frequency`, function(x) {sample(16:18, 1)}), 
                                                          ifelse(class1 == 4, 
                                                                 sapply(`high-level-frequency`, function(x) {sample(19:22, 1)}), 
                                                                 0)))),
           `middle-level-frequency` = ifelse(class1 == 1, 
                                             sapply(`middle-level-frequency`, function(x) {sample(13:14, 1)}), 
                                             ifelse(class1 == 2, 
                                                    sapply(`middle-level-frequency`, function(x) {sample(13:14, 1)}), 
                                                    ifelse(class1 == 3, 
                                                           sapply(`middle-level-frequency`, function(x) {sample(12:13, 1)}), 
                                                           ifelse(class1 == 4, 
                                                                  sapply(`middle-level-frequency`, function(x) {sample(12:13, 1)}), 
                                                                  0)))),
           `low-level-frequency` = ifelse(class1 == 1, 
                                          sapply(`low-level-frequency`, function(x) {sample(13:14, 1)}), 
                                          ifelse(class1 == 2, 
                                                 sapply(`low-level-frequency`, function(x) {sample(12:13, 1)}), 
                                                 ifelse(class1 == 3, 
                                                        sapply(`low-level-frequency`, function(x) {sample(12:13, 1)}), 
                                                        ifelse(class1 == 4, 
                                                               sapply(`low-level-frequency`, function(x) {sample(11:12, 1)}), 
                                                               0))))) %>% 
    mutate(`high-level-frequency` = ifelse(class2 == 1, 
                                           sapply(`high-level-frequency`, function(x) {x - sample(1:2, 1)}), 
                                           ifelse(class2 == 2, 
                                                  sapply(`high-level-frequency`, function(x) {x - sample(0:1, 1)}), 
                                                  ifelse(class2 == 3, 
                                                         sapply(`high-level-frequency`, function(x) {x + sample(0:1, 1)}),
                                                         ifelse(class2 == 4, 
                                                                `high-level-frequency` + 1,
                                                                0)))),
           `middle-level-frequency` = ifelse(class2 == 1, 
                                             `middle-level-frequency` - 2, 
                                             ifelse(class2 == 2, 
                                                    `middle-level-frequency` - 1, 
                                                    ifelse(class2 == 3, 
                                                           sapply(`middle-level-frequency`, function(x) {x + sample(0:1, 1)}), 
                                                           ifelse(class2 == 4, 
                                                                  `middle-level-frequency` + 1, 
                                                                  0)))),
           `low-level-frequency` = ifelse(class2 == 1, 
                                          `low-level-frequency` - 2, 
                                          ifelse(class2 == 2, 
                                                 `low-level-frequency` - 1, 
                                                 ifelse(class2 == 3, 
                                                        sapply(`low-level-frequency`, function(x) {x + sample(0:1, 1)}), 
                                                        ifelse(class2 == 4, 
                                                               `low-level-frequency` + 1, 
                                                               0))))) %>% 
    select(`representative-id`, `target-number`, `target-coverage`, `high-level-frequency`, `middle-level-frequency`, `low-level-frequency`)
  
  return(action_kpi)
}

##------------------------------------------------------------------------------
##--                 Generate reports
##------------------------------------------------------------------------------

get_report <- function(result, dat) {
  
  hosp_report <- results %>% 
    mutate(growth = round(sales / p_sales - 1, 2)) %>% 
    select(`dest_id`, `resource_id`, `goods_id`, `potential`, `sales`, `quota`, `market_share`, `quota_rate`, `growth`)
  colnames(hosp_report) <- c("dest-config-id", "resource-config-id", "goods-config-id", "potential", "sales", "sales-quota", "share", "quota-achievement", "sales-growth")
  
  rep_report <- results %>% 
    select(`resource_id`, `goods_id`, `potential`, `p_sales`, `sales`, `quota`) %>% 
    group_by(resource_id, goods_id) %>% 
    summarise(potential = sum(potential),
              p_sales = sum(p_sales),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    ungroup() %>% 
    mutate(market_share = round(sales / potential * 4, 2),
           quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)),
           growth = round(sales / p_sales - 1, 2)) %>% 
    select(`resource_id`, `goods_id`, `potential`, `sales`, `quota`, `market_share`, `quota_rate`, `growth`)
  colnames(rep_report) <- c("resource-config-id", "goods-config-id", "potential", "sales", "sales-quota", "share", "quota-achievement", "sales-growth")
  
  prod1_report <- results %>% 
    select(`goods_id`, `potential`, `p_sales`, `sales`, `quota`) %>% 
    group_by(goods_id) %>% 
    summarise(potential = sum(potential),
              p_sales = sum(p_sales),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    ungroup() %>% 
    mutate(market_share = round(sales / potential * 4, 2),
           quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)),
           growth = round(sales / p_sales - 1, 2)) %>% 
    select(`goods_id`, `sales`, `quota`, `market_share`, `quota_rate`, `growth`)
  
  market_share1 <- sample(50:55, 1)/100 - prod1_report$market_share
  market_share2 <- market_share1 * sample(60:75, 1)/100
  market_share3 <- market_share1 - market_share2
  
  potential <- prod1_report$sales / prod1_report$market_share
  
  prod2_report <- tibble(goods_id = p_product_sales_report_info$`goods-config-id`[2:3],
                         market_share = c(market_share2, market_share3)) %>% 
    mutate(sales = round(potential * market_share, 2),
           quota = round(sales, -5),
           quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)),
           growth = round(sales / p_product_sales_report_info$sales[2:3] - 1, 2),
           market_share = round(market_share, 2))
  
  prod_report <- bind_rows(prod1_report, prod2_report)
  
  colnames(prod_report) <- c("goods-config-id", "sales", "sales-quota", "share", "quota-achievement", "sales-growth")
  
  report <- list("hospital_report" = hosp_report,
                 "representative_report" = rep_report,
                 "product_report" = prod_report)
  
  return(report)
}


##------------------------------------------------------------------------------
##--                 Data postprocess
##------------------------------------------------------------------------------

postprocess <- function() {}





