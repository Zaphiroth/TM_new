# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  TM_new
# Purpose:      Functions of TM new
# programmer:   Zhe Liu
# Date:         08-07-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##---- Curve ----
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

##---- Preprocess ----
preprocess_tm <- function(receive) {
  
  data_list <- fromJSON(receive, simplifyVector = FALSE)[[1]][["value"]] %>% 
    toJSON(auto_unbox = TRUE) %>% 
    fromJSON(simplifyDataFrame = TRUE)
  
  headers <- list(
    "header" = data_list[["header"]],
    "account" = data_list[["account"]],
    "proposal" = data_list[["proposal"]],
    "paperInput" = data_list[["paperInput"]]
  )
  
  scenario <- data_list[["currentScenario"]][["id"]]
  phase <- data_list[["currentScenario"]][["phase"]]
  
  p_hospital <- data_list[["body"]][["histories"]][["hospitals"]] %>% 
    rename("hospital" = "hospital-name",
           "representative" = "representative-name",
           "product" = "product-name",
           "p_sales" = "sales",
           "p_quota" = "sales-quota",
           "p_share" = "share") %>% 
    select(hospital, product, potential, p_sales, p_quota, p_share)
  
  p_product <- data_list[["body"]][["histories"]][["products"]] %>% 
    rename("goods_id" = "goods-config-id",
           "product" = "product-name",
           "life_cycle" = "life-cycle",
           "p_share" = "share")
  
  p_representative <- data_list[["body"]][["histories"]][["representatives"]] %>% 
    rename("representative" = "representative-name",
           "p_product_knowledge" = "product-knowledge",
           "p_sales_skills" = "sales-ability",
           "p_territory_management_ability" = "regional-management-ability",
           "p_work_motivation" = "job-enthusiasm",
           "p_behavior_efficiency" = "behavior-validity",
           "representative_time" = "total-time",
           "target" = "target-number",
           "target_coverage" = "target-coverage",
           "high_target" = "high-level-frequency",
           "middle_target" = "middle-level-frequency",
           "low_target" = "low-level-frequency")
  
  p_representative_ability <- p_representative %>%
    select(representative, p_product_knowledge, p_sales_skills, p_territory_management_ability,
           p_work_motivation, p_behavior_efficiency)
  
  p_action_kpi <- p_representative %>% 
    select(representative, target, target_coverage, high_target, middle_target, low_target)
  
  input_hospital <- data_list[["body"]][["inputs"]] %>% 
    select(`dest-config-id`, `hospital-name`, `hospital-level`) %>% 
    rename("dest_id" = "dest-config-id",
           "hospital" = "hospital-name",
           "hospital_level" = "hospital-level")
  
  input_representative <- data_list[["body"]][["inputs"]][["representative"]] %>% 
    rename("resource_id" = "resource-config-id",
           "representative_id" = "representative-id",
           "representative" = "representative-name",
           "product_knowledge_training" = "product-knowledge-training",
           "sales_skills_training" = "sales-ability-training",
           "territory_management_training" = "region-training",
           "performance_review" = "performance-training",
           "career_development_guide" = "vocational-development",
           "field_work" = "assist-access-time",
           "one_on_one_coaching" = "ability-coach")
  
  input_manager <- data_list[["body"]][["inputs"]][["manager"]] %>% 
    rename("business_strategy_planning" = "strategy-analysis-time",
           "admin_work" = "admin-work-time",
           "employee_kpi_and_compliance_check" = "KPI-analysis-time",
           "team_meeting" = "team-meeting-time",
           "kol_management" = "client-management-time")
  
  input_product <- do.call(bind_rows, data_list[["body"]][["inputs"]][["products"]]) %>% 
    rename("goods_id" = "goods-config-id",
           "product" = "product-name",
           "quota" = "sales-target",
           "meeting_attendance" = "meeting-places",
           "call_time" = "visit-time")
  
  cal_data <- bind_cols(input_hospital, input_product, input_representative, input_manager) %>% 
    left_join(p_hospital, by = c("hospital", "product")) %>% 
    left_join(p_product[c("life_cycle", "product")], by = c("product")) %>% 
    left_join(p_representative[c("representative", "representative_time", "p_territory_management_ability", "p_sales_skills", 
                                 "p_product_knowledge", "p_behavior_efficiency", "p_work_motivation")], 
              by = c("representative"))
  
  manager_data <- data_list[["body"]][["histories"]][["manager"]]
  names(manager_data) <- c("total_kpi", "manager_time", "total_budget", "total_quota", "total_place")
  
  competition_data <- p_product %>% 
    filter(!(product %in% cal_data$product))
  
  dat <- list(
    "headers" = headers,
    "scenario" = scenario,
    "phase" = phase,
    "cal_data" = cal_data,
    "p_representative_ability" = p_representative_ability,
    "p_action_kpi" = p_action_kpi,
    "manager_data" = manager_data,
    "competition_data" = competition_data
  )
  
  return(dat)
}

##---- Postprocess ----
postprocess_tm <- function(headers, scenario, sales_report, representative_info, assessment) {
  
  names(sales_report$hospital_sales_report) <- c("dest-config-id", "goods-config-id", "resource-config-id", "potential", "sales", 
                                                 "sales-quota", "share", "quota-achievement", "sales-growth")
  names(sales_report$representative_sales_report) <- c("resource-config-id", "goods-config-id", "potential", "sales", "sales-quota", 
                                                       "share", "quota-achievement", "sales-growth")
  names(sales_report$product_sales_report) <- c("goods-config-id", "sales", "sales-quota", "share", "quota-achievement", "sales-growth")
  representative_info$representative_ability <- select(representative_info$representative_ability, -representative, -resource_id)
  names(representative_info$representative_ability) <- c("representative-id", "product-knowledge", "sales-ability", "regional-management-ability", 
                                                         "job-enthusiasm", "behavior-validity")
  names(representative_info$action_kpi) <- c("representative-id", "target-number", "target-coverage", "high-level-frequency", 
                                             "middle-level-frequency", "low-level-frequency")
  
  send_list <- list(
    "header" = headers$header,
    "account" = headers$account,
    "proposal" = headers$proposal,
    "scenario" = scenario,
    "paperInput" = headers$paperInput,
    "body" = list(
      "hospitalSalesReports" = sales_report$hospital_sales_report,
      "representativeSalesReports" = sales_report$representative_sales_report,
      "productSalesReports" = sales_report$product_sales_report,
      "representativeAbility" = representative_info$representative_ability,
      "actionKpi" = representative_info$action_kpi,
      "regionDivisionResult" = list(
        "level" = assessment$level[1],
        "code" = assessment$code[1]
      ),
      "targetAssignsResult" = list(
        "level" = assessment$level[2],
        "code" = assessment$code[2]
      ),
      "resourceAssignsResult" = list(
        "level" = assessment$level[3],
        "code" = assessment$code[3]
      ),
      "manageTimeResult" = list(
        "level" = assessment$level[4],
        "code" = assessment$code[4]
      ),
      "manageTeamResult" = list(
        "level" = assessment$level[5],
        "code" = assessment$code[5]
      ),
      "generalPerformanceResult" = list(
        "level" = assessment$level[6],
        "code" = assessment$code[6]
      )
    )
  )
  
  return(send_list)
}

##---- Calculation ----
get_result_tm <- function(cal_data, manager_data, curves, weightages) {
  
  # general ability
  dat01 <- cal_data %>% 
    mutate(level_factor = 0.8 * potential / sum(potential) + 0.2 * p_sales / sum(p_sales),
           level = ifelse(level_factor > 0.15,
                          3,
                          ifelse(level_factor <= 0.05,
                                 1,
                                 2))) %>% 
    mutate(work_motivation = p_work_motivation + (10 - p_work_motivation) * 0.15 * (performance_review + career_development_guide),
           territory_management_ability = p_territory_management_ability + (10 - p_territory_management_ability) * 0.3 * territory_management_training,
           sales_skills = p_sales_skills + (10 - p_sales_skills) * 0.3 * sales_skills_training,
           product_knowledge = p_product_knowledge + (10 - p_product_knowledge) * 0.3 * product_knowledge_training,
           behavior_efficiency_factor = sapply(one_on_one_coaching, function(x) {curve_func("curve09", curves, x)}),
           behavior_efficiency = p_behavior_efficiency + (10 - p_behavior_efficiency) * 0.3 * behavior_efficiency_factor,
           general_ability = (territory_management_ability * weightages[["weightage02"]]$territory_management_ability + 
                                sales_skills * weightages[["weightage02"]]$sales_skills + 
                                product_knowledge * weightages[["weightage02"]]$product_knowledge + 
                                behavior_efficiency * weightages[["weightage02"]]$behavior_efficiency + 
                                work_motivation * weightages[["weightage02"]]$work_motivation) * 10)
  
  # rep ability efficiency
  dat02 <- dat01 %>% 
    mutate(call_time_index = ifelse(level == 1,
                                    sapply(call_time, function(x) {curve_func("curve10", curves, x)}),
                                    ifelse(level == 2,
                                           sapply(call_time, function(x) {curve_func("curve11", curves, x)}),
                                           ifelse(level == 3,
                                                  sapply(call_time, function(x) {curve_func("curve12", curves, x)}),
                                                  0))),
           quota_restriction_factor = ifelse(quota / p_sales < 0.5 | quota / p_sales > 2, 
                                             0.6, 
                                             ifelse(quota / p_sales >= 0.5 & quota / p_sales <= 2, 
                                                    1, 
                                                    0)),
           quota_restriction_index = sapply(quota_restriction_factor, function(x) {curve_func("curve14", curves, x)}),
           rep_ability_efficiency = general_ability * weightages[["weightage03"]]$general_ability + 
             call_time_index * weightages[["weightage03"]]$call_time_index + 
             quota_restriction_index * weightages[["weightage03"]]$quota_restriction_index)
  
  # field work factor
  dat03 <- dat02 %>% 
    mutate(field_work_index = sapply(field_work, function(x) {curve_func("curve16", curves, x)}))
  
  # deployment quality
  dat04 <- dat03 %>% 
    mutate(business_strategy_planning_index = sapply(business_strategy_planning, function(x) {curve_func("curve18", curves, x)}),
           admin_work_index = sapply(admin_work, function(x) {curve_func("curve18", curves, x)}),
           employee_kpi_and_compliance_check_index = sapply(employee_kpi_and_compliance_check, function(x) {curve_func("curve18", curves, x)}),
           team_meeting_index = sapply(team_meeting, function(x) {curve_func("curve18", curves, x)}),
           kol_management_index = sapply(kol_management, function(x) {curve_func("curve18", curves, x)}),
           deployment_quality = business_strategy_planning_index * weightages[["weightage04"]]$business_strategy_planning_index + 
             admin_work_index * weightages[["weightage04"]]$admin_work_index + 
             employee_kpi_and_compliance_check_index * weightages[["weightage04"]]$employee_kpi_and_compliance_check_index + 
             team_meeting_index * weightages[["weightage04"]]$team_meeting_index + 
             kol_management_index * weightages[["weightage04"]]$kol_management_index)
  
  # sales performance
  dat05 <- dat04 %>% 
    mutate(sales_performance = rep_ability_efficiency * weightages[["weightage05"]]$rep_ability_efficiency + 
             field_work_index * weightages[["weightage05"]]$field_work_index + 
             deployment_quality * weightages[["weightage05"]]$deployment_quality)
  
  # customer relationship
  dat06 <- dat05 %>% 
    # left_join(p_customer_relationship, by = c("hospital")) %>% 
    mutate(budget_prop = budget / manager_data$total_budget * 100,
           budget_factor = ifelse(level == 1, 
                                  sapply(budget_prop, function(x) {curve_func("curve02", curves, x)}), 
                                  ifelse(level == 2, 
                                         sapply(budget_prop, function(x) {curve_func("curve03", curves, x)}), 
                                         ifelse(level == 3, 
                                                sapply(budget_prop, function(x) {curve_func("curve04", curves, x)}), 
                                                0))),
           meeting_attendance_factor = ifelse(level == 1, 
                                              sapply(meeting_attendance, function(x) {curve_func("curve05", curves, x)}), 
                                              ifelse(level == 2, 
                                                     sapply(meeting_attendance, function(x) {curve_func("curve06", curves, x)}), 
                                                     ifelse(level == 3, 
                                                            sapply(meeting_attendance, function(x) {curve_func("curve07", curves, x)}), 
                                                            0))),
           customer_relationship = (budget_factor * weightages[["weightage06"]]$budget_factor + 
                                      meeting_attendance_factor * weightages[["weightage06"]]$meeting_attendance_factor) * 100)
  
  # market share, sales
  dat07 <- dat06 %>% 
    mutate(offer_attractiveness = sales_performance * weightages[["weightage07"]]$sales_performance + 
             customer_relationship * weightages[["weightage07"]]$customer_relationship,
           share_delta_factor = sapply(offer_attractiveness, function(x) {curve_func("curve28", curves, x)}),
           # share = ifelse(life_cycle == "成熟期",
           #                ifelse(share_delta_factor >= 0,
           #                       p_share + (0.4 - p_share) * share_delta_factor,
           #                       p_share * (1 + share_delta_factor)),
           #                ifelse(life_cycle == "导入期",
           #                       ifelse(share_delta_factor >= 0,
           #                              p_share + (0.15 - p_share) * share_delta_factor,
           #                              p_share * (1 + share_delta_factor)),
           #                       0)),
           share = p_share * (1 + share_delta_factor),
           sales = potential / 4 * share)
  
  return(dat07)
}

##---- Report ----
get_representative_info_tm <- function(result, p_action_kpi) {
  
  representative_ability <- result %>% 
    group_by(resource_id, representative_id, representative, product_knowledge, sales_skills, territory_management_ability, 
             work_motivation, behavior_efficiency) %>% 
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
    select(`resource_id`, `representative_id`, `representative`, `product_knowledge`, `sales_skills`, 
           `territory_management_ability`, `work_motivation`, `behavior_efficiency`)
  # colnames(rep_ability) <- c("representative-id", "product-knowledge", "sales-ability", "regional-management-ability", 
  #                            "job-enthusiasm", "behavior-validity")
  
  action_kpi <- p_action_kpi %>% 
    left_join(representative_ability, by = c("representative")) %>% 
    mutate(class1 = ifelse(behavior_efficiency >= 0 & behavior_efficiency < 3, 
                           1, 
                           ifelse(behavior_efficiency >= 3 & behavior_efficiency < 6, 
                                  2, 
                                  ifelse(behavior_efficiency >= 6 & behavior_efficiency < 8, 
                                         3, 
                                         ifelse(behavior_efficiency >= 8 & behavior_efficiency <= 10, 
                                                4, 
                                                0))))) %>% 
    mutate(`target-coverage` = ifelse(class1 == 1, 
                                      sapply(target_coverage, function(x) {x - sample(5:10, 1)}), 
                                      ifelse(class1 == 2, 
                                             sapply(target_coverage, function(x) {x - sample(0:5, 1)}), 
                                             ifelse(class1 == 3, 
                                                    sapply(target_coverage, function(x) {x + sample(0:5, 1)}), 
                                                    ifelse(class1 == 4, 
                                                           sapply(target_coverage, function(x) {x + sample(5:10, 1)}), 
                                                           0))))) %>% 
    mutate(class2 = ifelse(work_motivation >= 0 & work_motivation < 3, 
                           1, 
                           ifelse(work_motivation >= 3 & work_motivation< 6, 
                                  2, 
                                  ifelse(work_motivation >= 6 & work_motivation < 8, 
                                         3, 
                                         ifelse(work_motivation >= 8 & work_motivation < 10, 
                                                4, 
                                                0))))) %>% 
    mutate(high_target =  ifelse(class1 == 1, 
                                 sapply(high_target, function(x) {sample(13:14, 1)}), 
                                 ifelse(class1 == 2, 
                                        sapply(high_target, function(x) {sample(14:15, 1)}), 
                                        ifelse(class1 == 3, 
                                               sapply(high_target, function(x) {sample(16:18, 1)}), 
                                               ifelse(class1 == 4, 
                                                      sapply(high_target, function(x) {sample(19:22, 1)}), 
                                                      0)))),
           middle_target = ifelse(class1 == 1, 
                                  sapply(middle_target, function(x) {sample(13:14, 1)}), 
                                  ifelse(class1 == 2, 
                                         sapply(middle_target, function(x) {sample(13:14, 1)}), 
                                         ifelse(class1 == 3, 
                                                sapply(middle_target, function(x) {sample(12:13, 1)}), 
                                                ifelse(class1 == 4, 
                                                       sapply(middle_target, function(x) {sample(12:13, 1)}), 
                                                       0)))),
           low_target = ifelse(class1 == 1, 
                               sapply(low_target, function(x) {sample(13:14, 1)}), 
                               ifelse(class1 == 2, 
                                      sapply(low_target, function(x) {sample(12:13, 1)}), 
                                      ifelse(class1 == 3, 
                                             sapply(low_target, function(x) {sample(12:13, 1)}), 
                                             ifelse(class1 == 4, 
                                                    sapply(low_target, function(x) {sample(11:12, 1)}), 
                                                    0))))) %>% 
    mutate(high_target = ifelse(class2 == 1, 
                                sapply(high_target, function(x) {x - sample(1:2, 1)}), 
                                ifelse(class2 == 2, 
                                       sapply(high_target, function(x) {x - sample(0:1, 1)}), 
                                       ifelse(class2 == 3, 
                                              sapply(high_target, function(x) {x + sample(0:1, 1)}),
                                              ifelse(class2 == 4, 
                                                     high_target + 1,
                                                     0)))),
           middle_target = ifelse(class2 == 1, 
                                  middle_target - 2, 
                                  ifelse(class2 == 2, 
                                         middle_target - 1, 
                                         ifelse(class2 == 3, 
                                                sapply(middle_target, function(x) {x + sample(0:1, 1)}), 
                                                ifelse(class2 == 4, 
                                                       middle_target + 1, 
                                                       0)))),
           low_target = ifelse(class2 == 1, 
                               low_target - 2, 
                               ifelse(class2 == 2, 
                                      low_target - 1, 
                                      ifelse(class2 == 3, 
                                             sapply(low_target, function(x) {x + sample(0:1, 1)}), 
                                             ifelse(class2 == 4, 
                                                    low_target + 1, 
                                                    0))))) %>% 
    select(representative_id, target, target_coverage, high_target, middle_target, low_target)
  
  representative_info <- list("representative_ability" = representative_ability,
                              "action_kpi" = action_kpi)
  
  return(representative_info)
}

get_sales_report_tm <- function(result, competition_data) {
  
  hospital_sales_report <- result %>% 
    mutate(quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)),
           growth = round(sales / p_sales - 1, 2)) %>% 
    select(dest_id, goods_id, resource_id, potential, sales, quota, share, quota_rate, growth)
  # colnames(hosp_report) <- c("dest-config-id", "resource-config-id", "goods-config-id", "potential", "sales", "sales-quota",
  #                            "share", "quota-achievement", "sales-growth")
  
  representative_sales_report <- result %>% 
    select(resource_id, goods_id, potential, p_sales, sales, quota) %>% 
    group_by(resource_id, goods_id) %>% 
    summarise(potential = sum(potential),
              p_sales = sum(p_sales),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    ungroup() %>% 
    mutate(share = round(sales / potential * 4, 2),
           quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)),
           growth = round(sales / p_sales - 1, 2)) %>% 
    select(resource_id, goods_id, potential, sales, quota, share, quota_rate, growth)
  # colnames(rep_report) <- c("resource-config-id", "goods-config-id", "potential", "sales", "sales-quota", "share",
  #                           "quota-achievement", "sales-growth")
  
  prod1_sales_report <- result %>% 
    select(goods_id, potential, p_sales, sales, quota) %>% 
    group_by(goods_id) %>% 
    summarise(potential = sum(potential),
              p_sales = sum(p_sales),
              sales = sum(sales),
              quota = sum(quota)) %>% 
    ungroup() %>% 
    mutate(share = round(sales / potential * 4, 2),
           quota_rate = ifelse(quota == 0, 
                               0, 
                               round(sales / quota, 2)),
           growth = round(sales / p_sales - 1, 2)) %>% 
    select(goods_id, sales, quota, share, quota_rate, growth)
  
  total_potential <- sum(result$potential)
  
  prod2_sales_report <- competition_data %>% 
    mutate(p_sales = total_potential / 4 * p_share,
           share = sapply(p_share, function(x) {x * sample(seq(0.9, 1.1, 0.01), 1)}),
           sales = total_potential / 4 * share,
           growth = sales / p_sales - 1,
           quota = 0,
           quota_rate = 0) %>% 
    select(goods_id, sales, quota, share, quota_rate, growth)
  
  product_sales_report <- bind_rows(prod1_sales_report, prod2_sales_report)
  # colnames(product_sales_report) <- c("product-id", "sales-quota", "share", "sales", "quota-achievement", "sales-growth",
  #                                     "quota-contribute", "quota-growth", "ytd-sales", "sales-contribute", "sales-year-on-year",
  #                                     "sales-month-on-month", "patient-count")
  
  sales_report <- list("hospital_sales_report" = hospital_sales_report,
                       "representative_sales_report" = representative_sales_report,
                       "product_sales_report" = product_sales_report)
  
  return(sales_report)
}

##---- Assessment ----
get_assessment_tm <- function(result, manager_data, standard_time, level_data) {
  
  ## 区域划分
  index1 <- result %>% 
    group_by(representative, general_ability) %>% 
    summarise(potential = sum(potential),
              p_sales = sum(p_sales)) %>% 
    ungroup() %>% 
    mutate(potential_prop = potential / sum(potential),
           p_sales_prop = p_sales / sum(p_sales),
           prop1 = 0.6 * potential_prop + 0.4 * p_sales_prop,
           prop2 = (general_ability - 50) / sum(general_ability - 50),
           score = abs(prop1 - prop2))
  
  score1 <- mean(index1$score)
  
  ## 指标分配
  index2 <- result %>% 
    select(potential, p_sales, quota, sales) %>% 
    mutate(potential_prop = potential / sum(potential),
           p_sales_prop = p_sales / sum(p_sales),
           prop1 = 0.6 * potential_prop + 0.4 * p_sales_prop,
           prop2 = quota / manager_data$total_quota,
           score1 = abs(prop1 - prop2)) %>% 
    mutate(quota_growth = quota - p_sales,
           sales_growth = sales - p_sales,
           prop3 = quota_growth / sum(quota_growth),
           prop4 = sales_growth / sum(sales_growth),
           score2 = abs(prop3 - prop4))
  
  score2 <- 0.7 * mean(index2$score1) + 0.3 * mean(index2$score2)
  
  ## 资源分配
  index3 <- result %>% 
    select(potential, p_sales, budget, call_time, representative_time, meeting_attendance) %>% 
    mutate(potential_prop = potential / sum(potential),
           p_sales_prop = p_sales / sum(p_sales),
           prop1 = 0.6 * potential_prop + 0.4 * p_sales_prop,
           prop2 = budget / manager_data$total_budget,
           prop3 = call_time / (representative_time * 5),
           prop4 = meeting_attendance / manager_data$total_place,
           score1 = abs(prop1 - prop2),
           score2 = abs(prop1 - prop3),
           score3 = abs(prop1 - prop4))
  
  score3 <- 0.45 * mean(index3$score1) + 0.25 * mean(index3$score2) + 0.3 * mean(index3$score3)
  
  ## 时间管理
  index4 <- result %>% 
    select(representative, field_work, one_on_one_coaching, employee_kpi_and_compliance_check, 
           admin_work, kol_management, business_strategy_planning, team_meeting) %>% 
    distinct() %>% 
    group_by(employee_kpi_and_compliance_check, admin_work, kol_management, 
             business_strategy_planning, team_meeting) %>% 
    summarise(field_work = sum(field_work),
              one_on_one_coaching = sum(one_on_one_coaching)) %>% 
    ungroup() %>% 
    melt(variable.name = "index", variable.factor = FALSE, value.factor = FALSE) %>% 
    left_join(standard_time, by = c("index")) %>% 
    mutate(prop1 = standard / manager_data$manager_time,
           prop2 = value / manager_data$manager_time,
           score = abs(prop1 - prop2))
  
  score4 <- mean(index4$score)
  
  ## 团队管理
  index5 <- result %>% 
    select(representative, general_ability, p_product_knowledge, p_sales_skills, 
           p_territory_management_ability, p_work_motivation, p_behavior_efficiency) %>% 
    distinct() %>% 
    mutate(p_general_ability = (0.2 * p_territory_management_ability + 0.25 * p_sales_skills + 
                                  0.25 * p_product_knowledge + 0.15 * p_behavior_efficiency + 0.15 * p_work_motivation) * 10,
           delta1 = 100 - p_general_ability,
           delta2 = general_ability - p_general_ability)
  
  score5 <- 0.2 - mean(index5$delta2) / mean(index5$delta1)
  
  ## overall
  assessment <- data.frame(
    "index" = c("region_division", "target_assigns", "resource_assigns", "manage_time", "manage_team"),
    "score" = c(score1, score2, score3, score4, score5),
    stringsAsFactors = FALSE
  ) %>% 
    left_join(level_data, by = c("index")) %>% 
    mutate(level = ifelse(score < level1,
                          1,
                          ifelse(score > level2,
                                 3,
                                 2))) %>% 
    select(index, code, level)
  
  general_performance <- data.frame("index" = "general_performance",
                                    "code" = 5,
                                    "level" = round(mean(assessment$level)),
                                    stringsAsFactors = FALSE)
  
  assessment <- bind_rows(assessment, general_performance)
  
  return(assessment)
}
