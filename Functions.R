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
  
  data_list <- fromJSON(receive, simplifyDataFrame = TRUE)[[1]][["value"]]
  
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
           "call_time_factor" = "visit-time")
  
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
        "level" = assessment$code[1],
        "code" = assessment$level_result[1]
      ),
      "targetAssignsResult" = list(
        "level" = assessment$code[2],
        "code" = assessment$level_result[2]
      ),
      "resourceAssignsResult" = list(
        "level" = assessment$code[3],
        "code" = assessment$level_result[3]
      ),
      "manageTimeResult" = list(
        "level" = assessment$code[4],
        "code" = assessment$level_result[4]
      ),
      "manageTeamResult" = list(
        "level" = assessment$code[5],
        "code" = assessment$level_result[5]
      ),
      "generalPerformanceResult" = list(
        "level" = assessment$code[6],
        "code" = assessment$level_result[6]
      )
    )
  )
  
  return(send_list)
}

##---- Calculation ----
get_result_tm <- function(cal_data, manager_data, p_customer_relationship, curves, weightages) {
  
  # dat <- input_data$business_input %>% 
  #   left_join(input_data$rep_input, by = c("resource_id", "rep_id")) %>% 
  #   bind_cols(input_data$manager_input[rep(1, each = 10), ]) %>% 
  #   left_join(p_data$p_hospital_sales_info, by = c("hosp_id", "prod_id")) %>% 
  #   left_join(p_data$p_rep_ability_info, by = c("rep_id")) %>% 
  #   select(`dest_id`, `hosp_id`, `hosp_size`, `p_sales`, `p_market_share`, `p_offer_attractiveness`, `p_customer_relationship`, 
  #          `p_potential`, `resource_id`, `rep_id`, `p_territory_management_ability`, `p_sales_skills`, `p_product_knowledge`, 
  #          `p_behavior_efficiency`, `p_work_motivation`, `goods_id`, `prod_id`, `life_cycle`, `quota`, `budget`, `meeting_attendance`, 
  #          `call_time_factor`, `total_budget`, `field_work`, `one_on_one_coaching`, `team_meeting`, `business_strategy_planning`, 
  #          `admin_work`, `employee_kpi_and_compliance_check`, `kol_management`, `territory_management_training`, `sales_skills_training`, 
  #          `product_knowledge_training`, `performance_review`, `career_development_guide`) %>% 
  #   mutate(budget = budget/total_budget)
  
  # general ability
  dat01 <- cal_data %>% 
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
    left_join(p_customer_relationship, by = c("hospital")) %>% 
    mutate(budget_prop = budget / manager_data$total_budget * 100,
           budget_factor = ifelse(hospital_level == "一级", 
                                  sapply(budget_prop, function(x) {curve_func("curve02", curves, x)}), 
                                  ifelse(hospital_level == "二级", 
                                         sapply(budget_prop, function(x) {curve_func("curve03", curves, x)}), 
                                         ifelse(hospital_level == "三级", 
                                                sapply(budget_prop, function(x) {curve_func("curve04", curves, x)}), 
                                                0))),
           meeting_attendance_factor = ifelse(hospital_level == "一级", 
                                              sapply(meeting_attendance, function(x) {curve_func("curve05", curves, x)}), 
                                              ifelse(hospital_level == "二级", 
                                                     sapply(meeting_attendance, function(x) {curve_func("curve06", curves, x)}), 
                                                     ifelse(hospital_level == "三级", 
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
    mutate(p_offer_attractiveness = sapply(p_share * 100, function(x) {curve_func("curve29", curves, x)}),
           offer_attractiveness = ifelse(life_cycle == "导入期", 
                                         current_oa * weightages[["weightage10"]]$current_oa + 
                                           p_offer_attractiveness * weightages[["weightage10"]]$p_offer_attractiveness, 
                                         ifelse(life_cycle == "成熟期", 
                                                current_oa * weightages[["weightage11"]]$current_oa + 
                                                  p_offer_attractiveness * weightages[["weightage11"]]$p_offer_attractiveness, 
                                                0)))
  
  # market share, sales
  dat09 <- dat08 %>% 
    mutate(share = sapply(offer_attractiveness, function(x) {curve_func("curve28", curves, x)}),
           share = round(share / 100, 2),
           sales = round(potential * share / 4, 2))
  
  return(dat09)
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
get_assessment_tm <- function(result, representative_ability, p_representative_ability, standard_time, level_data) {
  
  ## 区域划分
  index1 <- result %>% 
    group_by(representative_id) %>% 
    summarise(potential = sum(potential),
              p_sales = sum(p_sales)) %>% 
    ungroup()
  
  score1.1 <- sd(index1$potential) / mean(index1$potential)
  score1.2 <- sd(index1$p_sales) / mean(index1$p_sales)
  score1 <- 0.6 * score1.1 + 0.4 * score1.2
  
  ## 指标分配
  index2 <- result %>% 
    mutate(potential_contribution = potential / sum(potential),
           p_sales_contribution = p_sales / sum(p_sales),
           standard_contribution = 0.3 * potential_contribution + 0.7 * p_sales_contribution,
           quota_contribution = quota / sum(quota),
           quota_diff = abs(quota_contribution - standard_contribution) * 100) %>% 
    mutate(quota_sales_growth = quota / p_sales - 1,
           sales_growth = sales / p_sales - 1,
           quota_sales_growth_rank = dense_rank(-quota_sales_growth),
           sales_growth_rank = dense_rank(-sales_growth),
           growth_diff = abs(quota_sales_growth_rank - sales_growth_rank) / quota_sales_growth_rank)
  
  score2.1 <- sd(index2$quota_diff)
  score2.2 <- sum(index2$growth_diff)
  score2 <- 0.7 * score2.1 + 0.3 * score2.2
  
  ## 资源分配
  score3 <- sum(result$sales) / sum(result$quota)
  
  ## 时间管理
  index4.1 <- result %>% 
    mutate(field_work = sum(field_work),
           one_on_one_coaching = sum(one_on_one_coaching)) %>% 
    select(kol_management, employee_kpi_and_compliance_check, team_meeting, field_work, one_on_one_coaching, 
           business_strategy_planning, admin_work) %>% 
    distinct() %>% 
    gather("index", "value") %>% 
    left_join(standard_time, by = "index") %>% 
    mutate(time_diff1 = abs(value - standard) / standard * weightage)
  
  index4.2 <- result %>% 
    group_by(representative_id, field_work) %>% 
    summarise(quota = sum(quota)) %>% 
    left_join(representative_ability, by = "representative_id") %>% 
    mutate(field_work_prop = ifelse(sum(field_work) == 0,
                                    0,
                                    field_work / sum(field_work)),
           quota_prop = ifelse(sum(quota) == 0,
                               0,
                               quota / sum(quota)),
           field_work_rank = dense_rank(-field_work_prop),
           quota_rank = dense_rank(-quota_prop),
           time_diff2 = abs(field_work_rank - quota_rank) / quota_rank)
  
  score4 <- 0.4 * sum(index4.1$time_diff1) + 0.6 * sum(index4.2$time_diff2)
  
  ## 团队管理
  index5.1 <- representative_ability %>% 
    select(product_knowledge, sales_skills, territory_management_ability, work_motivation, behavior_efficiency) %>% 
    summarise_if(is.numeric, c("mean", "var")) %>% 
    gather("index", "value")
  
  index5.2 <- p_representative_ability %>% 
    select(p_product_knowledge, p_sales_skills, p_territory_management_ability, p_work_motivation, p_behavior_efficiency) %>% 
    summarise_if(is.numeric, c("mean", "var")) %>% 
    gather("index", "value")
  
  score5.1 <- index5.1$value[which(grepl(c("_mean"), index5.1$index))] / index5.2$value[which(grepl(c("_mean"), index5.2$index))] - 1
  score5.2 <- index5.1$value[which(grepl(c("_var"), index5.1$index))] / index5.2$value[which(grepl(c("_var"), index5.2$index))] - 1
  score5 <- 0.5 * sum(score5.1) + 0.5 * sum(score5.2)
  
  ## overall
  assessment <- data.frame(
    "index" = c("region_division", "target_assigns", "resource_assigns", "manage_time", "manage_team", "general_performance"),
    "score" = c(score1, score2, score3, score4, score5, mean(score1, score2, score3, score4, score5))
  ) %>% 
    left_join(level_data, by = c("index")) %>% 
    mutate(level_result = ifelse(score > level1,
                                 1,
                                 ifelse(score < level2,
                                        3,
                                        2))) %>% 
    select(index, score, code, level_result)
  
  return(assessment)
}













