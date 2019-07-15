# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  TM_new
# Purpose:      Main of TM_new
# programmer:   Zhe Liu
# Date:         17-06-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


library(plyr)
library(dplyr)
library(tidyr)
library(DT)
library(data.table)
library(jsonlite)
library(curl)
library(uuid)

source("./Kafka.R", encoding = "UTF-8")
source("./Functions.R", encoding = "UTF-8")
source("./Calculation.R", encoding = "UTF-8")
load("./intermedia.RData")
envir <- read_json("./envir.json")

# source("/Users/qianpeng/GitHub/pharbers-ntm-r-calculate/Kafka.R", encoding = "UTF-8")
# source("/Users/qianpeng/GitHub/pharbers-ntm-r-calculate/Functions.R", encoding = "UTF-8")
# source("/Users/qianpeng/GitHub/pharbers-ntm-r-calculate/Calculation.R", encoding = "UTF-8")
# load("/Users/qianpeng/GitHub/pharbers-ntm-r-calculate/intermedia.RData")
# envir <- read_json("/Users/qianpeng/GitHub/pharbers-ntm-r-calculate/envir.json")

options(scipen = 200,
        uri = envir$uri,
        groupName = envir$groupName,
        receiveTopics = envir$receiveTopics,
        sendTopics = envir$sendTopics,
        stringsAsFactors = FALSE)

main <- function() {
  start(callRConsumer, "", "")
}

main()