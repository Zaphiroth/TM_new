# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  UCB
# Purpose:      Kafka of UCB
# programmer:   Zhe Liu & Peng Qian
# Date:         06-06-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


run <- TRUE

##----
consumerInstance <- function() {
  
  uri <- options()$uri
  groupName = options()$groupName
  consumerName <- paste0(UUIDgenerate(TRUE),"_consumer") #"001" # paste0(UUIDgenerate(TRUE),"_consumer")
  
  handle <- new_handle()
  handle_setheaders(handle, "Content-Type" = "application/vnd.kafka.v2+json")
  
  data <- jsonlite::toJSON(list(name = consumerName, format = "json", "auto.offset.reset" = "earliest"), auto_unbox = TRUE)
  handle_setopt(handle, copypostfields = data);
  
  tryCatch({
    url <- paste0(uri, "/consumers/", groupName)
    
    out <- curl_fetch_memory(url, handle = handle)
    print("Consumer Instance")
    print(jsonlite::prettify(paste(out, collapse = "")))
  }, error = function(e){
    
    if (conditionMessage(e) == "HTTP error 409.") {
      
      return (list(consumerName = consumerName, groupName = groupName))
    }
  })
  
  return (list(consumerName = consumerName, groupName = groupName))
}

##----
subscription <- function(groupName, consumerName) {
  
  uri = options()$uri
  url <- paste0(uri, "/consumers/", groupName, "/instances/", consumerName, "/subscription")
  
  write.table(paste0(uri, "/consumers/", groupName, "/instances/", consumerName), file="./consumer.txt", 
              append = FALSE, row.names = FALSE, col.names = FALSE)
  
  handle <- new_handle()
  handle_setheaders(handle, "Content-Type" = "application/vnd.kafka.v2+json")
  
  data <- jsonlite::toJSON(list(topics = list(options()$receiveTopics)), auto_unbox = TRUE)
  handle_setopt(handle, copypostfields = data);
  
  curl_fetch_memory(url, handle = handle)
}

##----
callRConsumer <- function(consumerName, groupName) {
  tryCatch({
    uri <- options()$uri
    url <- paste0(uri, "/consumers/", groupName ,"/instances/", consumerName, "/records")
    
    handle <- new_handle()
    handle_setheaders(handle, "Accept" = "application/vnd.kafka.json.v2+json")
    
    response <- curl_fetch_memory(url, handle = handle)
    out <- rawToChar(response$content) 
    if (out != "[]") {
      if (grepl("\"error_code\"", out) == TRUE) {
        stop(out)
      } else {
        # print("进入")
        receive <- paste(out, collapse = "")
        # print(receive)
        calculation(receive)
      }
    }
  }, error = function(e) {
    print(conditionMessage(e))
    if (grepl("\"error_code\":404", conditionMessage(e)) == TRUE) {
      res <- consumerInstance() # 获取Consumer实例
      subscription(res$groupName, res$consumerName) # 订阅
      listening(callRConsumer, res$consumerName, res$groupName) # 重新监听
    }
  })
}

##----
sendResultMessage <- function(uri, topic, body) {
  handle <- new_handle()
  handle_setheaders(handle, 
                    "Content-Type" = "application/vnd.kafka.json.v2+json",
                    "Accept" = "application/vnd.kafka.v2+json"
  )
  handle_setopt(handle, copypostfields = body);
  
  url <- paste0(uri, "/" ,topic)
  curl_fetch_memory(url, handle = handle)
}

##----
listening <- function(fun, ...) {
  while(run == TRUE) {
    Sys.sleep(1)
    fun(...)
  }
}

start <- function(fun, ...) {
  readConsumerRecord()
  fun(...)
}

deleteConsumer <- function(url) {
  print(paste0("Delete Consumer => ", url))
  handle <- new_handle()
  handle_setopt(handle, customrequest = "DELETE");
  handle_setheaders(handle, "Content-Type" = "application/vnd.kafka.v2+json")
  curl_fetch_memory(url, handle = handle)
  print("Delete Ok")
}

readConsumerRecord <- function() {
  tryCatch({
    url <- as.character(read.table("./consumer.txt"))
    deleteConsumer(url)
  }, error = function(e) {
    print("Read File Error")
  })
}





