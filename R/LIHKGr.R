#########
### R6 version
library(R6)
library(purrr)
library(tibble)
library(dplyr)
library(RSelenium)
library(raster)
library(magrittr)
library(rvest)

.lay_low <- function(){
  Sys.sleep(sample(seq(1, 2, by = 0.001), 1))
}

.gen_remote_driver <- function(driver, ...) {
  if (is.null(driver)) {
    driver <- rsDriver(...)
    remote_driver <- driver[["client"]]
  } else {
    remote_driver <- driver
    driver <- NA
  }
  if (!is(remote_driver, "remoteDriver")) {
    stop("Invalid Selenium remote driver.")
  }
  remote_driver$open()
  remote_driver$navigate("https://lihkg.com")
  list(driver, remote_driver)
}

.crack_it <- function(url, remote_driver){
  remote_driver$navigate(url)
  Sys.sleep(sample(seq(3, 5, by = 0.001), 1))
  # collapsed comments: emoji-only & massive downvoted comments
  collapsed <- remote_driver$findElements("xpath", "//div[@class='_1d3Z5jQRq3WnuIm0hnMh0c']|
                                          //div[@class='_2cNsJna0_hV8tdMj3X6_gJ']/div/span[@class='_3vdrhTaWJyZ7pYkFWJM1_n']|
                                          //blockquote/div/div/span[@class='_3vdrhTaWJyZ7pYkFWJM1_n']")
  if (length(collapsed)) { # click collapsed comments if any
    for (x in collapsed) {
      x$clickElement()
      #Sys.sleep(sample(seq(0, 1, by = 0.001), 1))
    }
  }
  html <- remote_driver$getPageSource()
  if(grepl("recaptcha_widget", html[[1]])) {
    readline(prompt = "Captcha Detected. Press [enter] to continue after solving")
    return(.crack_it(url, remote_driver)) # make sure collapsed comments are expanded
  }
  pg <-  read_html(html[[1]])
  if (length(collapsed)) { # remove any additional page accidentally loaded when clicking collapsed comments
    xml_remove(xml_find_all(pg, "//div[@class='_3jxQCFWg9LDtkSkIVLzQ8L']")[-1])
  }
  pg
}

.scrape_page <- function(html, postid){
  ##get_number
  number <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div small ._3SqN3KZ8m8vCsD9FNcxcki") %>%
    html_text()
  ##get_date
  date <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div small .Ahi80YgykKo22njTSCzs_") %>%
    html_attr("data-tip")
  ##get_uid
  uid <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div small .ZZtOrmcIRcvdpnW09DzFk a") %>%
    html_attr('href')
  ##get_probation
  probation <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div small ._10ZVxePYNpBeLjzkQ88wtj") %>%
    html_text() %>%
    is.na() %>%
    not()
  ##get_text
  text <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div .GAagiRXJU88Nul1M7Ai0H ._2cNsJna0_hV8tdMj3X6_gJ") %>%
    html_text()
  ##get_member_only?
  private <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div div ._2cNsJna0_hV8tdMj3X6_gJ") %>%
    html_node("._2yeBKooY3VAK8NLhM4Esov") %>%
    html_text() %>%
    is.na() %>%
    not()
  ##get_quote
  quote <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div div > ._31B9lsqlMMdzv-FSYUkXeV > *:last-child") %>%
    html_text()
  ##get_upvote
  upvote <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("._1jvTHwVJobs9nsM0JDYqKB+ ._1drI9FJC8tyquOpz5QRaqf") %>% 
    html_text()
  ##get_downvote
  downvote <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("._2_VFV1QOZok8YhOTGa_3h9+ ._1drI9FJC8tyquOpz5QRaqf") %>% 
    html_text()
  ##get_collection_time
  collection_time <- Sys.time()
  ##get_title
  top.text <- html %>% html_nodes("._2k_IfadJWjcLJlSKkz_R2- span") %>%
    html_text()
  title <- top.text[2]
  board <- top.text[1]
  newdf <- tibble::as_tibble(cbind(number, date, uid, probation, text, private, quote, upvote, downvote))
  newdf$postid <- postid # This bit might fail if date etc is NULL
  newdf$title <- title
  newdf$board <- board
  newdf$collection_time <- collection_time
  newdf
}

.scrape_post <- function(postid, remote_driver, board) {
  posts <- tibble::tibble()
  message("Start crawling post ", postid)
  print("Start crawling page 1")
  # Page 1
  attempt <- 1
  notdone <- TRUE
  while (notdone && attempt <= 4) {
    print(paste0("Attempt: ", attempt))
    attempt <- attempt + 1
    try({
      html <- .crack_it(paste0("https://lihkg.com/thread/", postid, "/page/1"), remote_driver)
      titlewords <- html %>% html_nodes("._2k_IfadJWjcLJlSKkz_R2- span") %>% 
        html_text()
      if (titlewords[1] == "404") { # Deleted post
        title <- tibble::tibble(postid = postid, date = NA, uid = NA, probation = NA,
                                title = "Deleted Post", board = NA, upvote = NA,
                                downvote = NA, collection_time = Sys.time())
        print("Empty Post, Skipping")
        return(list(title = title))
      } else if (is.null(board) | titlewords[1] %in% board) { # Normal post
        posts <- .scrape_page(html, postid)
        if (any(is.na(posts$postid))) # prevent NA postid
          stop("postid is NA")
        title <- posts[1, c("postid", "date", "uid", "probation", "title", "board",
                            "upvote", "downvote", "collection_time")]
        notdone <- FALSE
      } else { # Unwanted post
        title <- .scrape_page(html, postid)
        if (any(is.na(title$postid))) # prevent NA postid
          stop("postid is NA")
        title <- title[1, c("postid", "date", "uid", "probation", "title", "board",
                            "upvote", "downvote", "collection_time")]
        print("Non-matching board, Fetched title only")
        return(list(title = title))
      }
    })
    if (notdone && attempt > 4) {
      stop("Error, Stopping")
    }
  }
  last_page <- html %>% html_node("._1H7LRkyaZfWThykmNIYwpH option:last-child") %>%
    html_attr("value") %>%
    as.numeric()
  while (!length(last_page)) { # To make sure
    last_page <- html %>% html_node("._1H7LRkyaZfWThykmNIYwpH option:last-child") %>%
      html_attr("value") %>%
      as.numeric()
  }
  # Page 2+
  if (last_page > 1) {
    print("Finished crawling page 1 (to be continued)")
    for (i in 2:last_page) {
      print(paste0("Start crawling page ", i, " of ", last_page))
      attempt <- 1
      notdone <- TRUE
      while (notdone && attempt <= 4) { # Auto restart when fails
        print(paste0("Attempt: ", attempt))
        attempt <- attempt + 1
        try({
          html <- .crack_it(paste0("https://lihkg.com/thread/", postid, "/page/", i), remote_driver)
          titlewords <- html %>% html_nodes("._2k_IfadJWjcLJlSKkz_R2- span") %>% 
            html_text()
          post <- .scrape_page(html, postid)
          posts <- dplyr::bind_rows(posts, post)
          if (i == last_page)
            print(paste0("Finished crawling page ", i, " (last page)"))
          else
            print(paste0("Finished crawling page ", i, " (to be continued)"))
          notdone <- FALSE
          .lay_low()
        })
      } # End of While Loop
      if (notdone && attempt > 4) {
        if (length(titlewords) == 2 && nrow(posts) > 1) {
          warning <- tibble::tibble(number = "ERROR", date = NA, uid = NA, probation = NA,
                                    text = "Deleted Last Page", private = NA, quote = NA, upvote = NA,
                                    downvote = NA, postid = postid, title = "Deleted Last Page",
                                    board = NA, collection_time = Sys.time())
          posts <- dplyr::bind_rows(posts, warning)
          print("Empty Last Page Detected")
          notdone <- FALSE
        } else {
          stop("Error, Stopping")
        }
      }
    }
  } else {
    print("Finished crawling page 1 (last page)")
  }
  list(title = title, posts = posts)
}

Lihkg_reader <- R6::R6Class(
  "lihkg_reader",
  public = list(
    initialize = function(driver = NULL, ...) {
      res <- .gen_remote_driver(driver, ...)
      private$remote_driver <- res[[2]]
      private$driver <- res[[1]]
    },
    scrape = function(postid, board = NULL) {
      scraped <- .scrape_post(postid, private$remote_driver, board)
      if (!is.null(scraped[["posts"]])) {
        self$posts <- rbind(self$posts, scraped[["posts"]])
      }
      self$titles <- rbind(self$titles, scraped[["title"]])
    },
    save = function(file_name, folder = NULL, ...) {
      folder <- ifelse(is.null(folder), "", paste0(folder, "/"))
      saveRDS(self$titles, file = paste0(folder, "title_", file_name), ...)
      saveRDS(self$posts, file = paste0(folder, "post_", file_name), ...)
    },
    clear = function() {
      self$titles <- tibble::tibble()
      self$posts <- tibble::tibble()
    },
    finalize = function() {
      private$remote_driver$close()
      if (!is.na(private$driver))
        private$driver[["server"]]$stop()            
    },
    titles = tibble::tibble(),
    posts = tibble::tibble()
  ),
  private = list(
    remote_driver = NULL,
    driver = NULL
  )
)
