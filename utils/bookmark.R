library(shiny)
library(dplyr)
library(purrr)
library(stringr)
library(htmltools)

APP_PARAMS <- "selected_language"
SHINYDASHBOARD_PARAMS <- "tabs"

# Buttons
createTwitterButton <- function(share_text, share_url){
  tags$button(
      type = "button",
      class = "btn btn-default btn-twitter", 
      onClick = stringr::str_interp("
        (function(){
          var share_url = encodeURIComponent('${share_url}');
          var url = 'http://twitter.com/intent/tweet?text=${share_text}&url=' + share_url + '&via=ksmzn&hashtags=rshiny'
          window.open(url,
          'tweetwindow',
          'width=550, height=450, personalbar=0, toolbar=0, scrollbars=1, resizable=1'
          );
          return false;
        })();
      "),
      icon("twitter"), "Share"
    )
}

# Arrange Bookmark URL
filterQueryParams <- function(url, input){
  # Distribution which is currently selected 
  dist_name <- input$tabs
  # Required Parameters
  bookmark_params <- c(APP_PARAMS, SHINYDASHBOARD_PARAMS, str_c(dist_name, "-"))

  base_url <- stringr::str_split_fixed(url, "&", n = 2)[[1]]
  # Create URL query from input list
  url_filtered <- purrr::imap(input, ~ {
      val <- .x
      if(class(val)=="character")
        val <- stringr::str_c("\"", val, "\"")
      if(class(val)=="logical")
        val <- stringr::str_to_lower(val)
      if(length(val)==2) # Range
        val <- stringr::str_c("[", stringr::str_c(val, collapse = ","), "]")
      val <- ifelse(
        is.null(val),
        "null", # Convert NULL to character
        URLencode(as.character(val), reserved = TRUE)
      )
      nm <- URLencode(as.character(.y))
      param_str <- stringr::str_c(nm, val, sep = "=")
      return(param_str)
    }) %>% 
    # Filter Parameters
    purrr::keep( ~ {
      stringr::str_detect(
        .x,
        stringr::str_c("^", bookmark_params, collapse = "|")
      )
    }) %>% 
    stringr::str_c(collapse = "&") %>% 
    stringr::str_c(base_url, ., sep = "&")
  return(url_filtered)
}

# Custom Modal which is supported i18n
urlBookmarkModal <- function(url, title, subtitle = NULL, dismiss_label = "Dismiss", copy_text = "Press Ctrl-C to copy.", copy_text_mac = "Press ⌘-C to copy.", share_text = "Probability Distributions Viewer") {
  subtitleTag <- tagList(
    br(),
    span(class = "text-muted",
         subtitle),
    span(id = "shiny-bookmark-copy-text", class = "text-muted")
  )

  # Buttons
  dismissButton <- modalButton(dismiss_label)

  share_url <- str_c("http://twitter.com/intent/tweet?text=", share_text, "&url=", url, "&via=ksmzn&hashtags=rshiny") 
  twitterButton <- createTwitterButton(share_text = share_text, share_url = url)
  modalDialog(
    title = title,
    footer = list(twitterButton, dismissButton),
    easyClose = TRUE,
    tags$textarea(
      class = "form-control",
      rows = "1",
      style = "resize: none;",
      readonly = "readonly",
      url
    ),
    subtitleTag,
    tags$script(stringr::str_interp(
      "$('#shiny-modal').
        one('show.bs.modal', function() {
          setTimeout(function() {
          var $textarea = $('#shiny-modal textarea');
          $textarea.innerHeight($textarea[0].scrollHeight);
          }, 200); 
        });
      $('#shiny-modal').
        one('shown.bs.modal', function() {
          $('#shiny-modal textarea').select().focus();
        });
      $('#shiny-bookmark-copy-text')
        .text(function() {
          if (/Mac/i.test(navigator.userAgent)) {
            return '${copy_text_mac}';
          } else {
            return '${copy_text}';
          }
        });
      "
    ))
  )
}

# Show custom modal
showBookmarkModal <- function(input, i18n) {
  f <- function (url){
    url <- filterQueryParams(url, reactiveValuesToList(input))

    # Translate characters
    title <- i18n()$t("Bookmarked application link")
    subtitle <- i18n()$t("This link stores the current state of this application.")
    dismiss_label <- i18n()$t("Dismiss")
    copy_text <- i18n()$t("Press Ctrl-C to copy.")
    copy_text_mac <- i18n()$t("Press ⌘-C to copy.")

    # Share text
    tabs <- input$tabs
    page_title <- switch (tabs,
      about = "About",
      i18n()$t(distributions[[tabs]]$name)
    )
    app_title <- i18n()$t("Probability Distributions Viewer")
    share_text <- stringr::str_c(page_title, app_title, sep = " - ")

    modal <- urlBookmarkModal(
      url,
      title = title,
      subtitle = subtitle,
      dismiss_label = dismiss_label,
      copy_text = copy_text,
      copy_text_mac = copy_text_mac,
      share_text = share_text
    )
    showModal(modal)
  }
  return(f)
}
