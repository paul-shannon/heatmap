library(httpuv)
library(jsonlite)
library(RUnit)
#--------------------------------------------------------------------------------
.lastMessage <- NULL;
#--------------------------------------------------------------------------------
configureWebSocketServer <- function(wsCon)
{
   wsCon <- new.env(parent=emptyenv())
   wsCon$open <- FALSE
   wsCon$wsID <- NULL
   wsCon$ws <- NULL
   wsCon$result <- NULL

    wsCon$call = function(req) { # "call" processes http requests
    wsUrl = paste(sep='',
                   '"',
                  "ws://",
                  ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                  '"')
    list(
      status = 200L,
      headers = list('Content-Type' = 'text/html'),
      body = c(file="index.html"))
     }
   wsCon$onWSOpen = function(ws) {
      wsCon$ws <- ws
      ws$onMessage(function(binary, rawMessage) {
          #print(fromJSON(rawMessage))
          .lastMessage <<- fromJSON(rawMessage);
         }) # onMessage
       wsCon$open <- TRUE
       } # onWSOpen

   return(wsCon)

} # configureWebSocketServer
#--------------------------------------------------------------------------------
my.send <- function(wsCon, msg)
{
  #  browser()
    wsCon$ws$send(toJSON(msg, auto_unbox=TRUE))

} # send
#--------------------------------------------------------------------------------
init <- function()
{
   wsCon <- configureWebSocketServer()
   port <- 8694
   browseURL(sprintf("http://localhost:%d", port))
   wsCon$id <- startDaemonizedServer("0.0.0.0", port, wsCon)
   return(wsCon)

} # init
#--------------------------------------------------------------------------------
