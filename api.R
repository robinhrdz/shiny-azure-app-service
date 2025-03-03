library(plumber)  # Load the plumber package

#* @post /api-endpoint
function(req) {
  data <- req$body
  # Process your data here
  return(list(
    status = "success",
    message = "Data received successfully",
    timestamp = as.character(Sys.time())
  ))
}
