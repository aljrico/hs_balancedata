setClass(
  Class = 'appError',
  slots = list(
    error_msg = 'character'
  )
)


setGeneric(
  'errorMessage', function(x){
    standardGeneric('errorMessage')
  }
)

setMethod(
  f = 'errorMessage',
  signature = 'appError',
  definition = function(x){
    cat(x@error_msg)
    
    sendSweetAlert(
      session = session,
      title = "Error!",
      text = x@error_msg,
      type = "error"
    )
  }
)
