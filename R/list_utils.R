#' @title capture_args
#' @description returns list of all arguments passed to a function, such that you could re-call the function using `do.call()`
#' @details If you pass in a `data.frame`, such as `data_in`, the whole thing is copied in the respective argument, which is returned. If you have an object in memory called `data_i`, defined as `data.frame(a=1:3,b=letters[1:3])` and you do `f(data_in = data_i)`, then the output is not `list()`
#' @return a list of arguments passed
#' @export
#' @examples
#' f <- function(x,y,...){
#'   capture_args()
#' }
#' 
#' g <- function(x=1,y){
#'   capture_args()
#' }
#' f(x=1,y=1:10)
#' f(x=1,y=1:10,z="constant",a="b")
#' g(y=1:10)

capture_args <- function() {
  # Get the parent environment (i.e., the environment of the function that called capture_args)
  parent_env <- parent.frame()
  
  # Retrieve the matched call for the parent function
  call <- match.call(definition = sys.function(sys.parent()), call = sys.call(sys.parent()), envir = parent_env)
  
  # Convert the call into a list, excluding the function name
  call_list <- as.list(call)[-1]
  
  # Get the formal arguments (including defaults) from the parent function
  formal_args <- formals(sys.function(sys.parent()))
  
  # Remove the '...' from the formal arguments if it exists
  if ("..." %in% names(formal_args)) {
    formal_args[["..."]] <- NULL
  }
  
  # Combine the default formal arguments with the explicitly provided ones
  combined_args <- modifyList(formal_args, call_list)
  
  # Evaluate the combined arguments in the parent environment
  evaluated_args <- lapply(combined_args, eval, envir = parent_env)
  
  # Check if '...' exists in the calling function, and capture if present
  if ("..." %in% names(formals(sys.function(sys.parent())))) {
    dots <- eval(quote(list(...)), envir = parent_env)
    # Combine the evaluated arguments and '...'
    all_args <- c(evaluated_args, dots)
  } else {
    # If no '...' is present, just return the evaluated arguments
    all_args <- evaluated_args
  }
  
  return(all_args)
}
