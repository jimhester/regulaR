#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @export
regulaR = function(...) {
  structure('', class='regex')
}

#' @export
start_with = function(obj, ...){
  if(nchar(obj) %!=% 0L){ stop('start_with called multiple times') }
  write(obj, '^%s', list(...))
}

#' @export
write = function(obj, str, args=NULL){
  if(attr(obj, 'ended') %!=% NULL){
    stop("end_with has already been called")
  }
  if(is.null(args)){
    paste0(obj, str)
  }
  else {
    paste0(obj, sprintf(str, interpret(args)))
  }
}

#' @export
append = function(obj, ...){
  write(obj, interpret(list(...)))
}
then = append

#' @export
end_with = function(obj, ...){
  obj = write(obj, '%s$', ...)
  attr(obj, 'ended') = TRUE
  obj
}

#' @export
maybe = function(obj, ...){
  write(obj, '%s?', ...)
}

#' @export
not = function(obj, ...){
  write(obj, '(?!%s)', ...)
}

#' @export
one_of = function(obj, ...){
  write(obj, no_escape(sprintf('(?:%s)', paste0(escape(...), collapse='|'))))
}

#' @export
at_least = function(obj, times, pattern){
  between(obj, c(times, NA), pattern)
}

#' @export
at_most = function(obj, times, pattern){
  between(obj, c(NA, times), pattern)
}

#' @export
zero_or_more = function(obj, pattern){
  write(obj, "%s*", pattern)
}

#' @export
one_or_more = function(obj, pattern){
  write(obj, "%s+", pattern)
}

#' @export
between = function(obj, range, pattern){
  if(length(range) %!=% 2L || !any(is.integer(range))){
    stop('must provide an array of 2 elements, one of them must be an integer')
  }

  range[is.na(range)] = ''

  write(obj, sprintf('%s{%s,%s}', interpret(pattern), range[1], range[2]))
}
#' @export
interpret = function(args){
  if(length(args) %==% 2L){
    numbered_constraint(args[[1]], args[[2]])
  }
  else if(length(args) %==% 1L){
    patterned_constraint(args)
  }
  else {
    stop("Incorrect number of arguments")
  }
}


#' @export
numbered_constraint = function(count, type){
    pattern = patterned_constraint(type)
    if(is.null(pattern) || length(pattern) %==% 0){
      stop('Unrecognized pattern')
    }
    sprintf('%s{%s}', pattern, count)
}
#' @export
patterned_constraint = function(pattern){
  escape(pattern)
}

ESCAPED_CHARS = c('*', '.', '?', '^', '+', '$', '|', '(', ')', '[', ']', '{', '}')

#' @export
escape = function(pattern){
  if(attr(pattern, 'no_escape') %==% TRUE){
    pattern
  }
  else {
    gsub(paste0('([\\', paste0(collapse='\\', ESCAPED_CHARS),'])'), '\\\\\\1', pattern, perl=TRUE)
  }
}

#' @export
no_escape = function(x) {
  attr(x, 'no_escape') = TRUE
  x
}
digit        = no_escape('[0-9]')
digits = digit
lowercase    = no_escape('[a-z]')
lowercases = lowercase
uppercase    = no_escape('[A-Z]')
uppercases = uppercase
letter       = no_escape('[A-Za-z]')
letters = letter
alphanumeric = no_escape('[A-Za-z0-9]')
alphanumerics = alphanumeric
whitespace   = no_escape('\\s')
whitespaces = whitespace
space        = no_escape(' ')
spaces = space
tab          = no_escape('\t')
tabs = tab

#' @export
"%==%" = function(x, y) { identical(x, y) }
#' @export
"%!=%" = function(x, y) { !identical(x, y) }
