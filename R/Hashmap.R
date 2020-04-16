#' Create empty hashmap
#'
#' @export
new.hashmap <- function() {
  structure(new.env(hash = TRUE), class = "hashmap")
}

#' Create hamap with values
#'
#' @param values a list of values
#' @export
hashmap <- function(values) {
  h <- new.hashmap()
  add.hashmap(h, values)
  h
}


#' Create generic
#'
#' @export
add <- function(where, ...){
  UseMethod("add")
}

#' Test if hashmap
#'
#' @param x object to test
#' @export
is.hashmap <- function(x) inherits(x, "hashmap")

#' Add values to hashmap
#'
#' @param where hashmap where you want to add keys and values
#' @param stuff Added stuff
#'
#' @export
add.hashmap <- function(where, stuff) {
  if(!is.hashmap(where))
    stop("parameter where must inherit hashmap")

  list2env(stuff, envir = where)
}

rm <- function(x) {
  UseMethod("rm")
}

#' Remove elements from hashmap
#'
#' @param where hashmap from where you want to remove keys and values
#' @param ... deleted stuff
rm.hashmap <- function(where, ...){
  if(!is.list(...)) {
    args <- list(...)
  } else {
    args <- ...
  }
  if(!is.hashmap(where))
    stop("parameter where must inherit hashmap")

  rm(list = args, envir = where)
}

#' Return keys od a hashmap
#'
#' @param hashmap A hashmap
#' @export
keys <- function(hashmap) {
  if(!is.hashmap(hashmap))
    stop("parameter hashmap must inherit hashmap")

  ls(hashmap)
}

#' Test if hashmap has a key
#'
#' @param key what key, or keys, can be a list or vector
#' @param where a hashmap
#' @export
has.key <- function(where, key) {
  ifelse(key %in% keys(where), TRUE, FALSE)
}

#' Get all values that a hashmap has
#'
#' @param where hashmap
#' @export
values <- function(where) {
  get(ls(where), envir = where)
}

#' Generic
#'
#' @export
print <- function(x) {
  UseMethod("print")
}

#' Prints a hashmap
#'
#' @param x object
#' @export
print.hashmap <- function(x) {
  str(h)
}



