

#' Run code parsed from text.
#'
#' @param ... Character string(s) to run.
#' You can use \code{"{ }"} to insert any R object in the environment.
#' @param silent Suppress error/warning messages. Default is \code{FALSE}.
#'
#' @return Invisibly return the running expression(s).
#'
#' @examples
#' Run("a=1", "b=2")
#' Run("print({a+b})")
#'
#' @export
Run = function(..., silent=FALSE) {
  text = glue(..., .sep="\n", .envir=parent.frame())
  if(silent) {
    suppressWarnings({
      eval(parse(text=text), envir=parent.frame())
    })
  } else {
    eval(parse(text=text), envir=parent.frame())
  }
  invisible(text)
}



#' Split up a string (with separators) into a character vector.
#'
#' Split up a string (with separators) into a character vector
#' (whitespace around separator is trimmed).
#'
#' @param ... Character string(s).
#' @param sep Pattern for separation.
#' Default is \code{"auto"}:
#' \code{,} \code{;} \code{|} \code{\\n} \code{\\t}
#' @param trim Remove whitespace from start and end of string(s)?
#' Default is \code{TRUE}.
#'
#' @return Character vector.
#'
#' @export
#' @examples
#' \dontrun{
#' cc("a,b,c,d,e")
#'
#' cc(" a , b , c , d , e ")
#'
#' cc(" a , b , c , d , e ", trim=FALSE)
#'
#' cc("1, 2, 3, 4, 5")
#'
#' cc("A 1 , B 2 ; C 3 | D 4 \t E 5")
#'
#' cc("A, B, C",
#'    " D | E ",
#'    c("F", "G"))
#'
#' cc("
#' American
#' British
#' Chinese
#' ")
#'}

#' @importFrom stringr str_split
cc = function(..., sep="auto", trim=TRUE) {
  dots = list(...)
  x = paste(sapply(dots, function(i) paste(i, collapse=",")), collapse=",")
  x = ifelse(trim, str_trim(x), x)
  sep = ifelse(sep=="auto",
               ifelse(trim, "\\s*[,;\\|\\n\\t]\\s*", "[,;\\|\\n\\t]"),
               sep)
  as.character(stringr::str_split(x, sep, simplify=TRUE))
}


cc_ci = function(llci, ulci, nsmall) {
  paste0("[",
         formatF(llci, nsmall), ", ",
         formatF(ulci, nsmall), "]")
}


cc_m_ci = function(mean, llci, ulci, nsmall) {
  paste0(formatF(mean, nsmall), " [",
         formatF(llci, nsmall), ", ",
         formatF(ulci, nsmall), "]")
}


#' Repeat a character string for many times and paste them up.
#'
#' @param char Character string.
#' @param rep.times Times for repeat.
#'
#' @return Character string.
#'
#' @examples
#' rep_char("a", 5)
#'
#' @export
rep_char = function(char, rep.times) {
  paste(rep(char, times=rep.times), collapse="")
}


## Capitalize the first letter of a string.
capitalize = function(string) {
  capped = grep("^[A-Z]", string, invert=TRUE)
  substr(string[capped], 1, 1) = toupper(substr(string[capped], 1, 1))
  return(string)
}





#### Significance Test and Report ####


#' Compute \emph{p} value.
#'
#' @param z,t,f,r,chi2 \emph{z}, \emph{t}, \emph{F}, \emph{r}, \eqn{\chi}^2 value.
#' @param n,df,df1,df2 Sample size or degree of freedom.
#' @param digits,nsmall Number of decimal places of output. Default is \code{2}.
#'
#' @return \emph{p} value statistics.
#'
#' @examples
#' p.z(1.96)
#' p.t(2, 100)
#' p.f(4, 1, 100)
#' p.r(0.2, 100)
#' p.chi2(3.84, 1)
#'
#' p(z=1.96)
#' p(t=2, df=100)
#' p(f=4, df1=1, df2=100)
#' p(r=0.2, n=100)
#' p(chi2=3.84, df=1)
#'
#' @export
#' @importFrom stats pt pf pnorm pchisq
p = function(z=NULL, t=NULL, f=NULL, r=NULL, chi2=NULL,
             n=NULL, df=NULL, df1=NULL, df2=NULL, digits=2, nsmall=digits) {
  if(!is.null(z)) {p = p.z(z); pstat = Glue("<<italic z>> = {z:.{nsmall}}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(t)) {p = p.t(t, df); pstat = Glue("<<italic t>>({df}) = {t:.{nsmall}}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(f)) {p = p.f(f, df1, df2); pstat = Glue("<<italic F>>({df1}, {df2}) = {f:.{nsmall}}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(r)) {p = p.r(r, n); pstat = Glue("<<italic r>>({n-2}) = {r:.{nsmall}}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(chi2)) {p = p.chi2(chi2, df); pstat = Glue("\u03c7\u00b2({df}{ifelse(is.null(n), '', ', <<italic N>> = ' %^% n)}) = {chi2:.{nsmall}}, <<italic p>> {p.trans2(p)} {sig.trans(p)}")}
  return(pstat)
}

p.plain = function(z=NULL, t=NULL, f=NULL, r=NULL, chi2=NULL,
                   n=NULL, df=NULL, df1=NULL, df2=NULL, digits=2, nsmall=digits) {
  if(!is.null(z)) {p = p.z(z); pstat = Glue("z = {z:.{nsmall}}, p {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(t)) {p = p.t(t, df); pstat = Glue("t({df}) = {t:.{nsmall}}, p {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(f)) {p = p.f(f, df1, df2); pstat = Glue("F({df1}, {df2}) = {f:.{nsmall}}, p {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(r)) {p = p.r(r, n); pstat = Glue("r({n-2}) = {r:.{nsmall}}, p {p.trans2(p)} {sig.trans(p)}")}
  if(!is.null(chi2)) {p = p.chi2(chi2, df); pstat = Glue("\u03c7\u00b2({df}{ifelse(is.null(n), '', ', N = ' %^% n)}) = {chi2:.{nsmall}}, p {p.trans2(p)} {sig.trans(p)}")}
  return(pstat)
}

#' @describeIn p Two-tailed \emph{p} value of \emph{z}.
#' @export
#' @importFrom stats pnorm
p.z = function(z) stats::pnorm(abs(z), lower.tail=FALSE)*2

#' @describeIn p Two-tailed \emph{p} value of \emph{t}.
#' @export
#' @importFrom stats pt
p.t = function(t, df) stats::pt(abs(t), df, lower.tail=FALSE)*2

#' @describeIn p One-tailed \emph{p} value of \emph{F}. (Note: \emph{F} test is one-tailed only.)
#' @export

p.f = function(f, df1, df2) stats::pf(f, df1, df2, lower.tail=FALSE)

#' @describeIn p Two-tailed \emph{p} value of \emph{r}.
#' @export
p.r = function(r, n) p.t(r/sqrt((1-r^2)/(n-2)), n-2)

#' @describeIn p One-tailed \emph{p} value of \eqn{\chi}^2. (Note: \eqn{\chi}^2 test is one-tailed only.)
#' @export
p.chi2 = function(chi2, df) ifelse(df==0, 1, stats::pchisq(chi2, df, lower.tail=FALSE))


## Transform \emph{p} value.
##
## @param p \emph{p} value.
## @param nsmall.p Number of decimal places of \emph{p} value. Default is \code{3}.
##
## @return A character string of transformed \emph{p} value.
##
## @examples
## p.trans(c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0.0000001, 1e-50)) %>% data.table(p=.)
##
## @seealso \code{\link{p.trans2}}
##
## @export
p.trans = function(p, nsmall.p=3) {
  mapply(function(p, nsmall.p) {
    ifelse(is.na(p) | p > 1 | p < 0, "",
           ifelse(p < 10^-nsmall.p, gsub("0(?=\\.)", "", Glue("<{10^-nsmall.p:.{nsmall.p}}"), perl=T),
                  gsub("0(?=\\.)", " ", Glue("{p:.{nsmall.p}}"), perl=T)))
  }, p, nsmall.p)
}


## Transform \emph{p} value.
##
## @inheritParams p.trans
## @param p.min Minimum of \emph{p}. Default is \code{1e-99}.
##
## @return A character string of transformed \emph{p} value.
##
## @examples
## p.trans2(c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0.0000001, 1e-50)) %>% data.table(p=.)
##
## @seealso \code{\link{p.trans}}
##
## @export
p.trans2 = function(p, nsmall.p=3, p.min=1e-99) {
  ifelse(is.na(p) | p > 1 | p < 0, "",
         ifelse(p < p.min, paste("<", p.min),
                ifelse(p < 10^-nsmall.p, paste("=", format(p, digits=1, scientific=TRUE)),
                       paste("=", formatF(p, nsmall=nsmall.p)))))
}


## Transform \emph{p} value to significance code.
##
## @inheritParams p.trans
##
## @return A character string of significance code.
##
## @examples
## sig.trans(c(1, 0.09, 0.049, 0.009, 0.001, 0.0001, 1e-50)) %>% data.table(sig=.)
##
## @export
sig.trans = function(p) {
  ifelse(is.na(p) | p > 1 | p < 0, "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "** ",
                       ifelse(p < .05, "*  ",
                              ifelse(p < .10, ".  ", "   ")))))
}

