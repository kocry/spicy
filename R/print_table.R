#' Print a three-line table (to R Console and Microsoft Word).
#'
#' This basic function prints any data frame as a three-line table
#' to either R Console or Microsoft Word (.doc).
#' It has been used in many other functions of \code{bruceR} (see below).
#'
#' @param x Matrix, data.frame (or data.table), or any model object (e.g., \code{lm, glm, lmer, glmer, ...}).
#' @param digits,nsmalls Numeric vector specifying the number of decimal places of output. Default is \code{3}.
#' @param nspaces Number of whitespaces between columns. Default is \code{1}.
#' @param row.names,col.names Print row/column names. Default is \code{TRUE} (column names are always printed).
#' To modify the names, you can use a character vector with the same length as the raw names.
#' @param title Title text, which will be inserted in <p></p> (HTML code).
#' @param note Note text, which will be inserted in <p></p> (HTML code).
#' @param append Other contents, which will be appended in the end (HTML code).
#' @param line Lines looks like true line (\code{TRUE}) or \code{=== --- ===} (\code{FALSE}).
#' @param file File name of MS Word (\code{.doc}).
#' @param file.align.head,file.align.text Alignment of table head or table text:
#' \code{"left"}, \code{"right"}, \code{"center"}.
#' Either one value of them OR a character vector of mixed values
#' with the same length as the table columns.
#' Default alignment (if set as \code{"auto"}):
#' left, right, right, ..., right.
#'
#' @return Invisibly return a list of data frame and HTML code.
#' @importFrom stats coef line
#' @importFrom stringr str_replace str_replace_all str_detect str_trim
#' @export
#'
#' @examples
#' \dontrun{
#' print_table(data.frame(x=1))
#'
#' print_table(airquality, file="airquality.doc")
#' unlink("airquality.doc")  # delete file for code check
#'
#' model = lm(Temp ~ Month + Day + Wind + Solar.R, data=airquality)
#' print_table(model)
#' print_table(model, file="model.doc")
#' unlink("model.doc")  # delete file for code check
#' }

print_table = function(x, digits=3, nsmalls=digits,
                       nspaces=1,
                       row.names=TRUE,
                       col.names=TRUE,
                       title="", note="", append="",
                       line=TRUE,
                       file=NULL,
                       file.align.head="auto",
                       file.align.text="auto") {
  ## Preprocess data.frame ##
  if(!inherits(x, c("matrix", "data.frame", "data.table"))) {
    coef.table = coef(summary(x))
    if(!is.null(coef.table)) x = coef.table
  }
  x = as.data.frame(x)
  sig = NULL
  if(length(nsmalls)==1) nsmalls = rep(nsmalls, length(x))
  for(j in 1:length(x)) {
    if(inherits(x[,j], "factor"))
      x[,j] = as.character(x[,j])
    if(grepl("Pr\\(|pval|p.value|<i>p</i>", names(x)[j])) {
      sig = formatF(sig.trans(x[,j]), 0)  # formatF will make * left-aligned
      if(grepl("<i>p</i>", names(x)[j])==FALSE)
        names(x)[j] = "p"
      x[,j] = p.trans(x[,j])
    } else {
      x[,j] = formatF(x[,j], nsmalls[j])
    }
    if(grepl("^S\\.E\\.$|^Std\\. Error$|^se$|^SE$|^BootSE$", names(x)[j])) {
      x[,j] = paste0("(", x[,j], ")")  # add ( ) to S.E.
      x[grepl("\\d", x[,j])==FALSE, j] = ""  # remove ( ) from blank S.E.
      if(grepl("S\\.E\\.", names(x)[j])==FALSE) names(x)[j] = "S.E."
    }
    if(grepl("^S\\.D\\.$|^Std\\. Deviation$", names(x)[j])) {
      x[,j] = paste0("(", x[,j], ")")  # add ( ) to S.D.
      x[grepl("\\d", x[,j])==FALSE, j] = ""  # remove ( ) from blank S.E.
      if(grepl("S\\.D\\.", names(x)[j])==FALSE) names(x)[j] = "S.D."
    }
    # if(grepl("\\[", names(x)[j])) x[,j] = paste0("[", x[,j], ",")
    # if(grepl("\\]", names(x)[j])) x[,j] = paste0(x[,j], "]")
    # if(grepl("^[Ee]stimate$", names(x)[j])) names(x)[j] = "Coef."
    names(x)[j] = gsub(" value$|val$", "", names(x)[j])
  }
  if(is.null(sig)==FALSE & "sig" %notin% names(x)) {
    p.pos = which(names(x) %in% c("p", "<i>p</i>"))
    nvars = length(names(x))
    if(p.pos<nvars)
      x = cbind(x[1:p.pos], ` ` = sig, x[(p.pos+1):nvars])
    else
      x = cbind(x, ` ` = sig)
    x$` ` = as.character(x$` `)
  }

  if(inherits(row.names, "character")) {
    row.names(x) = row.names
    row.names = TRUE
  }
  if(inherits(col.names, "character")) {
    names(x) = col.names
    col.names = TRUE
  }

  ## Compute length to generate line-chars ##
  linechar = ifelse(line, "\u2500", "-")
  title.length = nchar(names(x), type="width")
  vars.length = c()  # bug: vars.length = apply(apply(x, 2, nchar), 2, max)
  for(j in 1:length(x)) vars.length[j] = max(nchar(x[,j], type="width"))
  n.lines = apply(rbind(title.length, vars.length), 2, max)+nspaces
  n.lines.rn = max(nchar(row.names(x), type="width"))+nspaces
  if(row.names)
    table.line = rep_char(linechar, sum(n.lines)+n.lines.rn)
  else
    table.line = rep_char(linechar, sum(n.lines))

  ## Output ##
  if(is.null(file)) {
    if(title!="") Print(title)
    Print(table.line)
    if(row.names)
      cat(rep_char(" ", n.lines.rn))
    for(j in 1:length(x)) {
      name.j = names(x)[j]
      cat(rep_char(" ", n.lines[j]-nchar(name.j, type="width")) %^% name.j)
    }
    cat("\n")
    Print(table.line)
    for(i in 1:nrow(x)) {
      if(row.names) {
        row.name.i = row.names(x)[i]
        cat(row.name.i %^% rep_char(" ", n.lines.rn-nchar(row.name.i, type="width")))
      }
      for(j in 1:length(x)) {
        # cat(sprintf(glue("% {n.lines[j]}s"), ifelse(is.na(xr[i,j]) | grepl("NA$", xr[i,j]), "", xr[i,j])))
        x.ij = ifelse(is.na(x[i,j]) | grepl("NA$", x[i,j]), "", x[i,j])
        cat(rep_char(" ", n.lines[j]-nchar(x.ij, type="width")) %^% x.ij)
      }
      cat("\n")
    }
    Print(table.line)
    if(note!="") Print(note)
  }
  if(row.names) {
    x = cbind(rn=row.names(x), x)
    names(x)[1] = ""
  }
  if(!is.null(file)) {
    html = df_to_html(x, title=title, note=note, append=append,
                      file=file,
                      align.head=file.align.head,
                      align.text=file.align.text)
  } else {
    html = NULL
  }

  invisible(list(df=x, html=html))
}


df_to_html = function(df, title="", note="", append="",
                      file=NULL,
                      align.head="auto",
                      align.text="auto") {
  if(!is.null(file)) {
    if(file=="NOPRINT") {
      file = NULL
    } else {
      file = str_replace(file, "\\.docx$", ".doc")
      if(str_detect(file, "\\.doc$")==F)
        file = paste0(file, ".doc")
    }
  }

  TITLE = title
  TNOTE = note
  APPEND = append

  if(length(align.head)==1) {
    if(align.head=="auto")
      align.head = c("left", rep("right", times=ncol(df)-1))
    else
      align.head = rep(align.head, times=ncol(df))
  }
  if(length(align.text)==1) {
    if(align.text=="auto")
      align.text = c("left", rep("right", times=ncol(df)-1))
    else
      align.text = rep(align.text, times=ncol(df))
  }

  df = as.data.frame(df)
  for(j in 1:ncol(df)) {
    df[[j]] = "<td align='" %^% align.text[j] %^% "'>" %^%
      str_trim(str_replace_all(df[[j]], "^\\s*-{1}", "\u2013")) %^% "</td>"
  }

  THEAD = "<tr> " %^%
    paste("<th align='" %^%
            align.head %^%
            "'>" %^% names(df) %^% "</th>",
          collapse=" ") %^% " </tr>"

  TBODY = "<tr> " %^%
    paste(apply(df, 1, function(...) paste(..., collapse=" ")),
          collapse=" </tr>\n<tr> ") %^% " </tr>"
  TBODY = TBODY |>
    str_replace_all(">\\s*NA\\s*<", "><") |>
    str_replace_all("\\s+</td>", "</td>") |>
    str_replace_all("\\[\\s+", "[") |>
    str_replace_all("\\,\\s+", ", ") |>
    str_replace_all("<\\.001", "< .001")

  TABLE = paste0("
<table>
<thead>
", THEAD, "
</thead>
<tbody>
", TBODY, "
</tbody>
</table>
")

  HTML = paste0("<!DOCTYPE html>
<html>
<head>
<meta charset='utf-8'>
<title></title>
<style>
", ifelse(
  grepl("\\.doc$", file),
  "body, pre {font-size: 10.5pt; font-family: Times New Roman;}",
  ""
), "
p {margin: 0px;}
table {border-collapse: collapse; border-spacing: 0px; color: #000000;
       border-top: 2px solid #000000; border-bottom: 2px solid #000000;}
table thead th {border-bottom: 1px solid #000000;}
table th, table td {padding-left: 5px; padding-right: 5px; height: 19px;}
</style>
</head>
<body>
<p>", TITLE, "</p>", TABLE, "<p>", TNOTE, "</p>", APPEND, "
</body>
</html>")

  if(!is.null(file)) {
    if(file!="NOPRINT") {
      # sink(file)
      # cat(HTML)
      # sink()
      f = file(file, "w", encoding="UTF-8")
      cat(HTML, file=f)
      close(f)
      Print("<<green \u221a>> Table saved to << \"{paste0(getwd(), '/', file)}\">>")
      cat("\n")
    }
  }

  invisible(list(HTML=HTML, TABLE=TABLE))
}


#### Pipeline Functions ####


#' Paste strings together.
#'
#' Paste strings together. A wrapper of \code{paste0()}.
#' Why \code{\%^\%}? Because typing \code{\%} and \code{^} is pretty easy by
#' pressing \strong{Shift + 5 + 6 + 5}.
#'
#' @param x,y Any objects, usually a numeric or character string or vector.
#'
#' @return A character string/vector of the pasted values.
#'
#' @examples
#' \dontrun{
#' "He" %^% "llo"
#' "X" %^% 1:10
#' "Q" %^% 1:5 %^% letters[1:5]
#' }
#' @export
`%^%` = function(x, y) {
  paste0(x, y)
}


#' The opposite of \code{\%in\%}.
#'
#' @param x Numeric or character vector.
#' @param vector Numeric or character vector.
#'
#' @return A vector of \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' data = data.table(ID=1:10, X=sample(1:10, 10))
#' data
#' data[ID %notin% c(1, 3, 5, 7, 9)]
#' }
#' @export
`%notin%` = function(x, vector) {
  match(x, vector, nomatch=0) == 0
}


#' A simple extension of \code{\%in\%}.
#'
#' @inheritParams %notin%
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' 1:2 %allin% 1:3  # TRUE
#' 3:4 %allin% 1:3  # FALSE
#' }
#' @export
`%allin%` = function(x, vector) {
  all(x %in% vector)
}


#' A simple extension of \code{\%in\%}.
#'
#' @inheritParams %notin%
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' 3:4 %anyin% 1:3  # TRUE
#' 4:5 %anyin% 1:3  # FALSE
#' }
#' @export
`%anyin%` = function(x, vector) {
  any(x %in% vector)
}


#' A simple extension of \code{\%in\%}.
#'
#' @inheritParams %notin%
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' 3:4 %nonein% 1:3  # FALSE
#' 4:5 %nonein% 1:3  # TRUE
#' }
#'
#' @export
`%nonein%` = function(x, vector) {
  !any(x %in% vector)
}


#' A simple extension of \code{\%in\%}.
#'
#' @param pattern Character string containing \strong{regular expressions} to be matched.
#' @param vector Character vector.
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' "Bei" %partin% c("Beijing", "Shanghai")  # TRUE
#' "bei" %partin% c("Beijing", "Shanghai")  # FALSE
#' "[aeiou]ng" %partin% c("Beijing", "Shanghai")  # TRUE
#' }
#'
#' @export
`%partin%` = function(pattern, vector) {
  any(grepl(pattern, vector, perl=TRUE))
}



#' Format "1234" to "1,234".
#'
#' @param x A number or numeric vector.
#' @param mark Usually \code{","}.
#'
#' @return Formatted character string.
#'
#' @examples
#' \dontrun{
#' formatN(1234)
#' }
#' @seealso \code{\link[base:format]{format}}, \code{\link{formatF}}
#'
#' @export
formatN = function(x, mark=",") {
  format(x, big.mark=mark)
}


#' Format numeric values.
#'
#' @param x A number or numeric vector.
#' @param digits,nsmall Number of decimal places of output. Default is \code{3}.
#'
#' @return Formatted character string.
#'
#' @examples
#' \dontrun{
#' formatF(pi, 20)
#' }
#' @export
formatF = function(x, digits=3, nsmall=digits) {
  # format(x, digits=0, nsmall=nsmall, scientific=FALSE)
  if(inherits(x, "character")) {
    xf = sprintf(paste0("%-", max(nchar(x), na.rm=TRUE), "s"), x)  # left adjustment
  } else {
    x = sprintf(paste0("%.", nsmall, "f"), x)
    xf = sprintf(paste0("%", max(nchar(x), na.rm=TRUE), "s"), x)
  }
  return(xf)
}


#' A simple extension of \code{rgb()}.
#'
#' @param r,g,b Red, Green, Blue: 0~255.
#' @param alpha Color transparency (opacity): 0~1.
#' If not specified, an opaque color will be generated.
#'
#' @return \code{"#rrggbb"} or \code{"#rrggbbaa"}.
#'
#' @examples
#' \dontrun{
#' RGB(255, 0, 0)  # red: "#FF0000"
#' RGB(255, 0, 0, 0.8)  # red with 80\% opacity: "#FF0000CC"
#' }
#' @export
RGB = function(r, g, b, alpha) {
  grDevices::rgb(r/255, g/255, b/255, alpha)
}

#' Transform \emph{p} value.
#'
#' @param p \emph{p} value.
#' @param nsmall.p Number of decimal places of \emph{p} value. Default is \code{3}.
#'
#' @return A character string of transformed \emph{p} value.
#' @export
#'
#' @examples
#' \dontrun{
#' p.trans(c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0.0000001, 1e-50)) |> data.table(p=.)
#' }
p.trans = function(p, nsmall.p=3) {
  mapply(function(p, nsmall.p) {
    ifelse(is.na(p) | p > 1 | p < 0, "",
           ifelse(p < 10^-nsmall.p, gsub("0(?=\\.)", "", Glue("<{10^-nsmall.p:.{nsmall.p}}"), perl=T),
                  gsub("0(?=\\.)", " ", Glue("{p:.{nsmall.p}}"), perl=T)))
  }, p, nsmall.p)
}



#' Transform \emph{p} value to significance code.
#'
#' @inheritParams p.trans
#'
#' @return A character string of significance code.
#'
#' @examples
#' \dontrun{
#' sig.trans(c(1, 0.09, 0.049, 0.009, 0.001, 0.0001, 1e-50)) |> data.table(sig=.)
#' }
#' @export
sig.trans = function(p) {
  ifelse(is.na(p) | p > 1 | p < 0, "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "** ",
                       ifelse(p < .05, "*  ",
                              ifelse(p < .10, ".  ", "   ")))))
}



#' Repeat a character string for many times and paste them up.
#'
#' @param char Character string.
#' @param rep.times Times for repeat.
#'
#' @return Character string.
#'
#' @examples
#' \dontrun{
#' rep_char("a", 5)
#' }
#' @export
rep_char = function(char, rep.times) {
  paste(rep(char, times=rep.times), collapse="")
}
