print_table <-  function(x, digits=3, nsmalls=digits,
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
      if(str_detect(file, "\\.doc$")==FALSE)
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
  TBODY = TBODY %>%
    str_replace_all(">\\s*NA\\s*<", "><") %>%
    str_replace_all("\\s+</td>", "</td>") %>%
    str_replace_all("\\[\\s+", "[") %>%
    str_replace_all("\\,\\s+", ", ") %>%
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
      Print("<<green \u221a>> Table saved to <<bold \"{paste0(getwd(), '/', file)}\">>")
      cat("\n")
    }
  }

  invisible(list(HTML=HTML, TABLE=TABLE))
}
