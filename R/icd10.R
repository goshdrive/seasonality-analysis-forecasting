codes2str = function(codes, simp = FALSE, excl_brackets = FALSE) {
  str <- ifelse(simp | excl_brackets, '', '(')
  for (c in codes) {
    if (nchar(str) > 1) str <- paste0(str, ifelse(simp, '_', ', '))
    if (length(c) == 1) str <- paste0(str, c)
    else                str <- paste0(str, c[[1]], '-', c[[2]])
  }
  str <- ifelse(simp | excl_brackets, str, paste0(str,')'))
  return(str)
}
