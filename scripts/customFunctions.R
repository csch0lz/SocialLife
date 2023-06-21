numbers=c('one','two','three','four','five','six','seven','eight','nine','ten','eleven','twelve')
print_as_word=function(x,capitalize='FALSE'){
  if(x<13){
    if(capitalize=='FALSE'){
      return(numbers[x])
    }
    else{
      return(str_to_title(numbers)[x])
    }
  }
  else{
    return(x)
  }
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

round_format=function(vec,digits=2){
  format(round(vec,digits),nsmall=digits)
}

plot_aes=theme_bw() + 
  theme(legend.position='bottom',text=element_text(size=10))


#Get upper and lower triangle of correlation matrix for heatmap plotting
#http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}