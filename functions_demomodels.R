

### funcion dis ###
### display integers in comma notation

### begin function dis
dis <- function(x){format(round(as.numeric(x), 1),  big.mark=",")}
### end function dis


### function insertRows
### source: http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
### auxiliary function to add rows at specified place to exisiting data frame
### Arguments:  existingDF:       data frame
###             newrows:          new row(s) with same number of columns as existingDF or vector with length equal to number of columns of existingDF
###             r:                beginning of row index where new rows are to be inserted as a block
### Value:      returns data frame with new row added at r

#begin function insertRows
insertRows <- function(existingDF, newrows, r) {
  if(is.null(dim(newrows))){newrows<-as.data.frame(t(newrows))}
  if(r<=nrow(existingDF)){existingDF[seq(r+dim(newrows)[1],nrow(existingDF)+dim(newrows)[1]),] <- existingDF[seq(r,nrow(existingDF)),]}
  existingDF[r:(r+dim(newrows)[1]-1),] <- newrows
  existingDF
}
#end function insertRows



### function format_gt ###
### global trends rounding
### begin function format_gt
format_gt <- function(x, million = F) {
  limits <- c(1e0, 1e2, 1e24)
  # Vector with array indices according to position in intervals
  i <- findInterval(abs(x), limits)
  
  # Set prefix to " " for very small values < 1e-24
  i <- ifelse(i == 0, which(limits == 1e0), i)
  
  if(million){
    (if(x>50000){
      paste(format(round(x/1e6, 1),
                   trim = TRUE, scientific = FALSE), "million")}
     else format(round(x/100, 0)*100,
                 trim = TRUE, scientific = FALSE, big.mark=","))}
  else {if(x>=1000) {format(round(x/100, 0)*100,
                            trim = TRUE, scientific = FALSE, big.mark=",")}
    else x
  }
  
}
### end function format_gt




### function topn_gt ###
### create text element for global trends with top 10 countries plus figures
### begin function topn_gt
topn_gt <- function(dat, name, num, nu=10) {
  x <- unlist(
    dat %>% 
      select({{name}}, {{num}}) %>% 
      slice_max({{num}}, n={{nu}}) %>% 
      mutate("name_num" = paste0({{name}}, " (", "), ")) %>% 
      select({{name_num}})
  )
  paste0(x[1:(nu-1)], x[nu], collapse = "")
}
### end function topn_gt

