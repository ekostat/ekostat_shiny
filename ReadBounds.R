ReadBounds<-function(){
  df<-read.table("data/boundaries.txt", fileEncoding = "UTF-8", sep="\t", stringsAsFactors=F, header=T)
  df<-df %>% select(-c(Months,Param,Comment))
  return(df)
}
