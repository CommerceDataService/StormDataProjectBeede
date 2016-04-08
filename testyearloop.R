for(yearly in c(2007:2007)){
  start_date = paste(yearly,"-01-01", sep="")
  end_date = paste(yearly,"-12-31", sep="")
  print(start_date)
  print(end_date)
  x = paste("Nexrad_allstates_extract_", yearly, ".rda", sep="")
  print(x)
}  