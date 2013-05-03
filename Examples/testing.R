##Just some testing

for(i in 1:ncol(wtr)){
  
  plot(wtr[1:NS[i],i],elev[1:NS[i],i])
  browser()
  
}