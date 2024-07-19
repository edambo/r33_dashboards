# Create function for conditional determination of value box colors
  
# set color alert code
color_code <- function(value, total){
  # Good
  if((value/total)*100 >= 80){
    color = "vb-green"
  }
  
  # Warning
  if(((value/total)*100 >= 60) & ((value/total)*100 < 80)){
    color = "vb-yellow"
  }
  
  # Bad
  if((value/total)*100 < 60){
    color = "vb-red"
  }
  
  color
}
