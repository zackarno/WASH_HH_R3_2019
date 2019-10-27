wash_container_volume_bgd_2019<-function(x,y,z,w){
  if(is.na(x) & is.na(y) & is.na(w) & !is.na(z)){
    ((4*pi*(((z*2.54)/(2*pi))^3)/3)*(-0.0078*z+1.1217))/1000
  } else if(!is.na(x) & !is.na(y) & is.na(w) & is.na(z)){
    ((pi*(x*2.54)*((y*2.54)/2)^2)*0.85)/1000
    
  } else if(!is.na(x) & !is.na(y) & !is.na(w) & is.na(z)){
    ((x*2.54*y*2.54*w*2.54)*0.95)/1000
  } else{
    0
  }
}
