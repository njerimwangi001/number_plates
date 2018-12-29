cars<-function(plate1,plate2){
  
  x1<-substitute(plate1)
  x1<-unlist(strsplit(toString(x1),""))
  
  x2<-substitute(plate2)
  x2<-unlist(strsplit(toString(x2),""))
  
  #Assigning each letter a numeric value
  alphabets<-data.frame(LETTERS)
  
  #First difference of alphabets 
  p1<-which(alphabets[1:nrow(alphabets),]==x1[2])
  p2<-which(alphabets[1:nrow(alphabets),]==x2[2])
  
  #Absolute difference---------
  position2<-abs(p1-p2)
  
  #Second difference of alphabets
  p3<-which(alphabets[1:nrow(alphabets),]==x1[3])
  p4<-which(alphabets[1:nrow(alphabets),]==x2[3])
  
  #Absolute difference-------
  position3<-abs(p3-p4)
  
  #Computing difference between the seventh elements of alphabets Kenyan number plate
  p5<-which(alphabets[1:nrow(alphabets),]==x1[7])
  p6<-which(alphabets[1:nrow(alphabets),]==x2[7])
  
  #Absolute difference-------
  position7<-abs(p5-p6)
  
  #Computing the difference between the numeric elements in alphabets Kenyan number plate
  num1<-as.numeric(paste(x1[4:6],collapse=""))
  num2<-as.numeric(paste(x2[4:6],collapse=""))
  
  cars<-575424*position2+23976*position3+999*position7+num2
  cars

}

cars(KAA021J,KCC576K)
