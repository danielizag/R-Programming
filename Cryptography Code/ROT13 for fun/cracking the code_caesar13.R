wordCiph <-function(word){
  vect <- substring(word, seq(1,nchar(word),1), seq(1,nchar(word),1))
  alph <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
  for(i in 1:length(vect)){
    if(!is.na(match(vect[i], alph))){
      start <- match(vect[i], alph)
      if(start<14){
        start=start+13
        }
      else{
        start=start-13
        }
      vect[i]<- alph[start]
    }
  }
  y <- paste(vect, collapse = '')
  return(y)
}

longFormCiph <- function(string){
  words <- string
  print(words)
  newWords <- words
  #print(newWords)
  for(i in 1:length(words)){
    newWords[i] <- wordCiph(words[i])
    print(newWords[1])
    print(i)
  }
  #print(newWords)
  y <- paste(newWords, collapse = '')
  #print(y)
  return(y)
}

string="string have u seen the ox go pee pee?"
y <- longFormCiph(string)
print(y)
