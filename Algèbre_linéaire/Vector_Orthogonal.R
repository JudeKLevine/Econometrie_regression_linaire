Vect_Orth=function(Vecteur){
  
  V = Vecteur
  Vecteur_ortho = c()
  indice = which(V != 0)
  Nbr = 0
  
  if(length(indice) == 0){                # Si c'est le vecteur nul
    cat("\n","C'EST LE VECTEUR NULL","\n", 
        "IL EST ORTHOGONAL A TOUS LES VECTEURS DU PLAN", "\n")
    quit()
  }                                       # si ce n'est pas le vecteur nul              
  else{
    for(i in 1:length(V)){     # on recupere une coordonnée de Vecteur qui 
      if(i != indice[1]){      # n'est pas nul et on fait la somme des autres
        Nbr = Nbr + V[i]        
        Vecteur_ortho[i] = Vecteur[indice[1]] # on remplit les autres avec 
      }                                       # la même valeur
    }
    Vecteur_ortho[indice[1]] = -Nbr
  }
  return(as.vector(Vecteur_ortho))
}



