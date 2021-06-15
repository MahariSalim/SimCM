# ´chaine de markov a temps discret
# '@export
# '@param z vecteur numérique représentant les étapes
# ´@param mu vecteur numérique la distribution initiale
# ´@param p matrice de transition
# ´@param n nombre numérique représentant le nombre de pas


Simr <- fonction ( z , mu , P , n )
{
  y <- c(rep( 0 , n + 1 ))
  t < -c( 0 : n )
  y [ 1 ] <- rdist( z , mu )
  for ( i  in  1 : n ){
    y [ i + 1 ] <- rdist( z , P [ y [ i ],])
  }
  
  plot( t , y , type  =  " b " , main  =  " graphe des états pour tous les temps de 0 à n " , pch = 5 , xlim = c( 0 , n ), ylim = c( 0 ,length( mu ) + 1 ), xlab  =  " temps " , ylab  =  " états " , col = " bleu" )
  par( mfrow  = c( 2 , 1 ))
  retour ( y )

  

