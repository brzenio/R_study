## Atrybuty
x <- 1:10
str( x )
attributes( x )

names( x )
names( x ) <- paste( "x", 1:length(x), sep = "_" )
names( x )
x

x[ 3 ]; x[ "x_3" ]
attributes( x )

attr( x = x, which = "Imie_wektora") <- "Wektor"
x

str( x )
x + 1

attr( x, "wieksze_od_5" )
attr( x, "wieksze_od_5" ) <- x[ x > 5 ]
x + 1

attr( x, "wieksze_od_5" ) + 10

## Funkcje 
potega <- function( w ){
  w ^ 2
}
potega

potega( w = 1:5 )

potega <- function( w, p = 2 ){
  w ^ p
}
potega( w = 1:5 )
potega( w = 1:5, p = 3 )

potega <- function( w, p = 2 ){
  wynik1 <- w ^ p
  wynik2 <- 6
}
potega( w = 1:5, p = 3 )

wynik <- potega( w = 1:5, p = 3 )
wynik

potega <- function( w, p = 2 ){
  wynik1 <- w ^ p
  wynik2 <- 6
  return( wynik1 )
}
potega( w = 1:5, p = 3 )

#Wyrażenia warunkowe
znak <- function( x ){
  
  wynik <- if( x < 0 ){ "liczba jest ujemna" 
  } else if( x > 0 ){ "liczba jest dodatnia" 
      } else{ "liczba jest równa 0" }
  
  return( wynik )
  
}

znak(-5);znak(0);znak(5)
znak(-5:5)

znak_wek <- function( x ){
  
  wynik <- ifelse( x < 0, "liczba jest ujemna", ifelse( x > 0, "liczba jest dodatnia", "liczba jest równa 0") )
  
  return( wynik )
  
}
znak_wek(-5:5)

# Factory
p <- factor( c("K","M","K","K","M") )
str(p)
typeof(p)

porzadek <- factor( c("jeden","dwa","trzy","jeden","dwa") )
porzadek
str(porzadek)

porzadek <- factor( c("jeden","dwa","trzy","jeden","dwa"), ordered = T )
porzadek

porzadek <- factor( c("jeden","dwa","trzy","jeden","dwa"), levels = c("jeden","dwa"), ordered = T )
porzadek

porzadek <- factor( c("jeden","dwa","trzy","jeden","dwa"), levels = c("jeden","dwa","trzy"), ordered = T )
porzadek

porzadek[ porzadek == "dwa" ]
porzadek[ porzadek >= "dwa" ]

wzrost_char <- c("wysoki","niski","średni","wysoki","niski")
table( wzrost_char )

table( wzrost_char, p )

levels( p )
levels( p ) <- c("K","M","NA")
p

table( wzrost_char, p )

prop.table( table( wzrost_char, p ) )# domyślnie procenty z całości
prop.table( table( wzrost_char, p ), 1 )#procenty z wierszy
prop.table( table( wzrost_char, p ), 2 )#procenty z wierszy

# Petle
suma_while <- function( x ){
  
  wynik <- 0
  n <- length( x )
  
  i <- 1
  while( i <= n ){
    
    wynik <- wynik + x[ i ]
    i <- i + 1
    
  }
  
  return( wynik )
  
}
sum(1:10); suma_while(1:10)

suma_repeat <- function( x ){
  
  wynik <- 0
  n <- length( x )
  
  i <- 1
  repeat{
    
    wynik <- wynik + x[ i ]
    i <- i + 1
    if( i > n ) break # stopowanie calej petli
                # next (continue) - kolejna iteracja
    
  }
  
  return( wynik )
  
}
sum(1:10); suma_repeat(1:10)

suma_for1 <- function( x ){
  
  wynik <- 0
  n <- length( x )
  
  for( i in 1:n ){
    
    wynik <- wynik + x[ i ]
    
  }
  
  return( wynik )
  
}
sum(1:10); suma_for1(1:10)

suma_for2 <- function( x ){
  
  wynik <- 0
  
  k <- 1; j <- 10
  for( i in x ){
    
    wynik <- wynik + i
    k <- k + 2; j <- j -3
  }
  
  return( wynik )
  
}
sum(1:10); suma_for2(1:10)

