#
#
#
#
#
#
# zajecia 2 ------------------------------------------------------------------------------------------------------------
#
#
#
#
#


2 + 4;

3+6; 6-8

2 + 
  9
2 / 
  9

# komentarz

2+7 #komentarz# 5+3
# /*
#   komentarz
#   */

#### Typy danych
### atomowy
## logical
## numeric
# integer
# double
## complex
## character
##  *special

### złozony
## matrix
## array
## data.frame
## time.series
## factor

### rekurencyjny
## list
## function

typeof() # zwraca rodzaj danych
class() # klasa obiektu

## logical
TRUE; FALSE
T; F

true # wielkość liter ma znaczenie

c() #combine - łączenie elementów w wektor
c( TRUE, F, T, FALSE )
typeof( c( TRUE, F, T, FALSE ) )
class( c( TRUE, F, T, FALSE ) )

length( c( TRUE, F, T, FALSE ) ) # liczba elementów

rep() # repeat - powtrzanie elementów wektora 
?rep
rep

rep( x = c(T,F), each = 2 )
rep( x = c(T,F), times = 3 )
rep( x = c(T,F), length.out = 9 )

## numeric
# integer
1
typeof(1) # domyślnie double
class(1)

typeof( 1L ) # konwersja 

1:10 # sekwencja liczb z krokiem 1
seq( from = 1, to = 10, by = 1) # sequence 
seq( from = 1, to = 10, by = 2) 

-5:1
10:-7
seq( from = 0, to = 1, length.out = 5 ) 
seq( from = 0, to = 1, length.out = 100 ) 

# dobule
typeof( 3.14 )

( 0.1 + 0.1 ) == 0.2
( 0.1 + 0.1 + 0.1 ) == 0.3

print( 0.1, digits = 22 )
print( 0.3, digits = 22 )
print( ( 0.1 + 0.1 + 0.1 ), digits = 22 )

## complex 
typeof( 4+7i )
class( 4+7i )

# character
"napis"
'napis'
typeof( "napis" )
class( "napis" )

length( "napis" ) # liczba elementów
length( c("napis",'napis') )

nchar( "napis" ) # liczba znaków

print( "To jest pierwszy wiersz \n a to jest drugi wiersz" )
# wyświetla tekst takim jaki jest

cat( "To jest pierwszy wiersz \n a to jest drugi wiersz" )
# interpretuje znaki specjalne

## special 
NA # Not Available
typeof( NA )
class( NA )

NaN # Not a Number
sqrt(-2)
typeof( NaN )
class( NaN )

Inf; -Inf
typeof( Inf )
class( Inf )

NULL # obiekt pusty
typeof( NULL )
class( NULL )

### Rzutowanie typów danych (koercja)
# logical < integer < double < complex < character

c(T)
typeof(c(T))

c(T, 1L)
typeof(c(T, 1L))

c(T, 1L, 3.14)
typeof(c(T, 1L, 3.14))

c(T, 1L, 3.14, 5+7i)
typeof(c(T, 1L, 3.14, 5+7i))

c(T, 1L, 3.14, 5+7i, "napis")
typeof(c(T, 1L, 3.14, 5+7i, "napis"))

# Wymuszone rzutowanie i testowanie wektorów
is.logical( 5L )
is.logical( T )

as.logical( c(5L,0,3.14) )
as.character( c(5L,0,3.14) )

# Alokacja wektorów
vector("double", 10) # generyczna funkcja tworząca obiekt o wartościach domyślnych
vector("character", 10)

double(5)
logical(5)

# Tworzenie obiektów
x = 1:10
x

x <- 1:10
x
1:10 -> x
x

( x <- 1:10 )# automatyczne wyświetlanie obiektu w terminalu

assign( x = "x", value = 1:10, envir = .GlobalEnv )

x <- 4; y <- 4; z <- 4

x <- y <- z <- 4

# Operacje na wektorach
# arytmetyczne: +,-,*,?,^
# logiczne: <, >, <=, >=, !, !=
# koniunkcja & (&&), alternatywa | (||)
# Powyższe operatory są zwektoryzowane
x <- 1; y <- 11
x + y

x <- 1:2; y <- 11:12
x + y

x <- 1:4; y <- 11:12 #reguła zwijania, duplikowanie elementów
x + y
c(1,2,3,4) + c(11,12,11,12)

x <- 1:4; y <- 11:13
x + y
c(1,2,3,4) + c(11,12,13,11)

# Indeksowanie wektorów
x <- 11:20
x

x[ 1 ]
x[ 2:5 ]
x[ c(1,4:6,length(x)) ]

x[ c(8, rep(2,4), 1, 5 ) ]

x[ -6 ]
x[ -c(5,8,9) ] # elementy, których nie chcemy

x[ c(T,F) ] # wartości musi być tyle ile elementów ma wektor wejściowy

x[ c(c(T,F),c(T,F),c(T,F),c(T,F),c(T,F)) ] 
x[ rep(c(T,F),5) ] 

set.seed( 666 )
wektor <- sample( x = 1:1000, size = 10, replace = F )
wektor

wektor > 500
wektor > 300 & wektor < 600
wektor > 300 && wektor < 600

wektor[ wektor > 300 & wektor < 600 ]

# Sortowanie wektorów
sort( x = wektor )# niemalejąco
sort( x = wektor, decreasing = T )# nierosnąco

wektor
order( wektor ) #permutacja elementów, wymagana kolejnosć ustawienia
wektor[ order( wektor ) ]

# Łączenie wektorów (tekst)
paste( "x", 1:10 )#reguła zwijania
paste( "x", 1:10, sep = "_" )
paste( "x", 1:10, sep = "_", collapse = " + " )

letters
LETTERS

## Listy
y <- c( double = 3.14, character = "napis" )
y
str(y)
typeof(y)

lista <- list( double = 3.14, character = "napis", funkcja = mean )
lista
str( lista )
typeof( lista )

lista1 <- list( elem1 = 1:10, 
                11:20,
                elem3 = list( el1 = 2,
                              10) )
lista1[ 1 ]
lista1[ 1 ] + 1
typeof( lista1[ 1 ] )

lista1[[ 1 ]]
lista1[[ 1 ]] + 1
typeof( lista1[[ 1 ]] )

lista1$elem1

lista1$elem3
lista1$elem3$el1
lista1[[ 3 ]][[ 2 ]]
lista1[[ 3 ]]$el1

sum( lista1[[1]] )

lista_suma <- lapply(X = lista1[1:2], FUN = sum )
lapply(X = lista1[1:2], FUN = function(x){ sum(x) } )

do.call( what = "c", args = lista_suma )
do.call( what = "rbind", args = lista_suma )

#
#
#
#
#
#
#
#
# zajecia 3 ------------------------------------------------------------------------------------------------------------
#
#
#
#
#
#
#
#
#




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


#
#
#
#
#
#
#
#
# zajecia 4 ------------------------------------------------------------------------------------------------------------
#
#
#
#
#
#
#
#
#
#


# Tabele
# Matrix
x <- 1:6
x

attributes( x )
dim( x ) <- c( 3, 2 )
dim( x )
attributes( x )
x

x[1,2]
x[1:2,1]

str(x)
str(x[1:2,1])

x[1:2, 1, drop = T]
x[1:2, 1, drop = F]

colnames( x ) <- c("c1","c2")
x

rownames( x ) <- c("r1","r2","r2")
x

x[3,2]
x[3,"c2"]
x["r2","c2"] # pierwsze wystąpnie

x <- cbind( x, c3 = 10 ) #regula zwijania
x

x <- rbind( x, 20 )
x

apply(X = x, MARGIN = 1, FUN = sum)# suma po wierszach
apply(X = x, MARGIN = 2, FUN = sum)# suma po kolumnach

# Ramka danych
plec <- factor( c("K","M","K","K","M") )
wzrost <- c(200,160,180,190,165)

ramka_1 <- data.frame( klucz = 1:5, wzrost, plec )
ramka_1
typeof( ramka_1 )
str( ramka_1 )

as.matrix( ramka_1 )

ramka_1[order(ramka_1$plec),]
ramka_1[order(ramka_1$plec,-ramka_1$wzrost),]

ramka_2 <- data.frame( key = 1:5, waga = c(80,90,100,75,60), plec )
ramka_2

ramka_merge <- merge(x = ramka_1, y = ramka_2, by.x = "klucz", by.y = "key" )# iloczyn zbiorów
ramka_merge

aggregate( x = ramka_merge[,c("wzrost","waga")], by = list(ramka_merge$plec.x), FUN = mean )




#
#
#
#
#
#
#
#
# zajecia 5 ------------------------------------------------------------------------------------------------------------
#
#
#
#
#
#
#
#
#

# Srodowiska
x <- 1:10

ls()
ls(envir = .GlobalEnv)

environment()

lokalna <- function(){
  print( environment() )
  print( ls() )
  print( ls( envir = .GlobalEnv ) )
}
lokalna()

srod1 <- new.env()
srod1
typeof( srod1 )
class( srod1 )
str( srod1 ) 

assign( "z", 666 )
z
rm( z )

assign( "z", 666, envir = srod1 )
ls()
ls( envir = srod1 )

srod1[["z"]]
srod1$z

get( x = "z" )
get( x = "z", envir = srod1 )

l1 <- list( w = 1 )
s1 <- as.environment( l1 )
l1$w
s1$w

dodaj <- function( x ){
  cat("Przed zmiana", x$w ,"\n")
  x$w <- x$w + 1 
  cat("Po zmianie", x$w ,"\n")
}

l1$w
dodaj(l1)
l1$w

s1$w
dodaj(s1)
s1$w

wektor <- c(1,1)
lokalna_globalna <- function( x ){
  x[1] <- 666
  # wektor[1] <- 666
  wektor[1] <<- 666 # super assigment
  assign( "globalny", 999, envir = .GlobalEnv )
  assign( "lokalnysrod1", 999, envir = srod1 )
}

lokalna_globalna( wektor )
wektor
ls()

globalny
srod1$lokalnysrod1

# Przetwarzanie tekstu
tekst <- "Przetwarzanie danych w R"

length( tekst )
nchar( tekst ) 

charToRaw( tekst )
as.integer( charToRaw( tekst ) )

#UTF-8
"łąka"

tolower( tekst )
toupper( tekst )

rawToChar( as.raw( 1:127 ) )

cbind( Liczby = 1:127, Znaki = rawToChar( as.raw( 1:127 ), multiple = T ) )

for( i in 1:10 ){
  
  cat( sprintf( "To jest liczba %d", i ), "\n")
  # %d - integer, %5.2f - double, %s - character
  assign(x = sprintf( "x_%d", i ), value = 100 + i )
  
}

substr( x = tekst, start = 1, stop = 13 )
strsplit( x = tekst, split = " " )

sub( pattern = "w", replacement = "_w_", x = tekst )
gsub( pattern = "w", replacement = "_w_", x = tekst )

regexpr( pattern =  " ", text = tekst )
gregexpr( pattern =  " ", text = tekst )

# Kwantyfikatory
wyrazenie <- c("a","ab","acb","accb","acccb","accccb")

grep( pattern = "ab", x = wyrazenie, value = T )

# * - co najmniej 0 znakow
grep( pattern = "ac*b", x = wyrazenie, value = T )

# + - co najmniej 1 znak
grep( pattern = "ac+b", x = wyrazenie, value = T )

# ? - co najwyzej 1 znak
grep( pattern = "ac?b", x = wyrazenie, value = T )

# {n,m} - co najmniej n znakow, i co najwyzej m znak
grep( pattern = "ac{2,3}b", x = wyrazenie, value = T )

# Pozycja w tekscie
wyrazenie <- c("abcd","cdab","cadb","c abd")

# ^ - poczatek tekstu
grep( pattern = "ab", x = wyrazenie, value = T )
grep( pattern = "^ab", x = wyrazenie, value = T )

# $ - koniec tekstu
grep( pattern = "ab$", x = wyrazenie, value = T )

# Klasy znakow specjalnych
# [::]
# [:digit:] - cyfry
# [:lower:] - litery male
# [:upper:] - litery wielkie
# [:punct:] - znaki interpunkcyjne
# [:blank:] - biale znaki

# [] - definicja znakow, ktore moga pojawic sie w danym miejscu
# [ [:digit:] [:lower:] ] 
# [^] - definicja znakow, ktore nie moga pojawic sie w danym miejscu
# | - laczenie klas znakow

wyrazenie <- c("^ab","ab","abc","abd","abe","ab 100")

grep( pattern = "ab[cde]", x = wyrazenie, value = T )
grep( pattern = "ab[c-e]", x = wyrazenie, value = T )
grep( pattern = "abc|abd", x = wyrazenie, value = T )
grep( pattern = "[[:digit:]]", x = wyrazenie, value = T )



#
#
#
#
#
#
#
#
# zajecia 6 ------------------------------------------------------------------------------------------------------------
#
#
#
#
#
#
#
#
#






# Wykresy
szereg_1 <- c(1,3,9,4,7)
szereg_2 <- c(2,5,12,8,9)

plot( szereg_1, type = "l" )
title( main = "Wykres1", col.main = "red" )

dev.off()

par( mar = c(2,2,1,0), mfrow = c(2,2) )

rozstep <- range( szereg_1, szereg_2 )
plot( szereg_1, main = "Wykres2", type = "o", ylim = rozstep )
lines( szereg_2, col = "red", lty = 2, lwd = 3 )
# points()

set.seed( 123 )
r_norm <- rnorm( 1000 )

hist( x = r_norm, col = "blue", main = "Rozklad normalny", freq = F, xlab = "Kwantyle", ylab = "Gestosc" )
lines( density( r_norm ), col = "red", lwd = 5, lty = 2 )

# Bisekcja
funkcja <- function( x ){
  x ^ 3 - 2 * x - 5
}

dziedzina <- seq( -3, 3, by = 0.1 )
wartosci_y <- funkcja( dziedzina )

plot( x = dziedzina, y = wartosci_y, col = "blue", lwd = 2, type = "l" )
abline(h = 0)

Bisekcja <- function( f, a, b, maxiter = 100, eps = 1e-16 ){
  stopifnot( f(a) * f(b)  < 0 ) # Twierdzenie Darboux
  for( i in 1:maxiter ){
    cc <- ( a + b ) / 2
    if( abs( f(cc) ) < eps ) break
    if( f(cc) * f(a) > 0 ){ a <- cc }else{ b <- cc }
  }
  if( i == maxiter ){ warning("Osiagnieto max liczbe iteracji") }
  return( list( m_zerowe = cc, wartosc = f(cc) ) )
}

m_zerowe_nasze <- Bisekcja( f = funkcja, a = -3, b = 3, maxiter = 100, eps = 1e-16 )
m_zerowe_R <- uniroot( f = funkcja, interval = c(-3,3) )

points( x = m_zerowe_nasze$m_zerowe, y = m_zerowe_nasze$wartosc, col = "red", lwd = 5 )

## Data i czas
dzis_data <- Sys.Date()

typeof( dzis_data )
class( dzis_data )
str( dzis_data )

as.double( dzis_data )

as.double( as.Date( "1970-01-01" ) )
as.double( as.Date( "1960-01-01" ) )

# %d - dzien
# %a - dzien tygodnia skrotowo
# %A - dzien tygodnia
# %m - miesiac
# %b - nazwa miesiaca skrotowo
# %B - nazwa miesiaca
# %Y - rok

format( dzis_data, "%B %d %Y" )
format( dzis_data, "Dzis mamy %d (%A) %b roku %Y" )

as.Date( "1970--01::07", format = "%Y--%m::%d" )

dzis_czas <- Sys.time()
dzis_czas

typeof( dzis_czas )
class( dzis_czas )
str( dzis_czas )

as.double( dzis_czas )

dzis_data + 1

seq( dzis_data, length.out = 6, by = "week")

# Pliki i katalogi
getwd()
setwd("")

dir.exists( "/home/krzysztof/Virtual_machine/Zajecia/Zadania" )

# Integracja R z C++

install.packages("Rcpp")
library("Rcpp")

install.packages("installr")
library("installr")

install.Rtools()

cppFunction({'
  String znak1(double x){
    if(x>0){ return("liczba jest dodatnia"); }
    else if(x<0){ return("liczba jest ujemna"); }
    else{ return("liczba jest równa 0"); }
  }
'})

znak1(-5);znak1(0);znak1(5);
znak1(-5:5)

sourceCpp("znak2.cpp")

cppFunction({'
  IntegerVector seq_C(int start, int koniec){
    int n = koniec - start + 1;
    IntegerVector wyjscie( n );
    for( int i=0; i<n; i++ ){
      wyjscie[i] = start + i; 
    }
    return( wyjscie );
  }
'})

seq( 1, 10, by = 1 )
seq_C( 1, 10 )

sumWb <- sum

sumaR <- function(x){
  wyjscie <- 0
  for( i in 1:length(x) ){
    wyjscie <- wyjscie + x[i] 
  }
  return(wyjscie)
}

cppFunction({'
  double sumaC( NumericVector x ){
    int n = x.size();
    double wyjscie = 0.0;
    for( int i=0; i<n; i++ ){
      wyjscie += x[i]; 
    }
    return( wyjscie );
  }
'})

sumWb(1:10)
sumaR(1:10)
sumaC(1:10)

install.packages("microbenchmark")
library("microbenchmark")

los <- runif( 1000000 )
microbenchmark( sumWb = sumWb(los), sumaR = sumaR(los), sumaC = sumaC(los) ) 

cppFunction({'
  List lapplyC( List lista, Function f ){
    int n = lista.size();
    List wyjscie(n);
    for( int i=0; i<n; i++ ){
      wyjscie[i] = f(lista[i]); 
    }
    return( wyjscie );
  }
'})
lapply( list(1:4,3,20:60), length )
lapplyC( list(1:4,3,20:60), length )


