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

