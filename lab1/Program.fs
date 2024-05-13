open System
//Zad 1.1
let poleKola r = Math.PI * r * r

printfn "Zad 1.1 Pole kola wynosi: %A" (poleKola 5)

//Zad 1.2
let pierwiastkiRownaniaKwadratowego a b c =
    let delta = b * b - 4.0 * a * b

    if delta < 0.0 then
        printfn "Delta ujemna"
    elif delta > 0.0 then
        let x1 = (-b - Math.Sqrt(delta)) / 2.0 * a
        let x2 = (-b + Math.Sqrt(delta)) / 2.0 * a
        printfn "x1 = %A x2 = %A" x1 x2
    else
        let x0 = (-b) / 2.0 * a
        printfn "x0 = %A" x0


printf "Zad 1.2 Pierwiastki Rownania Kwadratowego: "
pierwiastkiRownaniaKwadratowego 4 2 6

//Zad 1.3
let sprawdzTrojkat (a: float) (b: float) (c: float) =
    if a > b + c || b > a + c || c > a + b then true else false

printfn "Zad 1.3 Zbodowanie Trojkata: %A" (sprawdzTrojkat 1 2 4)

//Zad 1.4
let poleTrojkata (a: float) (b: float) (c: float) =
    if sprawdzTrojkat a b c then
        let p = (a + b + c) / 2.0
        let P = Math.Sqrt(p * (p * a) * (p * b) * (p * c))
        printfn "Pole trojkata wynosi: %A" P
    else
        printfn "Nie mozna zbodowac trojkata"

printf "Zad 1.4 "
poleTrojkata 1 2 4

//Zad 1.5
let sumaLiczbNaturalnych n =
    let rec pomocnicza n' liczba =
        if n' > 0 then
            liczba + pomocnicza (n' - 1) (liczba + 1)
        else
            0

    pomocnicza n 1

printfn "Zad 1.5 Suma liczb naturalnych: %A" (sumaLiczbNaturalnych 5)

//Zad 1.6
let rec potega x n =
    if n > 0 then x + potega (n - 1) (x) else 0

printfn "Zad 1.6 Potega wynosi: %A" (potega 2 2)

//Zad 1.7
let rec fibonaci n =
    if n = 0 then 0
    elif n = 1 then 1
    else n + fibonaci (n - 1)

printfn "Zad 1.7 Ciag fibonacciego: %A" (fibonaci 6)
//Zad 1.8
let rec dwumianNewtona n k =
    if k = 0 then
        1
    elif k = n then
        1
    else
        dwumianNewtona (n - 1) k + dwumianNewtona (n - 1) (k - 1)

printfn "Zad 1.8 Dwumian Newtona: %A" (dwumianNewtona 10 5)

//Zad 1.9
let liczbaPierwsza a =
    let rec jestPodzielne a' dzielnik =
        if dzielnik * dzielnik > a' then false
        elif a' % dzielnik = 0 then true
        else jestPodzielne a' (dzielnik + 1)


    let sprawdzPodzielnosc a' =
        if a' = 1 then false
        elif a' = 2 then true
        else not (jestPodzielne a' 2)

    sprawdzPodzielnosc a

printfn "Zad 1.9 Liczba pierwsza: %A" (liczbaPierwsza 5)

//Zad 1.10
let prawdopodobienstwo =
    let rec rzut n =
        if n > 0 then (1.0 / 6.0) + rzut (n - 1) else 0.0

    rzut 1000

printfn "Zad 1.10 Prawdopodobienstwo wypadniecia 6 w 1000 rzutow: %A" prawdopodobienstwo

//Zad 1.11
let prawdopodobienstwo2 =
    let rec rzut n =
        if n > 0 then (2.0 / 36.0) + rzut (n - 1) else 0.0

    rzut 1000

printfn "Zad 1.11 Prawdopodobienstwo wypadniecia 6 w 1000 rzutow 2 kostkami: %A" prawdopodobienstwo2

//Zad 1.12
let nwd a b =
    let rec sprawdzNajwiekszyDzielnik a' dzielnik =
        if a' > dzielnik then
            if a' % dzielnik = 0 then
                dzielnik
            else
                sprawdzNajwiekszyDzielnik a' (dzielnik + 1)
        else
            2

    (sprawdzNajwiekszyDzielnik a 2) * (sprawdzNajwiekszyDzielnik b 2)

printfn "Zad 1.12 Najwiekszy wspolny dzielnik: %A" (nwd 5 25)

//Zad 1.13
printfn "test: %A" (10.0 ** -7.0)

printfn "Tessstttttttttt"
