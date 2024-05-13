//Zad 3.1
type Lista<'a> = 
  | Pusta
  | Wezel of 'a*Lista<'a>

let glowa =
  function
  | Pusta -> failwith "Nie mozna pobrac glowy z listy pustej"
  | Wezel(glowa,_) -> glowa

let ogon =
  function
  | Pusta -> failwith "Nie mozna pobrac ogona z listy pustej"
  | Wezel(_,ogon) -> ogon


let nPierwszych n =
  let rec pomocnicza i acc =
    if i > n then
      acc
    else 
      let nowyAcc = 
        match acc with
        | Pusta -> Wezel(i, Pusta)
        | _ -> Wezel(i, acc)
      pomocnicza (i + 1) nowyAcc
  pomocnicza 1 Pusta

let rec wypiszListe = 
  function
    | Pusta -> ()
    | Wezel(x, Pusta) -> printf "%A" x
    | Wezel(x, xs) -> 
      printf "%A, " x
      wypiszListe xs

let lista = nPierwszych 5
printfn "Zadanie 3.1: "
wypiszListe lista
printfn " "

//Zad 3.2
let generujeListe min max krok =
  let rec pomocnicza min max krok = 
    if min > max then Pusta
    else Wezel(min, pomocnicza (min + krok) max krok)
  pomocnicza min max krok

let lista2 = generujeListe 1 10 2
printfn "Zadanie 3.2: "
wypiszListe lista2
printfn " "

//Zad 3.3
let rec ntyElement n lista =
    match n, lista with
    | 0, Wezel(glowa, _) -> glowa
    | _, Pusta -> failwith "Indeks poza zakresem listy"
    | _, Wezel(_, ogon) -> ntyElement (n - 1) ogon

printfn "Zadanie 3.3: %A" (ntyElement 3 lista)

//Zad 3.4
let rec czyElementNaLiscie element lista =
    match lista with
    | Pusta -> false
    | Wezel(glowa, _) -> glowa = element || czyElementNaLiscie element (ogon lista)

printfn "Zadanie 3.4: %A" (czyElementNaLiscie 6 lista)

//Zad 3.5
type Wynik =
 | Znaleziono of int
 | NieZnaleziono

let rec indeksElementu element lista =
    let rec pomocnicza element lista indeks =
        match lista with
        | Pusta -> NieZnaleziono
        | Wezel(glowa, _) ->
            if glowa = element then Znaleziono indeks
            else pomocnicza element (ogon lista) (indeks + 1)
    pomocnicza element lista 0

printf "Zadanie 3.5: "
match indeksElementu 3 lista with
| Znaleziono i -> printfn "Element znaleziony na indeksie: %d" i
| NieZnaleziono -> printfn "Element nie znaleziony na liście"

//Zad 3.6
let rec usunElementPozycji n lista =
  match n, lista with
  | 0, Wezel(_, ogon) -> ogon
  | _, Pusta -> failwithf "Indeks poza zakresem listy"
  | _, Wezel(glowa, ogon) -> Wezel(glowa, usunElementPozycji (n - 1) ogon)

let lista3 = Wezel(1, Wezel(2, Wezel(3, Wezel(5, Pusta))))
let nowaLista = usunElementPozycji 3 lista3

printfn "Zadanie 3.6: "
printfn "Lista podstawowa:"
wypiszListe lista3
printfn ""
printfn "Lista po usunieciu:"
wypiszListe nowaLista
printfn ""

//Zad 3.7
let rec suma lista =
  match lista with
  | Pusta -> 0.0
  | Wezel(glowa, ogon) -> glowa + suma ogon

let rec dlugosc lista = 
  match lista with
  | Pusta -> 0
  | Wezel(_, ogon) -> 1 + dlugosc ogon

let srednia lista =
  let sumaWartosci = suma lista
  let dlugoscListy = dlugosc lista
  if dlugoscListy = 0 then 
    failwith "Lista jest pusta"
  else
    sumaWartosci / float dlugoscListy

let lista4 = Wezel(1.0, Wezel(2.0, Wezel(3.0, Wezel(4.0, Pusta))))

printfn "Zad 3.7: %A" (srednia lista4)

//Zad 3.8
let polaczZSeparatorem separator tablica =
  String.concat separator tablica

let tablica = [|"Hello"; "World!"; "Test";|]
let polaczonyLancuch = polaczZSeparatorem " " tablica
printfn "Zadanie 3.8: %A" polaczonyLancuch

//Zad 3.9
let dlugosciStringow lista = 
  List.map (fun (s: string) -> s.Length) lista

let listaStringow = ["Hello"; "World"; "F#"]
printfn "Zad 3.9: %A" (dlugosciStringow listaStringow)

//Zad 3.10
let najdluzszyINajkrotszy lista =
  let najdluzszy = List.maxBy (fun (s: string) -> s.Length) lista
  let najkrotszy = List.minBy (fun (s: string) -> s.Length) lista
  (najdluzszy, najkrotszy)

let (najdluzszy, najkrotszy) = najdluzszyINajkrotszy listaStringow

printfn "Zadanie 3.10: Najdluzszy: %A, Najkrotszy: %A" najdluzszy najkrotszy

//Zad 3.11
let imieZenskie imie: string list = 
  List.filter (fun name -> name.EndsWith("a")) imie

let imiona = ["Adam"; "Anna"; "Piotr"; "Maria"; "Jan"]
let listaZenskichImion = imieZenskie imiona
printfn "Zadanie 3.11: %A" listaZenskichImion

//Zad 3.12
let rec odwrocenieListy lista =
  match lista with
  | [] -> []
  | x::rest -> (odwrocenieListy rest) @ [x]

let lista1 = [1; 2; 3; 4; 5]
printfn "Zadanie 3.12: Oryginalna Lista: %A, Odwrocona Lista: %A" lista1 (odwrocenieListy lista1)

//Zad 3.13
let osobneImiona (imiona: string list) : string list * string list= 
  let (damskieImiona, meskieImiona) = List.partition (fun (imie: string) -> imie.EndsWith("a")) imiona
  (damskieImiona, meskieImiona)

let (listaImionZenskich, listaImionMeskich) = osobneImiona imiona
printfn "Zadanie 3.13: Damskie Imiona: %A, Meskie Imiona: %A" listaImionZenskich listaImionMeskich

//Zad 3.14
let porownajListy lista1 lista2 =
  if List.length lista1 <> List.length lista2 then
    failwith "Listy maja rozna dlugosc"
  else
    List.map2 (fun x y -> x > y) lista1 lista2

let listaLiczb1 = [1; 2; 3; 4]
let listaLiczb2 = [2; 1; 3; 4]
let wynikPorownania = porownajListy listaLiczb1 listaLiczb2
printfn "Zadanie 3.14: Lista pierwsza: %A, Lista druga: %A, Porownanie List: %A" listaLiczb1 listaLiczb2 wynikPorownania

//Zad 3.15
type WynikPorownaina2 =
  | Pierwsza
  | Druga

let porownanieList lista1 lista2 = 
  let rec porownianieRec lista1 lista2 acc =
    match lista1, lista2 with
    | [], [] -> acc
    | [], _ ->  List.append acc (List.map (fun _ -> Druga) lista2)
    | _, [] -> List.append acc (List.map (fun _ -> Pierwsza) lista1)
    | x::xs, y::ys ->
      if x > y then porownianieRec xs ys (acc @ [Pierwsza])
      else porownianieRec xs ys (acc @ [Druga])
  porownianieRec lista1 lista2 []

let listaLiczb3 = [1; 2; 3]
let listaLiczb4 = [2; 1; 3; 4]
let porownanieListWynik = porownanieList listaLiczb3 listaLiczb4
printfn "Zadanie 3.15: Lista pierwsza: %A, Lista Druga: %A, Wynik Porownania: %A" listaLiczb3 listaLiczb4 porownanieListWynik

