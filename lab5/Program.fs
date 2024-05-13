open System


//Zad 5.1
type RozwiazanieRownaniaKwadratowego =
    | BrakRozwiazan
    | JednoRozwiazanie of float
    | DwaRozwiazania of float * float
    | RowanieLiniowe

let rowanieKwadratowe (a: float, b: float, c: float) =
    if a = 0.0 then
        RowanieLiniowe
    else
        let delta = b ** 2.0 - 4.0 * a * c

        if delta < 0.0 then
            BrakRozwiazan
        elif delta = 0.0 then
            JednoRozwiazanie(-b / (2.0 * a))
        else
            DwaRozwiazania(((-b - (sqrt delta)) / (2.0 * a)), ((-b + (sqrt delta)) / (2.0 * a)))

type LiczbaRozwiazan =
    | Brak
    | Jedno
    | Dwa
    | Liniowe

let okreslLiczbeRozwiazan =
    function
    | DwaRozwiazania _ -> Dwa
    | JednoRozwiazanie _ -> Jedno
    | RowanieLiniowe -> Liniowe
    | BrakRozwiazan -> Brak

let wczytajWspolczynnik nazwaPliku =
    let linie = System.IO.File.ReadLines nazwaPliku
    let linie1 = Seq.map (fun (linia: string) -> List.ofArray (linia.Split(' '))) linie
    let wartosci = Seq.map (List.map float) linie1
    Seq.map (fun (wsp: float list) -> (wsp.[0], wsp.[1], wsp.[2])) wartosci

let Zadanie1 () =
    "zad8.txt"
    |> wczytajWspolczynnik
    |> Seq.map (fun wsp -> rowanieKwadratowe wsp)
    |> Seq.groupBy okreslLiczbeRozwiazan
    |> Seq.toList
    |> List.iter (fun (k, g) -> Console.WriteLine(sprintf "%A %A" k (Seq.length g)))

//Zad 5.2
(* let zamienNaPunkt (ls: string[]) = (float ls.[0], float ls.[1]) *)

(* let wczytajZad2 nazwaPliku = *)
(*     nazwaPliku *)
// |> IO.File.ReadLines
// |> Seq.map (fun (l: string) -> l.Split(' '))
// |> Seq.map zamienNaPunkt
// |> Seq.toList

// let wynikZad2 (punkty1: (float * float) list) (punkty2: (float * float) list) =
// let ilosc = punkty1.Length

// let rec funkcja i listaWynikowa =
// if i < ilosc / 2 then
// let (x1, y1) = punkty1.[i]
// let (x2, y2) = punkty2.[i]
// let odleglosc = Math.Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
// funkcja (i + 1) (((x1, y1), (x2, y2), odleglosc) :: listaWynikowa)
// else
// listaWynikowa

// funkcja 0 [] |> List.sortBy (fun ((x1, y1), (x2, y2), odleglosc) -> odleglosc)

// let rec ListToStringZad2 (lista: ((float * float) * (float * float) * float) list) =
// let rec funkcja tekst i =
// if i < lista.Length then
// funkcja (tekst + $"{lista.[i]}\n") (i + 1)
// else
// tekst

// funkcja "" 0

//Zad 5.3
let wczytajWartosc napis =
    Console.Write($"{napis}")
    Console.ReadLine()

let wczytajLiczbeCalkowita () =
    let jestLiczba, liczba = Int32.TryParse(wczytajWartosc "Podaj liczbe calkowita: ")
    if jestLiczba then Some(liczba * liczba) else None

let Potegowanie3 () =
    let liczba = wczytajLiczbeCalkowita ()

    match liczba with
    | Some a -> Console.WriteLine($"{a}")
    | _ -> Console.WriteLine("Nie podales wartosci, to ja nie podam wyniku.")

//Zad 5.4
let Potegowanie4 () =
    let liczba = wczytajLiczbeCalkowita ()

    if ((Option.Some(liczba.IsSome)) = Some true) then
        Console.WriteLine($"{liczba.Value}")
    else
        Console.WriteLine("Nie podales wartosci, to ja nie podam wyniku")

//Zad 5.5
let wczytajWartoscZad5 komunikat =
    printf "%s" komunikat
    let input = Console.ReadLine()
    if String.IsNullOrWhiteSpace(input) then "100" else input

let Zadanie5 () =
    match System.Int32.TryParse(wczytajWartoscZad5 "Podaj liczbe calkowita: ") with
    | true, int -> Some int
    | _ -> None

//Zad 5.6
let wczytajLiczbeCalkowitaObowizakowa () =
    let jestLiczba, liczba = Int32.TryParse(wczytajWartosc "Podaj liczbe calkowita: ")

    if jestLiczba then
        Ok(liczba * liczba)
    else
        Error("Nie zostala podana liczba calkowita")

let Zadanie6 () =
    let liczba = wczytajLiczbeCalkowitaObowizakowa ()

    match liczba with
    | Ok a -> Console.WriteLine($"{a}")
    | Error n -> Console.WriteLine($"{n}")

//Zad 5.7
let rec Potegowanie7 () =
    let liczba = wczytajLiczbeCalkowita ()

    if ((Option.Some(liczba.IsSome)) = Some true) then
        Console.WriteLine($"{liczba.Value}")
    else
        Console.Clear()
        Console.WriteLine("Nie podano wartosci, lub jest ona nieprawidlowa.\nProsze ponownie wprowadzic wartosc.\n")
        Potegowanie7()




[<EntryPoint>]
let main argv =
    //Zad 5.1
    Zadanie1()

    //Zad 5.2
    // let punkty1: (float * float) list = wczytajZad2 "zad9.txt"
    // let punkty2: (float * float) list = punkty1 |> List.rev

    //let stringWynikowy = wynikZad2 punkty1 punkty2 |> ListToStringZad2
    //IO.File.WriteAllText("rozwiazanie9.txt", stringWynikowy)

    //Zad 5.3
    Potegowanie3()

    //Zad 5.4
    Potegowanie4()

    //Zad 5.5
    match Zadanie5() with
    | Some int -> Console.WriteLine($"{int * int}")
    | None -> Console.WriteLine("Podano nieprawidlowa wartosc.")

    //Zad 5.6
    Zadanie6()

    //Zad 5.7
    Potegowanie7()

    0
