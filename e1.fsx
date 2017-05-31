open System
open System.Text.RegularExpressions

let location = "C:/Users/Meri/Documents/advent-of-code/D4"
let input = System.IO.File.ReadAllLines(location + "/input.txt") |> Array.toList

let getLetters (room : string) : string = 
    Regex.Match(room, "\D+").Value

let getID (room : string) : string =
    Regex.Match(room, "\d+").Value

let getChecksum (room : string) : string =
    Regex.Match(room, "\[(\S+)\]").Groups.[1].Value

let rec splitToParts (list : string list) : string list list = 
    match list with
    | []     -> [] 
    | h :: t -> [getLetters h; getID h; getChecksum h] :: splitToParts t

let getCharacters (letters : string) : string =
    letters.Replace("-", "")

let topFive (characters : string) = 
    let rec addToMap (characters : string) (map : Map<string, int>) = 
        match characters with
        | s     -> let letter = s.Substring(0, 1)   
                   if map.ContainsKey (letter) then 
                        let newMap = map.Add(letter, map.[letter] + 1) 
                        if s.Length = 1 then newMap else addToMap (s.Substring(1)) newMap
                   else 
                        let newMap = map.Add(letter, 1)
                        if s.Length = 1 then newMap else addToMap (s.Substring(1)) newMap

    let map = addToMap characters Map.empty<string, int>

    let sortFun (pair1 : (string * int)) (pair2 : (string * int))=
        if (snd pair1 > snd pair2) then
            -1
        else if (snd pair1 < snd pair2) then
            1
        else
            String.Compare(fst pair1, fst pair2)



    let rec takeLetters (list : (string * int) list) (acc : string) =
        match list with
        | []     -> acc
        | h :: t -> takeLetters t (acc + (fst h))
        
    let list = map 
               |> Map.toList 
               |> List.sortWith sortFun 
               |> List.take 5
    
    takeLetters list ""

let realRooms (rooms : string list list) = 
    rooms |> List.filter (fun room -> 
        let actualTop = room.[0] |> getCharacters |> topFive
        let givenTop = room.[2]
        actualTop = givenTop
        )

let rooms = input |> splitToParts

let realOnes = rooms |> realRooms

let rec sumOfIDs (realOnes : string list list) =
    match realOnes with
    | []     -> 0
    | h :: t -> Int32.Parse(h.[1]) + sumOfIDs t
