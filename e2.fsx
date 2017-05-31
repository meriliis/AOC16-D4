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
    | h :: t -> let words = getLetters h
                let id = getID h 
                let checksum = getChecksum h
                [words; id; checksum] :: splitToParts t

let replaceDash (letters : string) : string =
    letters.Replace("-", " ")

let abc = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]

let rec findLetter (current : string) (id : string) = 
    let noOfLetters = abc.Length
    let shift = Int32.Parse(id) % noOfLetters
    let currentIndex = List.findIndex (fun letter -> letter = current) abc

    if shift > (noOfLetters - currentIndex - 1) then 
        abc.[shift - (noOfLetters - currentIndex - 1) - 1]
    else 
        abc.[currentIndex + shift]

let rec replaceLetters (room : string list) =
    let words = room.[0]
    let id = room.[1]
    let rec helper (words : string) acc = 
        match words with
        | s -> let oldLetter = s.Substring(0, 1)
               let newLetter = 
                    if oldLetter = "-" then " "
                    else findLetter oldLetter id 
               if s.Length = 1 then 
                    acc + newLetter 
               else 
                    helper (s.Substring(1)) (acc + newLetter)

    let actualWords = helper words ""
    [actualWords; id; room.[2]]
            
let replaced = input 
               |> splitToParts 
               |> List.map replaceLetters

let northPoleObjects = replaced
                       |> List.filter (fun room -> room.[0].Contains("north"))