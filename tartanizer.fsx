#r "System.Drawing.Common.dll"

open System

module Int =

    let tryParse(s : string) =
        match Int32.TryParse s with
        | true, i -> Ok i
        | false, _ -> Error "Invalid integer"

module Array =

    /// Applies a function returning a Result type to every element of an array.
    /// If all the applications return Result.Ok, returns an array of the results,
    /// itself wrapped in Result.Ok. If any application returns Result.Error, 
    /// returns the first such error.
    let traverseM<'T, 'R, 'E> (f : 'T -> Result<'R, 'E>) (a : 'T[]) : Result<'R[], 'E> =
        let result = Array.zeroCreate a.Length
        let mutable i = 0
        let mutable e = ValueNone
        while e.IsNone && i < a.Length do
            let x = a.[i]
            match f x with
            | Ok r -> 
                result.[i] <- r
                i <- i+1
            | Error err -> 
                e <- ValueSome err
        match e with
        | ValueNone ->
            Ok result
        | ValueSome err ->
            Error err

module Color =

    open System.Drawing
    open System.Text.RegularExpressions

    let hexRe = Regex(@"[\d|A-F]{6}", RegexOptions.IgnoreCase)

    /// Tries to parse a Color from a string, in the forms:
    /// - Single letter (e.g. R for Red)
    /// - A known .NET color (e.g. "PeachPuff")
    /// - A hex value (e.g. "FF00FF").
    let tryParse (s : string) : Result<Color, string> =
        // TODO color modifiers L and D for Light and Dark
        // TODO pick nicer colors
        // TODO add all rainbow colors
        if s = "R" then
            Ok Color.Red
        elif s = "G" then
            Ok Color.Green
        elif s = "B" then
            Ok Color.Blue
        elif s = "Y" then
            Ok Color.Yellow
        elif s = "K" then
            Ok Color.Black
        elif s = "W" then
            Ok Color.White
        elif s = "T" then
            Ok Color.Brown
        elif s = "N" then
            Ok Color.Gray
        else
            let color = Color.FromName(s)
            if color.IsKnownColor then
                Ok color
            elif hexRe.IsMatch(s) then
                let argb = Convert.ToInt32(s, 16)
                let color = Color.FromArgb(argb)
                Ok color
            else
                Error (sprintf "Unknown colour name: %s" s)

    /// Performs a naive color mix between two colors by taking weighted averages of the R G and B components.
    let blend (color1Proportion : float) (color1 : Color) (color2 : Color) =
        let mix (a : byte) (b : byte) =
            ((a |> float) * color1Proportion) + ((b |> float) * (1. - color1Proportion)) |> int
        let r = mix color1.R color2.R
        let g = mix color1.G color2.G
        let b = mix color1.B color2.B
        Color.FromArgb(r, g, b)

/// A solid band of color of a specified width.
type Band = { Color : System.Drawing.Color; Count : int }

module Band =

    let private digit = [|'0'..'9'|]

    /// Tries to parse the spec of one band of color, comprising a color spec and a count
    /// in the forms "#ff0000#32", "Red32" or "R32".
    let tryParse(s : string) =

        let invalid e = Error <| sprintf "Invalid band specification: %s" e

        let colorAndCount = 
            // "#ff0000#32"
            if s.StartsWith '#' then
                let parts = s.Split('#', StringSplitOptions.RemoveEmptyEntries)
                match parts with
                | [|colorString; countString|] ->
                    (colorString, countString) |> Ok
                | _ ->
                    invalid s
            // "Red32", "R32"
            else
                let firstDigit = s.IndexOfAny(digit)
                if firstDigit > 0 then
                    (s.Substring(0, firstDigit), s.Substring(firstDigit)) |> Ok
                else
                    invalid s
                    
        match colorAndCount with
        | Ok(colorString, countString) -> 
            let color = colorString |> Color.tryParse
            let count = countString |> Int.tryParse
            match color, count with
            | Ok col, Ok n ->
                { Color = col; Count = n } |> Ok
            | Error e, _ | _, Error e ->
               Error e
        | Error e ->
            Error e

    /// Expands a color band to a sequence of repetitions of the same color.
    let expand (band : Band) =
        Seq.replicate band.Count band.Color

/// A tartan "Sett" - a series of color bands.
type Sett = Band[]

type RepeatStyle = Symmetrical | Asymmetrical

module Sett =

    /// Tries to parse a sett from a string consisting of a series of space-separated
    /// band specifications, e.g "K4 R24 K24 Y4" or "#000000#4 #ff0000#24 #000000#4 #ffff00#4"
    let tryParse(s : string) : Result<Sett, _> =
        s.Split(' ')
        |> Array.traverseM Band.tryParse

    // Returns the total number of threads in the Sett.
    let width (sett : Sett) =
        sett
        |> Array.sumBy (fun band -> band.Count)

    /// Expands a Sett into a sequence of colors. For repeatStyle = Symmetrical the Sett
    /// is alternated between its expansion and its reverse.  Otherwise the Sett is simply repeated.
    let expand (repeatStyle : RepeatStyle) (sett : Sett) =
        let repeat =
            sett
            |> Seq.collect Band.expand
            |> Array.ofSeq
        match repeatStyle with
        | Symmetrical ->
            let reverse = repeat |> Array.rev
            Array.append repeat reverse
        | Asymmetrical ->
            repeat

module Weave =

    open System.Drawing

    type Component = Warp | Weft

    /// Determines which of the Warp or the Weft is at the top
    /// for a given position on the tartan.
    let top (x : int) (y : int) : Component =
        match ((x + y) / 2) % 2 with
        | 0 -> Warp
        | _ -> Weft

    /// Given an array of colors, determines which color is present at 
    /// the specified index position.  Indexes beyond the end of the 
    /// array are wrapped.
    let colorAt i (threads : Color[]) =
        threads.[i % threads.Length]

module Tartan =

    open System.Drawing

    /// Creates a 2D color array representing a tartan.
    let create (width : int) (height : int) (topColorWeight : float) (repeatStyle : RepeatStyle) (threadCount : Sett) =
        let colors = threadCount |> Sett.expand repeatStyle
        Array2D.init width height (fun x y ->
            let warpColor = colors |> Weave.colorAt x
            let weftColor = colors |> Weave.colorAt y
            match Weave.top x y with
            | Weave.Component.Warp ->
                if warpColor <> weftColor then
                    Color.blend topColorWeight warpColor weftColor
                // When the two colors are the same and the Warp is on top,
                // darken the results very slightly to give some texture to these
                // single-coloured areas:
                else
                    Color.blend topColorWeight warpColor weftColor
                    |> Color.blend 0.025 Color.Black
            | _ -> 
                if warpColor <> weftColor then
                    Color.blend topColorWeight weftColor warpColor
                // When the two colors are the same and the Warp is on top,
                // lighten the results very slightly to give some texture to these
                // single-coloured areas:
                else
                    Color.blend topColorWeight weftColor warpColor
                    |> Color.blend 0.025 Color.White)

    /// Converts a color array into a bitmap.
    let asBitMap (threadPixels : int) (colors : Color[,]) =
        let width = (colors.GetUpperBound(0)+1) * threadPixels
        let height = (colors.GetUpperBound(1)+1) * threadPixels
        let bitMap = new Bitmap(width, height)
        colors
        |> Array2D.iteri (fun x y c -> 
            for i in 0..threadPixels-1 do
                for j in 0..threadPixels-1 do
                    // Could use LockBits for performance but it's much more code.
                    bitMap.SetPixel((x*threadPixels)+i, (y*threadPixels)+j, c))
        bitMap

// Some examples.  These will save to files as soon as the module is sent to FSI.

let savePath = @"c:\temp\Tartans"

// Example from https://en.wikipedia.org/wiki/Tartan
module WikipediaExample =

    let threadCount = Sett.tryParse "K4 R24 K24 Y4"
    let pixelsPerThread = 4
    let bitMap =
        threadCount 
        |> Result.map (fun tc ->
            let size = (tc |> Sett.width) * pixelsPerThread
            Tartan.create size size 0.9 Symmetrical tc)
        |> Result.map (Tartan.asBitMap pixelsPerThread)
    match bitMap with
    | Ok b ->
        b.Save(System.IO.Path.Join(savePath,  "example.png"))
    | Error e ->
        printfn "%s" e

/// A tribute to Jackie Weaver.
module JackieWeaver = 

    let threadCount = Sett.tryParse "DarkSeaGreen32 DarkRed8 MistyRose32 LightGray16 CornSilk64 LightGray16 MistyRose32 DarkRed8 DarkSeaGreen32"
    let pixelsPerThread = 4
    let bitMap =
        threadCount 
        |> Result.map (fun tc ->
            let size = (tc |> Sett.width) * pixelsPerThread
            Tartan.create size size 0.9 Symmetrical tc)
        |> Result.map (Tartan.asBitMap pixelsPerThread)
    match bitMap with
    | Ok b ->
        b.Save(System.IO.Path.Join(savePath,  "JackieWeaver.png"))
    | Error e ->
        printfn "%s" e

/// One variation of the official Malcolm tartan - an asymmetrical tartan.
module MalcolmAncientHeavy = 

    let threadCount = Sett.tryParse "Black4 Gold4 Black4 AliceBlue4 Black4 MediumSeaGreen36 Black36 CornflowerBlue36 Tomato4 CornflowerBlue4 Tomato4 CornflowerBlue36 Black36 MediumSeaGreen36"
    let pixelsPerThread = 4
    let bitMap =
        threadCount 
        |> Result.map (fun tc ->
            let size = (tc |> Sett.width) * pixelsPerThread
            Tartan.create size size 0.9 Asymmetrical tc)
        |> Result.map (Tartan.asBitMap pixelsPerThread)
    match bitMap with
    | Ok b ->
        b.Save(System.IO.Path.Join(savePath,  "MalcolmAncientHeavy.png"))
    | Error e ->
        printfn "%s" e

/// An example showing that 'Hounds Tooth' is actually a special case of tartan.
module HoundsTooth = 

    let threadCount = Sett.tryParse "Moccasin4 SaddleBrown4"
    let pixelsPerThread = 16
    let bitMap =
        threadCount 
        |> Result.map (fun tc ->
            let size = (tc |> Sett.width) * pixelsPerThread
            Tartan.create size size 0.9 Asymmetrical tc)
        |> Result.map (Tartan.asBitMap pixelsPerThread)
    match bitMap with
    | Ok b ->
        b.Save(System.IO.Path.Join(savePath,  "HoundsTooth.png"))
    | Error e ->
        printfn "%s" e

// Call Random.generate() to generate and save a random tartan.

module Random =

    /// Generate a random symmetrical tartan.
    let generate() =
        let r = System.Random()
        let colorCount = r.Next(2, 7)
        let randomColorHex() =
            let rgb = r.Next(0, 256*256*256+1)
            rgb.ToString("x").PadLeft(6, '0')
        let randomBandWidth() = r.Next(4, 32)
        let counts =
            Array.init colorCount (fun _ -> sprintf "#%s#%i" (randomColorHex()) (randomBandWidth()))
        let threadCountString = String.Join(" ", counts)
        // TODO skip parser and allow more direct method of specifying colors.
        let threadCount = threadCountString |> Sett.tryParse
        let size = 512
        let bitMap =
            threadCount 
            |> Result.map (Tartan.create size size 0.9 Symmetrical)
            |> Result.map (Tartan.asBitMap 4)
        match bitMap with
        | Ok b ->
            let fileName = threadCountString.Replace(" ", "_")
            b.Save(sprintf @"c:\temp\RandomTartans\%s.png" fileName)
        | Error e ->
            printfn "%s" e