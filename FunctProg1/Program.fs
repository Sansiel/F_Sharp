// Дополнительные сведения о F# см. на http://fsharp.org
// Дополнительную справку см. в проекте "Учебник по F#".

//[<EntryPoint>]
//let main argv = 
    //printfn "%A" argv
    //0

// Лабораторная №1
let countCratnFour  =
        let listNum =  [1 .. 100]
        let accFunc (sum) num =
            if   num % 3 <> 0 && num % 5 = 0  then (sum + num)
            else   (sum)
        List.fold accFunc (0) listNum


// Лабораторная №2
let getwords (s:string) = s.Split(" ,:-.!?;()\t\r\n".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
let findMostFrequentWord (s:string) =
    getwords s
    |> Seq.countBy id
    |> Seq.maxBy snd
let text = System.IO.File.ReadAllText("d:/file.txt")
//findMostFrequentWord text |> printfn "%A"
//System.Console.ReadLine()

// Лабораторная №3
let mas3 = 
        let mutable arr = [| 1; 3; 4; 5; 6  |]
        for i=1 to arr.Length-1 do
            if arr.[i] % 2 <> 0 then arr.[i-1] <- 0
        arr

// Лабораторная №4
type IConsumable = 
        abstract goInDekanat : unit -> unit

    type Worker=
        val sex : bool //true - man, false - women
        val age : int
        new (s, a, p) = { sex = s; age = a }
        interface IConsumable with
            member this.goInDekanat () =
                printfn "Worker going in dekanat"
        member this.workInDekanat () =
            printfn "Work in dekanat"
        member this.getAge =
            this.age

    type Student =
        val sex : bool //true - man, false - women
        val age : int
        val exam : bool
        new (s, a, e) = { sex = s; age = a; exam = e }
        interface IConsumable with
            member this.goInDekanat () =
                printfn "Student going in dekanat"
        member this.getDiplom () =
            printfn "Get diplom in Dekanat"
        member this.getAge =
            this.age
    
    type Univer =
        val mutable adress : string
        val name : string
        
        new (a,n) = {adress = a; name = n}

        member this.setAdress(adr) =
            this.adress <- adr


    type IDisposable = interface
        abstract member Dispose : unit -> unit 
        end

    type Dekanat (adr,name,cult) =
        inherit Univer(adr,name)
        let facult = adr

        let w1 = new Worker(true, 45,"Dekan")
        let w2 = new Worker(false, 35,"Teacher")

        let c1 = new Student(true,30,true)
        let c2 = new Student(true,37,false)

        member this.setCult (cult) =
            facult = cult

        member this.serviceStudent = 
            w1.workInDekanat()
            c1.getDiplom()
            printfn "Student is happy!"

        interface IDisposable with
            member this.Dispose() =
                printfn "Cleaning up..."

    //let m = new Dekanat("Rosa","ULGTU","FIST")
    //m.serviceStudent


// Лабораторная №5
open System
open System.IO
let getMax path =
        let s = File.ReadAllText(path).Replace('\n',' ').Split [|' '|]
        let n = s |> Seq.countBy id
                  |> Seq.maxBy snd
        let str : string = string n;
        File.AppendAllText(@"D:\textFiles\finish.txt", path + " : " + str + "\r\n", System.Text.Encoding.UTF8)
        
    
//let files = Directory.GetFiles(@"D:\textFiles\files\", "*.txt")
//System.Threading.Tasks.Parallel.ForEach(files,  new Action<string>(getMax))


// Лабораторная №6
open System.Drawing
type Rule = char * char list
type Grammar = Rule list
let FindSubst c (gr:Grammar) = 
   match List.tryFind (fun (x,S) -> x=c) gr with
     | Some(x,S) -> S
     | None -> [c]
let Apply (gr:Grammar) L =
   List.collect (fun c -> FindSubst c gr) L
let rec NApply n gr L = 
   if n>0 then Apply gr (NApply (n-1) gr L)
   else L
let TurtleBitmapVisualizer n delta cmd =
    let W,H = 12000,12000
    let b = new Bitmap(W,H)
    let g = Graphics.FromImage(b)
    let pen = new Pen(Color.Black)
    let NewCoord (x:float) (y:float) phi =
       let nx = x+n*cos(phi)
       let ny = y+n*sin(phi)
       (nx,ny,phi)
    let ProcessCommand x y phi = function
     | 'f' -> NewCoord x y phi
     | '+' -> (x,y,phi+delta)
     | '-' -> (x,y,phi-delta)
     | 'F' -> 
         let (nx,ny,phi) = NewCoord x y phi
         g.DrawLine(pen,(float32)x,(float32)y,(float32)nx,(float32)ny)
         (nx,ny,phi)
     | _ -> (x,y,phi)     

    let rec draw x y phi = function
     | [] -> ()
     | h::t ->
         let (nx,ny,nphi) = ProcessCommand x y phi h
         draw nx ny nphi t
    draw (float(W)/2.0) (float(H)/2.0) 0. cmd
    b
let str (s:string) = s.ToCharArray() |> List.ofArray
let gr = [('F',str "F+F--F+F")]
let lsys = NApply 3 gr (str "+FF-FF-FF+")
let PI = 3.141592653589
let B = TurtleBitmapVisualizer 40.0 (PI/180.0*60.0) lsys 
B.Save(@"d:\bitmap.jpg")