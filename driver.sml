(*  COMP 212 Homework 3:  Recursive function definitions challenge problems.
*   
*   Driver for mountain functions.
*
*   TODO:  Handle OS.FileSys errors usefully.
*   
*   N. Danner
*)


structure Driver =
struct

  exception UsageError

  val usage = 
    String.concatWith "\n" [
      "Usage:  ./driver mountain width left right out_file",
      "          Visualize mountain range using mountain",
      "        ./driver surface width height ll lr ul ur out_file",
      "          Visualize surface using surface.",
      "\tNote:  width must be 1 or a multiple of 4."
    ]

  fun printnl (s : string) : unit =
    print (s ^ "\n")

  fun getMountainArgs (args : string list) : int*real*real*string =
  let
    val [widthStr, leftStr, rightStr, outFile] = args

    val width : int = valOf(Int.fromString widthStr)

    val left : real = valOf(Real.fromString leftStr)

    val right : real = valOf(Real.fromString rightStr)
  in
    if width <> 4*(width div 4)
    then raise UsageError
    else (width, left, right, outFile)
  end
  handle _ => raise UsageError

  fun getSurfaceArgs (args : string list) : int*int*real*real*real*real*string =
  let
    val [widthStr, heightStr, llStr, lrStr, ulStr, urStr, outFile] = args

    val width : int = valOf(Int.fromString widthStr)
    val height : int = valOf(Int.fromString heightStr)

    val [ll, lr, ul, ur] =
      map (valOf o Real.fromString) [llStr, lrStr, ulStr, urStr]
  in
    if width > 1 andalso width <> 4*(width div 4)
    then raise UsageError
    else (width, height, ll, lr, ul, ur, outFile)
  end
  handle _ => raise UsageError


  fun main(arg0 : string, args : string list) : OS.Process.status =
  let

    val cmd :: args = args

    val runFn =
      case cmd of
           "mountain" => (fn () => Ch1P.runMountain (getMountainArgs args))
         | "surface" => (fn () => Ch1P.runSurface (getSurfaceArgs args))
         | _ => raise UsageError

    (*
    val _ = 
      print (
        String.concatWith "\n" [
          "Command: " ^ cmd,
          "width = " ^ Int.toString width,
          "left = " ^ Real.toString left,
          "right = " ^ Real.toString right,
          "outFile = " ^ outFile,
          "\n"
        ]
      )
    *)

  in
    BackTrace.monitor( fn () => (runFn () ; OS.Process.success))
    handle e => (
      List.app printnl (SMLofNJ.exnHistory e) ;
      OS.Process.failure
    )
  end
  handle 
      _ =>
      let
        val () = print (usage ^ "\n")
      in
        OS.Process.failure
      end

end
