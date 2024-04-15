(*  COMP 212 Challenge 1 (Programming):  Recursive function definitions.
*
*   Jeremy Zay
*)

(*  ml-build driver.cm Driver.main
*   sml @SMLload=driver mountain 1000 0.0 250.0 mountain.bmp
* ` sml @SMLload=driver surface 400 600 10.0 20.0 30.0 40.0 surface.bmp
*   sml @SMLload=driver surface 64 1 10.0 20.0 30.0 40.0 surface.bmp
*)
structure Ch1P =
struct

  (*  Needed for file creation.
  *)
  structure FS = OS.FileSys

  (*  A structure for 2-dimensional real arrays.
  *
  *  Think of a 2-dimensional array as a grid of numbers, each indexed by
  *  its (column, row) position.  We write a[i][j] for the value at column i,
  *  row j.  It might help to think of "column" and "row" as "horizontal
  *  position" and "vertical position."
  *
  *  DO NOT CHANGE ANY PART OF THIS STRUCTURE!
  *)
  structure RealArray2 =
  struct
    (* The type of a 2-dimensional real array.
    *)
    type array = RealArray.array array

    (*  array(m, n, x) = a, where a has `m` columns and `n` rows and
    *  a[i][j] = `x` for all i and j.
    *)
    fun array(m : int, n : int, x : real) : array =
      Array.tabulate(m, fn _ => RealArray.array(n, x))

    (* sub(a, i, j) = a[i][j].
    *
    * Pre-condition:  0 <= `i`, `j`; a has at least `i` columns and `j` rows.
    *)
    fun sub(a : array, i : int, j : int) : real =
      RealArray.sub(Array.sub(a, i), j)

    (* update(a, i, j, x):  change a[i][j] to `x`, leaving other values
    * unchanged.
    *)
    fun update(a : array, i : int, j : int, x : real) : unit =
      RealArray.update(Array.sub(a, i), j, x)

  end


  (*  mountainRec(heights, left, right, rng) = ()
  *
  *   Pre-condition:  sub(heights, left), sub(heights, right) >= 0.0.
  *   Post-condition:  for left <= i <= right, sub(heights, i) >= 0.0 and is a
  *   randomly-chosen value so that 
  *     sub(heights, left), sub(heights, left+1),..., sub(heights, right) 
  *   forms a reasonable mountainscape.
  *
  *   YOU MUST IMPLEMENT THIS FUNCTION.
  *)
  fun mountainRec(
      heights : RealArray.array, 
      left : int, 
      right : int, 
      rng : Random.rand) : unit =
    let 
      val midpoint : int = (left + right) div 2 
      

      val left_height : real = RealArray.sub(heights, left)
      val right_height : real = RealArray.sub(heights, right)

      
      val ave_elevation : real = (left_height + right_height) / 2.0 
      val distance: real = real(right) - real(left)
      val rand_real: real = Random.randReal(rng) * 2.0*distance - distance (*random real between -l, l*)

      val updated_elevation: real = ave_elevation + rand_real (*calculate realistic Δ elevation*)
      val () = RealArray.update(heights, midpoint, if updated_elevation >= 0.0 then updated_elevation else 0.1) (*update elevation to realistic elevation*)
      

      val () = if left < midpoint then mountainRec(heights, left, midpoint, rng) else () (*recursive call to calculate between left and midpoint. ends when left = midpoint*)

      val () = if midpoint < right-1 then mountainRec(heights, midpoint, right, rng) else () (*recursive call to calculate between midpoint and right. end when midpoint = right-1*)

    in
      ()
    end;

  (* mountain(width, left, right) = a size-`width` array a of randomly-chosen
  * values that represent heights such that a[0] = left, a[width-1] = right,
  * and the values form a reasonable mountainscape.
  *
  * DO NOT CHANGE ANY PART OF THIS IMPLEMENTATION.
  *)
  fun mountain(
      width : int,
      left : real,
      right : real
    ) : RealArray.array =
  let
    (* Create an array of length `width` with all values set to 0.0 except
    * that `heights[0]` = `left` and `heights[width-1]` = `right`.
    *)
    val heights = RealArray.array(width, 0.0)
    val () = RealArray.update(heights, 0, left)
    val () = RealArray.update(heights, width-1, right)

    (*  Create a random number generator seeded by the current time.
    *)
    val seed = 
      LargeInt.toInt(
        Time.toSeconds(Time.now()) mod (LargeInt.fromInt(valOf(Int.maxInt)))
      )
    val rng : Random.rand = Random.rand(seed, 0)

    val () = mountainRec(heights, 0, width-1, rng)
  in
    heights
  end
  
  

  (*  updateHeight(heights, x, y, new_height) = ()
  *  
  *   Pre-conditions: (x, y) a valid index in heights
  *                   sub(heights, x, y) ≥ 0.0.
  *
  *   Post-condition: sub(heights, x, y) = new_height, UNLESS:  if sub(heights, x, y) ≠ 0.0, then leave unchanged
                                                                if new_height ≤ 0.0, then sub(heights, x, y) = 0.1
  *)
  fun updateHeight(
      heights : RealArray2.array, 
      x: int, (*initial x_coord of bitmap*)
      y: int, (*initial y_coord*)
      new_height: real): unit =
    let 
      val () =  if RealArray2.sub(heights, x, y) <= 0.0  (*check if height has been updated already, if not, update it*)
                (*if height has already been updated, don't update it. if height = 0.0, has not yet been updated, therefore set  heights ≤ 0.0 to low val > 0.0*)
                then RealArray2.update(heights, x, y, if new_height >= 0.0 then new_height else 0.1) else () 
    in ()
    end;
  
  (*  calculateNewSideHeight(heights, xi, yi, xf, yf, rng) = new_height, where new_height = ave_height + rand_real,
  *                   where ave_height = (sub(heights, xi, yi) + ub(heights, xf, yf)) / 2.0, 
  *                   and rand_real = random real in range (-distance between (xi,yi) and (xf, yf), distance between (xi,yi) and (xf, yf))
  *  
  *   Calculates height of midpoint of side of rectangle, given initial point and final point.
  *
  *   Pre-conditions: (xi, yi), (xf, yf) valid indices in heights
  *                   xf=xi or yf=yi, i.e. points must be parallel
  *                   (xi,yi) ≠ (xf,yf), i.e. points must not be the same point                   
  *)
  fun calculateNewSideHeight(
    heights: RealArray2.array,
    xi: int, 
    yi: int,
    xf: int,
    yf: int,
    rng: Random.rand) : real =
    let
      (*get initial heights of (xi, yi), (xf, yf)*)
      val xi_yi_height: real = RealArray2.sub(heights, xi, yi)
      val xf_yf_height: real = RealArray2.sub(heights, xf, yf)

      val ave_height: real = (xi_yi_height + xf_yf_height) / 2.0

      (*calculate difference between the pair of x-coords or y-coords that are not equal*)
      val distance: real = if xf = xi then real(yf-yi) else real(xf-xi)
      val rand_real: real = Random.randReal(rng) * 2.0 * distance - distance (*random real between (-distance, distance)*)

      val new_height: real = ave_height + rand_real
    in 
      new_height
    end;

  (*  calculateNewMidpointHeight(heights, xi, yi, xf, yf, rng) = midpoint_new_height, where midpoint_new_height = midpoint_ave_height + midpoint_rand_real,
  *                   where midpoint_ave_height = (ub(heights, xi, yi) + ub(heights, xf, yi) + ub(heights, xi, yf) + ub(heights, xf, yf)) / 4.0, 
  *                   and midpoint_rand_real = random real in range (-distance between (xi,yi) and (xf, yf), distance between (xi,yi) and (xf, yf))
  *  
  *   Calculates height of midpoint of rectangle, given initial point and final point.
  *
  *   Pre-conditions: (xi, yi), (xf, yf) valid indices in heights
  *                   xf-xi>1 and yf-yi>1 i.e. side lengths must be > 1                   
  *)
  fun calculateNewMidpointHeight(
    heights: RealArray2.array,
    xi: int,
    yi: int,
    xf: int,
    yf: int,
    rng: Random.rand) : real =
    let 
      (*get initial heights of 4 corners of rect at points (xi, yi), (xf, yf), (xf, yi), (xi, yf)*)
      val xi_yi_height: real = RealArray2.sub(heights, xi, yi)
      val xf_yi_height: real = RealArray2.sub(heights, xf, yi)
      val xi_yf_height: real = RealArray2.sub(heights, xi, yf)
      val xf_yf_height: real = RealArray2.sub(heights, xf, yf)

      val midpoint_ave_height: real = (xi_yi_height + xf_yi_height + xi_yf_height + xf_yf_height) / 4.0
      
      val x_distance: real = real(xf - xi)
      val y_distance: real = real(yf - yi)
      (*distance to midpoint*)
      val midpoint_distance: real = Math.sqrt(x_distance*x_distance + y_distance*y_distance)
      
      
      (*random offset of height based on distance to midpoint*)
      val midpoint_rand_real: real = Random.randReal(rng) * 2.0*midpoint_distance - midpoint_distance (*random real between -midpoint_distance, midpoint_distance (opposite corners of rect)*)
      
      (*calculate midpoint new height as average of corner heights plus random*)
      val midpoint_new_height: real = midpoint_ave_height + midpoint_rand_real
    in 
      midpoint_new_height
    end;
      

  
  (*  surfaceRec(heights, xi, yi, xf, yf, rng) = ()
  *
  *   Pre-condition:  sub(heights, xi, yi), sub(heights, xf, yf) ≥ 0.0.
  *   
  *   Post-condition:  for xi ≤ i ≤ xf, for yi ≤ j ≤ yf, sub(heights, i, j) > 0.0 and is a
  *   randomly-chosen value so that 
  *     sub(heights, xi, yi), sub(heights, xi+1, yi), sub(heights, xi, yi+1), sub(heights, xi+1, yi+1),..., sub(heights, xf, yf) 
  *   forms a reasonable mountainscape.
  *)
  fun surfaceRec(
      heights : RealArray2.array, 
      xi: int, (*initial x_coord of bitmap*)
      yi: int, (*initial y_coord*)
      xf: int, (*final x_coord*)
      yf: int, (*final y_coord*)
      (*runs: int,*)
      rng : Random.rand) : unit =
    let 

      (*calculate midpoint coords*)
      val x_midpoint : int = (xf - xi) div 2 + xi
      val y_midpoint : int = (yf - yi) div 2 + yi
      
      (*new height between lower left (xi, yi) and lower right (xf, yi)*)
      val ll_lr_new_height: real = calculateNewSideHeight(heights, xi, yi, xf, yi, rng)
      (*new height between upper left (xi, yf) and upper right (xf, yf)*)
      val ul_ur_new_height: real = calculateNewSideHeight(heights, xi, yf, xf, yf, rng)
      (*new height between lower left (xi, yi) and upper left (xi, yf)*)
      val ll_ul_new_height: real = calculateNewSideHeight(heights, xi, yi, xi, yf, rng)
      (*new height between lower right (xf, yi) and upper right (xf, yf)*)
      val lr_ur_new_height: real = calculateNewSideHeight(heights, xf, yi, xf, yf, rng)

      (*new height of midpoint (center) of rect*)
      val midpoint_new_height: real = calculateNewMidpointHeight(heights, xi, yi, xf, yf, rng)


      val () = 
        (*if runs > 0 then *)
        if xf-xi > 1 andalso yf-yi > 1 (*x-side > 1, y-side > 1, not a special case with side = 1 or side = 0, able to calculate all side midpoints and midpoint of rect*)
        then (*divide rectangle into 4 rectangles, each with corners as one original corner, the midpoint, and 2 side midpoints *)
          let
            (*update heights: midpoint height, x/y-side midpoint heights*)
            val () = updateHeight(heights, x_midpoint, y_midpoint, midpoint_new_height)
            val () = updateHeight(heights, x_midpoint, yi, ll_lr_new_height)
            val () = updateHeight(heights, xi, y_midpoint, ll_ul_new_height)
            val () = updateHeight(heights, xf, y_midpoint, lr_ur_new_height)
            val () = updateHeight(heights, x_midpoint, yf, ul_ur_new_height)
            (*make recursive calls*)
            val () = surfaceRec(heights, xi, yi, x_midpoint, y_midpoint,(*runs-1,*) rng)
            val () = surfaceRec(heights, x_midpoint, y_midpoint, xf, yf,(*runs-1,*) rng)
            val () = surfaceRec(heights, x_midpoint, yi, xf, y_midpoint,(*runs-1,*) rng)
            val () = surfaceRec(heights, xi, y_midpoint, x_midpoint, yf,(*runs-1,*) rng)
          in ()
          end
        else 
          if yf-yi > 1 (*andalso xf-xi = 1*) (*special case with x-side = 1, y-side > 1, calculate y-side midpoints*)
          then (*divide into 2 rectangles by splitting y-sides in half*)
            let 
              (*update heights: y-side midpoint heights*) 
              val () = updateHeight(heights, xi, y_midpoint, ll_ul_new_height)      
              val () = updateHeight(heights, xf, y_midpoint, lr_ur_new_height)       
              (*make recursive calls*)
              val () = surfaceRec(heights, xi, yi, xf, y_midpoint,(*runs-1,*) rng)
              val () = surfaceRec(heights, xi, y_midpoint, xf, yf,(*runs-1,*) rng)
            in ()
            end
          else 
            if xf-xi > 1(* andalso yf-yi = 1*) (*special case with x-side > 1, y-side = 1, calculate x-side midpoints*)
            then (*divide into 2 rectangles by splitting x-sides in half*)
              let 
                (*update heights: x-side midpoint heights*) 
                val () = updateHeight(heights, x_midpoint, yi, ll_lr_new_height)
                val () = updateHeight(heights, x_midpoint, yf, ul_ur_new_height)
                (*make recursive calls*)
                val () = surfaceRec(heights, xi, yi, x_midpoint, yi,(*runs-1,*) rng)
                val () = surfaceRec(heights, x_midpoint, yi, xf, yf,(*runs-1,*) rng)
              in ()
              end
            else () 
            (*else () runs=0*)

    in
      ()
    end;

  (* surface(w, h, ll, lr, ul, ur) = hs, where RealArray2.sub(hs, 0, 0) =
  * llheight, RealArray2.sub(hs, 0, w-1) = lrheight, etc, and the remaining
  * values for a reasonable surface, where RealArray2.sub(hs, i, j)
  * represents the elevation at x-coordinate i and y-coordinate j.
  *
  * YOU MUST IMPLEMENT THIS FUNCITON, ALONG WITH ANY AUXILIARY FUNCTIONS
  * THAT IT DEPENDS ON.
  *)
  fun surface(
      width : int,
      height : int,
      llheight : real,
      lrheight : real,
      ulheight : real,
      urheight : real
    ) : RealArray2.array =
  let
    (* Create a 2-dimensional array of dimensions `width` and 'height' with all values set to 0.0*)
    val heights = RealArray2.array(width, height, 0.0)
    (*set initial heights of 4 corners*)
    val () = RealArray2.update(heights, 0, 0, llheight) (*(0, 0) = llheight*)
    val () = RealArray2.update(heights, width-1, 0, lrheight) (*(width-1, 0) = lrheight*)
    val () = RealArray2.update(heights, 0, height-1, ulheight) (*(0, height-1) = ulheight*)
    val () = RealArray2.update(heights, width-1, height-1, urheight) (*(width-1, height-1) = urheight*)
    

    (*  Create a random number generator seeded by the current time.
    *)
    val seed = 
      LargeInt.toInt(
        Time.toSeconds(Time.now()) mod (LargeInt.fromInt(valOf(Int.maxInt)))
      )
    val rng : Random.rand = Random.rand(seed, 0)

    val () = surfaceRec(heights, 0, 0, width-1, height-1 , (*1,runs*) rng)
  in
    heights
  end

  (* ****************************************
  * DO NOT CHANGE ANY CODE BELOW THIS LINE!
  * *****************************************
  *)

  (* fillForward width heights (i, j) = `true` if `i` < `width` - 1 and
  * pixel (i, j) should be colored black when drawing a line from 
  * (i, heights[i]) to (i+1, heights[i+1]), `false` otherwise.
  *)
  fun fillForward 
      (width : int) (heights : RealArray.array) ((i, j) : int*int) : bool =
    i < width - 1 andalso
    let
      (*  Calculate parameters of the line segment from i to i+1.
      *)
      val h = RealArray.sub(heights, i)
      val hnext = RealArray.sub(heights, i+1)
      val m = hnext - h
    in
      (
      (*  Slope is non-negative and... 
      *)
      m >= 0.0 andalso j >= i andalso
        (*  Line segment crosses bottom of box.
        *)
        Real.abs((real(j) - 0.5 - h)/m + 0.5) <= 1.0
        orelse
        (*  Line segment cross left of box.
        *)
        Real.abs((m*(~0.5) + h) - (real(j)-0.5)) <= 1.0
      ) orelse
      (
      (*  Slope is negative and...
      *)
      m < 0.0 andalso j <= i andalso
        (*  Line segment crosses left of box.
        *)
        Real.abs((m*(~0.5) + h) - (real(j)-0.5)) <= 1.0
        orelse
        (*  Line segment cross top of box.
        *)
        Real.abs((real(j) + 0.5 - h)/m + 0.5) <= 1.0
      )
    end

  (* fillBackward heights (i, j) = `true` if `i` > 0 and pixel (i, j) should
  * be colored black when drawing a line from (i-1, heights[i-1]) to 
  * (i, heights[i]), `false` otherwise.
  *)
  fun fillBackward heights (i, j) =
    i > 0 andalso
    let
      val h = RealArray.sub(heights, i)
      val hprev = RealArray.sub(heights, i-1)
      val m = h - hprev
    in
      (
      (*  Slope is non-negative and... 
      *)
      m >= 0.0 andalso j <= i andalso
        (*  Line segment crosses bottom of box.
        *)
        Real.abs((real(j) - 0.5 - h)/m + 0.5) <= 1.0
        orelse
        (*  Line segment cross left of box.
        *)
        Real.abs((m*(~0.5) + h) - (real(j)-0.5)) <= 1.0
      ) orelse
      (m < 0.0 andalso j >= i andalso
        (*  Line segment cross left of box.
        *)
        Real.abs((m*(~0.5) + h) - (real(j)-0.5)) <= 1.0
        orelse
        (*  Line segment cross top of box.
        *)
        Real.abs((real(j) + 0.5-h)/m + 0.5) <= 1.0
      )
    end

  (* runMountain(width, left, right, outFile):  create a bitmap file named
  * `outFile` that represents the mountainscape generated by
  * `mountain(width, left, right)`.
  *)
  fun runMountain(
      width : int,
      left : real,
      right : real,
      outFile : string) : unit =
  let
    (*  Remove out_file if it exists.  remove raises SysError if the argument
    *  does not exist, so we just ignore the exception.  That's a bit dangerous,
    *  because it also raises SysError if the argument exists and is not
    *  writable.
    *)
    val () = FS.remove outFile handle SysError => ()

    (*  Create the surface.
    *)
    val heights : RealArray.array = mountain(width, left, right)

    (* Get the maximum height.
    *)
    val maxHeight =
      RealArray.foldl 
        Real.max
        0.0
        heights

  in
    (* If height is 1, plot the heights in a "graph" where the value at
    * x-coordinate i is heights[i][0].
    *
    * If width is 1, plot the heights in a "graph" where the value at 
    * x-coordinate i is heights[0][i].
    *)
    Bitmap.writeBitmapBW(
      width,
      Real.ceil(maxHeight),
      fn (i, j) => 
        fillForward width heights (i, j) orelse fillBackward heights (i, j),
      outFile
    )
  end

  (*  runSurface(width, height, llheight, lrheight, ulheight, urheight,
  *  outFile):  create a bitmap file named `outFile` that represents the
  *  surface generated by `surface(width, height, llheight, lrheight,
  *  ulheight, urheight)`.  But as a special case, if `width` or `height` is
  *  1, generate a bitmap in the same style as `runMountain`.
  *)
  fun runSurface(
      width : int,
      height : int,
      llheight : real,
      lrheight : real,
      ulheight : real,
      urheight : real,
      outFile : string) : unit =
  let

    (*  Remove out_file if it exists.  remove raises SysError if the argument
    *  does not exist, so we just ignore the exception.  That's a bit dangerous,
    *  because it also raises SysError if the argument exists and is not
    *  writable.
    *)
    val () = FS.remove outFile handle SysError => ()

    (*  Create the surface.
    *)
    val heights = 
      surface(width, height, llheight, lrheight, ulheight, urheight)

    (* Get the maximum height.
    *)
    val maxHeight =
      Array.foldl 
        (fn (a, m) => 
          Real.max((RealArray.foldl (fn (x, m) => Real.max(x, m)) 0.0 a), m)
        )
        0.0
        heights

  in
    (* If width and height are > 1, do a greyscale surface plot, where black
    * is minimum height and white is maximum height.
    *)
    if width > 1 andalso height > 1 then
      Bitmap.writeBitmap(
        width,
        height,
        fn (i, j) => round(255.0*(RealArray2.sub(heights, i, j)/maxHeight)),
        outFile
      )
    else 
      (* If height is 1, plot the heights in a "graph" where the value at
      * x-coordinate i is heights[i][0].
      *
      * If width is 1, plot the heights in a "graph" where the value at 
      * x-coordinate i is heights[0][i].
      *)
      let
        val (bmWidth, hs) =
          if height = 1 then
            ( width
            , RealArray.tabulate(width, fn i => RealArray2.sub(heights, i, 0))
            )
          else
            ( height
            , RealArray.tabulate(height, fn i => RealArray2.sub(heights, 0, i))
            )
      in
        Bitmap.writeBitmapBW(
          bmWidth,
          Real.ceil(maxHeight),
          fn (i, j) => 
            fillForward width hs (i, j) orelse fillBackward hs (i, j),
          outFile
        )
      end
  end

end
