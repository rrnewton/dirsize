
(* Ryan Newton this can make histograms and simple graphs *)
open List;;
open Printf;;
open Graphics;;
(*open Gsl_randist;;*)
open Rutils;;

(*Gsl_rng.env_setup();;*)

(*let rangen = Gsl_rng.make Gsl_rng.TAUS;;
let init_rangen time = Gsl_rng.set rangen time;;*)

let inf = 1./.0.
let oldxmin = ref inf
let oldxmax = ref (-. inf)
let oldymin = ref inf
let oldymax = ref (-. inf)

(** This takes a stream, and widens up the oldxmin/etc variables, but
  draws nothing.  *)
let rec widen_bounds ls =   
  oldymax := max !oldymax (fold_left (fun s (_,n)-> max n s) 0. ls);
  oldymin := min !oldymin (fold_left (fun s (_,n)-> min n s) (1./.0.) ls)


(****************************************************************************)

let rec draw_graph 
  ls  ~xmin ~xmax  ?(fit=false)
  ?(ymax = 10.) ?(ymin = (-. 5.)) 
  ?(wait=true) ?(openwin=true) ?(closewin=true)
  ?(bgcolor=black) ?(color=red) ?(width=2) ?(title="") () =
  
  (*  printf "DRAWING GRAPH   wait:%b   openwin:%b \n" wait openwin;
      flush stdout;*)
  if fit then widen_bounds ls;
  let ymax =  if fit then !oldymax else ymax 
  and ymin =  if fit then !oldymin else ymin in

   (* printf "oldymin %f  ymin %f  oldymax %f  ymax %f \n"
      !oldymin ymin !oldymax ymax;  flush stdout;*)

  let to_screen (x,y) =
    (int_of_float ((x-.xmin ) /. (xmax-.xmin) *. float_of_int (size_x())),
     int_of_float ((y-.ymin) /. (ymax-.ymin) *. float_of_int (size_y())))
  and from_screen (x,y) =
    ((float_of_int x /. float_of_int (size_x())) *. (xmax -. xmin) +. xmin,
     (float_of_int y /. float_of_int (size_y())) *. (ymax -. ymin) +. ymin)         
  in
    if openwin then 
      (open_graph "";
       if title="" 
       then set_window_title 
	 (sprintf "OCamlGraph  Xmin/max: %g/%g  Ymin/max: %g/%g"
	    xmin xmax ymin ymax)
       else set_window_title title;
       set_color bgcolor;    
       fill_rect 0 0 (size_x()) (size_y());
       
       let x1,y1 = to_screen (xmin,0.)
       and x2,y2 = to_screen (xmax,0.)
       and x3,y3 = to_screen (0.,ymin)
       and x4,y4 = to_screen (0.,ymax) in
	 set_line_width 0;
	 set_color (if bgcolor=black then white else black);
	 moveto x1 y1; lineto x2 y2;
	 moveto x3 y3; lineto x4 y4;);
    let startx,starty = to_screen (fst (hd ls),snd (hd ls)) in
      moveto startx starty;
    set_line_width width;
    set_color color;
    iter (fun (x,y) ->
	    let sx,sy = to_screen (x,y) in
	      lineto sx sy)
      (tl ls);
    if wait then 
      (while key_pressed() do ignore (read_key()) done;
       
       let len = List.length ls in
       let rec loop () = 
	 let status = wait_next_event [ Button_down; Key_pressed ] in
	   if status.keypressed 
	   then () 
	   else (moveto status.mouse_x status.mouse_y;
		 set_color green;
		 let x,y = from_screen (status.mouse_x,status.mouse_y) in
		   printf "Position (%d) %f,%f\n" 
		     (int_of_float 
			((x -. xmin)/.(xmax-.xmin) 
			 *. (float_of_int len)))
		     x y;
		   flush stdout;
		   draw_string (sprintf "%4.1f,%4.1f" x y);
		   loop ())
       in loop ();

);
    if closewin
    then (close_graph ();
	  oldxmin := inf;
	  oldxmax := -. inf;
	  oldymin := inf;
	  oldymax := -. inf;)
(*    else (oldxmin := xmin;
	  oldxmax := xmax;
	  oldymin := ymin;
	  oldymax := ymax;)*)
	    


(*(** This holds a window open until a key is pressed, it redraws
   the window every second  *)
  let hold_open drawfun =
  let status = wait_next_event [ Button_down; Button_up ] in
  if not (key_pressed ()) 
  then drawfun ();;*)
  
(****************************************************************************)

(** This takes a stream of numbers, and graphs them out at an interval..*)
let graph_stream f ?(delta=0.1) ?(xmin=0.) ?(xmax=10.) =
  let i = ref xmin 
  and points = ref [] in
    while !i < xmax +. delta do
      points := (!i,f ())::!points;
      i := !i +. delta;
    done; 
    let ls = rev !points in
      draw_graph ls ~xmin:xmin ~xmax:xmax;;

let stream_array a = 
  (let i = ref 0 
   and v = ref 0. in
     fun () ->
       if !i < Array.length a
       then v := a.(!i);
       incr i;
       !v)

let stream_fun f delta = 
  (let i = ref 0. in
     fun () ->
       let ret = f !i in
	 i := !i +. delta;
	 ret)

(** This is a front end that takes a float->float function and
  makes it into a unit->float stream *)
let graph f ?(delta=0.1) = graph_stream (stream_fun f delta) ~delta:delta

(****************************************************************************)

(** This takes a list of streams *) 
let multigraph_stream funs ?(delta=0.1) ?(fit=false)
  ?(xmin=0.) ?(xmax=10.) ?(ymax = 10.) ?(ymin = (-. 5.)) 
  ?(wait=true) ?(openwin=true) ?(closewin=true)
  ?(bgcolor=black) 
  ?(colors=RList.make (List.length funs) red)
  ?(widths=RList.make (List.length funs) 2) 
  ?(title="") () =
  let funs = Array.of_list funs 
  and colors = Array.of_list colors 
  and widths = Array.of_list widths in
  let last = Array.length funs - 1 in

  let points = int_of_float ((xmax -. xmin) /. delta) in
  let lists = Array.map 
		(fun f -> List.rev (RList.init points (fun _ -> f())))
		funs in
  let arrays = Array.map Array.of_list lists in
    if fit 
    then Array.iter 
      (fun ls -> widen_bounds (List.combine ls ls))
      lists;

    let rec loop () =    
      let funs = Array.map stream_array arrays in    			   
	Array.iteri (fun i f->
		       let o = i=0 && openwin in
			 (*and c = i=last && closewin*)
			 (*and w = i=last && wait in*)
			 graph_stream f ~delta:delta ~fit:fit
			   ~xmin:xmin ~xmax:xmax ~ymin:ymin ~ymax:ymax
			   ~wait:false ~openwin:o ~closewin:false
			   ~color:colors.(i)
			   ~width:widths.(i) 
			   ~title:title ())
	  funs;	
	if wait then 
	  (while key_pressed() do ignore (read_key()) done;
	   let status = wait_next_event [ Key_pressed ] in
	     if status.key = 'r' 
	     then loop ());
	if closewin then close_graph ()
    in loop ();;


(*********************************************************************)

let histogram ?(bars=20) arr = 
  open_graph "";
  set_color black;
  fill_rect 0 0 (size_x()) (size_y());
  let temp = Array.copy arr in 
    Array.fast_sort compare temp;
    let low = temp.(0)
    and hi  = temp.(Array.length temp - 1) in 
      set_window_title (sprintf "%f to %f" hi low);
      printf "Hi: %f   Low: %f   Mean: %f \n" 
	hi low 
	((Array.fold_left (+.) 0.0 temp) 
	 /. (float_of_int (Array.length temp)));
      flush stdout;
    let range = hi -. low in
    let stepsize = range /. (float_of_int bars) in
    let marker = ref (low +. stepsize) 
    and ind = ref 0 
    and current = Array.make bars 0 in 
    let draw = 
      let xsize = float_of_int (size_x())
      and ysize = float_of_int (size_y()) in
      let barstep = xsize /. (float_of_int bars) in
	function (bar,heightstep) ->
	  let left = barstep *. (float_of_int bar) in 
	  let height = heightstep *. (float_of_int current.(bar)) in
	    set_color (rgb 80 150 80);
	    fill_rect 
	      (int_of_float left) 0
	       (int_of_float barstep) (int_of_float height);
	    set_line_width 1;
	    set_color red;
	    draw_rect 
	      (int_of_float left) 0
	      (int_of_float barstep) (int_of_float height);
      in      
      for i=0 to bars-1 do
	while (!ind <> Array.length temp &&
	       temp.(!ind) < !marker) do
	  current.(i) <- current.(i) + 1;
	  incr ind;
	done;
	marker := !marker +. stepsize;
      done;
      
      let temp = Array.copy current in
	Array.fast_sort compare temp;
	let max = float_of_int (temp.(Array.length temp - 1)) in
	let heightstep =
	  ((float_of_int (size_y())) *. 0.75) /. max
	in
	  for i=0 to bars-1 do
	    draw (i,heightstep);
	  done;
	  ignore (read_key()) ;
          close_graph();;


(*let a : float array = Array.init 10_000 (fun _-> gaussian rangen 1.0);;
histogram a;;*)

(*histogram ~bars:600 a;;*)

(*graph_stream
  (let acc = ref 0. in
     fun () -> acc:=Random.float 10.0; !acc)
  ();;*)


