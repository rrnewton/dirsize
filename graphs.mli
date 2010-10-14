
val inf : float
val oldxmin : float ref
val oldxmax : float ref
val oldymin : float ref
val oldymax : float ref
val widen_bounds : ('a * float) list -> unit
val draw_graph :
  (float * float) list ->
  xmin:float ->
  xmax:float ->
  ?fit:bool ->
  ?ymax:float ->
  ?ymin:float ->
  ?wait:bool ->
  ?openwin:bool ->
  ?closewin:bool ->
  ?bgcolor:Graphics.color ->
  ?color:Graphics.color -> ?width:int -> ?title:string -> unit -> unit
val graph_stream :
  (unit -> float) ->
  ?delta:float ->
  ?xmin:float ->
  ?xmax:float ->
  ?fit:bool ->
  ?ymax:float ->
  ?ymin:float ->
  ?wait:bool ->
  ?openwin:bool ->
  ?closewin:bool ->
  ?bgcolor:Graphics.color ->
  ?color:Graphics.color -> ?width:int -> ?title:string -> unit -> unit
val stream_array : float array -> unit -> float
val stream_fun : (float -> 'a) -> float -> unit -> 'a
val graph :
  (float -> float) ->
  ?delta:float ->
  ?xmin:float ->
  ?xmax:float ->
  ?fit:bool ->
  ?ymax:float ->
  ?ymin:float ->
  ?wait:bool ->
  ?openwin:bool ->
  ?closewin:bool ->
  ?bgcolor:Graphics.color ->
  ?color:Graphics.color -> ?width:int -> ?title:string -> unit -> unit
val multigraph_stream :
  (unit -> float) list ->
  ?delta:float ->
  ?fit:bool ->
  ?xmin:float ->
  ?xmax:float ->
  ?ymax:float ->
  ?ymin:float ->
  ?wait:bool ->
  ?openwin:bool ->
  ?closewin:bool ->
  ?bgcolor:Graphics.color ->
  ?colors:Graphics.color list ->
  ?widths:int list -> ?title:string -> unit -> unit
val histogram : ?bars:int -> float array -> unit
