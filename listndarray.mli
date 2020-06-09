  
(** A [Multiarray] is either a One-Dimensional or Two-Dimensional array of data, 
    belonging to the same type. Items are stored and indexed in a multiarray *)
module type Multiarray = sig

  (** type t representing the 'a data*)
  type 'a t 

  (** [is_empty m] is [true] iff [m] is a multiarray containing no data*)
  val is_empty : 'a t  -> bool

  (** [len m] is the number of rows of multiarray [m].
       [num_rows m] is [0] if [is_empty m] is true  *)
  val len : 'a t -> int

  (** [shape m] is a tuple representing the shape of the multiarray [m].
       [shape m] is [([num_rows m], [num_cols m])] if [num_cols] in each row 
       of [m] is the same. [shape m] is [([num_rows m], [-1])] 
       if [num_cols] in each row of [m] is not the same. *)
  val shape : 'a t -> int * int

  (** [zeros m n] is a rectangular int multiarray of zeros of shape [(m, n)]. *)
  val zeros : int -> int -> int t 

  (** [ones m n] is a rectangular int multiarray of ones of shape [(m, n)]. *)
  val ones: int -> int -> int t

  (** [constant m n val] is a rectangular int multiarray of [vals]
       of shape [(m, n)]. *)
  val constant: int -> int -> int -> int t

  (** [create lst true] is a two-dimesnional multiarray representing the 
      elements of list [lst]. 
      [create lst false] is a one-dimensional multiarray representing the 
      elements of [List.flatten lst].
      [create []] |> is_empty is true.  *)   
  val create : 'a list list-> bool -> 'a t 

  (** [index m row col] is the data item represented at index [row]
      [col] in multiarray [m]. 
      If [m] is a one-dimensional multi-array then the expression placed
      in [col] is ignored and [index m row] is evaluated. 
      Requires: [m] is a multiarray, [row] and [col] are ints
      Raises: Failure if [row] or [col] is beyond the number of items
              stored in the  multiarray or if [m] is empty  *)  
  val index : 'a t -> int -> int -> 'a 

  (** [slice m (startrow, stoprow, steprow) (startcol, stopcol, stepcol)] is
      multiarray m sliced from row [startrow] to row [startcol] with a 
      [steprow] increment and from column [startcol] to column [stepcol] 
      with a [stepcol] increment. 
      If [m] is a one-dimensional multi-array then 
      [(startcol, stopcol, stepcol)] is simply ignored and 
      [slice m (startrow, stoprow, steprow)] is evaluated. 
      Requires: [m] is a multiarray
      Raises: Failure if [steprow] is 0. *)  
  val slice : 'a t -> int * int * int -> int * int * int -> 'a t

  (** [to_list_1D m] is a list representing the elements of multiarray
      [m]. If [m] is a two-dimensional multi-array then [to_list_1D m]
      is the flattened representation of [m]  *)   
  val to_list_1D : 'a t -> 'a list 

  (** [to_list_1D m] is a list of lists representing the elements of multiarray
      [m]. If [m] is a one-dimensional multi-array then [to_list_2D m]
      is simply [[to_list_1D m]]  *)    
  val to_list_2D : 'a t -> 'a list list

  (** [flatten m] is a flattened representation of multiarray [m]
      Examples:
      - [flatten {{1,2}} is {1,2}  *)      
  val flatten : 'a t -> 'a t

  (** [arrange n] is an int multiarray representing all the elements from 
      0 to [n] in ascending order.  *)  
  val arrange : int -> int t

  (** [max m] is the maximium value based on the Standard compare function
      of the multiarray [m]. *)   
  val max : 'a t -> 'a 

  (** [min m] is the minimum value based on the Standard compare function
      of the multiarray [m] *)
  val min : 'a t -> 'a

  (** [replace m row col elt] is a multiarray representing all the elements 
      of multiarray [m] except element at index [row][col] in [m], which is 
      replaced by value [elt]. 
      If [row] or [col] is out of the bounds of [m], then [m] is returned back.
      Requires: [row] and [col] should be integers.
  *)
  val replace : 'a t -> int -> int -> 'a -> 'a t

  (** [sum m] is the sum of all elements in the int multiarray [m.] *) 
  val sum : int t -> int

  (** [add m1 m2] is an int multiarray representing the element-wise 
      addition of int multiarrays [m1] and [m2]
      Raises: Failure if [shape m1] <> [shape m2]
              Failure if [m1] and [m2] are of different dimensions *)
  val add : int t -> int t  -> int t 

  (** [subtract m1 m2] is an int multiarray representing the element-wise 
      subtraction of int multiarrays [m1] and [m2]
      Raises: Failure if [shape m1] <> [shape m2]
              Failure if [m1] and [m2] are of different dimensions *)
  val subtract : int t -> int t -> int t 

  (** [dot m1 m2] is the dot product of the two one-dimensional int multiarrays 
      represented by [m1] and [m2]
       Raises: Failure if [shape m1] <> [shape m2]
               Failure if either [m1] or [m2] is a two-dimensional multiarray *)
  val dot : int t -> int t -> int

  (** [multiply m1 m2] is an int multiarray representing the element-wise 
      multiplication of int multiarrays [m1] and [m2]
      Raises: Failure if [shape m1] <> [shape m2] 
              Failure if [m1] and [m2] are of different dimensions*)
  val multiply : int t -> int t -> int t 

  (** [divide m1 m2] is an int multiarray representing the element-wise 
        division of int multiarrays [m1] and [m2]
        Raises: Failure if [shape m1] <> [shape m2]
                Failure if [m1] and [m2] are of different dimensions *)
  val divide: float t -> float t -> float t 

  (** [power m val] is a float multiarray representing the element-wise 
      raise of float multiarray [m] to the power of [val]. 
      Elements also include elements inside respective rows. *)
  val power : float t -> float -> float t 

  (** [square_root m val] is a float multiarray representing the element-wise
      square-root of float multiarray [m].
       Elements also include elements inside respective rows. *)
  val square_root : float t -> float t

  (** [logarithm m] is a float multiarray representing the element-wise
      natural logaritm of float multiarray [m].
      Elements also include elements inside respective rows. *)
  val logarithm : float t -> float t

  (** [exponentiate m] is a float multiarray representing the element-wise
      raise of float multiarray [m] as the power of the natural exponent.
      Elements also include elements inside respective rows.*)
  val exponentiate : float t -> float t

  (** [sort m] is the elements of multiarray [m] sorted according to the 
       standard compare function.
       Requires: [m] is a one-dimensional multiarray
       Raises: Failure if [m] is two-dimensional *)
  val sort : 'a t -> 'a t 

  (** [median m] is the median of the one-dimensional float multiarray
      represented by [m]
       Raises: Failure if [m] is two-dimensional *)
  val median : float t -> float

  (** [mean m] is the mean of the one-dimensional float multiarray
      represented by [m]
       Raises: Failure if [m] is two-dimensional *)
  val mean : float t -> float

  (** [norm m] is the normalized value of all the elements of the 
      one-dimensional float multiarray represented by [m]
      Raises: Failure if [m] is two-dimensional *)
  val norm : float t -> float

  (** [all_close m1 m2 limit] is a boolean representing whether all elements 
      of int multiarray [m1] are within value [limit] of all the elements 
      of int multiarray [m2] and vice-versa. 
      Raises: Failure if [m1] and [m2] are of different dimensions
              Failure if [shape m1] <> [shape m2]  *)
  val all_close : int t -> int t -> int -> bool

  (** [clip m min max] is an int mutltiarray representing all the elements of 
      int multiarray [m] clipped between values [min] and [max], i.e. if any 
      element of [m] is less than min or greater than max based on the standard
      compare function, then that element is replaced by [min] and [max] 
      respectively.  *)
  val clip: int t -> int -> int -> int t

  (** [save m path] is a unit that saves the int multiarray represented by [m] 
       in the path of the local machine given by string [path]. Supported 
       file formats are .txt and .csv *)
  val save: int t -> string -> unit

  (** [load path istwo] is the int multiarray that is converted from the files
      given by [path]. Supported file formats are .txt and .csv. If [istwo] is 
      true then [load path istwo] is a two-dimensional multiarray and is 
      one dimensioanl otherwise. *)
  val load: string -> bool -> int t 

end

module Listndarray:Multiarray 

