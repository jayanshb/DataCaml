module type Multiarray = sig

  type 'a t 

  val is_empty : 'a t  -> bool

  val len : 'a t -> int

  val shape : 'a t -> int * int

  val zeros : int -> int -> int t 

  val ones: int -> int -> int t

  val constant: int -> int -> int -> int t

  val create : 'a list list -> bool -> 'a t 

  val index : 'a t -> int -> int -> 'a 

  val slice : 'a t -> int * int * int -> int * int * int -> 'a t

  val to_list_1D : 'a t -> 'a list

  val to_list_2D : 'a t -> 'a list list

  val flatten : 'a t -> 'a t

  val arrange : int -> int t

  val max : 'a t -> 'a 

  val min : 'a t -> 'a 

  val replace : 'a t -> int -> int -> 'a -> 'a t

  val sum : int t -> int

  val add : int t -> int t -> int t 

  val subtract : int t -> int t -> int t 

  val dot : int t -> int t -> int

  val multiply: int t -> int t -> int t

  val divide: float t -> float t -> float t 

  val power : float t -> float  -> float t 

  val square_root : float t -> float t

  val logarithm : float t -> float t

  val exponentiate : float t -> float t

  val sort : 'a t -> 'a t 

  val median : float t -> float

  val mean : float t -> float

  val norm : float t -> float

  val all_close : int t -> int t -> int -> bool

  val clip: int t -> int -> int -> int t

  val save: int t -> string -> unit

  val load: string -> bool -> int t 
end

module Listndarray:Multiarray = struct 

  type 'a t =  Vector of 'a list | Matrix of 'a list list

  let is_empty lst = match lst with 
    | Vector vec -> vec = []
    | Matrix matr -> matr = [[]]

  let len arr = match arr with 
    | Vector a -> List.length a
    | Matrix b -> List.length b

  (** [num_cols m] is the number of elements in a list [m]
      [m]. *)   
  let rec num_cols lst = match lst with 
    | [] -> 0 
    | hdlist:: _ -> List.length hdlist

  (** [is_rectangular_helper lst check] is [true] number of elements if 
      [List.length lst] = [check] *)   
  let rec is_rectangular_helper lst check = match lst with 
    | [] -> true
    | hd::t -> if List.length hd <> check then false 
      else is_rectangular_helper t check

  let is_rectangular lst = match lst with 
    | [] -> true 
    | hd::t -> is_rectangular_helper (t) (List.length hd) 

  let shape lst = match lst with 
    | Vector _ -> (len lst, -1)
    | Matrix matr -> if is_rectangular matr then (len lst, num_cols matr) 
      else (len lst, -1)

  (** [zero_col_helper cols ] is list with all zeros. Length of the list 
      is [cols] *)   
  let rec zero_col_helper cols = if cols = 0 then [] 
    else 0::zero_col_helper (cols - 1)

  (** [create_zero_vec elem] is a Vector with all zeros. Number of elements 
      in vector is [elem] *) 
  let rec create_zero_vec elem = Vector (zero_col_helper elem)

  let rec zeros n_rows n_cols = if n_cols = -1 then 
      create_zero_vec n_rows
    else if n_rows = 0 then Matrix ([])
    else (begin match zeros (n_rows - 1) (n_cols) with 
        | Matrix matr -> Matrix (zero_col_helper n_cols::matr)
        | _ -> failwith "Nope never here" end)

  let create lst is_mul = if is_mul then Matrix (lst) 
    else Vector (List.flatten lst)

  (** [at_index_one lst index] is the data item represented at index 
      [index] in a 1-D list [lst] . 
      Requires: index is an int
      Raises: Failure if [index] is beyond the number of items
              stored in [lst] or if [lst] is empty  *)  
  let rec at_index_one lst index = if index >= 0 then 
      if index < List.length lst then 
        match lst with 
        | [] -> failwith "MultiArray cannot be empty"
        | h::t -> if index = 0 then h else at_index_one (t) (index - 1)
      else failwith "Index out of bounds"
    else if Stdlib.abs (index) > List.length lst 
    then failwith "Index out of bounds"
    else at_index_one lst (index + List.length lst)

  (** [at_index_mult lst rows cols] is the data item represented at 
      ([row],[col]) index in a 2-D [lst] . 
      Requires: [row] and [col] are ints
      Raises: Failure if [index] is beyond the number of items
             stored in [lst] or if [lst] is empty  *)  
  let rec at_index_mult lst rows cols = if rows >= 0 then 
      if rows < List.length lst then 
        match lst with 
        | [] -> failwith "MultiArray cannot be empty"
        | hd::t -> if rows = 0 then at_index_one hd cols 
          else at_index_mult t (rows - 1) cols 
      else failwith "Index out of bounds"
    else if Stdlib.abs (rows) > List.length lst 
    then failwith "Index out of bounds"
    else at_index_mult lst (rows + List.length lst) cols

  let index lst rows cols = match lst with 
    | Vector vec -> at_index_one vec rows
    | Matrix matr -> at_index_mult matr rows cols

  (** [fst tup] is the first data item represented in the tuple [tup] 
      containing 3 elements *)  
  let fst (x, _, _) = x 

  (** [snd tup] is the second data item represented in the tuple [tup] 
      containing 3 elements *) 
  let snd (_, y, _) = y 

  (** [thd tup] is the third data item represented in the tuple [tup] 
      containing 3 elements *) 
  let thd (_, _, z) = z

  (** [slice_one lst (startcol, stopcol, stepcol)] is
        1-D list  sliced from col [startcol] to row [stopcol] with a 
        [stepcol] increment.
        Requires: [lst] is a 1-D list
        Raises: Failure if [stepcol] is 0. *)  
  let rec slice_one lst slice = 
    if (thd slice) = 0 then failwith "Step cannot be 0" 
    else if (snd slice - fst slice = 0) then [] 
    else let e = try ([at_index_one lst (fst slice)]) with
        | Failure _ -> [] 
      in begin match e with 
        | [] -> [] 
        | [v] -> if ((fst slice) + (thd slice)) >= (snd slice) then v::[] 
          else v::slice_one (lst) 
                 (((fst slice) + (thd slice)), snd slice, thd slice)
        | _ -> [] end

  (** [slice m (startrow, stoprow, steprow) (startcol, stopcol, stepcol)] is
      2-D list [m] sliced from row [startrow] to row [startcol] with a 
      [steprow] increment and from column [startcol] to column [stepcol] 
      with a [stepcol] increment. 
      Requires: [m] is a 2-D list
      Raises: Failure if [steprow] or [stepcol] is 0. *) 
  let rec slice_mult matr slice_row slice_col = 
    if (thd slice_row) = 0 || (thd slice_col) = 0 
    then failwith "Step cannot be 0" 
    else let e = 
           try (slice_one (at_index_one (matr) (fst slice_row)) (slice_col)) 
           with
           | Failure _ -> [] 
      in begin match e with 
        | [] -> [] 
        | hd::t as v -> 
          if ((fst slice_row) + (thd slice_row)) >= (snd slice_row) then v::[] 
          else v::slice_mult (matr) 
                 (((fst slice_row) + (thd slice_row)), snd slice_row, 
                  thd slice_row)
                 (slice_col) end

  let slice lst row_slice col_slice = match lst with 
    | Vector vec -> Vector (slice_one vec row_slice)
    | Matrix matr -> Matrix (slice_mult matr row_slice col_slice)

  let to_list_1D lst = match lst with 
    | Vector vec -> vec
    | Matrix matr -> List.flatten matr

  let to_list_2D lst = match lst with 
    | Vector vec -> [vec]
    | Matrix matr -> matr

  let flatten lst = match lst with 
    | Vector vec -> Vector (vec)
    | Matrix matr -> Vector (List.flatten matr)

  let arrange number  = 
    let rec arrange_helper number out_lst init = 
      if(number = -1) then out_lst 
      else arrange_helper (number - 1) ((init) :: out_lst) (init + 1)
    in Vector (List.rev (arrange_helper number [] 0))

  (** [max_one lst] is maximum element in the 1-D list [lst] *)
  let max_one lst = 
    let rec max_one_helper lst max = match lst with 
      | [] -> max
      | h::t -> if h > max then max_one_helper t h else max_one_helper t max
    in max_one_helper lst (at_index_one lst 0)

  (** [max_mult m] is maximum element among all rows and columns in the 
      2-D list [m].  *)
  let max_mult lst = lst |> List.flatten |> max_one

  let max lst = match lst with 
    | Vector vec -> max_one vec
    | Matrix matr -> max_mult matr

  (** [min_one lst] is minimum element in the 1-D list [lst] *)
  let min_one lst = 
    let rec min_one_helper lst min = match lst with 
      | [] -> min
      | h::t -> if h < min then min_one_helper t h else min_one_helper t min
    in min_one_helper lst (at_index_one lst 0)

  (** [min_mult m] is minimum element among all rows and columns in the 
      2-D list [m].  *)
  let min_mult lst = lst |> List.flatten |> min_one

  let min lst = match lst with
    | Vector vec -> min_one vec
    | Matrix matr -> min_mult matr

  (** [replace_one_helper lst col elt] is a 1-D list representing all the 
      elements of 1-D list [lst] except element at index [col] in [lst], which 
      is replaced by value [elt]. 
      If [col] is out of the bounds of [lst], then [lst] is returned back.
      Requires: [col] should be an integer.
  *)
  let replace_helper_vec lst index replace_with = 
    if index >= List.length lst then lst
    else (slice_one lst (0, index, 1)) @ 
         (replace_with::(slice_one (lst) (index + 1, List.length lst, 1)))

  (** [replace_helper_matr m row col elt] is a 2-D list representing all the 
      elements of 2-D list [m] except element at index ([row],[col]) in [m], 
      which is replaced by value [elt]. 
      If [row] or [col] is out of the bounds of [m], then [m] is returned back.
      Requires: [row] and [col] should be integers.
  *)
  let replace_helper_matr matr row col replace_with = 
    if row >= List.length matr then matr 
    else (slice_one matr (0, row, 1)) @
         (replace_helper_vec (at_index_one matr row) (col) (replace_with)::
          slice_one (matr) (row + 1, List.length matr, 1))

  let replace lst row col replace_with = match lst with 
    | Vector vec -> Vector (replace_helper_vec vec row replace_with)
    | Matrix matr -> Matrix (replace_helper_matr matr row col replace_with)

  (** [sum_in_row lst] is the sum of all elements in [lst]
            Requires:
      - [lst] should be a 1-D list of integers

        Examples:
      - [sum_list [1;2;3]]  is [6]
      - [sum_list []] is [0] *) 
  let rec sum_in_row = function 
    | [] -> 0
    | h::t -> h + sum_in_row t

  (** [sum_in_row_floats lst] is the sum of all elements in [lst]
            Requires:
      - [lst] should be a 1-D list of floats

        Examples:
      - [sum_list [1.;2.;3.]]  is [6.]
      - [sum_list []] is [0.] *) 
  let rec sum_in_row_floats = function 
    | [] -> 0.
    | h::t -> h +. sum_in_row_floats t

  (** [sum_in_row_floats m] is the sum of all elements in all rows of [m]
           Requires:
      - [lst] should be a 2-D list of integers

       Examples:
      - [sum_list [[1;2;3]]]  is [[6]]
      - [sum_list [[]]] is [[0]] *) 
  let rec sum_list = function 
    | [] -> 0 
    | h::t -> sum_in_row h + sum_list t

  (** [sum_in_row_floats m] is the sum of all elements in all rows of [m]
            Requires:
      - [lst] should be a 2-D list of floats

        Examples:
      - [sum_list [[1.;2.;3.]]]  is [[6.]]
      - [sum_list [[]]] is [[0.]] *) 
  let rec sum_list_floats = function 
    | [] -> 0.
    | h::t -> sum_in_row_floats h +. sum_list_floats t

  let sum lst = match lst with 
    | Vector vec -> sum_in_row  vec 
    | Matrix matr -> sum_list matr

  (** [add_one lst1 lst2] is the sum of all elements of [lst1] 
      added up with [lst2]. 
       Elements also include elements inside respective rows.
         Requires:
      - [lst1] and [lst2] should be 1-D lists
      - [shape lst1] = [shape lst2]

         Examples:
      - [add [1;2;3] [0;0;0]]  is [1;2;3]
      - [add [] []] is [0]

         Raises:
      - failure if [shape lst1] <> [shape lst2] *) 
  let rec add_one arr1 arr2 = match arr1, arr2 with 
    | [], [] -> [] 
    | h1::t1, h2::t2 -> (h1 + h2):: (add_one t1 t2)
    | _ -> failwith "Shapes must be the same"

  (** [add_matr m1 m2] is the element-wise addition of [m1] with [m2].
      Elements also include elements inside respective rows.
         Requires:
      - [m1] and [m2] should be 2-D lists
      - [shape m1] = [shape m2]

         Examples:
      - [add [[1;2;3]] [[0;0;0]]]  is [[1;2;3]]
      - [add [] []] is []

         Raises:
      - failure if [shape m1] <> [shape m2] *) 
  let rec add_matr arr1 arr2 = match arr1, arr2 with 
    | [], [] -> []
    | hd1::t1, hd2::t2 -> (add_one hd1 hd2)::(add_matr t1 t2)
    | _ -> failwith "Shapes must be the same"

  let add lst1 lst2 = match lst1, lst2 with 
    | Vector vec1, Vector vec2 -> Vector (add_one vec1 vec2)
    | Matrix matr1, Matrix matr2 -> Matrix (add_matr matr1 matr2)
    | _ -> failwith 
             "Cannot add a One-Dimensional Array to a multi-dimensional array"

  (** [sub_one lst1 lst2] is the difference of all elements of
        [lst2] subtracted from  corresponding elements in [lst1]
        Requires:
      - [lst] should be 1-D lists
      - [shape lst1] = [shape lst2]

        Examples:
      - [sub_one [1;2;3] [0;0;0]]  is [1;2;3]
      - [sub_one [] []] is [0]

           Raises:
      - failure if [shape lst1] <> [shape lst2] *)
  let rec sub_one arr1 arr2 = match arr1, arr2 with 
    | [], [] -> [] 
    | h1::t1, h2::t2 -> (h1 - h2):: (sub_one t1 t2)
    | _ -> failwith "Shapes must be the same"

  (** [subtract_matr  m1 m2] is the element-wise subtraction of [m1] 
      with [m2]. Elements also include elements inside respective rows.
         Requires:
      - [m1] and [m2] should be 2-D lists
      - [shape m1] = [shape m2]

         Examples:
      - [subtract_matr [[1;2;3]] [[0;0;0]]]  is [[1;2;3]]
      - [subtract_matr [] []] is []

         Raises:
      - failure if [shape m1] <> [shape m2] *) 
  let rec subtract_matr arr1 arr2 = match arr1, arr2 with 
    | [], [] -> []
    | hd1::t1, hd2::t2 -> (sub_one hd1 hd2)::(subtract_matr t1 t2)
    | _ -> failwith "Shapes must be the same"

  let subtract lst1 lst2 = match lst1, lst2 with 
    | Vector vec1, Vector vec2 -> Vector (sub_one vec1 vec2)
    | Matrix matr1, Matrix matr2 -> Matrix (subtract_matr matr1 matr2)
    | _ -> failwith 
             "Cannot subtract a One-Dimensional Array to a 
             multi-dimensional array"

  (** [dot_helper lst1 lst2] is the dot product of [lst1] and [lst2]
        Requires:
      - [lst1] and [lst2] should be 1-D lists
      - [shape lst1] = [shape lst2]

        Examples:
      - [dot_helper [1;2;3] [1;1;1]]  is [6]
      - [dot_helper [] []] is [0]

        Raises:
      - failure if [shape lst1] <> [shape lst2] *) 
  let rec dot_helper vec1 vec2 = match vec1, vec2 with 
    | [], [] -> 0
    | h1::t1, h2::t2 -> h1 * h2 + (dot_helper t1 t2)
    | _ -> failwith "Shapes must be the same"

  let dot lst1 lst2 = match lst1, lst2 with 
    | Vector vec1, Vector vec2 -> dot_helper vec1 vec2
    | Matrix matr1, Matrix matr2 -> failwith "Call Matrix Multiplication 
    method"
    | _ -> failwith "Cannot compute dot product of a One-Dimensional Array 
    and a multi-dimensional array"

  (** [multiply_helper_vector lst1 lst2] is the element-wise multiplication of 
      [lst1] and [lst2]. 
         Requires:
      - [lst1] and [lst2] should be 1-D lists
      - [shape m1] = [shape m2]

         Examples:
      - [multiply_helper_matr [1;2;3] [0;0;0]]  is [0;0;0]
      - [multiply_helper_matr [] []] is []

         Raises:
      - failure if [shape lst1] <> [shape lst2] *) 
  let rec multiply_helper_vector vec1 vec2 = match vec1, vec2 with 
    | [], [] -> []
    | (h1::t1), (h2::t2) -> (h1 * h2) :: (multiply_helper_vector t1 t2)
    | _ -> failwith "Shapes must be the same"

  (** [multiply_helper_matr  m1 m2] is the element-wise multiplication of [m1] 
      and [m2]. Elements also include elements inside respective rows.
         Requires:
      - [m1] and [m2] should be 2-D lists
      - [shape m1] = [shape m2]

         Examples:
      - [multiply_helper_matr [[1;2;3]] [[0;0;0]]]  is [[0;0;0]]
      - [multiply_helper_matr [] []] is []

         Raises:
      - failure if [shape m1] <> [shape m2] *) 
  let rec multiply_helper_matr matr1 matr2 = match matr1, matr2 with 
    | [], [] -> []
    | hd1::t1, hd2::t2 -> (multiply_helper_vector hd1 hd2)::
                          multiply_helper_matr t1 t2
    | _ -> failwith "Shapes must be the same size"

  let multiply lst1 lst2 = match lst1, lst2 with 
    | Vector vec1, Vector vec2 -> Vector (multiply_helper_vector vec1 vec2)
    | Matrix m1, Matrix m2 -> Matrix (multiply_helper_matr m1 m2)
    | _ -> failwith "Expected arguments of the same dimensions"

  (** [divide_helper lst1 lst2] is the element-wise division of 
      [lst1] and [lst2]. 
        Requires:
      - [lst1] and [lst2] should be 1-D lists of elements of type float.
      - [shape m1] = [shape m2]

        Examples:
      - [divide_helper [0;0;0] [1;2;3]]  is [0;0;0]
      - [divide_helper [] []] is []

        Raises:
      - failure if [shape lst1] <> [shape lst2] *) 
  let rec divide_helper vec1 vec2 = match vec1, vec2 with 
    | [], [] -> []
    | (h1::t1), (h2::t2) -> (h1 /. h2) :: (divide_helper t1 t2)
    | _ -> failwith "Columns must be the same shape"

  (** [divide_matr m1 m2] is the element-wise division of 
      [m1] and [m2]. Elements also include elements inside respective rows.
       Requires:
      - [m1] and [m2] should be 2-D lists of rows with elements of type float.
      - [shape m1] = [shape m2]

       Examples:
      - [divide_helper [[1;2;3]] [[0;0;0]]]  is [[0;0;0]]
      - [divide_helper [] []] is []

       Raises:
      - failure if [shape lst1] <> [shape lst2] *) 
  let rec divide_matr arr1 arr2 = match arr1, arr2 with 
    | [], [] -> []
    | hd1::t1, hd2::t2 -> (divide_helper hd1 hd2)::(divide_matr t1 t2)
    | _ -> failwith "Rows must be the same shape"

  let divide lst1 lst2 = match lst1, lst2 with 
    | Vector vec1, Vector vec2 -> Vector (divide_helper vec1 vec2)
    | Matrix m1, Matrix m2 -> Matrix (divide_matr m1 m2)
    | _ -> failwith "Expected arguments of the same dimensions"

  (** [power_one lst val] is a float 1-D list representing the element-wise 
      raise of float list [lst] to the power of [val].  *)
  let power_one vec pow = 
    List.map (fun x -> x**pow) vec

  (** [power_two m val] is a float 2-D list representing the element-wise 
      raise of float list [m] to the power of [val]. 
      Elements also include elements inside respective rows. *)
  let rec power_two m1 pow = match m1 with 
    | [] -> []
    | h::t -> (power_one h pow) :: (power_two t pow)

  let power lst pow = match lst with 
    | Vector v1 -> Vector (power_one v1 pow)
    | Matrix m1 -> Matrix (power_two m1 pow)

  (** [square_root_one lst val] is a float 1-D list representing the 
      element-wise square-root of float list [lst] *)
  let square_root_one vec = 
    List.map (fun x -> sqrt x) vec

  (** [square_root_two m val] is a float 2-D list representing the 
      element-wise square-root of float list [m] 
       Elements also include elements inside respective rows.
  *)
  let rec square_root_two m1 = match m1 with 
    | [] -> []
    | h::t -> (square_root_one h) :: (square_root_two t)

  let square_root lst = match lst with 
    | Vector v1 -> Vector (square_root_one v1)
    | Matrix m1 -> Matrix (square_root_two m1)

  (** [logarithm_one lst] is a float 1-D list representing the element-wise
      natural logaritm of float list [lst] *)
  let logarithm_one vec = 
    List.map (fun x -> log x) vec

  (** [logarithm_two m] is a float 2-D list representing the element-wise
      natural logaritm of float list [m].

       Elements also include elements inside respective rows.
  *)
  let rec logarithm_two m1 = match m1 with 
    | [] -> []
    | h::t -> (logarithm_one h) :: (logarithm_two t)

  let logarithm lst = match lst with 
    | Vector v1 -> Vector (logarithm_one v1)
    | Matrix m1 -> Matrix (logarithm_two m1)

  (** [exponentiate_one lst] is a float multiarray representing the element-wise
      raise of float multiarray [lst] as the power of the natural exponent*)
  let exponentiate_one vec = 
    List.map (fun x -> exp x) vec

  (** [exponentiate_two m] is a float multiarray representing the element-wise
      raise of float multiarray [m] as the power of the natural exponent
      Elements also include elements inside respective rows.
  *)
  let rec exponentiate_two m1 = match m1 with 
    | [] -> []
    | h::t -> (exponentiate_one h) :: (exponentiate_two t)

  let exponentiate lst = match lst with 
    | Vector v1 -> Vector (exponentiate_one v1)
    | Matrix m1 -> Matrix (exponentiate_two m1)

  let sort lst = match lst with 
    | Vector v1 -> Vector (List.sort Stdlib.compare v1)
    | Matrix m1 -> failwith "Not implemented for matrices"

  let median vec = match vec with 
    | Vector v1 -> let size = List.length v1 in
      if (size mod 2)= 0 
      then ((index (sort vec) (size/2 - 1) 0) +. 
            (index (sort vec) (size/2) 0))/. 2.0
      else  index (sort vec) (size/2) 0
    | _ -> failwith "Median works only on vectors"

  (** [num_rows_helper lst] the number of rows in the 2-D list [lst].
  *)
  let rec num_rows_helper m = match m with 
    | [[]] -> 1
    | h :: t -> 1 + num_rows_helper t
    | _ -> failwith "Not a matrix"

  let mean vec = match vec with 
    | Vector v1 -> (sum_in_row_floats v1) /. float_of_int(List.length v1)
    | Matrix m1 -> (sum_list_floats m1) /. 
                   float_of_int((num_rows_helper m1) * (num_cols m1))

  (** [norm_vector_helper lst] is the normalized value of all the elements of 
      the one-dimensional float 1-D list [lst]
      Raises: Failure if [lst] is two-dimensional *)
  let rec norm_vector_helper v1 = match v1 with 
    | [] -> 0.
    | h::t -> h ** 2.0 +. norm_vector_helper t

  let norm vec = match vec with 
    | Vector v1 -> sqrt (norm_vector_helper v1)
    | _ -> failwith "Normalize only works on 1-Dimensional multiarrays"

  (** [all_close lst1 lst2 limit] is a boolean representing whether all elements 
      of int 1-D list [lst1] are within value [limit] of all the elements 
      of int 1-D list [lst2] and vice-versa. 
      Raises: Failure if [lst1] and [lst2] are of different dimensions
              Failure if [shape lst1] <> [shape lst2]  *)
  let rec all_close_vector vec1 vec2 limit = match vec1, vec2 with 
    | [], [] -> true
    | h1::t1, h2::t2 -> if Stdlib.abs h1 >= Stdlib.abs h2 
      then 
        if Stdlib.abs h1 - Stdlib.abs h2 <= limit 
        then all_close_vector t1 t2 limit
        else false
      else if Stdlib.abs h2 - Stdlib.abs h1 <= limit 
      then all_close_vector t1 t2 limit
      else false
    | _ -> failwith "Size mismatch"

  (** [all_close m1 m2 limit] is a boolean representing whether all elements 
      of int 2-D list [m1] are within value [limit] of all the elements 
      of int 2-D list [m2] and vice-versa. 
      Raises: Failure if [m1] and [m2] are of different dimensions
              Failure if [shape m1] <> [shape m2]  *)
  let rec all_close_matrix matr1 matr2 limit = match matr1, matr2 with 
    | [], [] -> true 
    | hd1::t1, hd2::t2 -> all_close_vector hd1 hd2 limit && 
                          all_close_matrix t1 t2 limit
    | _ -> failwith "Size mismatch"

  let all_close lst1 lst2 limit = match lst1, lst2 with 
    | Vector v1, Vector v2 -> all_close_vector v1 v2 limit
    | Matrix m1, Matrix m2 -> all_close_matrix m1 m2 limit
    | _ -> failwith "Expected arguments of the same dimensions"

  (** [clip lst min max] is an int 1-D list representing all the elements of 
      int 1-D list [lst] clipped between values [min] and [max], i.e. if any 
      element of [lst] is less than min or greater than max based on the 
      standard compare function, then that element is replaced by [min] 
      and [max] respectively.  *)
  let rec clip_vector lst start stop = match lst with 
    | [] -> []
    | h::t -> if h > stop then stop::clip_vector t start stop
      else if h < start then start::clip_vector t start stop
      else h::clip_vector t start stop

  (** [clip m min max] is an int 2-D list representing all the elements of 
        int 2-D list [m] clipped between values [min] and [max], i.e. if any 
        element of [m] is less than min or greater than max based on the 
        standard compare function, then that element is replaced by [min] 
        and [max] respectively.  *)
  let rec clip_matrix matr start stop = match matr with 
    | [] ->[]
    | hd::tl -> clip_vector hd start stop::clip_matrix tl start stop

  let clip lst start stop = match lst with 
    | Vector v1 -> Vector (clip_vector v1 start stop)
    | Matrix m1 -> Matrix (clip_matrix m1 start stop)


  (** [one_col_helper cols] is list with all ones. Length of the list 
      is [elem] *)  
  let rec one_col_helper cols = if cols = 0 then [] 
    else 1::one_col_helper (cols - 1)

  (** [create_zero_vec elem] is a Vector with all zeros. Number of elements 
      in the vector is [elem] *) 
  let create_one_vec elem = Vector (one_col_helper elem)

  let rec ones row cols = if cols = -1 then 
      create_one_vec row
    else if row = 0 then Matrix ([])
    else (begin match ones (row - 1) (cols) with 
        | Matrix matr -> Matrix (one_col_helper cols::matr)
        | _ -> failwith "Nope never here" end)

  (** [one_col_helper cols value] is list with all elements = [value]. 
      Number of elements in the list is [cols] *)
  let rec constant_col_helper cols value = if cols = 0 then [] 
    else value::constant_col_helper (cols - 1) (value)


  (** [create_constant_vec elem value] is a vector with all elements = [value]. 
      Number of elements in the vector is [elem] *)
  let create_constant_vec elem value = Vector 
      (constant_col_helper elem value)

  let rec constant row cols value = if cols = -1 then 
      create_constant_vec row value
    else if row = 0 then Matrix ([])
    else (begin match constant (row - 1) (cols) (value) with 
        | Matrix matr -> Matrix (constant_col_helper cols value::matr)
        | _ -> failwith "Nope never here" end)

  (** [save_vec_helper lst path] is a unit that saves the int 1-D list [lst] 
      in the path of the local machine given by string [path]. Supported 
      file formats are .txt and .csv *)
  let rec save_vec_helper lst path = match lst with 
    | [] -> Printf.fprintf path "\n"; 
    | h::t -> Printf.fprintf path "%d," h; save_vec_helper t path

  (** [save_matr_helper m path] is a unit that saves the int 2-D list [m] 
      in the path of the local machine given by string [path]. Supported 
      file formats are .txt and .csv *)
  let rec save_matr_helper matr path = match matr with 
    | [] -> () 
    | hd::t -> save_vec_helper hd path; save_matr_helper t path

  let save arr path = let channel = (open_out path) in match arr with 
    | Vector vec -> save_vec_helper vec channel; close_out channel
    | Matrix matr -> save_matr_helper matr channel; close_out channel 


  (** [string_to_list lst] is the 1-D list with every string element with 
      an equivalent numerical representation converted to [int_of_string elt]
       where elt is an element in [lst]. The operation disregards empty strings*)
  let rec string_to_list lst = match lst with
    | [] -> [] 
    | h::t -> if String.equal h "" then string_to_list t 
      else (int_of_string h)::string_to_list t

  (** [load_helper path] is the 1-D int list that is converted from the files
      given by [path]. Supported file formats are .txt and .csv. *)
  let load_helper str = string_to_list (String.split_on_char ',' str)

  (** [load_helper path] is the 2-D int list that is converted from the files
      given by [path]. Supported file formats are .txt and .csv. *)
  let rec load_helper_matrix ic = let new_line = try input_line ic with
      | End_of_file -> ""
    in begin match (load_helper new_line) with 
      | [] -> [] 
      | _ -> (load_helper new_line)::load_helper_matrix ic end

  let load path is_multi = let ic = open_in path in 
    let line = input_line ic in if is_multi then Matrix (load_helper_matrix ic)
    else Vector (load_helper line)

end