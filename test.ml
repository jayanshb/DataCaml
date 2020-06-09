(** TESTING PLAN
We have tested all of our functionality using OUnit testing. The given 
50 tests provided test the functionality implemented by module Listndarray. 
which implements the signature MultiArray. Based on what we have learnt 
about testing modules in this class, we split all our functions into 3 types: 
1. Producers: functions that don't take an object of 'a t 
but return an object of 'a t). An xxample of this kind of functin is the 
create which takes in an 'a list list and a bool and returns an 'a t object. 
2. Consumers: functions that take an 'a t object, but return something that 
it isn't an 'a t object. An exampple of this function would be shape or max 
that take in a multiarray of type 'a t and return an (int * int) and int
respectively. 
3. Both producers and consumers that take an ' a t object 
and return a new 'a object, i.e. modify state. An example of this would 
be the replace function. 

All our tests are black-box tests that are generated 
based on the idea that we use the consumer functions to test out the properties
 of the producer and both functions. For example, 
a lot of our functions test the input using the to_list_1D or to_list_2D 
function that is a consumer. 

We feel that this is the best way to test our system since we are not relying 
on the actual implementation of the .ml file but seeing how functionality 
should work based on the specifications of the functions that we wrote 
in the .mli file, thereby making full use of the modularity features 
provided by OCaml. Also, in this way our tests are more dependable since
they are simply relying on testing the equality of primitive OCaml
types returned by the consumer functions. 
 *)

open OUnit2
open Listndarray 

let a = Listndarray.create ([[]]) (false)

let non_rect = Listndarray.create ([[1;2;3];[5;2]]) (true)

let rect_sample = Listndarray.create ([[1;2;3];[5;2;4]]) (true)

let add_sample = Listndarray.create ([[1;2;3];[5;2;4]]) (true)

let zeros_sample = Listndarray.zeros 2 2

let ones_sample = Listndarray.ones 2 4 

let zeros_sample_test = Listndarray.create ([[0;0];[0;0]]) (true)

let b = Listndarray.create ([[1;2;3;4;5]]) (false)

let c_mul = Listndarray.create ([[10;10;10;10;10]]) (false)

let d = Listndarray.constant 2 3 15

let float_sample = Listndarray.create ([[1.0; 2.0; 3.0; 4.0]]) (false)

let tests = [
  "is_empty should return true for empty" >:: 
  (fun _ -> (assert_equal (true) (Listndarray.is_empty a))); 

  "Shape of empty Multiarray should be (0,0)" >:: 
  (fun _ -> (assert_equal ((0,-1)) (Listndarray.shape a)));  

  "Shape of non-rectangular matrix non_rect should be (2,-1)" >:: 
  (fun _ -> (assert_equal ((2,-1)) (Listndarray.shape non_rect))); 

  "Shape of rectangular matrix rect_sample should be (2,3)" >:: 
  (fun _ -> (assert_equal ((2,3)) (Listndarray.shape rect_sample))); 

  "len non_rect is 2" >:: 
  (fun _ -> (assert_equal ((2)) (Listndarray.len non_rect))); 

  "zeros_sample creates a 2 x 2 matrix of zeros" >:: 
  (fun _ -> (assert_equal (zeros_sample_test) (zeros_sample))); 

  "ones_sample creates a 2 x 4 matrix of ones" >:: 
  (fun _ -> (assert_equal ([[1;1;1;1];[1;1;1;1]]) 
  (ones_sample |> Listndarray.to_list_2D))); 

  "Indexing 0 returns first element of 1-d array b" >:: 
  (fun _ -> (assert_equal (1) (Listndarray.index b 0 0))); 

  "Indexing 4 returns first element of 1-d array b" >:: 
  (fun _ -> (assert_equal (5) (Listndarray.index b 4 0))); 

  "Indexing -1 returns last element of 1-d array b" >:: 
  (fun _ -> (assert_equal (5) (Listndarray.index b (-1) (0)))); 

  "Indexing a 1-D array while changing the cols 
  parameter doesn't affect result" >:: 
  (fun _ -> (assert_equal (5) (Listndarray.index b (-1) (4)))); 

  "Indexing 0 0 returns first element of 2-d array rect_sample" >:: 
  (fun _ -> (assert_equal (1) (Listndarray.index rect_sample 0 0))); 

  "Slicing with step size 1 and end beyond array is the entire array" >:: 
  (fun _ -> (assert_equal (b) (Listndarray.slice b (0, 7, 1) (0,0,0)))); 

  "Slicing with step size 1 and end just before array is the  
    entire array except for last element" >:: 
  (fun _ -> (assert_equal (Listndarray.create ([[1;2;3;4]]) (false)) 
               (Listndarray.slice b (0, 4, 1) (0,0,0)))); 

  "Slicing with step size 1, end just before array and start equal to 1 is  
    the entire array except for last and first element" >:: 
  (fun _ -> (assert_equal (Listndarray.create ([[2;3;4]]) (false)) 
               (Listndarray.slice b (1, 4, 1) (0, 0, 0)))); 

  "Slicing with step size 2 and end beyond array are the first, third 
    and last elements" >:: 
  (fun _ -> (assert_equal (Listndarray.create ([[1;3;5]]) (false)) 
               (Listndarray.slice b (0, 7, 2) (0, 0, 0)))); 

  "Slicing with step size 2 and end as the third element is the 
    first element" >:: (fun _ -> 
    (assert_equal (Listndarray.create ([1]::[]) (false))) 
    (Listndarray.slice b (0, 1, 2) (0, 0, 0))); 

  "to list_1D converts multiarray to list" >:: 
  (fun _ -> (assert_equal (List.sort Stdlib.compare [1;2;3;4;5]) 
               (Listndarray.to_list_1D b))); 


  "flatten converts 2-d array rect_sample into 1-d array" >:: 
  (fun _ -> (assert_equal (Listndarray.create [[1;2;3;5;2;4]] (false)) 
               (Listndarray.flatten rect_sample))); 

  "arrange 0 returns array representing [0]" >:: 
  (fun _ -> (assert_equal (Listndarray.create ([[0]]) (false)) 
               (Listndarray.arrange 0))); 

  "arrange 4 is array representing [0;1;2;3;4]" >:: 
  (fun _ -> (assert_equal (Listndarray.create ([[0;1;2;3;4]]) (false)) 
               (Listndarray.arrange 4))); 
 
  "max b is 5" >:: 
  (fun _ -> (assert_equal (5) 
               (Listndarray.max b))); 

  "max rect_sample is 5" >:: 
  (fun _ -> (assert_equal (5) 
               (Listndarray.max rect_sample))); 

  "max non_rect is 5" >:: 
  (fun _ -> (assert_equal (5) 
               (Listndarray.max non_rect))); 

  "max non_rect is 5" >:: 
  (fun _ -> (assert_equal (5) 
               (Listndarray.max non_rect)));

  "max zeros_sample is 0" >:: 
  (fun _ -> (assert_equal (0) 
               (Listndarray.max zeros_sample)));

  "min b is 1" >:: 
  (fun _ -> (assert_equal (1) 
               (Listndarray.min b))); 

  "min rect_sample is 1" >:: 
  (fun _ -> (assert_equal (1) 
               (Listndarray.min rect_sample)));

  "min zeros_sample is 0" >:: 
  (fun _ -> (assert_equal (0) 
               (Listndarray.min zeros_sample)));

  "replace b 1 0 105 is the array representing [1;105;3;4;5]" >:: 
  (fun _ -> (assert_equal (Listndarray.create ([[1;105;3;4;5]]) (false)) 
               (Listndarray.replace b 1 0 105)));

  "replace b 5 0 105 is b itself" >:: (fun _ -> (assert_equal (b)) 
                                        (Listndarray.replace b 5 0 105));

  "sum a is 15" >:: 
  (fun _ -> (assert_equal (0)) (Listndarray.sum a));

  "sum non_rect is 13" >:: 
  (fun _ -> (assert_equal (13)) (Listndarray.sum non_rect));

  "add add_sample rect_sample is the array representing [[2;4;6];[10;4;8]]"
  >:: (fun _ -> (assert_equal (Listndarray.create ([[2;4;6];[10;4;8]]) (true))) 
          (Listndarray.add (add_sample) (rect_sample)));

  "sub add_sample rect_sample is the array representing [[0;0;0];[0;0;0]]"
  >:: (fun _ -> (assert_equal (Listndarray.create ([[0;0;0];[0;0;0]]) (true))) 
          (Listndarray.subtract add_sample rect_sample));

  "dot b b is 55"
  >:: (fun _ -> (assert_equal (55) 
                   (Listndarray.dot b b)));
  
  "replace add_sample 1 0 105 is array representing [[1;2;3];[105;2;4]]"
   >:: (fun _ -> (assert_equal (Listndarray.create [[1;2;3];[105;2;4]] (true))) 
    (Listndarray.replace add_sample 1 0 105));

  "constant 2 3 15 is array representing [[15;15;15];[15;15;15]]"
   >:: (fun _ -> 
   (assert_equal (Listndarray.create [[15;15;15];[15;15;15]] (true))) 
   (Listndarray.constant 2 3 15));

  "all_close b c_mul 5 is false"
   >:: (fun _ -> 
   (assert_equal (false) 
   (Listndarray.all_close b c_mul 5)));

  "all_close b c_mul 10 is true"
   >:: (fun _ -> 
   (assert_equal (true) 
   (Listndarray.all_close b c_mul 10)));

  "create works with floats"
   >:: (fun _ -> 
   (assert_equal (List.sort Stdlib.compare [1.0;2.0;3.0;4.0]) 
   (float_sample |> Listndarray.to_list_1D)));

  "sort float_sample is float_sample itself"
   >:: (fun _ -> 
   (assert_equal (List.sort Stdlib.compare [1.0;2.0;3.0;4.0]) 
   (Listndarray.sort float_sample |> Listndarray.to_list_1D)));

  "mean float_sample is 2.5"
   >:: (fun _ -> 
   (assert_equal (2.5) 
   (Listndarray.mean float_sample)));

  "median b is 3"
   >:: (fun _ -> 
   (assert_equal (2.5) 
   (Listndarray.median float_sample)));

  "norm float_sample is root 30"
   >:: (fun _ -> 
   (assert_equal (sqrt 30.0) 
   (Listndarray.norm float_sample)));

  "power float_sample 2.0 is array representing [1.0; 4.0; 9.0; 16.0]"
   >:: (fun _ -> 
   (assert_equal (Listndarray.create [[1.0; 4.0; 9.0; 16.0]] (false)) 
   (Listndarray.power float_sample (2.0))));

  "multiply c c is array representing [100;100;100;100;100]"
  >:: (fun _ -> 
  (assert_equal (Listndarray.create [[100;100;100;100;100]] (false)) 
                   (Listndarray.multiply c_mul c_mul)));

  "clip c_mul 5 15 is c_mul itself"
  >:: (fun _ -> 
  (assert_equal (c_mul) (Listndarray.clip c_mul 5 15)));

  "clip c_mul 20 100 is array representing [20;20;20;20;20]"
  >:: (fun _ -> 
  (assert_equal (Listndarray.create [[20;20;20;20;20]] (false)) 
  (Listndarray.clip c_mul 20 100)));

  "clip b 3 7 is array representing [3;3;3;4;5]"
  >:: (fun _ -> 
  (assert_equal (Listndarray.create [[3;3;3;4;5]] (false)) 
  (Listndarray.clip b 3 7)));
                
]

let suite = "search test suite" >::: tests

let _ = run_test_tt_main suite
