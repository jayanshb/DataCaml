# DataCaml
Team : Jayansh Bhartiya and Saksham Mohan

# Core Vision: 
For the project, we built our own data science libraries for OCaml from
scratch. Our core vision was to understand how to use the functionalities of the
language to perform basic data science operations such as I/O tools that work with CSV
files, indexing, slicing data and providing linear algebra functionalities. We use the
python library Numpy for reference.
Ideally, we would like full support for scientific computing operations such as
reshaping, padding and even some complex domain specific functionalities like
computer vision. In addition, we would also like to offer support for series and
dataframe type objects, similar to what is provided by Pandas. We believe the ease of
modular programming and well as working with data structures in functional languages
makes it a highly underutilized avenue for something like data science.
From our previous report, we shifted more of our focus into providing robust linear
algebra functionality rather than focusing on visualizations. We thought that this was
both more feasible and also made sense from a development perspective since our
module was primarily a scientific computing module. 

# The project:
We implemented the signature using an ‘a list to implement the
MultiArray data structure. The data structure is basically a vector or 2-D matrix of data. We also completed
functionality for this data structure, allowing creating, indexing (both positive and
negative) and slicing (both positive and negative, along with step size parameters)
elements of this MultiArray. We also have implemented linear algebra functionality like
dot product of a vector, matrix multiplication, sum of a vector, maximum value,
minimum value etc. We implemented a test suite to test our functions using OUnit and
ensured all our tests passed for the demo. 

We successfully changed the implementation of our multiarray data structure to a variant rather than a list. This
meant changing up all the functions we had implemented previously but the main
benefit this gave us was that now we had the same functions for both our onedimensional and multi-dimensional data structures which not only a better
development practice but also enabled us to follow the principles of abstraction and
modular programming better. Each function is now simply pattern matched against
whether it’s a one-dimensional or multi-dimensional structure and the corresponding
evaluation is performed after that.
In addition, we added functionality for manipulation with csv files and txt files. Our data
structures can now both be loaded from and saved into files of the local machine calling
the functions.

As mentioned before, we also added some complex linear algebra functionality
including multiplication, dot products, slicing for multi-dimensional structures and
normalizing vectors and matrices as well as functions like all close and clip that are
commonly used in data science applications.
For our demo, we loaded our data structure onto utop and performed some of the
functionality on dummy structures. We also loaded and saved these structures into csv
and txt files. 

# How to use 
The given system is intended to be a Data Science library that allows users to manipulate and work with both one-dimensional and multi-dimensional arrays. 
The mli file gives a module signature of this multi-dimensional array which is implemented by us using a variant that is either a Vector of 'a list or a Matrix of 'a list list.
This version allows functionality like creating, indexing and slicing these arrays while some more involved linear algebra functionality is provided as well as saving and loading data from csv and txt files is provided..
We have tested the functions written in tests.ml and pass all the tests. We have also written documentation for all our functions. 

Instructions on how to install and build system : 
1. Running make build  to build the system 
2. Running make test to run all the tests in test.ml (currently passing all 50)


