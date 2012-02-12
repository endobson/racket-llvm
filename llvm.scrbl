#lang scribble/manual
 
@title{LLVM}
 
This is the start of the documentation for the Racket bindings to the LLVM API.

There are currently two version of the API; Safe and Unsafe. Through the Safe API
all memory managment of the LLVM objects and does not allow improper use of the API.
The Unsafe API puts memory management in the users hands and may not catch all
improper uses of the API. This may lead to corrupted memory or segfaults;
You have been warnedis done.

On top of the safe API is the Simple API. The Safe API is just a wrapper around
the C API that checks for correct usage, the Simple API on the other hand does
conversions and is intended to be more Racket-like.


@section{Simple}

The 
