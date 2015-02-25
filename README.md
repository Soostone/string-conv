# string-conv

A type class to standardize string conversions.  With this type class you only
need to remember one function for converting between any two string variants.
This package includes support for String, ByteString, and Text as well as the
Lazy and Strict variants where necessary.

StringConv's `toS` function is most useful when you have a fully defined
string conversion with a fixed (non-polymorphic) input and output type.  Of
course you can still use it when you don't have a fixed type.  In that case
you might need to specify a type class constraint such as `StringConv s
String`.
