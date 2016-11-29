(*Refer to F# value restrictions and how to deal with them 
by using type annotations, or the F# "escape hatch" of the
[<GeneralizableValue>] attribute for value restriction
errors that will arise when using FParsec:
*)
type 'a LazyInner = Delayed of (unit -> 'a) | Value of 'a | Exception of exn
type 'a Lazy = 'a LazyInner ref
type 'a cell = Nil | CCons of 'a * 'a lazylist
and 'a lazylist = 'a cell Lazy
//If we need to have a generic type without compiler errors for value restrictions:
[<GeneralizableValue>]
let create f = ref (Delayed f)
let empty<'T> : 'T lazylist = create (fun () -> Nil)
(*For more in-depth read: 
https://blogs.msdn.microsoft.com/mulambda/2010/05/01/finer-points-of-f-value-restriction/
*)