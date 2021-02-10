// T-501-FMAL, Spring 2021, Assignment 1
(*
STUDENT NAMES HERE: Svanhildur Einarsdottir and Roslin Erla Tomasdottir
*)

module Assignment1

open System

// Problem 1

// nf : int -> int

let rec nf n =
    match n with
    | n when n < 1 -> 1
    | 1 -> 2
    | n when n > 1 -> 2 * nf (n - 1) + 3 * nf (n - 2)

// Problem 2

// (i)

// lastTrue : (int -> bool) -> int -> int

let rec lastTrue f n =
    match n with
    | n when n - 1 < 0 -> -1
    | n when f (n - 1) -> n - 1
    | _ -> lastTrue f (n - 1)

// (ii)

// lastEqual : 'a -> (int -> 'a) -> int -> int when 'a : equality

let lastEqual x f n = lastTrue (fun y -> f y = x) n

// (iii)

// firstTrue : (int -> bool) -> int -> int
let firstTrue f n =
    let rec firstTrueCounter' f n counter =
        match n with
        | n when n < 0 -> -1
        | n when n = counter -> -1
        | n when f (counter) -> counter
        | _ -> firstTrueCounter' f n (counter + 1)

    firstTrueCounter' f n 0

// (iv)

// If  lastTrue (fun x -> f x > f (x + 1)) 100  evaluates to -1,
// what can you say about  f?

(*
ANSWER 2(iv)(a) HERE:
    That in the range of 0-99 the function f always returns a
    larger value for a larger input.
    x + 1 will always return a larger value than x.
*)

// How about if  lastTrue f 100 = firstTrue f 100  is  true?

(*
ANSWER 2(iv)(b) HERE:
    It means there is only one value in the range 0-99 that returns true for function f.
    Only one right answer.
*)


// Problem 3

// repeat_map : ('a -> 'a) -> 'a list -> 'a list

let rec repeat_map f xs =
    match xs with
    | [] -> []
    | x :: xs -> f x :: List.map f (repeat_map f xs)

// Problem 4

// (i)

// sum_some : int option list -> int

let rec sum_some xs =
    match xs with
    | [] -> 0
    | None :: xs -> sum_some xs
    | Some x :: xs -> x + sum_some xs

// (ii)

let sum_some2 xs =
    List.fold
        (fun s o ->
            match o with
            | None -> s
            | Some o -> o + s)
        0
        xs

// (iii)  (uncomment the definition below when you've completed it)

let sum_some3 xs =
    let f o =
        match o with
        | None -> 0
        | Some x -> x

    List.fold (+) 0 (List.map f xs)


// Problem 5

type 'a nelist =
    | One of 'a
    | Cons of 'a * 'a nelist


// (i)

// ne_product : int nelist -> int

let rec ne_product nelist =
    match nelist with
    | One x -> x
    | Cons (x, n) -> x * ne_product n

// (ii)

// ne_append : 'a nelist -> 'a nelist -> 'a nelist

let rec ne_append nelist nelist2 =
    match nelist with
    | One x -> Cons(x, nelist2)
    | Cons (x, n) -> Cons(x, ne_append n nelist2)

// (iii)

// to_list : 'a nelist -> 'a list

// let to_list (xs: 'a nelist): 'a list = failwith "to implement"

let rec to_list nelist =
    match nelist with
    | One x -> [ x ]
    | Cons (x, n) -> x :: to_list n

// (iv)

// ne_map : ('a -> 'b) -> 'a nelist -> 'b nelist

let rec ne_map f nelist =
    match nelist with
    | One x -> One(f x)
    | Cons (x, n) -> Cons(f x, ne_map f n)

// (v)

// to_pair : 'a nelist -> 'a * 'a list

let to_pair xs =
    match xs with
    | One x -> (x, [])
    | Cons (x, xs) -> (x, to_list xs)

// from_pair : 'a * 'a list -> 'a nelist

let rec from_pair xs =
    match xs with
    | (x, []) -> One x
    | (x, y :: ys) -> Cons(x, from_pair (y, ys))

// (vi)

// Is it possible to write a function  from_list : 'a list -> 'a nelist
// such that the expressions  to_list (from_list xs) = xs
// and  from_list (to_list ys) = ys  evaluate to  true?
// Explain why.

(*
ANSWER 5(vi) HERE:
    This is quite possible just as above to_pair and from_pair do the opposite thing.
    This is pobbible because to_list cancels out from_list and from_list cancels out
    to_list so you would always end up with the same list you started with (xs or ys).

    We tried to implement this and it worked:

    let rec from_list xs =
        match xs with
        | [ x ] -> One x
        | x :: xs -> Cons(x, from_list xs)

*)


// Problem 6

type product_tree =
    { value: int
      children: product_tree list
      product: int option }

// (i)

// are_same : product_tree -> product_tree -> bool

let rec are_same xs ys =
    match xs, ys with
    | xs,ys when xs.value <> ys.value -> false
    | xs,ys when xs.children = ys.children -> true  //If empty list
    | xs,ys -> List.forall2 are_same xs.children ys.children



// (ii)

// get_product : product_tree -> int

let helper o =
    match o with
    | Some x -> x

let rec get_product xs =
    match xs with
    | xs when xs.product <> None -> helper xs.product
    | xs -> xs.value * List.fold (fun acc o -> acc * get_product o ) 1 xs.children

// (iii)

// fill_products : product_tree -> product_tree
let rec fill_products xs = 
    {value = xs.value; children = List.map fill_products xs.children; product = Some (get_product xs)}
