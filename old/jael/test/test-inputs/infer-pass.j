data Unit = unit ;
data Bool = false | true ;
data Maybe(a) = nothing | just(a) ;
data Either(a, b) = left(a) | right(b) ;
data UpToThree = zero(Int) | one(Int) | two(Int) | three(Int) ;

 test0 : Bool ;
$test0 = true;

 test1 : Int ;
$test1 = 42 ;

 test2 : [Int; 4] ;
$test2 = [1,2,3,4] ;

 test3: Unit ;
$test3 = unit ;

 test4: {} ;
$test4 = {} ;

 test5: { foo: Int } ;
$test5 = {foo=42} ;

 test6 : Int ;
$test6 = 1+2 ;

 test7 : Int ;
$test7 = case true of { false -> 0; $x -> 1 } ;

 test8 : Maybe(Int) -> Int ;
$test8 = \($x) -> case x of { nothing -> 0; just($y) -> y } ;

 test9 : forall a. a -> a ;
$test9 = \($x) -> case x of { $y -> y; _ -> x } ;

 test10 : Int ;
$test10 = if true then 1 else 2 ;

 test11 : Int ;
$test11 = if { true -> 0 ; false -> 1 ; true -> 2 } ;

 test12 : Bool -> Int ;
$test12 = \case { true -> 0; false -> 1 } ;

 test13 : Int -> Int ;
$test13 = \case { 1 -> 0; 2 -> 1; $x -> x } ;

 test14 : forall a. Maybe(a) -> a ;
$test14 = \case { just($x) -> x } ;

 test15 : forall a b. (a -> b) -> a -> b ;
$test15 = \($f) -> \case { $x -> f(x) } ;

 test16 : forall a. a -> Maybe(a) ;
$test16 = just ;

 test17 : forall a. a -> a ;
$test17 = \($a) -> a ;

 test18 : forall a. a->Int ;
$test18 = \($a) -> 1 ;

 test19 : forall a b. (a,b) -> ((a,b),a,b) ;
$test19 = \( $a@($b, $c) ) -> (a, b, c) ;

 test20 : forall a. (a,a,Int)->a ;
$test20 = \(($a,_,1) || (_,$a,_)) -> a ;
 
 test21 : Int ;
$test21 = let { $x = 1 } in x ;

 test22 : Maybe(Int) ;
$test22 = let { $x = just } in x(1) ;

 test23 : forall a. Maybe(a) ;
$test23 = let { $x = nothing } in x ;

 test24 : forall a b. (Maybe(a), Maybe(b)) ;
$test24 = let { $x@($y) = nothing } in (x,y) ;

 test25 : ([Int; 3], Int, Int) ;
$test25 = let { $x@[_,$y,$z] = [1, 2, 3] } in (x,y,z) ;

 test26 : Int ;
$test26 = let { $x = 1+1 } in x ;

 test27 : Maybe(Int) ;
$test27 = let { $x@(nothing) = just(1) } in x ;

 test28 : Maybe(Int) ;
$test28 = let { $x@(just(1)) = nothing } in x ;

 test29 : forall a. a ;
$test29 = let { just($y) = nothing } in y ;

 test30 : Int ;
$test30 = let { just($x) = just(1) } in x ;

// Order of labels does not matter, labels are presented sorted
 test31 : forall a. { x:Int, y:Bool, z:Maybe(a) } ;
$test31 = { z=nothing, y=true, x=1 } ;

// Order of the types of x do matter
 test32 : forall a. { x: Int, x: Bool, x: Maybe(a) } ;
$test32 = { x=1, x=true, x=nothing } ;

// Order of x's and y's types matter only amongst labels of the same name
 test33 : { x:Int, x:Bool, y:Bool, y:Int } ;
$test33 = { y=true, x=1, y=0, x=false} ;

 test34 : forall r. {r}-> {x:Int|r} ;
$test34 = \($x) -> { x = 1 | x } ;

// infer record types where unification is necessary
 test35 : forall a. { a:Maybe(a) } ;
$test35 = let { {y=4, z=$y | _} = {x=1,y=2,y=just,z=nothing} }
          in  { a=y } ;

 test36 : forall a b. { a:Maybe(a), x:Int, y:b->Maybe(b) } ;
$test36 = let { {y=4, z=$y | $r} = {x=1,y=2,y=just,z=nothing} }
          in  { a=y|r } ;

 test37 : forall a b c.
          { a: { x:Int, y:Int, y:a->Maybe(a), z:Maybe(b) }
          , b: { x:Int, y:c->Maybe(c)}
          } ;
$test37 = let { $r@{y=4, z=$y | $s} = {x=1,y=2,y=just,z=nothing} }
          in  {a=r, b=s} ;

 test38 : forall a r. {x:a|r} -> a ;
$test38 = \($r) -> r.x ;

 test39 : forall a r. {x:a|r} -> {r} ;
$test39 = \($r) -> r \ x ;

 test40: {} ;
$test40 = {foo=42} \ foo ;

 test41 : forall a b r. {x:a|r} -> b -> {x:b|r} ;
$test41 = \($r, $a) -> { x=a | r \ x} ;

 test42 : forall a r. {x:a|r} -> {y:a|r} ;
$test42 = \($r) -> { y=r.x | r\x} ;

 test43 : forall a b r. {x:a|r} -> b -> {x:b|r} ;
$test43 = \($r, $a) -> { x:=a | r} ;

 test44 : forall a r. {x:a|r} -> {y:a|r} ;
$test44 = \($r) -> { y <- x | r} ;
