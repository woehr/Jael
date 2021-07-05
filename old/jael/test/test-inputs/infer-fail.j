test1 : forall a. a -> Bool ;
$test1 = \(_) -> 5; // ErrorType


// "{ x=1, x=true }" `shouldNotHaveType` "{ x: Bool, x: Int }"
// "{ y=true, x=1, y=0, x=false}" `shouldNotHaveType` "{ x: Bool, x:Int, y:Int, y:Bool }"

// "should terminate (and not unify)" $ do
// shouldNotUnify "\\($r) -> if true then { x=1 | r } else { y = 2 | r }"
// shouldNotUnify "\\($r) -> if true then { a=1, x=1, z=1 | r } else { a=2, y=2, z=2 | r }"

// "should typecheck multi-if expressions" $ do
// shouldNotUnify "if | true then 0 | else true"
// shouldNotUnify "if | true then 0 | false then false"
// shouldNotUnify "if | 1 then 0 | false then 0"
