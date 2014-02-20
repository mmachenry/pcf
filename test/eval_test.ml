open OUnit

let tests = "Test eval" >::: [
    "Number" >:: (fun () -> assert_equal (run_program "1") 1)
    ]
"(fn x => x 412) succ"
"if iszero(pred (pred 2)) then succ (pred 413) else 2"
"(fn m => (fn n => if iszero n then n else n)) 0 2"
"(fn mul =>
    (rec fact => (fn n =>
        if iszero n
        then 1
        else mul n (fact (pred n))))
)
(
(fn add =>
    (rec _mul => fn n => fn m =>
        if iszero n
        then 0
        else add m (_mul (pred n) m)))

(rec _add => fn n => fn m =>
    if iszero n
    then m
    else succ(_add (pred n) m))
)
5"

