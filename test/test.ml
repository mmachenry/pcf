open OUnit

let _ =
    run_test_tt_main
        ("PCF Tests" >::: [
            Read_test.tests
        ])
