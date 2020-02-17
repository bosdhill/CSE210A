load ../../harness

@test "94f64999b8a7" {
  check 'if (¬true)    then 

 
skip    else {y     := 1    +   y;z  :=   1 +   -1}    ' '⇒ y := (1+y); z := (1+-1), {}
⇒ skip; z := (1+-1), {y → 1}
⇒ z := (1+-1), {y → 1}
⇒ skip, {y → 1, z → 0}'
}
