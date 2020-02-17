load ../../harness

@test "f6b5c8bbf18c" {
  check 'if (x    <    z-   2 ∧     y    +     -3 =    -3   + 0)    then 
 skip    else z :=    x*y' '⇒ z := (x*y), {}
⇒ skip, {z → 0}'
}
