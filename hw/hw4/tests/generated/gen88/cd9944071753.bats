load ../../harness

@test "cd9944071753" {
  check 'if (D2 +x   <    zT *    1   ∧-1 -     4   =  x     + 1)   then 
   skip    else y    :=  x    +  x     ' '⇒ y := (x+x), {}
⇒ skip, {y → 0}'
}
