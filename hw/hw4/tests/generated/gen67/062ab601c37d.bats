load ../../harness

@test "062ab601c37d" {
  check 'if (true    ∨ true)     then 
y:=  3     +   C2    else 
   skip   ' '⇒ y := (3+C2), {}
⇒ skip, {y → 3}'
}
