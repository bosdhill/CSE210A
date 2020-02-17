load ../../harness

@test "dc557a5e29b6" {
  check 'if (¬(z     - z <    -3    -  4))  then 
y   :=   x    +  3   else  skip    ' '⇒ y := (x+3), {}
⇒ skip, {y → 3}'
}
