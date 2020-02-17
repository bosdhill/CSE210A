load ../../harness

@test "f55930f60746" {
  check 'while false    ∨ z     +     y  < x   -   x    do 
   z     :=y +    y   ' '⇒ skip, {}'
}
