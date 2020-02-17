load ../../harness

@test "f8ae9956e74f" {
  check 'if (-1  =     x -   x    ∧ true) then 
 
 x   := x    +3    else skip   ' '⇒ skip, {}'
}
