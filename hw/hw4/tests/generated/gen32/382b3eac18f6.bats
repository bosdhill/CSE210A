load ../../harness

@test "382b3eac18f6" {
  check 'while -1  - 4 =x  *    -3    ∨  false   do 
    y:=   -3 +  y ' '⇒ skip, {}'
}
