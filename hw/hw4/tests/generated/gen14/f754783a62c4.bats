load ../../harness

@test "f754783a62c4" {
  check 'if (3*   z <  0     + y ∨  false)   then  O:=    1  +  2    else  
   skip   ' '⇒ skip, {}'
}
