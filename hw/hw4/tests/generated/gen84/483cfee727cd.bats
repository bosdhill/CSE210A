load ../../harness

@test "483cfee727cd" {
  check 'if (2    *  x   <-3* z ∨  false)   then 

z    :=    x    +     4 else skip  ' '⇒ skip, {}'
}
