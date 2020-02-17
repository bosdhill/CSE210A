load ../../harness

@test "6bda6d215862" {
  check 'z    :=    y  +  x;

 

y   := x  *  y  ' '⇒ skip; y := (x*y), {z → 0}
⇒ y := (x*y), {z → 0}
⇒ skip, {y → 0, z → 0}'
}
