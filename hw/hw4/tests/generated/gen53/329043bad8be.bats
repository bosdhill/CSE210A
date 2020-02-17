load ../../harness

@test "329043bad8be" {
  check 'if (false  ∨     x  -    4  =     x +    -1) then  skip  else 

skip   ' '⇒ skip, {}'
}
