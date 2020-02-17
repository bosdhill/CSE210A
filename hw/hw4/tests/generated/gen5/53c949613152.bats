load ../../harness

@test "53c949613152" {
  check 'if (false  ∧     1   --2  <-3-     px)     then 
    skip   else   skip  ' '⇒ skip, {}'
}
