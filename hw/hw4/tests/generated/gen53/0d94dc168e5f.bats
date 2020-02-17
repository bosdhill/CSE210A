load ../../harness

@test "0d94dc168e5f" {
  check 'if (z    +     x   < -1   *   -3) then     z   :=     y     *    y    else  
 skip' '⇒ z := (y*y), {}
⇒ skip, {z → 0}'
}
