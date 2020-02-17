load ../../harness

@test "be16c804101b" {
  check 'if (x   -  1= ZA    +    z)    then  y  :=    1   *    z     else  y     :=Y   *  3     ' '⇒ y := (Y*3), {}
⇒ skip, {y → 0}'
}
