load ../../harness

@test "703bf69f4ec2" {
  check 'if (bv  -     4 *   -1 =     -3    *  y∨ -2 *     x =y*  n)  then y    :=     -2  +    -1  else skip   ' '⇒ y := (-2+-1), {}
⇒ skip, {y → -3}'
}
