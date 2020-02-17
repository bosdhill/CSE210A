load ../../harness

@test "586a52fee9de" {
  check 'if (false  ∨-1*     -3     < -3* -2)      then 

skip    else z:=    0 * z     ' '⇒ skip, {}'
}
