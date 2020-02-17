load ../../harness

@test "60c55cef4a4f" {
  check 'if (y   +     -3  = 4 *    y   ∨    false) then  y :=-1  -   -3 else  skip' '⇒ skip, {}'
}
