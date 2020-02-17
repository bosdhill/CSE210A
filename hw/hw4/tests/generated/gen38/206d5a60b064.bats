load ../../harness

@test "206d5a60b064" {
  check 'if (4     <  4+ 1     ∨    1     -   x < 2   +     1)   then  
  rQ    :=    y   -    4      else skip    ' '⇒ rQ := (y-4), {}
⇒ skip, {rQ → -4}'
}
