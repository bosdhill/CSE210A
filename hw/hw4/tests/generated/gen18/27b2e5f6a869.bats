load ../../harness

@test "27b2e5f6a869" {
  check 'if (false∧    true)      then 
 
skip    else    y    :=-1  *  1   ' '⇒ y := (-1*1), {}
⇒ skip, {y → -1}'
}
