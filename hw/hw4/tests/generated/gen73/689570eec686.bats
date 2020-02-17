load ../../harness

@test "689570eec686" {
  check 'if (¬(y* -2  <    -2- 3))    then x :=    SU     +y      else 
skip  ' '⇒ x := (SU+y), {}
⇒ skip, {x → 0}'
}
