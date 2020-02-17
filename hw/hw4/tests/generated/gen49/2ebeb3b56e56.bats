load ../../harness

@test "2ebeb3b56e56" {
  check 'if (false     ∧  x- y     =    x  + zl)      then y :=-2 +     3      else    x  :=-2*  -1  ' '⇒ x := (-2*-1), {}
⇒ skip, {x → 2}'
}
