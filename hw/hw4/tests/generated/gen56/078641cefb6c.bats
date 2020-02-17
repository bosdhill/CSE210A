load ../../harness

@test "078641cefb6c" {
  check 'if (0 +   1  <  0    *-3     ∧    false) then     skip    else 
  skip     ' '⇒ skip, {}'
}
