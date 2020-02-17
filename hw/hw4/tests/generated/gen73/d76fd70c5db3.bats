load ../../harness

@test "d76fd70c5db3" {
  check 'if (false   ∨  x +     x = f1*     -4) then  
  skip else   skip' '⇒ skip, {}'
}
