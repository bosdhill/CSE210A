load ../../harness

@test "1edc4d35c7ec" {
  check 'if (x+   x <    4     +    b ∧    false) then 
y:=    y *    -1  else  skip     ' '⇒ skip, {}'
}
