load ../../harness

@test "3c2c7cf91976" {
  check 'if (¬(x   <    -4* 3))     then 
y :=   y -     z    else skip     ' '⇒ y := (y-z), {}
⇒ skip, {y → 0}'
}
