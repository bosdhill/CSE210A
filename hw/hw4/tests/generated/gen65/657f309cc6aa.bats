load ../../harness

@test "657f309cc6aa" {
  check 'if (¬false)    then  My:= x  -     z else  skip   ' '⇒ My := (x-z), {}
⇒ skip, {My → 0}'
}
