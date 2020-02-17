load ../../harness

@test "7534a673b384" {
  check 'if (3    *  x= z)     then skip      else  y   :=  -4  ' '⇒ skip, {}'
}
