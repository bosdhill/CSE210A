load ../../harness

@test "4cf03aa88cc3" {
  check 'if (x *  1   <   y-3)     then skip    else skip     ' '⇒ skip, {}'
}
