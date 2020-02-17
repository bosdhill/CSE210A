load ../../harness

@test "b4aec3cbfb7c" {
  check 'z:=    -2  - z  ;

x:=    x     +-1 ' '⇒ skip; x := (x+-1), {z → -2}
⇒ x := (x+-1), {z → -2}
⇒ skip, {x → -1, z → -2}'
}
