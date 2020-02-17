load ../../harness

@test "4b4dba41a107" {
  check 'z     :=    j3+    z   -4  ; 
 x   :=   -2   +z ' '⇒ skip; x := (-2+z), {z → -4}
⇒ x := (-2+z), {z → -4}
⇒ skip, {x → -6, z → -4}'
}
