load ../../harness

@test "ccbeadbe70b0" {
  check 'x   := y + -4   ;
 
VG  :=   3     *     x    ' '⇒ skip; VG := (3*x), {x → -4}
⇒ VG := (3*x), {x → -4}
⇒ skip, {VG → -12, x → -4}'
}
