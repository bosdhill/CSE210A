load ../../harness

@test "dae9b8cf5c15" {
  check 'z:=y   *     y;  
 nx   :=   4* x   ' '⇒ skip; nx := (4*x), {z → 0}
⇒ nx := (4*x), {z → 0}
⇒ skip, {nx → 0, z → 0}'
}
