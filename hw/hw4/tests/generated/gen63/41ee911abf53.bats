load ../../harness

@test "41ee911abf53" {
  check 'if (-2 =   1 *  1     ∧    false)    then w  :=  t+  3     else skip     ' '⇒ skip, {}'
}
