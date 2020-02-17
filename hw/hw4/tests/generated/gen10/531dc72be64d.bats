load ../../harness

@test "531dc72be64d" {
  check 'x :=     2    -y   ' '⇒ skip, {x → 2}'
}
