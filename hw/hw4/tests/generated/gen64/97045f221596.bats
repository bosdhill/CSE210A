load ../../harness

@test "97045f221596" {
  check 'Lv :=  E * 0     ' '⇒ skip, {Lv → 0}'
}
