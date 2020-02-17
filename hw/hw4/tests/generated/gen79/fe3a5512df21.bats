load ../../harness

@test "fe3a5512df21" {
  check 'x   :=2  * z ' '⇒ skip, {x → 0}'
}
