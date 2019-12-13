type Unit* = tuple[]



func unit* (): Unit =
  ()


func default* (T: typedesc[Unit]): Unit =
  unit()


func default* [T: Unit](): Unit =
  T.default()
