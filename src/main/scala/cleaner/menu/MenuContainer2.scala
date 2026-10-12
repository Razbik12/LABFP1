package cleaner.menu

// Подменю: содержит обычные пункты или другие подменю
// например карантин, восстановить обьект и другие
case class MenuContainer2(name: String, content: Seq[MenuNode]) extends MenuNode:
  override def show: String = name
