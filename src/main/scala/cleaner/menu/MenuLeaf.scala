package cleaner.menu

// один пункт меню, хранит название и действие при выборе
// например пункт про карантин хранит вызов moveSelected()
// сам меню не выводит, это делает MenuLoop
case class MenuLeaf(name: String, action: () => Unit) extends MenuNode:
  // возвращаем название пункта, чтобы MenuLoop мог его показать
  override def show: String = name