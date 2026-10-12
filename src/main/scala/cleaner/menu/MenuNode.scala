package cleaner.menu
//у каждого пункта меню должен быть метод show, который возвращает название
trait MenuNode:
  def show: String
