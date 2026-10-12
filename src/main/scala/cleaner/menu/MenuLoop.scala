package cleaner.menu

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal
// управляет меню: показывает пункты и читает номер от пользователя
// если выбран MenuLeaf, выполняет его действие, например восстановление файла
// если выбран MenuContainer2, открывает вложенное меню
// предыдущие меню хранит в prevs, чтобы можно было вернуться назад
// 0 означает назад, а в главном меню — выход
// при неправильном вводе показывает сообщение и снова ждёт выбор

class MenuLoop(
  onError: Throwable => Unit = error => println("Ошибка: " + error.getMessage),
  onInvalid: () => Unit = () => ()
):
  def run(root: MenuContainer2): Unit =
    menuLoop(root, Seq.empty)

  @tailrec
  private def menuLoop(current: MenuContainer2, prevs: Seq[MenuContainer2]): Unit =
    println("\n" + current.show)

    for ((item, index) <- current.content.zipWithIndex)
      println(s"${index + 1}. ${item.show}")

    // «Назад» не добавляется в content и не занимает номер последнего пункта
    if prevs.isEmpty then
      println("0. Выход")
    else
      println("0. Назад")

    print("Введите номер: ")
    val input = StdIn.readLine()

    // Если ввод закрыт, завершаем работу, а не повторяем чтение бесконечно
    if input == null then
      return

    Try(input.trim.toInt) match
      case Failure(_) =>
        println("Нужно ввести целое число.")
        onInvalid()
        menuLoop(current, prevs)

      case Success(0) =>
        if prevs.nonEmpty then
          menuLoop(prevs.last, prevs.dropRight(1))
        // Если prevs пуст, это главное меню. Метод просто заканчивается

      case Success(choice) =>
        if choice < 1 || choice > current.content.size then
          println("Пункта с таким номером нет.")
          onInvalid()
          menuLoop(current, prevs)
        else
          current.content(choice - 1) match
            case next: MenuContainer2 =>
              menuLoop(next, prevs.appended(current))

            case MenuLeaf(_, action) =>
              try
                action()
              catch
                case _: java.io.EOFException => return
                case NonFatal(error) => onError(error)
              menuLoop(current, prevs)
