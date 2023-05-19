//import Config.{name => prefix}
//
//object main extends App {
//  def greeting(name: String) {
//    println(prefix + name)
//  }
//  val name = "Oleg"
//  greeting(name)
//}
//
//
import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}
import scala.io.Source
import scala.util.control.Breaks._


object main extends App {
  /**
   * Задать кол-во повторений ключа key во множестве map
   */
  private def inc(map: Map[String, Int], key: String): Map[String, Int]  = {
    if (!map.contains(key)) {
      // если элемента с таким key нет во множестве, то добавляем его
      map + (key -> 1)
    } else {
      // иначе берем текущее кол-во вхождений этого key и увеличиваем на 1
      map + (key -> (map(key) + 1))
    }
  }

  /**
   * Записать данные из коллекции map в файл с указанием кол-ва обработанных строк
   */
  private def writeToFile(map: Map[String, Int], filename: String, strCount: Int, label: String): Unit = {
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename, true)))
    writer.write("Кол-во читемых строк: " + strCount + "\n")
    writer.write("--------------------------------------------------------------\n")
    for ((k, v) <- map) writer.write(s"$label: $k, Кол-во вхождений: $v\n")
    writer.write("\n\n")
    writer.close()
  }

  private var filename = "flask.log"
  private var filenameRes = "results.txt"
  private val MAX_COUNT = 11614694
  private val strCount = MAX_COUNT  // max кол-во выводимых строк из входного файла
  private var strNum = 0 // для остановки чтения файла

  if (args.length != 0) {
    filename = args(0)
    filenameRes = args(1)
  }
  private val buffSource = Source.fromFile(filename)

  private var ipAddrMap: Map[String, Int] = Map()
  private var loginMap: Map[String, Int] = Map()

  // .r - преобразование строки в экземпляр класса Regex
  private val regex = """.*:(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3}).*""".r
  // .unanchored - чтобы не вся стройка соответствовала регулярному выражению
  private val regexLogin = """\?login=(\w*)&""".r.unanchored

  // Scala не содержит концепции оператора break (в версиях выше 2.8),
  // вместо оператора break используется соответствующий метод,
  // который импортируется из пакета scala.util.control.Breaks._

  // breakable определяет зону, в которой будет вызван метод break
  breakable {
    for (line <- buffSource.getLines()) {
      strNum += 1
      // match - Сопоставление с примером (Pattern matching)
      // (аналог switch в java)
      line match {
        // При вызове regexLogin и regex, внутри них вызывается метод unapply,
        // который принимает строку и пытается извлечь и вернуть строку,
        // соответствующую регулярному выражению
        case regexLogin(login) =>
          loginMap = this.inc(loginMap, login)
        // Используя круглые скобки можно объединять сразу несколько групп регулярных выражений
        // Соответственно будет извлечено столько параметров, сколько указано групп
        case regex(ip1, ip2, ip3, ip4) =>
          val ipAddr = ip1 + '.' + ip2 + '.' + ip3 + '.' + ip4
          ipAddrMap = this.inc(ipAddrMap, ipAddr)
        // если не удалось сопоставить ни одно значение с существующими case'ми...
        case _ => // ничего не делаем
      }
      if (strNum == strCount) {
        break
      }
    }
  }
  buffSource.close

  this.writeToFile(loginMap, filenameRes, strCount, "login")
  this.writeToFile(ipAddrMap, filenameRes, strCount, "ip-адрес")
}
