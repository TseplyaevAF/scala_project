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

  private val filename = "flask.log"
  private val buffSource = Source.fromFile(filename)
  private val MAX_COUNT = 11614694
  private val strCount = MAX_COUNT  // max кол-во выводимых строк из входного файла
  private var strNum = 0 // для остановки чтения файла

  private var ipAddrMap: Map[String, Int] = Map()
  private var loginMap: Map[String, Int] = Map()

  private val regex = """.*:(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3}).*""".r
  private val regexLogin = """\?login=(\w*)&""".r.unanchored

  breakable {
    for (line <- buffSource.getLines) {
//      println(line)
      strNum += 1
      // TODO: для подчета использовать мультимножества
      //  (вроде как сделал это)
      line match {
        case regexLogin(login) =>
          loginMap = this.inc(loginMap, login)
        case regex(ip1, ip2, ip3, ip4) =>
          val ipAddr = ip1 + '.' + ip2 + '.' + ip3 + '.' + ip4
          ipAddrMap = this.inc(ipAddrMap, ipAddr)
        case _ =>
      }
      if (strNum == strCount) {
        break
      }
    }
  }
  buffSource.close

  private val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("results.txt")))
  writer.write("Кол-во читемых строк: " + strCount + "\n")
  writer.write("--------------------------------------------------------------\n")
  for ((k, v) <- ipAddrMap) writer.write(s"ip-адрес: $k, Кол-во вхождений: $v\n")
  writer.write("--------------------------------------------------------------\n")
  for ((k, v) <- loginMap) writer.write(s"login: $k, Кол-во вхождений: $v\n")
  writer.close()
}
