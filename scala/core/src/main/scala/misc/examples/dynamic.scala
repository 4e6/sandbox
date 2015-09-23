package misc.examples

case class Key(name: String, code: Int)

sealed trait Version
sealed trait v12 extends Version
sealed trait v15_2 extends Version

sealed trait KeyLayout[T <: Version] extends Dynamic {

  def keys: Set[Key]

  def selectDynamic(name: String): Option[Key] =
    keys.find(_.name == name)
}

object KeyLayout {

  def apply[V <: Version: KeyLayout] = implicitly[KeyLayout[V]]

  implicit object `12` extends KeyLayout[v12] {
    val keys = Set(Key("KEY_HOME", 0x007a))
  }

  implicit object `15_2` extends KeyLayout[v15_2] {
    val keys = Set(Key("KEY_HOME", 0x003a))
  }
}

object Test1 {

  val x = KeyLayout[v15_2].KEY_HOME
}
