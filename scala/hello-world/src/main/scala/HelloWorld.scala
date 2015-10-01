object HelloWorld {
  
  def hello(): String = hello("World")

  def hello(name: String): String = s"Hello, $name!"
}
