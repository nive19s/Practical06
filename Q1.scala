import scala.io.StdIn.{readLine, readInt}

object InventorySystem{

  case class Product(id: Int, name:String, quantity:Int, price:Double)

  val inventory1: Map[Int,Product] = Map(
    1001 -> Product(1001,"Pen",50,10.0),
    1002 -> Product(1002,"Pencil",30,20.0),
    1003 -> Product(1003,"Book",20,30.0)
  )

  val inventory2: Map[Int,Product] = Map(
    1002 -> Product(1002,"Pencil",10,25.0),
    1004 -> Product(1004,"Box",60,15.0)
  )

  def retrieveProductNames(inventory: Map[Int,Product]): List[String] = {
    inventory.values.map(_.name).toList
  }

  def calculateTotalValue(inventory: Map[Int,Product]): Double = {
    inventory.values.map(product => product.quantity * product.price).sum
  }

  def isInventoryEmpty(inventory: Map[Int,Product]): Boolean = {
    inventory.isEmpty
  }

  def mergeInventories(inv1: Map[Int,Product], inv2: Map[Int,Product]): Map[Int,Product] = {
    inv2.foldLeft(inv1) { case (acc, (id, newProduct)) =>
      acc.get(id) match {
        case Some(existingProduct) =>
          acc + (id -> existingProduct.copy(
            quantity = existingProduct.quantity + newProduct.quantity,
            price = math.max(existingProduct.price, newProduct.price)
          ))
        case None => acc + (id -> newProduct)
      }
    }
  }

  def checkProductById(inventory: Map[Int,Product], id: Int): Unit = {
    inventory.get(id) match {
      case Some(product) => println(s"Product is found: $product")
      case None => println(s"Product with ID $id is not found.")
    }
  }

  def main(args : Array[String]): Unit={

  println("Products in inventory1: " + retrieveProductNames(inventory1))
  println("Total value of products in inventory1: " + calculateTotalValue(inventory1))
  println("Is inventory1 empty? " + isInventoryEmpty(inventory1))
  
  val mergedInventory = mergeInventories(inventory1, inventory2)
  println("Merged Inventory: " + mergedInventory)

  checkProductById(inventory1, 1002)
  }
}