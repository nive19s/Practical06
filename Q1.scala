import scala.collection.mutable
import scala.io.StdIn.{readLine,readInt}

object inventory_warehouse {
    def main(args: Array[String]): Unit = {
        
        //the data type of products inside
        type Detail_of_products = (String,Int,Double)

        val inventory1 : Map[Int,Detail_of_products] = Map(
            2020 -> ("Product_1", 32,10.00),
            2021 -> ("Product_2", 20,100.00),
            2022 -> ("Product_3",10,5.00),
            2023 -> ("Product_4",40,25.00)
        )

        val inventory2 : Map[Int,Detail_of_products]=Map(
            2021 -> ("Product_2", 30,50.00),
            2025 -> ("Product_54",10,10.00),
        )

        //Q1 product names in inventory 1
        def view_products(n:Map[Int,Detail_of_products]):Unit ={
            println(n.values.map(_._1))
        }

        //Q2 total value of products
        def total_value(m:Map[Int,Detail_of_products]): Double ={
            m.values.map{case (_,quantity,price) => quantity* price}.sum
        }

        //Q3 merging inventory1 and inventory2
        def mergeInventory(inv1: Map[Int,Detail_of_products], inv2: Map[Int,Detail_of_products]): Map[Int, Detail_of_products] = {
            inv2.foldLeft(inv1) { case (mergedInventory, (id, (name, quantity, price))) =>
                mergedInventory.get(id) match {
                    case Some((_, existingQuantity, existingPrice)) =>
                        mergedInventory.updated(id, (name, existingQuantity + quantity, Math.max(existingPrice, price)))
                    case None =>
                        mergedInventory + (id -> (name, quantity, price))
                } 
            }
        }

        def checkInventory(inventSearch:Map[Int,Detail_of_products],Id:Int): Unit = {
            inventSearch.get(Id) match {
                case Some((name,quantity,price)) => println(s"Product is found and Product details: $Id, Name: $name , Quantity: $quantity , Price : $price")
                case None => println(s"Product Id is not exist in inventory1")
            }
        }
        

        view_products(inventory1)
        println(s"The total value of proudcts is : ${total_value(inventory1)}")
        println(s"The inside contains of inventory1 is ${inventory1.isEmpty}")
        println(mergeInventory(inventory1,inventory2))
        checkInventory(inventory1,2022)
        checkInventory(inventory1,2000)
    }   

    
}