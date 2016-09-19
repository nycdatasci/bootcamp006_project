import org.apache.spark.graphx._
import org.apache.spark.rdd.RDD

import scala.util.MurmurHash
import org.apache.spark.graphx.GraphLoader
import org.apache.spark.sql.SparkSession


object GraphxExperiment {
  def main(args: Array[String]): Unit = {
    // Creates a SparkSession.
    val spark = SparkSession
      .builder
      .appName(s"${this.getClass.getSimpleName}")
      .getOrCreate()
    val sc = spark.sparkContext


    val vertexArray = Array(
      (1L, ("Alice", 28)),
      (2L, ("Bob", 27)),
      (3L, ("Charlie", 65)),
      (4L, ("David", 42)),
      (5L, ("Ed", 55)),
      (6L, ("Fran", 50))
      )
    val edgeArray = Array(
      Edge(2L, 1L, 7),
      Edge(2L, 4L, 2),
      Edge(3L, 2L, 4),
      Edge(3L, 6L, 3),
      Edge(4L, 1L, 1),
      Edge(5L, 2L, 2),
      Edge(5L, 3L, 8),
      Edge(5L, 6L, 3)
      )

    val vertexRDD: RDD[(Long, (String, Int))] = sc.parallelize(vertexArray)
    val edgeRDD: RDD[Edge[Int]] = sc.parallelize(edgeArray)

    val graph: Graph[(String, Int), Int] = Graph(vertexRDD, edgeRDD)

    println(s"===========================")

    // (1)
    // display the names of the users that are at least 30 years old
    graph.vertices.filter { case (id, (name, age)) => age > 30 }.collect.foreach {
      case (id, (name, age)) => println(s"$name is $age")
    }

    // (2)
    // Use the graph.triplets view to display who likes who
    for (triplet <- graph.triplets.collect) {
      println(s"${triplet.srcAttr._1} likes ${triplet.dstAttr._1}")
    }


    val inDegrees: VertexRDD[Int] = graph.inDegrees

    // Define a class to more clearly model the user property
    case class User(name: String, age: Int, inDeg: Int, outDeg: Int)
    // Create a user Graph
    val initialUserGraph: Graph[User, Int] = graph.mapVertices{ case (id, (name, age)) => User(name, age, 0, 0) }

    // Fill in the degree information
    val userGraph = initialUserGraph.outerJoinVertices(initialUserGraph.inDegrees) {
      case (id, u, inDegOpt) => User(u.name, u.age, inDegOpt.getOrElse(0), u.outDeg)
    }.outerJoinVertices(initialUserGraph.outDegrees) {
      case (id, u, outDegOpt) => User(u.name, u.age, u.inDeg, outDegOpt.getOrElse(0))
    }


    // (3)
    // we restrict our graph to the users that are 30 or older.
    val olderGraph = userGraph.subgraph(vpred = (id, user) => user.age >= 30)
    // compute the connected components
    val cc = olderGraph.connectedComponents
    // display the component id of each user:
    olderGraph.vertices.leftJoin(cc.vertices) {
      case (id, user, comp) => s"${user.name} is in component ${comp.get}"
    }.collect.foreach{ case (id, str) => println(str) }


    spark.stop()
  }
}
// scalastyle:on println
