package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](8)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f, 20f, 22f, 24f, 26f), output, 2)
    //lineOfSight(Array[Float](0f, 1f, 8f, 9f, 20f, 22f, 24f, 26f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f, 5f, 5f, 5f, 5f))
  }

  test("upsweep should correctly compute the tree on the indices 1 until 5 of a 5 element array for threshold 1") {
    val output = new Array[Float](5)
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f, 20f), 1, 5, 1)
    assert(res == Node(Node(Leaf(1,2,1.0F),Leaf(2,3,4.0F)),Node(Leaf(3,4,3.0F),Leaf(4,5,5.0F))))
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f, 20f, 22f, 24f, 26f), 1, 8)
    assert(res == 5f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](8)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f, 20f, 22f, 24f, 26f), output, 0f, 1, 8)
    assert(output.toList == List(0f, 1f, 4f, 4f, 5f, 5f, 5f, 5f))
  }

}

