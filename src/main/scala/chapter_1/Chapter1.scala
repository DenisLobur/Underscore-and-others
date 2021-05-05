package chapter_1

import poker._

import scala.io.StdIn.readInt
import scala.math.BigDecimal.RoundingMode.HALF_UP

object Chapter1 {
  def main(args: Array[String]): Unit = {
    val list = 1::2::3::4::Nill
    println(list.forAll(_ < 10))

    println(normalDistribution(1.0, 1.0, 1.0))
    println(crispsWeight(90.0, 0.9, 0.1))
    getOnes()
  }

  def normalDistribution(mu: Double, sigma: Double, x: Double): Double = {
    val denom = sigma * scala.math.sqrt(2 * scala.math.Pi)
    val first = 1 / denom
    val second = scala.math.exp(-1 / 2 * (math.pow(x-mu / sigma, 2)))
    first * second
  }

  def crispsWeight(weight: BigDecimal, potatoWaterRatio: Double, crispsWaterRatio: Double): BigDecimal = {
    val x: BigDecimal = weight / BigDecimal.valueOf(potatoWaterRatio) * BigDecimal.valueOf(crispsWaterRatio)
    x.setScale(5, HALF_UP)
  }

  def getOnes(): Unit = {
    val x = readInt()
    val i = x.toBinaryString.replaceAll("0", "").length

    println(i)
  }

}
