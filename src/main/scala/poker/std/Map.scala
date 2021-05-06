package poker.std

import poker.{MyList, Option}

trait Map[K, +V] {
  def get(key: K): Option[V]

  def getOrElse[VV >: V](key: K, default: => VV): VV

  def +[VV >: V](kv: (K, VV)): Map[K, VV]

  def -(key: K): Map[K, V]

  def values: MyList[V]

  def keys: MyList[K]

  def toList: MyList[(K, V)]

}

object Map {
  def empty[K, V]: Map[K, V] = ListMap(MyList.empty)
}
