package poker.std

import poker.MyList
import poker.Option

case class ListMap[K, V](ns: MyList[(K, V)]) extends Map[K, V] {

  override def get(key: K): Option[V] =
    ns.find(k => k._1 == key).map(_._2)

  override def getOrElse[VV >: V](key: K, default: => VV): VV =
    get(key).getOrElse(default)

  override def +[VV >: V](kv: (K, VV)): Map[K, VV] =
    ListMap(kv :: ns.filter(k => k._1 != kv._1))

  override def -(key: K): Map[K, V] =
    ListMap(ns.filter(k => k._1 != key))

  override def values: MyList[V] =
    ns.map(k => k._2)

  override def keys: MyList[K] =
    ns.map(k => k._1)

  override def toList: MyList[(K, V)] = ns
}
