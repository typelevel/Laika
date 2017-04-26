package laika.parse.core

import java.util.WeakHashMap

/**
 * @author Tomáš Janoušek
 */
private[core] trait PositionCache {
  private lazy val indexCacheTL =
    // not DynamicVariable as that would share the map from parent to child :-(
    new ThreadLocal[java.util.Map[CharSequence, Array[Int]]] {
      override def initialValue = new WeakHashMap[CharSequence, Array[Int]]
    }

  private[core] def indexCache = indexCacheTL.get
}
