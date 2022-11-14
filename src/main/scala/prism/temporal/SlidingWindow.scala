package prism.temporal

import prism.core.lattice.Timestamp

final case class TimeWindow(start: Long, end: Long) extends Ordered[TimeWindow] {
  def contains(time: Long): Boolean = time >= start && time < end
  def compare(that: TimeWindow): Int = java.lang.Long.compare(this.start, that.start)
  override def toString: String = s"[$start, $end)"
}

sealed trait WindowDef extends Serializable {
  def assignWindows(time: Long): Seq[TimeWindow]
}

final case class TumblingWindow(duration: Long) extends WindowDef {
  require(duration > 0, "Window duration must be > 0")

  def assignWindows(time: Long): Seq[TimeWindow] = {
    val start = (time / duration) * duration
    Seq(TimeWindow(start, start + duration))
  }
}

final case class SlidingWindow(size: Long, slide: Long) extends WindowDef {
  require(size > 0 && slide > 0, "Size and slide must be > 0")

  def assignWindows(time: Long): Seq[TimeWindow] = {
    val lastStart = (time / slide) * slide
    val firstStart = math.max(0L, lastStart - size + slide)
    val windows = collection.mutable.ArrayBuffer[TimeWindow]()
    var start = firstStart
    while (start <= lastStart) {
      if (time >= start && time < start + size) {
        windows += TimeWindow(start, start + size)
      }
      start += slide
    }
    windows.toSeq
  }
}

final case class SessionWindow(inactivityGap: Long) extends WindowDef {
  require(inactivityGap > 0, "Inactivity gap must be > 0")

  def assignWindows(time: Long): Seq[TimeWindow] = {
    Seq(TimeWindow(time, time + inactivityGap))
  }
}
