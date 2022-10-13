package prism.storage

import java.io._
import prism.core.data.{Datum, Tuple}
import scala.collection.mutable

/**
 * Snapshot manager for checkpointing and restoring state.
 */
object Snapshot {

  def save(file: File, relations: Map[String, Set[Tuple]]): Unit = {
    val parent = file.getParentFile
    if (parent != null && !parent.exists()) parent.mkdirs()

    val fos = new FileOutputStream(file)
    val dos = new DataOutputStream(new BufferedOutputStream(fos))
    try {
      dos.writeInt(0x534E4150) // "SNAP"
      dos.writeInt(1) // version
      dos.writeInt(relations.size)

      for ((pred, tuples) <- relations) {
        dos.writeUTF(pred)
        dos.writeInt(tuples.size)
        for (tup <- tuples) {
          dos.writeInt(tup.arity)
          for (i <- 0 until tup.arity) {
            writeDatum(dos, tup(i))
          }
        }
      }
      dos.flush()
    } finally {
      dos.close()
      fos.close()
    }
  }

  def load(file: File): Map[String, Set[Tuple]] = {
    if (!file.exists()) return Map.empty

    val fis = new FileInputStream(file)
    val dis = new DataInputStream(new BufferedInputStream(fis))
    val result = mutable.Map[String, Set[Tuple]]()

    try {
      val magic = dis.readInt()
      val ver = dis.readInt()
      require(magic == 0x534E4150, s"Invalid snapshot magic: $magic")
      require(ver == 1, s"Unsupported snapshot version: $ver")

      val numRelations = dis.readInt()
      for (_ <- 0 until numRelations) {
        val pred = dis.readUTF()
        val numTuples = dis.readInt()
        val tupleSet = mutable.Set[Tuple]()
        for (_ <- 0 until numTuples) {
          val arity = dis.readInt()
          val data = new Array[Datum](arity)
          for (i <- 0 until arity) {
            data(i) = readDatum(dis)
          }
          tupleSet += Tuple.fromArray(data)
        }
        result.put(pred, tupleSet.toSet)
      }
    } finally {
      dis.close()
      fis.close()
    }

    result.toMap
  }

  private def writeDatum(dos: DataOutputStream, d: Datum): Unit = {
    d match {
      case Datum.I64(v) =>
        dos.writeByte(1)
        dos.writeLong(v)
      case Datum.F64(v) =>
        dos.writeByte(2)
        dos.writeDouble(v)
      case Datum.Str(v) =>
        dos.writeByte(3)
        dos.writeUTF(v)
      case Datum.Bool(v) =>
        dos.writeByte(4)
        dos.writeBoolean(v)
      case Datum.Sym(v) =>
        dos.writeByte(5)
        dos.writeUTF(v)
      case Datum.NullVal =>
        dos.writeByte(0)
      case Datum.TupleVal(elems) =>
        dos.writeByte(6)
        dos.writeInt(elems.length)
        for (e <- elems) writeDatum(dos, e)
    }
  }

  private def readDatum(dis: DataInputStream): Datum = {
    dis.readByte() match {
      case 0 => Datum.NullVal
      case 1 => Datum.I64(dis.readLong())
      case 2 => Datum.F64(dis.readDouble())
      case 3 => Datum.Str(dis.readUTF())
      case 4 => Datum.Bool(dis.readBoolean())
      case 5 => Datum.Sym(dis.readUTF())
      case 6 =>
        val len = dis.readInt()
        val elems = (0 until len).map(_ => readDatum(dis)).toVector
        Datum.TupleVal(elems)
      case other => throw new IOException(s"Unknown datum type: $other")
    }
  }
}
