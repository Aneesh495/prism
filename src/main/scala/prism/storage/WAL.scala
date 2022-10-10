package prism.storage

import java.io._
import java.nio.ByteBuffer
import java.util.zip.CRC32
import prism.core.data.{Datum, Tuple}
import prism.core.lattice.Timestamp
import scala.collection.mutable.ArrayBuffer

import scala.compiletime.uninitialized

final case class WalRecord(
  epoch: Long,
  predicate: String,
  tuple: Tuple,
  weight: Long
)

/**
 * Append-only Write-Ahead Log (WAL) with CRC32 integrity checksums.
 * Ensures crash fault tolerance and deterministic mutation replay.
 */
final class WAL(val file: File) extends AutoCloseable {
  private val magic = 0x50524953 // "PRIS" in ASCII
  private val version = 1

  private var out: DataOutputStream = uninitialized
  ensureInitialized()

  private def ensureInitialized(): Unit = {
    if (!file.exists()) {
      val parent = file.getParentFile
      if (parent != null && !parent.exists()) {
        parent.mkdirs()
      }
      val fos = new FileOutputStream(file)
      val dos = new DataOutputStream(fos)
      dos.writeInt(magic)
      dos.writeInt(version)
      dos.flush()
      fos.close()
    }
    out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file, true)))
  }

  /**
   * Appends a mutation record with CRC32 checksum validation.
   */
  def append(record: WalRecord): Unit = synchronized {
    val baos = new ByteArrayOutputStream()
    val dos = new DataOutputStream(baos)

    dos.writeLong(record.epoch)
    dos.writeUTF(record.predicate)
    dos.writeInt(record.tuple.arity)
    for (i <- 0 until record.tuple.arity) {
      writeDatum(dos, record.tuple(i))
    }
    dos.writeLong(record.weight)
    dos.flush()

    val payload = baos.toByteArray
    val crc = new CRC32()
    crc.update(payload)
    val checksum = crc.getValue

    out.writeInt(payload.length)
    out.writeLong(checksum)
    out.write(payload)
    out.flush()
  }

  /**
   * Replays all valid records from the WAL, verifying CRC32 checksums.
   */
  def replay(): Seq[WalRecord] = synchronized {
    out.flush()
    val records = ArrayBuffer[WalRecord]()
    if (!file.exists()) return Nil

    val fis = new FileInputStream(file)
    val dis = new DataInputStream(new BufferedInputStream(fis))

    try {
      val readMagic = dis.readInt()
      val readVer = dis.readInt()
      require(readMagic == magic, s"Invalid WAL magic bytes: $readMagic")
      require(readVer == version, s"Unsupported WAL version: $readVer")

      while (dis.available() > 0) {
        val payloadLen = dis.readInt()
        val expectedCrc = dis.readLong()
        val payload = new Array[Byte](payloadLen)
        dis.readFully(payload)

        val crc = new CRC32()
        crc.update(payload)
        val computedCrc = crc.getValue
        if (computedCrc != expectedCrc) {
          throw new IOException(s"WAL CRC32 corruption detected! Expected $expectedCrc, computed $computedCrc")
        }

        val recordDis = new DataInputStream(new ByteArrayInputStream(payload))
        val epoch = recordDis.readLong()
        val predicate = recordDis.readUTF()
        val arity = recordDis.readInt()
        val data = new Array[Datum](arity)
        for (i <- 0 until arity) {
          data(i) = readDatum(recordDis)
        }
        val weight = recordDis.readLong()
        records += WalRecord(epoch, predicate, Tuple.fromArray(data), weight)
      }
    } catch {
      case _: EOFException => ()
    } finally {
      dis.close()
      fis.close()
    }

    records.toSeq
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
      case other => throw new IOException(s"Unknown datum type tag: $other")
    }
  }

  def close(): Unit = synchronized {
    if (out != null) {
      out.flush()
      out.close()
    }
  }
}
