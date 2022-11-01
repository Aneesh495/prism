package prism.storage

import java.io.File
import java.nio.file.Files
import munit.FunSuite
import prism.core.data.{Datum, Tuple}

class StorageSuite extends FunSuite {

  test("WAL appends and replays mutations with CRC32 integrity verification") {
    val tempDir = Files.createTempDirectory("prism-wal-test").toFile
    val walFile = new File(tempDir, "test.wal")

    val wal = new WAL(walFile)
    val r1 = WalRecord(1L, "edge", Tuple.of(1L, 2L), 1L)
    val r2 = WalRecord(2L, "node", Tuple.of("alice"), 1L)
    val r3 = WalRecord(3L, "edge", Tuple.of(1L, 2L), -1L) // Retraction

    wal.append(r1)
    wal.append(r2)
    wal.append(r3)
    wal.close()

    // Replay in fresh instance
    val walReplay = new WAL(walFile)
    val replayed = walReplay.replay()
    walReplay.close()

    assertEquals(replayed.length, 3)
    assertEquals(replayed(0), r1)
    assertEquals(replayed(1), r2)
    assertEquals(replayed(2), r3)

    // Cleanup
    walFile.delete()
    tempDir.delete()
  }

  test("Snapshot saves and restores relation states accurately") {
    val tempDir = Files.createTempDirectory("prism-snap-test").toFile
    val snapFile = new File(tempDir, "state.snap")

    val relations = Map(
      "users" -> Set(Tuple.of(1L, "Alice"), Tuple.of(2L, "Bob")),
      "roles" -> Set(Tuple.of(1L, "admin"), Tuple.of(2L, "guest"))
    )

    Snapshot.save(snapFile, relations)
    assert(snapFile.exists())

    val loaded = Snapshot.load(snapFile)
    assertEquals(loaded, relations)

    // Cleanup
    snapFile.delete()
    tempDir.delete()
  }

  test("Catalog registers relation schemas and validates metadata") {
    val catalog = new Catalog()
    val schema = catalog.registerRelation(
      "employee",
      List(
        ColumnDef("id", "i64"),
        ColumnDef("name", "str"),
        ColumnDef("salary", "f64")
      )
    )

    assertEquals(schema.arity, 3)
    assertEquals(catalog.hasRelation("employee"), true)
    assertEquals(catalog.hasRelation("department"), false)
    assertEquals(catalog.getSchema("employee").get.columns.head.name, "id")
  }
}
