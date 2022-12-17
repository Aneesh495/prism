package prism.engine.parallel

import java.util.concurrent.{ConcurrentLinkedDeque, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable.ArrayBuffer

/**
 * Lightweight work-stealing thread pool coordinator for parallel dataflow stages.
 * Each worker maintains a local deque: it pops tasks from the bottom, while other
 * idle workers steal tasks from the top.
 */
final class WorkStealingScheduler(val numWorkers: Int) {
  require(numWorkers > 0, "numWorkers must be strictly positive")

  private val deques: Array[ConcurrentLinkedDeque[Runnable]] =
    Array.fill(numWorkers)(new ConcurrentLinkedDeque[Runnable]())

  private val running = new AtomicBoolean(true)
  private val threads: Array[Thread] = new Array[Thread](numWorkers)

  for (i <- 0 until numWorkers) {
    val workerId = i
    threads(i) = new Thread(new Runnable {
      def run(): Unit = {
        while (running.get()) {
          val task = pollTask(workerId)
          if (task != null) {
            try {
              task.run()
            } catch {
              case e: Throwable =>
                e.printStackTrace()
            }
          } else {
            try {
              Thread.sleep(1) // Back off when queues are empty
            } catch {
              case _: InterruptedException =>
                // Clean interruption during thread termination
            }
          }
        }
      }
    }, s"prism-worker-$workerId")
    threads(i).setDaemon(true)
    threads(i).start()
  }

  private def pollTask(workerId: Int): Runnable = {
    // 1. Try local deque (bottom pop)
    val local = deques(workerId).pollLast()
    if (local != null) return local

    // 2. Try stealing from other deques (top poll)
    var victim = (workerId + 1) % numWorkers
    var attempts = 0
    while (attempts < numWorkers) {
      val stolen = deques(victim).pollFirst()
      if (stolen != null) return stolen
      victim = (victim + 1) % numWorkers
      attempts += 1
    }
    null
  }

  /**
   * Submits a task to a designated worker's deque.
   */
  def submit(workerId: Int, task: Runnable): Unit = {
    val target = (workerId % numWorkers + numWorkers) % numWorkers
    deques(target).offerLast(task)
  }

  /**
   * Submits a batch of tasks and blocks until all tasks complete execution.
   */
  def executeAll(tasks: Seq[Runnable]): Unit = {
    if (tasks.isEmpty) return
    val latch = new CountDownLatch(tasks.length)
    var idx = 0
    for (t <- tasks) {
      val wrapped = new Runnable {
        def run(): Unit = {
          try { t.run() }
          finally { latch.countDown() }
        }
      }
      submit(idx % numWorkers, wrapped)
      idx += 1
    }
    latch.await()
  }

  /**
   * Shuts down worker threads gracefully.
   */
  def shutdown(): Unit = {
    running.set(false)
    for (t <- threads) {
      t.interrupt()
    }
    for (t <- threads) {
      t.join(500)
    }
  }
}
