import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * @author <a href="mailto:mattthias.zober@outlook.de">Matthias Zober</a>
 *         On 08.04.16 - 16:14
 */
public class Counter {

	static int z = 0;
	static IDLock idLock = new IDLock();
	static TTASLock ttasLock = new TTASLock(false);
	static TASLock tasLock = new TASLock(false);

	public static void main(String[] args) {
		testLockWithCounter(tasLock);
		testLockWithCounter(ttasLock);
		testLockWithCounter(idLock);
	}

	private static void testLockWithCounter(Lock lock) {
		z = 0;
		long begin = System.currentTimeMillis();
		List<Thread> allThreads = IntStream.range(0, 10000).boxed().map(
				(i) -> new Thread(() -> {
					inc(lock);
				})).collect(Collectors.toList());
		allThreads.forEach(Thread::start);
		allThreads.forEach((thread) -> {
			try {
				thread.join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		});
		long end = System.currentTimeMillis();
		long diff = end - begin;
		System.out.println("counter:" + z);
		System.out.println("time:" + diff + " ms");
	}

	private static void inc(Lock lock) {
		lock.lock();
		try {
			z++;
		} finally {
			lock.unlock();
		}
	}
}
