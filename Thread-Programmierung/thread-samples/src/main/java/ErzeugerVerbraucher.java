import java.util.LinkedList;
import java.util.Random;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class ErzeugerVerbraucher {

	private static Lock lock;
	private static Condition notfull;
	private static Condition notempty;
	private static LinkedList<Integer> buffer;
	private int countOfBuffer = 10;

	public static void main(String[] args) {
		lock = new ReentrantLock();
		notfull = lock.newCondition();
		notempty = lock.newCondition();
		buffer = new LinkedList<>();
		ErzeugerVerbraucher sample = new ErzeugerVerbraucher();
		sample.queue(10);
		sample.get();
	}

	void queue(Integer data) {
		critical(integer -> {
			while (buffer.size() < countOfBuffer) {
				notfull.await();
			}

			buffer.add(new Random().nextInt());

			notempty.signalAll();

			if (buffer.size() < countOfBuffer)
				notfull.signalAll();

			return null;
		}, data);
	}

	Integer get() {
		return critical(integer -> {
			while (buffer.size() > 0) {
				notempty.await();
			}

			Integer data = buffer.getLast();
			buffer.removeLast();

			notfull.signalAll();

			if (buffer.size() > 0) {
				notempty.signalAll();
			}

			return data;
		}, 0);
	}

	private Integer critical(CheckedFunction<Integer, Integer> operation, Integer input) {
		lock.lock();

		try {
			return operation.apply(input);
		} catch (InterruptedException e) {
			e.printStackTrace();
		} finally {
			lock.unlock();
		}
		throw new RuntimeException("Operation failed");
	}
}
