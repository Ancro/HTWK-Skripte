import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

public class TTASLock extends AtomicBoolean implements Lock {

	public TTASLock(boolean b) {
		super(b);
	}

	public static void main(String[] args) {
		TTASLock locker = new TTASLock(false);
		Thread threadOne = new Thread(
				() -> {
					locker.lock();
					System.out.println("one:" + locker);
					locker.unlock();
				});
		Thread threadTwo = new Thread(
				() -> {
					System.out.println("two:" + locker);
				});
		Thread threadThree = new Thread(
				() -> {
					locker.lock();
					locker.unlock();
					System.out.println("three:" + locker);
				});
		threadOne.run();
		threadTwo.run();
		threadThree.run();
	}

	@Override
	public void lock() {
		while (get() || getAndSet(true)) {
		}
	}

	@Override
	public void lockInterruptibly() throws InterruptedException {

	}

	@Override
	public boolean tryLock() {
		return false;
	}

	@Override
	public boolean tryLock(long l, TimeUnit timeUnit) throws InterruptedException {
		return false;
	}

	@Override
	public void unlock() {
		set(false);
	}

	@Override
	public Condition newCondition() {
		return null;
	}
}
