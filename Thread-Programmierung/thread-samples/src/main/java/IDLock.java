import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

public class IDLock extends AtomicInteger implements Lock {

	public static void main(String[] args) {
		IDLock lockingInt = new IDLock(0);
		Thread threadOne = new Thread(
				() -> {
					lockingInt.lock();
					lockingInt.getAndDecrement();
					System.out.println("one:" + lockingInt);
					lockingInt.unlock();
				});
		Thread threadTwo = new Thread(
				() -> {
					lockingInt.getAndDecrement();
					System.out.println("two:" + lockingInt);
				});
		Thread threadThree = new Thread(
				() -> {
					lockingInt.lock();
					lockingInt.getAndIncrement();
					lockingInt.getAndIncrement();
					lockingInt.getAndIncrement();
					lockingInt.unlock();
					System.out.println("three:" + lockingInt);
				});
		threadOne.run();
		threadTwo.run();
		threadThree.run();
	}

	public IDLock(int i) {
		super(i);
	}

	public IDLock() {
		super(0);
	}

	public void lock() {
		//b==0 -> wir sind die ersten
		int b = this.getAndIncrement();
		while (b > 0) {
			//werden unter gegenseitigen Ausschluss aufgerufen
			//-> funktioniert bei Aufruf eines anderen nicht bei einem selbst
			getAndDecrement();
			b = this.getAndIncrement();
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
		this.getAndDecrement();
	}

	@Override
	public Condition newCondition() {
		return null;
	}
}
