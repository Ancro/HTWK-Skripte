import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Umgang mit Sperren
 *
 * Behauptung: anz(n,n) >= 2^n - 1 für n >= 1
 */
public class Listen3 {

    public static void main(String[] args) {
        Lock lock = new ReentrantLock();
        lock.lock();
        try {
            //critical path
        } catch (Exception e){

        }finally {
            lock.unlock(); // very important!
        }
    }
}
