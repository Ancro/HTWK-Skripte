import java.util.function.Consumer;

/**
 * @author <a href="mailto:mattthias.zober@outlook.de">Matthias Zober</a>
 *         On 08.04.16 - 16:17
 */
public class Util {
    static Consumer<Thread> threadStartAndJoin() {
        return thread -> {
            thread.start();
            try {
                thread.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        };
    }
}
