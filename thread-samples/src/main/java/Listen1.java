import java.util.stream.IntStream;

/**
 * @author <a href="mailto:mattthias.zober@outlook.de">Matthias Zober</a>
 *         On 08.04.16 - 15:48
 */
public class Listen1 {
    public static void main(String[] args) {
        IntStream.range(0, 8).boxed().map(
                (i) -> new Thread(
                        () -> System.out.println("Hello world from Thread " + i)))
                .forEach(Util.threadStartAndJoin());
        //mit system.out.write würden alle Chars wild durcheinander gewürfelt werden
    }

}
