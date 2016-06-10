import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * @author <a href="mailto:mattthias.zober@outlook.de">Matthias Zober</a>
 *         On 08.04.16 - 16:14
 */
public class Listen2 {

	static int z = 0;

	public static void main(String[] args) {
		IntStream.range(0, 10000).boxed().map(
				(i) -> new Thread(
						() -> {
							inc();
							//inc mit synchronized -> sollte 10000 ergeben
							//inc ohne synchonized -> sollte eigentlich keine 10000 erreichen
							//boxed Streams sichern dieses Verhalten aber ab
							System.out.println(z);
						}))
				.forEach(Util.threadStartAndJoin());
	}

	private static void inc() {
		z++;
	}
}
