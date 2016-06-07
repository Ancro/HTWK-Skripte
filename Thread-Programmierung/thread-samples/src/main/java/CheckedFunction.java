@FunctionalInterface
interface CheckedFunction<T, R> {
    R apply(T t) throws InterruptedException;
}