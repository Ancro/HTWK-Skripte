package de.peporal;

import jdk.nashorn.internal.runtime.regexp.joni.exception.InternalException;

import java.util.LinkedList;
import java.util.Random;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Main
{

	abstract private class BetterRunnable
	{
		public abstract Integer run() throws InterruptedException;
	}

	private static Lock _l;
	private static Condition _notfull;
	private static Condition _notempty;
	private static LinkedList<Integer> _buffer;

	private int _n = 10;

	public static void main(String[] args)
	{
		_l = new ReentrantLock();
		_notfull = _l.newCondition();
		_notempty = _l.newCondition();
		_buffer = new LinkedList<Integer>();
	}

	void queue(Integer data)
	{
		BetterRunnable operation = new BetterRunnable() {
			@Override
			public Integer run() throws InterruptedException {
				while (_buffer.size() < _n)
					_notfull.await();

				_buffer.add(new Random().nextInt());

				_notempty.signalAll();

				if (_buffer.size() < _n)
					_notfull.signalAll();

				return null;
			}
		};

		critical(operation);
	}

	Integer get()
	{
		BetterRunnable operation = new BetterRunnable() {
			@Override
			public Integer run() throws InterruptedException {
				while (_buffer.size() > 0)
					_notempty.await();

				Integer data = _buffer.getLast();
				_buffer.removeLast();

				_notfull.signalAll();

				if (_buffer.size() > 0)
					_notempty.signalAll();

				return data;
			}
		};

		return critical(operation);
	}

	private Integer critical(BetterRunnable operation)
	{
		_l.lock();

		try {
			return operation.run();
		}
		catch (InterruptedException e) {
			e.printStackTrace();
		}
		finally {
			_l.unlock();
		}
	}
}