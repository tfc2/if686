package trabalho13;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Fichas {
	
	public long contador;
	public Lock l;
	
	public Fichas(){
		this.contador = 0;
		l = new ReentrantLock();
	}
	
	synchronized void obterFicha(){
		this.l.lock();
		try{
			contador++;
			System.out.println("Obteve ficha " + contador);
		} finally{
			this.l.unlock();
		}
	}
	
	public static void main(String[] args) {

		Fichas fichas = new Fichas();
		
		Thread[] threads = new Thread[50]; 
		
		MyThread t = new MyThread(fichas);
		
		for (int i = 0; i < 50; i ++){															
			threads[i] = (new Thread(t));
			threads[i].start();	
		}
		
		for (int i = 0; i < 50; i ++){
			try {
				threads[i].join();	
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
	}
}

class MyThread implements Runnable {

	Fichas fichas;
	
	public MyThread (Fichas fichas){
		this.fichas = fichas;
	}
	
	public void run() {
		for (int i = 0; i < 10; i++){
			try {
				Thread.sleep(10);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			fichas.obterFicha();
		}
	}

}

/*
 * Parte teorica aqui
 * 
 */