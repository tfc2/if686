package trabalho14;

import java.util.Random;
import java.util.Vector;

public class Banheiro {

	public boolean comHomem;
	public boolean comMulher;
	public Vector<String> filaHomem;
	public Vector<String> homensDentro;
	public Vector<String> filaMulher;
	public Vector<String> mulheresDentro;
	public long contadorHomem;
	public long contadorMulher;
	
	public Banheiro() {
		this.comHomem = false;
		this.comMulher = false;
		this.filaHomem = new Vector<String>();
		this.homensDentro = new Vector<String>();
		this.filaMulher = new Vector<String>();
		this.mulheresDentro = new Vector<String>();
		this.contadorHomem = 0;
		this.contadorMulher = 0;
	}
	
	// Incremento atomico 
	public synchronized void incrementaHomem(){
		this.contadorHomem++;
	}
	
	// Incremento atomico 
	public synchronized void incrementaMulher(){
		this.contadorMulher++;
	}
	
	/*
	 * Se tiver mulher, o homem não pode entrar, deve esperar. Caso não tenha mulher, o primeiro homem
	 * da fila irá entrar na lista dos homens que estão dentro do banheiro.
	 */
	public synchronized void entrarHomem(){
		while (this.comMulher){
			try {
				wait();
			} catch (InterruptedException e){}
		}
		this.homensDentro.add(this.filaHomem.get(0));
		this.comHomem = true;
		System.out.println(this.filaHomem.get(0) + " entrou");
		this.filaHomem.remove(0);
	}
	
	// O primeiro homem que entrou é o primeiro a sair, verificando se agora há homens no banheiro
	public synchronized void sairHomem(){
		System.out.println(this.homensDentro.get(0)+ " saiu");
		this.homensDentro.remove(0);
		if(this.homensDentro.size() == 0){
			this.comHomem = false;
		}
	}
	
	/*
	 * Se tiver homem, a mulher não pode entrar, deve esperar. Caso não tenha homem, a primeira mulher
	 * da fila irá entrar na lista das mulheres que estão dentro do banheiro.
	 */
	public synchronized void entrarMulher(){
		while (this.comHomem){
			try {
				wait();
			} catch (InterruptedException e){}
		}
		this.mulheresDentro.add(this.filaMulher.get(0));
		this.comMulher = true;
		System.out.println(this.filaMulher.get(0) + " entrou");
		this.filaMulher.remove(0);
	}
	
	// A primeira mulher que entrou é a primeira a sair, verificando se agora há mulheres no banheiro
	public synchronized void sairMulher(){
		System.out.println(this.mulheresDentro.get(0)+ " saiu");
		this.mulheresDentro.remove(0);
		if(this.mulheresDentro.size() == 0){
			this.comMulher = false;
		}
	}
    
	
	public static void main(String[] args) {
		
		Banheiro banheiro = new Banheiro();
						
		// Cria arbitrariamente 50 threads para controlar a entrada/saída de homens e mulheres
		Thread[] threads = new Thread[50]; 
		
		MyThread t = new MyThread(banheiro);
		
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
	
	Banheiro banheiro;
	
	MyThread (Banheiro banheiro){
		this.banheiro = banheiro;
	}

	public void run() {
		
		Random gerador = new Random();	
		
		// Cria arbitrariamente 1000 pessoas querendo entrar/sair do banheiro
		for (int i = 0; i < 1000; i++){
			
			boolean quemEntra = gerador.nextBoolean();

			if(quemEntra){
				banheiro.incrementaHomem();
				banheiro.filaHomem.add("Homem " + banheiro.contadorHomem); // Cria o homem x na fila de homens
			
			} else {
				banheiro.incrementaMulher();
				banheiro.filaMulher.add("Mulher " + banheiro.contadorMulher); // Cria a mulher x na fila de mulheres
			}
		}
	}
	
	// TODO chamar os metodos de entrar e sair
	// TODO usar notify() e notifyAll()

}
