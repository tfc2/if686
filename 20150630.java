package trab13;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Fichas {

	public int[][] repositorios; //10 arrays de fichas
	public Lock[] locks;
	int contador; // RECURSO COMPARTILHADO

	public Fichas() {
		repositorios = new int[10][10000];
		contador = 0;
		locks = new ReentrantLock[10];
	}

	public int obterFicha(int[][] rep, int[] index) {
		boolean trava = false;
		for(int i = 0; (i < 10) && !trava; i++) {
			trava = locks[i].tryLock();
			if(trava) {
				//pegar a chave
				//como fazer prioridade?
			} else if (i == 9) {
				i = 0;
			}
		}
		return 0;
	}


	public static void main(String[] args) {

		Fichas f = new Fichas();

		//Preencher os arrays de fichas:
		//Primeira linha = 0, 10, 20...
		//Segunda linha = 1, 11, 21... Até 99999

		int ficha = 0;
		for (int coluna = 0; coluna < 10000; coluna++) {
			for (int linha = 0; linha < 10; linha++) {
				f.repositorios[linha][coluna] = ficha;
				ficha++;
			}
		}
	}

}

class MyThread implements Runnable {

	@Override
	public void run() {

	}


}