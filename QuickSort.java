/**
 * The QuickSort class.
 *
 * Used to test a quicksort function.
 *
 * The idea is that for certain array sizes n, insertion sort is actually better
 * than quick sort. In addition, there is controversy whether it is better to do
 * this insertion sort at the bottom level, or only partially sort each sub
 * array and do the insertion sort at the end. This class aims to address this
 * solution.
 */

import java.util.Random;
import java.lang.Math;
import java.lang.Long;

/**
 * QuickSort class.
 *
 * Contains functions for testing quicksort.
 */
public class QuickSort { 


    /**
     * The main funciton.
     *
     * Creates a new array to be filled with random word length integers to
     * be sorted, and then calls our testing functions, and times them.
     */
	public static void main(String[] args) { 
		int[] try1 = new int[1000000000];
		Random rand = new Random();
		for(int i = 0; i < try1.length; i++) {
			try1[i] = rand.nextInt();
		}
        // System.out.println(toString1(try1));
		Long startTime = new Long(System.currentTimeMillis());
		quickSortIn(try1, 0, try1.length - 1, 20);
		Long endTime = new Long(System.currentTimeMillis());
		System.out.println(endTime.intValue() - startTime.intValue());
        // System.out.println(toString1(try1));


        // Which test to run?
	    // test();
	    // test2();
	    // test3();
	    // test4();
	
	}

    /**
     * partition.
     *
     * The standard two-pointer partition from Sedgewick.
     */
	public static int partition(int[] A, int p, int r) {
		int pivot = A[p];
		int i = p;
		int j = r;
		int temp = 0;
		while(j > i) {
			while(A[i] <= pivot && j > i) { i++; }
			while(A[j] > pivot && j >= i) { j--; }
			if (j > i) {
				temp = A[i];
				A[i] = A[j];
				A[j] = temp;
			}
		}
		temp = A[p];
		A[p] = A[j];
		A[j] = temp;
		return j;
	
	}

    /**
     * quickSort.
     *
     * The traditional quicksort algorithm.
     */
	public static void quickSort(int[] array, int left, int right) {

		if (left < right) {
			int pivotNewIndex = partition(array, left, right);
			quickSort(array, left, pivotNewIndex - 1);
			quickSort(array, pivotNewIndex + 1, right);
		}

	}

    /**
     * quickSortIn.
     *
     * A version of quicksort that once we get below the n value, we use
     * insertion sort.
     */
	public static void quickSortIn(int[] array, int left, int right, int n) {

		if (right - left < n) {
			insertionSort(array, left, right);
		}
		else{
			if (left < right) {
				int pivotNewIndex = partition(array, left, right);
				quickSortIn(array, left, pivotNewIndex - 1, n);
				quickSortIn(array, pivotNewIndex + 1, right, n);
			}
		}

	}

    /**
     * quickSortOut.
     *
     * A version of quicksort that once we get below the n value, we don't do
     * any sorting, but then sort everything at the end.
     */
	public static void quickSortOut(int[] array, int left, int right, int n) {

		if (right - left >= n) {
			if (left < right) {
				int pivotNewIndex = partition(array, left, right);
				quickSortOut(array, left, pivotNewIndex - 1, n);
				quickSortOut(array, pivotNewIndex + 1, right, n);
			}
		}
		insertionSort(array, 0, array.length - 1);

	}

    /**
     * insertionSort.
     *
     * Simple insert sort funciton.
     */
	public static void insertionSort(int[] arr, int left, int right) {
		  int i, j, newValue;
		  for (i = left; i <= right; i++) {
				newValue = arr[i];
				j = i;
				while (j > 0 && arr[j - 1] > newValue) {
					  arr[j] = arr[j - 1];
					  j--;
				}
				arr[j] = newValue;
		  }
	}

    //toString funcitons
	public static String toString1(int[] array) {

		String retString = "";

		for (int i = 0; i < array.length; i++) { 
			retString = retString + array[i]+" ";
		}

		return retString;

	}



	public static String toString(double[][] array) {

		String retString = "";
		int width = array[0].length;

		for (int i = 0; i < array.length; i++) { 
			for (int j = 0; j < width; j++) {

				retString = retString + (array[i][j] + ",\t\t");
			}
			retString = retString + "\n";
		}

		return retString;

	}


    /**
     * A suite of testing functions.
     *
     * The main purpose of these functions was to test correctness of the code
     * but also to find the optimal n values. After we found the optimal n
     * values, we then had more tests to find which method was better.
     */
	public static void test() {
		double[][] data = new double[90][4];
		int trials = 10;
		int count = 0;
		Random rand = new Random();

		for(int n = 10000000; n < 100000000; n = n + 1000000) {
			int sum = 0;
			for(int k = 0; k < trials; k++) {
				int[] array = new int[n];
				for(int j = 0; j < array.length; j++) {
					array[j] = rand.nextInt();
				}
				Long startTime = new Long(System.currentTimeMillis());
				quickSort(array, 0, array.length - 1);
				Long endTime = new Long(System.currentTimeMillis());
				sum = sum + (endTime.intValue() - startTime.intValue());
			}
		
			double average = sum / ((double) trials);
			data[count][0] = count;
			data[count][1] = n;
			data[count][2] = average;
			data[count][3] = average / (n * (Math.log(n)/Math.log(2)));
			count++;
		}
		System.out.println(toString(data));
	}

	public static void test2() {
		double[][] data = new double[50][4];
		int trials = 1000000;
		int count = 0;
		Random rand = new Random();

		for(int n = 90; n < 140; n = n + 1) {
			int sum = 0;
			int sum2 = 0;
			for(int k = 0; k < trials; k++) {
				int[] array = new int[n];
				int[] array2 = new int[n];
				for(int j = 0; j < array.length; j++) {
					int temp = rand.nextInt();
					array[j] = temp;
					array2[j] = temp;
				}
				Long startTime = new Long(System.currentTimeMillis());
				quickSort(array, 0, array.length - 1);
				Long endTime = new Long(System.currentTimeMillis());
				sum = sum + (endTime.intValue() - startTime.intValue());

				Long startTime2 = new Long(System.currentTimeMillis());
				insertionSort(array2, 0, array.length);
				Long endTime2 = new Long(System.currentTimeMillis());
				sum2 = sum2 + (endTime2.intValue() - startTime2.intValue());

			}
		
			double average = sum / ((double) trials);
			double average2 = sum2 / ((double) trials);
			data[count][0] = count;
			data[count][1] = n;
			data[count][2] = average;
			data[count][3] = average2;
			count++;
		}
		System.out.println(toString(data));
	}

	public static void test3() {
		double[][] data = new double[60][7];
		int trials = 1000000;
		int count = 0;
		Random rand = new Random();

		// Now the size of when we call insert sort
		for(int n = 40; n < 100; n++) {
			int sum = 0;
			int sum2 = 0;
			int sum3 = 0;
			for(int k = 0; k < trials; k++) {
				int[] array = new int[10000];
				int[] array2 = new int[10000];
				int[] array3 = new int[10000];
				for(int j = 0; j < array.length; j++) {
					int temp = rand.nextInt();
					array[j] = temp;
					array2[j] = temp;
					array3[j] = temp;
				}
				Long startTime = new Long(System.currentTimeMillis());
				quickSort(array, 0, array.length - 1);
				Long endTime = new Long(System.currentTimeMillis());
				sum = sum + (endTime.intValue() - startTime.intValue());

				Long startTime2 = new Long(System.currentTimeMillis());
				quickSortIn(array2, 0, array2.length - 1, n);
				Long endTime2 = new Long(System.currentTimeMillis());
				sum2 = sum2 + (endTime2.intValue() - startTime2.intValue());

				Long startTime3 = new Long(System.currentTimeMillis());
				quickSortIn(array3, 0, array.length - 1, n);
				Long endTime3 = new Long(System.currentTimeMillis());
				sum3 = sum3 + (endTime3.intValue() - startTime3.intValue());
			}
		
			double average = sum / ((double) trials);
			double average2 = sum2 / ((double) trials);
			double average3 = sum3 / ((double) trials);

			data[count][0] = count;
			data[count][1] = n;
			data[count][2] = average;
			data[count][3] = average2;
			data[count][4] = average3;
			data[count][5] = average - average2;
			data[count][6] = average - average3;
			count++;
		}
		System.out.println(toString(data));
	}


	public static void test4() {
		double[][] data = new double[1][5];
		int trials = 1;
		int count = 0;
		Random rand = new Random();

		// Now the size of when we call insert sort
		for(int n = 1000000000; n < 1000000001; n++) {
			int sum = 0;
			int sum2 = 0;
			int sum3 = 0;
			for(int k = 0; k < trials; k++) {
				int[] array = new int[n];
				int[] array2 = new int[n];
				int[] array3 = new int[n];
				for(int j = 0; j < array.length; j++) {
					int temp = rand.nextInt();
					array[j] = temp;
					array2[j] = temp;
					array3[j] = temp;
				}
				Long startTime = new Long(System.currentTimeMillis());
				quickSort(array, 0, array.length - 1);
				Long endTime = new Long(System.currentTimeMillis());
				sum = sum + (endTime.intValue() - startTime.intValue());

				Long startTime2 = new Long(System.currentTimeMillis());
				quickSortIn(array2, 0, array2.length - 1, 60);
				Long endTime2 = new Long(System.currentTimeMillis());
				sum2 = sum2 + (endTime2.intValue() - startTime2.intValue());

				Long startTime3 = new Long(System.currentTimeMillis());
				quickSortIn(array3, 0, array.length - 1, 59);
				Long endTime3 = new Long(System.currentTimeMillis());
				sum3 = sum3 + (endTime3.intValue() - startTime3.intValue());
			}
		
			double average = sum / ((double) trials);
			double average2 = sum2 / ((double) trials);
			double average3 = sum3 / ((double) trials);

			data[count][0] = count;
			data[count][1] = n;
			data[count][2] = average;
			data[count][3] = average2;
			data[count][4] = average3;
			count++;
		}
		System.out.println(toString(data));
	}
		


}
