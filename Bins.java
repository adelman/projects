/**
 * Bins And Balls Problem.
 *
 * COMP360
 * Matt Adelman
 */

import java.util.Random;
import java.lang.Math;

/**
 * The Bins Class.
 * This class constructs instances of the bins and balls problem. It then does
 * some tests to find the average and max move distance for diffent numbers of
 * bins and balls.
 */
public class Bins {

    // Main function.
	public static void main(String[] args) {
        // Invoke our primary testing fascility.
		test();
	}

    /**
     * Creates an array of "Bins" filled with "Balls".
     *
     * @param n the number of "bins."
     * @param m the number of "balls."
     * @return an array of bins filled with balls.
     */
	public static int[] create(int n, int m) {

		int[] bins = new int[n];
		Random rand = new Random();
        // Place m balls randomly in the array.
		for(int i = m; i > 0; i--) {
			bins[rand.nextInt(n)]++;
		}
		return bins;
	}

    /**
     * Moves the "balls" in the array so there is at least one in each bin
     * by the time the function finishes. This program moves the balls
     * according to the specification from the assignment.
     * @param bins an array filled with balls.
     * @return an array of move distances where dist[i] = the distance we had
     * to look in order to find a ball to move into bins[i].
     * */
     public static int[] move(int[] bins) {
		int[] dist = new int[bins.length];
        // Moving along from left to right.
		for(int i = 0; i < bins.length; i++) {
			if (bins[i] == 0) {
				int j = i;
				int k = i;
                // As soon as we place a ball, we can move on.
				while (bins[i] == 0) {
                    // This is all of our edge cases
					if (j == 0 || k == bins.length - 1) {
						if (j == 0 && k < bins.length - 1) {
							k++;
						}
						else if (j > 0 && k == bins.length - 1) {
							j--;
						}
						else {
							continue;
						}
					}
					else {
						j--;
						k++;
					}
                    // Take from the left first.
					if (bins[j] > 1 && bins[k] > 1) {
						bins[j]--;
						bins[i]++;
						dist[i] = i - j;
					}
                    // The rest of the move clauses.
					else if (bins[j] > 1) {
						bins[j]--;
						bins[i]++;
						dist[i] = i - j;
					}
					else if (bins[k] > 1) {
						bins[k]--;
						bins[i]++;
						dist[i] = k - i;
					}
					else {
						continue;
					}
				}
			}
            // If our current bin has a ball, the move distance is 0.
			else {
				dist[i] = 0;
			}
		}
		return dist;
	}

    /**
     * Our testing facility.
     * This makes arrays from 2^10, to 2^20 by powers of 2. The ball sizes go
     * from m = n to m = 8n. The reason we go to 8n, is because for all of our
     * array sizes, 8n is greater than nlg(n). We also test each n, m
     * combination "trials" number of times. 5000 seemed to give us 3 digits
     * of accuracy.
     */
	public static void test() {

        // Number of trials for each n, m combination.
		int trials = 5000;
        // Data Array
		double[][] data = new double[44][4];
        // Counter for the data array.
		int count = 0;
        // 2^10 to 2^20 by powers of 2.
		for (double n = Math.pow(2,10); n < Math.pow(2,21); n = n * 2) {
            // Note that for all of our tests, n lg(n) < 8n
			for (double m = n; m < 9 * n; m = m * 2) {
                // Average move distance.
				double averageDist = 0.0;
                // Average max move distance.
				double averageMax = 0.0;
                // Find averages over trials amount of times.
				for (int k = 0; k < trials; k++) {
					int[] array = create((int) n, (int) m);
					int[] dist = move(array);
					averageMax += maxVal(dist);
					averageDist += average(dist);
				}
                // Filling our data array.
				data[count][0] = n;
				data[count][1] = m;
				data[count][2] = averageDist / trials;
				data[count][3] = averageMax / trials;
				count++;
			}
		}
        // Print the data array to obtain it in a csv file.
		System.out.println(toString(data));
	}

    // Finds the average value of an array.
	private static double average(int[] array) {
		int sum = 0;
		for (int i = 0; i < array.length; i++) {
			sum += array[i];
		}
		return sum / (double) array.length;
	}

    // Finds the maximum value of an array.
	private static int maxVal(int[] array) {
		int max = -1;
		for (int i = 0; i < array.length; i++) {
			if (array[i] > max) {
				max = array[i];
			}
		}
		return max;
	}

    // Simple toString for a two dimensional array.
    private static String toString(double[][] array) {

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

}
