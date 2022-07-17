public class HelloWorldLong {
    public static void main(String args[]) {
        long[] arr = {10, 20}; // var 1
        long j = 1; // var 2
        long val_to_insert = 2; // var 4

        boolean a = j > 5 && j == val_to_insert && arr[0] == j;
        boolean b = j != val_to_insert;
        boolean c = arr[0] > val_to_insert;
        if (a && b && c) {
            System.out.println("j > 0 && arr[j - 1] > val_to_insert");
        }
    }
}