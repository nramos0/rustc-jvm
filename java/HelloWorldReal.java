public class HelloWorldReal {
    public static void main(String args[]) {
        int a = 5;
        int b = 0;
        boolean cond = a > 0 && b <= 5;
        System.out.println(cond);
        while (cond) {
            System.out.println("Hello World!");
            a--;
            b++;
            cond = a > 0 && b <= 5;
        }
    }

    public static int ___main() {
        return 3;
    }
}