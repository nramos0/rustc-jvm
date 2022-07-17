// compiler knows about this function, compiler will convert to java println
fn print_arr(arr: [usize; 20]) {
    // println!("{:?}", arr);
}

fn main() {
    let mut arr: [usize; 20] = [
        32, 532, 1, 3, 23, 1, 6, 7, 2, 12, 4, 2, 1, 5, 10, 4343, 6, 23, 999999999, 53,
    ];
    print_arr(arr);

    for i in 1..20 {
        let val_to_insert: usize = arr[i];
        let mut j: usize = i;
        while j > 0 && arr[j - 1] > val_to_insert {
            arr[j] = arr[j - 1];
            j = j - 1;
        }
        arr[j] = val_to_insert;
    }

    print_arr(arr);
}
