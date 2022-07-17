fn insertion_sort(arr: &mut [i32]) {
    let len = arr.len();

    for i in 1..len {
        let val_to_insert = arr[i];
        let mut j = i;
        while j > 0 && arr[j - 1] > val_to_insert {
            arr[j] = arr[j - 1];
            j -= 1;
        }
        arr[j] = val_to_insert;
    }
}

fn main() {
    let mut nums = vec![3, 1, 5, 7, -1, 23, 5, 13];
    println!("nums unsorted: {:?}", nums);
    insertion_sort(&mut nums);
    println!("nums sorted: {:?}", nums);
}


// {
//     let arr: i128;
//     let i: i32 = 0;
//     let val_to_insert: i32 = arr[i];
//     let len: usize = 5;
//     let mut j: usize = i;
//     let e: i32 = len + i;
//     while j > 0 && arr[j - 1] > val_to_insert {
//         let q: i32 = 3;
//         let j: i32 = e + 1;
//         let j: i32 = e + 1;
//     }
// }


