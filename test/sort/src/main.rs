trait SelectionSort {
    fn selection_sort(&mut self);
}

impl<T> SelectionSort for [T] where T: std::cmp::PartialOrd {
    fn selection_sort(&mut self) {
        let len = self.len();
        for i in 0..len {
            let mut min = &self[i];
            let mut min_index = i;
            for j in i+1..len {
                if &self[j] < min {
                    min = &self[j];
                    min_index = j;
                }
            }
            self.swap(i, min_index);
        }
    }
}

impl<T> SelectionSort for Vec<T> where T: std::cmp::PartialOrd {
    fn selection_sort(&mut self) {
        (&mut self[..]).selection_sort();
    }
}

trait InsertionSort {
    fn insertion_sort(&mut self);
}

impl<T> InsertionSort for [T] where T: std::cmp::PartialOrd {
    fn insertion_sort(&mut self) {
        let len = self.len();

        // pointer to self[1]
        let mut ptr = unsafe { self.as_mut_ptr().add(1) };
        
        // invariant: ptr points to self[i]
        for i in 1..len {
            let val_to_insert = unsafe {
                ptr.read()
            };
            let mut inner_ptr = ptr;
            let mut j = i;
            // invariant: inner_ptr points to self[j]
            while j > 0 && &val_to_insert < &self[j - 1] 
            {
                inner_ptr = unsafe {
                    let inner_ptr_prev = inner_ptr.sub(1);
                    *inner_ptr = inner_ptr_prev.read();
                    inner_ptr_prev
                };
                j -= 1;
            }
            unsafe {
                *inner_ptr = val_to_insert;
                ptr = ptr.add(1);
            }
        }
    }
}

impl<T> InsertionSort for Vec<T> where T: std::cmp::PartialOrd {
    fn insertion_sort(&mut self) {
        (&mut self[..]).insertion_sort();
    }
}

trait IsSorted {
    fn sorted(&self) -> bool;
}

impl<T> IsSorted for &[T] where T: std::cmp::PartialOrd {
    fn sorted(&self) -> bool {
        let len_m_1 = self.len() - 1;
        let mut i = 0;
        while i < len_m_1 && self[i] <= self[i + 1] {
            i += 1;
        }
        i == len_m_1
    }
}

impl<T> IsSorted for Vec<T> where T: std::cmp::PartialOrd {
    fn sorted(&self) -> bool {
        (&self[..]).sorted()
    }
}

fn main() {
    let mut nums = vec![3, 1, 5, 7, -1, 23, 5, 13];
    println!("nums unsorted: {:?}", nums);
    nums.selection_sort();
    println!("nums sorted: {:?}", nums);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn insertion_test() {
        let mut nums = vec![3, 1, 5, 7, -1, 23, 5, 13];
        nums.insertion_sort();
        assert_eq!(nums.sorted(), true);
    }

    #[test]
    fn selection_test() {
        let mut nums = vec![3, 1, 5, 7, -1, 23, 5, 13];
        nums.selection_sort();
        assert_eq!(nums.sorted(), true);
    }

}
