use std::{collections::HashMap, collections::HashSet, env};

type SnekVal = u64;

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(i64)]
pub enum ErrCode {
    InvalidArgument = 1,
    Overflow = 2,
    IndexOutOfBounds = 3,
    InvalidVecSize = 4,
    OutOfMemory = 5,
}

const TRUE: u64 = 7;
const FALSE: u64 = 3;

static mut HEAP_START: *const u64 = std::ptr::null();
static mut HEAP_END: *const u64 = std::ptr::null();

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64, heap_start: *const u64, heap_end: *const u64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    if errcode == ErrCode::InvalidArgument as i64 {
        eprintln!("invalid argument");
    } else if errcode == ErrCode::Overflow as i64 {
        eprintln!("overflow");
    } else if errcode == ErrCode::IndexOutOfBounds as i64 {
        eprintln!("index out of bounds");
    } else if errcode == ErrCode::InvalidVecSize as i64 {
        eprintln!("vector size must be non-negative");
    } else {
        eprintln!("an error ocurred {}", errcode);
    }
    std::process::exit(errcode as i32);
}

#[export_name = "\x01snek_print"]
pub unsafe extern "C" fn snek_print(val: SnekVal) -> SnekVal {
    println!("{}", snek_str(val, &mut HashSet::new()));
    val
}

/// This function is called when the program needs to allocate `count` words of memory and there's no
/// space left. The function should try to clean up space by triggering a garbage collection. If there's
/// not enough space to hold `count` words after running the garbage collector, the program should terminate
/// with an `out of memory` error.
///
/// Args:
///     * `count`: The number of words the program is trying to allocate, including an extra word for
///       the size of the vector and an extra word to store metadata for the garbage collector, e.g.,
///       to allocate a vector of size 5, `count` will be 7.
///     * `heap_ptr`: The current position of the heap pointer (i.e., the value stored in `%r15`). It
///       is guaranteed that `heap_ptr + 8 * count > HEAP_END`, i.e., this function is only called if
///       there's not enough space to allocate `count` words.
///     * `stack_base`: A pointer to the "base" of the stack.
///     * `curr_rbp`: The value of `%rbp` in the stack frame that triggered the allocation.
///     * `curr_rsp`: The value of `%rsp` in the stack frame that triggered the allocation.
///
/// Returns:
///
/// The new heap pointer where the program should allocate the vector (i.e., the new value of `%r15`)
///
#[export_name = "\x01snek_try_gc"]
pub unsafe fn snek_try_gc(
    count: isize,
    heap_ptr: *const u64,
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> *const u64 {
    let new_heap_ptr = snek_gc(heap_ptr, stack_base, curr_rbp, curr_rsp);
    if (HEAP_END as isize - new_heap_ptr as isize)/8 >= count { return new_heap_ptr; }

    eprintln!("out of memory");
    std::process::exit(ErrCode::OutOfMemory as i32)
}

/// Helper function that finds roots of heap trees in the stack
unsafe fn find_roots(
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> Vec<*mut u64> {
    let mut roots: Vec<*mut u64> = Vec::new();
    let mut ptr = stack_base;
    while ptr >= curr_rsp {
        let val = *ptr;
        if val & 1 == 1 && val >= (HEAP_START as u64)+1 && val <= (HEAP_END as u64)+1 {
            // subtracts 1 to remove the heap data tag
            roots.push((val-1) as *mut u64);
        }
        // advance the stack_ptr
        ptr = ptr.sub(1);
    }
    roots
}

/// Helper function that calls recursive mark function
fn mark(roots: &Vec<*mut u64>) {
    for &root in roots {
        mark_heap(root);
    }
}

/// Helper function that recursively marks entries of the heap to preserve
fn mark_heap(mut root: *mut u64) {
    let flag = unsafe { *root };
    // if root is marked, just return
    if flag & 1 == 1 { return; }
    // mark the root
    unsafe { *root = 1 };
    root = unsafe { root.offset(1) };
    let size = unsafe { *root };
    // iterate through the root's vec
    for _ in 0..size {
        root = unsafe { root.offset(1) };
        let val = unsafe { *root };
        // recurse on each entry that is a vec
        if val != TRUE && val != FALSE && val != 1 && val & 1 == 1 {
            mark_heap((val-1) as *mut u64);
        }
    }
}

/// Helper function that sets forwarding headers for marked heap elements and stores this info in a
/// Hashmap of pairs (old_addr, updated_addr)
fn set_fwd_headers(heap_ptr: *const u64) -> HashMap<*mut u64, *mut u64> {
    let mut pairs = HashMap::new();
    let mut from:*mut u64 = unsafe { HEAP_START as *mut u64 };
    let mut to:*mut u64 = unsafe { HEAP_START as *mut u64 };
    let mut size:isize;
    while from < heap_ptr as *mut u64 {
        let flag = unsafe { *from };
        // check if marked
        if flag & 1 == 1 {
            // set header to forwarding address (preserves flag with +1)
            unsafe { *from = (to as u64) + 1; }
            pairs.insert(from, to);

            // prep to shift the `from` pointer
            from = unsafe { from.offset(1) };
            size = unsafe { *from } as isize;

            // shift the `to` pointer
            to = unsafe { to.offset(size+2) };
        } else {
            // prep to shift the `from` ptr for unmarked data
            from = unsafe { from.offset(1) };
            size = unsafe { *from } as isize;
        }
        // always shift
        from = unsafe { from.offset(size+1) };
    }
    pairs
}

/// Helper function that updates references on the heap and on the stack
unsafe fn update_refs(
    roots: Vec<*mut u64>, 
    fwd: &HashMap<*mut u64, *mut u64>,
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) {
    // updates references on the stack
    let mut stack_ptr = stack_base as *mut u64;
    while stack_ptr >= (curr_rsp as *mut u64) {
        let val = *stack_ptr;
        if val & 1 == 1 && val >= (HEAP_START as u64)+1 && val <= (HEAP_END as u64)+1 {
            unsafe { *stack_ptr = (*fwd.get(&((val-1) as *mut u64)).unwrap() as u64)+1; }
        }
        // advance the stack_ptr
        stack_ptr = stack_ptr.sub(1);
    }

    // recursively update references on the heap
    let mut forwarded:HashSet<*mut u64> = HashSet::new();
    for root in roots {
        update_heap_refs(root, fwd, &mut forwarded);
    }
}

/// Helper function that recursively updates references to their forwarded counterparts in the heap
fn update_heap_refs(
    mut root: *mut u64, 
    fwd: &HashMap<*mut u64, *mut u64>, 
    forwarded: &mut HashSet<*mut u64>
) {
    if !forwarded.insert(root) { return; }
    
    root = unsafe { root.offset(1) };
    let size = unsafe { *root };
    // iterate through the root's vec
    for _ in 0..size {
        root = unsafe { root.offset(1) };
        let val = unsafe { *root };
        // update ref and recurse on each entry that is a vec
        if val != TRUE && val != FALSE && val != 1 && val & 1 == 1 {
            // updates the ref
            unsafe { *root = *fwd.get(&((val-1) as *mut u64)).unwrap() as u64 + 1; }
            update_heap_refs((val-1) as *mut u64, fwd, forwarded);
        }
    }
}

/// Helper function that compacts the heap and returns a pointer to the end of the compacted heap
fn compact(
    heap_ptr: *const u64, 
    fwd: HashMap<*mut u64, *mut u64>
) -> *const u64 {
    let mut to:*mut u64 = heap_ptr as *mut u64;
    let mut from:*mut u64 = unsafe { HEAP_START as *mut u64 };
    let mut size:isize;
    while from < heap_ptr as *mut u64 {
        let flag = unsafe { *from };
        // check if marked
        if flag & 1 == 1 {
            // prepares to move the data
            to = *fwd.get(&from).unwrap();

            // resets the header
            unsafe { *from = 0; }

            // moves the two words of header data
            unsafe { *to = *from; }
            from = unsafe{ from.offset(1) };
            to = unsafe{ to.offset(1) };
            unsafe { *to = *from; }
            size = unsafe { *from } as isize;

            // moves the vector data, entry by entry
            for _ in 0..size {
                from = unsafe{ from.offset(1) };
                to = unsafe{ to.offset(1) };
                unsafe { *to = *from; }
            }
            // shifts from pointer to next heap data
            from = unsafe{ from.offset(1) };
            
            // shifts the to pointer in case this is the last move
            to = unsafe { to.offset(1) };
        } else {
            // shift from pointer to next heap data
            from = unsafe{ from.offset(1) };
            size = unsafe { *from } as isize;
            from = unsafe { from.offset(size+1) };
        }
    }
    // new heap pointer should look at space after the last moved object
    to as *const u64
}

/// This function should trigger garbage collection and return the updated heap pointer (i.e., the new
/// value of `%r15`). See [`snek_try_gc`] for a description of the meaning of the arguments.
#[export_name = "\x01snek_gc"]
pub unsafe fn snek_gc(
    heap_ptr: *const u64,
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> *const u64 {
    // 1. search through the stack to determine the root set
    let roots = find_roots(stack_base, curr_rbp, curr_rsp);
    // 2. Mark the heap by tree traversal starting from each root
    // pass ref to roots, so mark just borrows instead of owning
    mark(&roots);
    // 3. Set the headers of the marked heap entries
    let pairs = set_fwd_headers(heap_ptr);
    // 4. Update the references of marked heap entries on both heap and stack
    update_refs(roots, &pairs, stack_base, curr_rbp, curr_rsp);
    // 5. Compact the heap and return a ptr to the end of the compacted heap
    compact(heap_ptr, pairs)
}

/// A helper function that can called with the `(snek-printstack)` snek function. It prints the stack
/// See [`snek_try_gc`] for a description of the meaning of the arguments.
#[export_name = "\x01snek_print_stack"]
pub unsafe fn snek_print_stack(stack_base: *const u64, curr_rbp: *const u64, curr_rsp: *const u64) {
    let mut ptr = stack_base;
    println!("-----------------------------------------");
    while ptr >= curr_rsp {
        let val = *ptr;
            println!("{ptr:?}: {:#0x}", val);
        ptr = ptr.sub(1);
    }
    println!("-----------------------------------------");
}

unsafe fn snek_str(val: SnekVal, seen: &mut HashSet<SnekVal>) -> String {
    if val == TRUE {
        format!("true")
    } else if val == FALSE {
        format!("false")
    } else if val & 1 == 0 {
        format!("{}", (val as i64) >> 1)
    } else if val == 1 {
        format!("nil")
    } else if val & 1 == 1 {
        if !seen.insert(val) {
            return "[...]".to_string();
        }
        let addr = (val - 1) as *const u64;
        let size = addr.add(1).read() as usize;
        let mut res = "[".to_string();
        for i in 0..size {
            let elem = addr.add(2 + i).read();
            res = res + &snek_str(elem, seen);
            if i < size - 1 {
                res = res + ", ";
            }
        }
        seen.remove(&val);
        res + "]"
    } else {
        format!("unknown value: {val}")
    }
}

fn parse_input(input: &str) -> u64 {
    match input {
        "true" => TRUE,
        "false" => FALSE,
        _ => (input.parse::<i64>().unwrap() << 1) as u64,
    }
}

fn parse_heap_size(input: &str) -> usize {
    input.parse::<usize>().unwrap()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() >= 2 { &args[1] } else { "false" };
    let heap_size = if args.len() >= 3 { &args[2] } else { "10000" };
    let input = parse_input(&input);
    let heap_size = parse_heap_size(&heap_size);

    // Initialize heap
    let mut heap: Vec<u64> = Vec::with_capacity(heap_size);
    unsafe {
        HEAP_START = heap.as_mut_ptr();
        HEAP_END = HEAP_START.add(heap_size);
    }

    let i: u64 = unsafe { our_code_starts_here(input, HEAP_START, HEAP_END) };
    unsafe { snek_print(i) };
}
