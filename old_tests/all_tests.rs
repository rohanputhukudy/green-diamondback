mod infra;

// Your tests go here!
success_tests! {
    {
        name: make_vec_succ,
        file: "make_vec.snek",
        input: "5",
        expected: "[0, 0, 0, 0, 0]",
    },
    {
        name: vec_succ,
        file: "vec.snek",
        expected: "[0, 1, 2, 3]",
    },
    {
        name: vec_get_succ,
        file: "vec_get.snek",
        input: "3",
        expected: "3",
    },
    {
        name: linked_list_manipulations,
        file: "linked_list_manipulations.snek",
        expected: "1\n2\n3\n4\n5\n5\n4\n3\n2\n1\nnil"
    },
    {
        name: cleanup_nested,
        file: "cleanup_nested.snek",
        input: "1000",
        heap_size: 4008,
        expected: "1000",
    },
    {
        name: gc_odd,
        file: "gc_odd.snek",
        input: "10000",
        heap_size: 40002,
        expected: "10001"
    },
    {
        name: gc_odd2,
        file: "gc_odd2.snek",
        input: "128",
        expected: "89"
    },
    {
        name: merge_sort,
        file: "merge_sort.snek",
        input: "1000",
        expected: "89"
    },
    {
        name: insertion_sort,
        file: "insertion_sort.snek",
        input: "1000",
        heap_size: 1002,
        expected: "true",
    },
}

runtime_error_tests! {
    {
        name: make_vec_oom,
        file: "make_vec.snek",
        input: "5",
        heap_size: 5,
        expected: "out of memory",
    },
    {
        name: vec_get_oob,
        file: "vec_get.snek",
        input: "5",
        expected: "",
    },
    {
        name: insertion_sort_oom,
        file: "insertion_sort.snek",
        input: "1000",
        heap_size: 1001,
        expected: "out of memory",
    }
}

static_error_tests! {}
