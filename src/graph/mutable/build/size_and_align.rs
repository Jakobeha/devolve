use crate::rust_type::StructuralRustType;

pub fn calculate_size(elems: &Vec<StructuralRustType>) -> Option<usize> {
    let mut cumulative_size = 0;
    for elem in elems {
        let size = elem.size?;
        let align = elem.align?;
        if cumulative_size % align != 0 {
            cumulative_size += align - (cumulative_size % align);
        }
        cumulative_size += size;
    }
    Some(cumulative_size)
}

pub fn calculate_align(elems: &Vec<StructuralRustType>) -> Option<usize> {
    let mut max_align = 0;
    for elem in elems {
        let align = elem.align?;
        if max_align < align {
            max_align = align;
        }
    }
    Some(max_align)
}

pub fn calculate_array_size(elem_type: &StructuralRustType, length: usize) -> Option<usize> {
    let size = elem_type.size?;
    let align = elem_type.align?;
    let aligned_size = if size % align != 0 {
        size + align - (size % align)
    } else {
        size
    };
    Some(aligned_size * length)
}