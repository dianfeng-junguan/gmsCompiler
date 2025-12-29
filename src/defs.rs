/*
    some definitions
*/

static VARTYPE_SIZE:&[(&str, usize)]=&[
    ("int", 8),
    ("float", 8),
    ("double", 8),
    ("char", 1),
    ("void", 0),
];

pub fn get_vartype_size(vartype:&str)->Option<usize>{
    for (vt, size) in VARTYPE_SIZE.iter(){
        if *vt==vartype{
            return Some(*size);
        }
    }
    None
}