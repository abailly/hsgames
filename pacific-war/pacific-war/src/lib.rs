use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    pub fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet(name: &str, a: u32, b: u32) {
    alert(&format!(
        "Hello, {}: The sum of {} and {} is {}!",
        name,
        a,
        b,
        add(a, b)
    ));
}

// // Called by our JS entry point to run the example
// #[wasm_bindgen(start)]
// fn run() {
//     // Use `web_sys`'s global `window` function to get a handle on the global
//     // window object.
//     let window = web_sys::window().expect("no global `window` exists");
//     let document = window.document().expect("should have a document on window");
//     let body = document.body().expect("document should have a body");

//     // Manufacture the element we're gonna append
//     let val = document.create_element("p")?;
//     val.set_text_content(Some(&format!(
//         "Hello from Rust! {} + {} = {}",
//         2,
//         2,
//         add(2, 2)
//     )));

//     body.append_child(&val)?;
// }

pub fn add(left: u32, right: u32) -> u32 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
