# Regex to NFA Converter

This OCaml project converts **Regular Expressions (Regex)** into **Nondeterministic Finite Automata (NFA)**. It is useful for students and researchers working with **formal languages** and **automata theory**.

## ✨ Features
- Converts **Regular Expressions** into **NFA** using **Thompson’s Construction**.
- Supports **concatenation**, **union (`|`)**, and **Kleene star (`*`)**.
- Outputs the **NFA structure** in a formatted representation.
- Handles **epsilon (ε) transitions**.

## 🚀 Installation & Usage
To run the program, use:
```bash
cat test/test01.in | dune exec -- ./re.exe
