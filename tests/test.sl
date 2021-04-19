let a := 60
let b := 9
let c := a + b

class Person {
    let name: str
    let age: u8

    init(name: str, age: str) {
        self.name = name
        self.age = age
    }

    func test() {}
}

func main() {
    println(c)

    let arr := [1, 2, 3]

    for el in arr at idx {
        println(el, idx)
    }

    let yunho := Person("Helloyunho", 15)
    yunho.sayHi()
}
