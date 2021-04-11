class Person {
    name: str

    init(name: str) {
        this.name = name
    }
}

func main() {
    const yunho = new Person("Helloyunho")
    println("Hello,", yunho.name)
}
