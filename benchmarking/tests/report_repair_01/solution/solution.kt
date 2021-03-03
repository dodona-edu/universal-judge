fun repair(l: List<Int>): Int {
    for(x in l) for(y in l) if(x+y==2020) return x*y
    return 0
}
