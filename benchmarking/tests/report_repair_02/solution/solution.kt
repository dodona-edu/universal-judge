fun repair(l: List<Int>): Int {
    for(x in l) for(y in l) for(z in l) if(x+y+z==2020) return x*y*z
    return 0
}
