fun isValidPassword(password : String, requirement : String): Boolean{
    var character = requirement.split(" ")[1][0];
    var amount = requirement.split(" ")[0].split("-");
    var occurrences = 0;
    for(i in 0..password.length - 1){
        if(character == password[i]) occurrences ++;
    }
    return occurrences >= amount[0].toInt() && occurrences <= amount[1].toInt();
}
