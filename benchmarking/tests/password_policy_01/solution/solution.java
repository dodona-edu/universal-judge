public class Submission{
    public static boolean isValidPassword(String wachtw,String key){
        String[] keys=key.split(" ");
        char letter=(keys[1]).charAt(0);
        int upper=Integer.parseInt(((keys[0]).split("-"))[1]);
        int lower=Integer.parseInt(((keys[0]).split("-"))[0]);
        int cnt=0;
        for (int i=0;i<wachtw.length();i++){
            if (letter==wachtw.charAt(i)){
                cnt++;
            }
        }
        if (cnt<=upper && cnt>=lower){
            return true;
        }
        return false;
    }
}
