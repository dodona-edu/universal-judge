import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.Random;

class Submission {
    /**
     * Checks whether the given ISBN-10 code is valid.
     */
    public static boolean isIsbn10(String isbn) {
        if (isbn == null || isbn.length() != 10) {
            return false;
        }
        int checksum = 0;
        for (int i = 1; i < 10; i++) {
            var el = isbn.charAt(i - 1);
            var asNumber = Character.getNumericValue(el);
            if (asNumber < 0) {
                return false;
            }
            checksum += i * asNumber;
        }
        checksum = checksum % 11;
        int checkDigit;
        if (isbn.charAt(9) == 'X') {
            checkDigit = 10;
        } else {
            checkDigit = Character.getNumericValue(isbn.charAt(9));
        }
        return checksum == checkDigit;
    }

    /**
     * Checks whether the given ISBN-13 code is valid.
     */
    public static boolean isIsbn13(String isbn) {
        if (isbn == null || isbn.length() != 13) {
            return false;
        }
        int o = 0;
        int e = 0;
        for (int i = 1; i < 13; i++) {
            var el = isbn.charAt(i - 1);
            var asNumber = Character.getNumericValue(el);
            if (asNumber < 0) {
                return false;
            }
            if (i % 2 == 0) {
                e += asNumber;
            } else {
                o += asNumber;
            }
        }
        int checkSum = (10 - (o + 3 * e) % 10) % 10;
        int checkDigit = Character.getNumericValue(isbn.charAt(12));
        return checkSum == checkDigit;
    }

    public static boolean isIsbn(String isbn, boolean isbn13) {
        if (isbn13) {
            return isIsbn13(isbn);
        } else {
            return isIsbn10(isbn);
        }
    }

    public static Object areIsbn(List<String> codes, boolean isbn13) {
        var stream = codes.stream()
                .map(isbn -> isIsbn(isbn, isbn13));
        return stream.collect(Collectors.toList());
    }
}
