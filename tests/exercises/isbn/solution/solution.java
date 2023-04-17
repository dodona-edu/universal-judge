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

    public static boolean isIsbn(Object isbn, boolean isbn13) {
        if (!(isbn instanceof String)) {
            return false;
        }
        if (isbn13) {
            return isIsbn13((String) isbn);
        } else {
            return isIsbn10((String) isbn);
        }
    }

    public static boolean isIsbn(Object isbn) {
        return isIsbn(isbn, true);
    }

    public static boolean isIsbnCheck(Object isbn, Boolean isbn13) {
        boolean isIsbn13 = true;
        if (isbn instanceof String) {
            isIsbn13 = Objects.requireNonNullElse(isbn13, ((String) isbn).length() == 13);
        }
        return isIsbn(isbn, isIsbn13);
    }

    public static Object areIsbn(List<?> codes, Boolean isbn13) {
        var stream = codes.stream()
                .map(isbn -> isIsbnCheck(isbn, isbn13));
        return stream.collect(Collectors.toList());
    }

    public static Object areIsbn(List<?> code) {
        return areIsbn(code, null);
    }

    public static void main(String[] args) {
        // Some checks from the python doctests.
        assert isIsbn10("9971502100");
        assert !isIsbn("9971502108");
        assert isIsbn13("9789743159664");
        assert !isIsbn13("9787954527409");
        assert !isIsbn13("8799743159665");
        assert !isIsbn("9789027439642", false);
        assert isIsbn("9789027439642", true);
        assert isIsbn("9789027439642");
        assert !isIsbn("080442957X");
        assert isIsbn("080442957X", false);
        var codes = List.<Object>of("0012345678", "0012345679", "9971502100", "080442957X", 5, true,
                "The Practice of Computing Using Python", "9789027439642", "5486948320146");
        assert areIsbn(codes).equals(List.of(false, true, true, true, false, false, false, true, false));
        assert areIsbn(codes, true).equals(List.of(false, false, false, false, false, false, false, true, false));
        assert areIsbn(codes, false).equals(List.of(false, true, true, true, false, false, false, false, false));
    }
}