using System;
using System.Linq;
using System.Collections.Generic;
using System.Diagnostics;

public class Submission
{
    public static bool IsIsbn10(string isbn) {
        if (isbn == null || isbn.Length != 10) {
            return false;
        }
        int checksum = 0;
        for (int i = 1; i < 10; i++) {
            var el = isbn[i - 1];
            var asNumber = (int) Char.GetNumericValue(el);
            if (asNumber < 0) {
                return false;
            }
            checksum += i * asNumber;
        }
        checksum = checksum % 11;
        int checkDigit;
        if (isbn[9] == 'X') {
            checkDigit = 10;
        } else {
            checkDigit = (int) Char.GetNumericValue(isbn[9]);
        }
        return checksum == checkDigit;
    }

    /**
     * Checks whether the given ISBN-13 code is valid.
     */
    public static bool IsIsbn13(string isbn) {
        if (isbn == null || isbn.Length != 13) {
            return false;
        }
        int o = 0;
        int e = 0;
        for (int i = 1; i < 13; i++) {
            var el = isbn[i - 1];
            var asNumber = (int) Char.GetNumericValue(el);
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
        int checkDigit = (int) Char.GetNumericValue(isbn[12]);
        return checkSum == checkDigit;
    }

    public static bool IsIsbn(object isbn, bool isbn13 = true) {
        if (!(isbn is string)) {
            return false;
        }
        if (isbn13) {
            return IsIsbn13((string) isbn);
        } else {
            return IsIsbn10((string) isbn);
        }
    }

    public static bool IsIsbnCheck(object isbn, bool? isbn13) {
        bool isIsbn13 = true;
        if (isbn is string) {
          if (isbn13 == null) {
            isIsbn13 = ((string) isbn).Length == 13;
          } else {
            isIsbn13 = (bool) isbn13;
          }
        }
        return IsIsbn(isbn, isIsbn13);
    }

    public static List<bool> AreIsbn(IEnumerable<object> codes, bool? isbn13 = null) {
        return codes.Select(isbn => IsIsbnCheck(isbn, isbn13)).ToList();
    }

    public static void Main(string[] args) {
        // Some checks from the python doctests.
        Debug.Assert(IsIsbn10("9971502100"));
        Debug.Assert(!IsIsbn("9971502108"));
        Debug.Assert(IsIsbn13("9789743159664"));
        Debug.Assert(!IsIsbn13("9787954527409"));
        Debug.Assert(!IsIsbn13("8799743159665"));
        Debug.Assert(!IsIsbn("9789027439642", false));
        Debug.Assert(IsIsbn("9789027439642", true));
        Debug.Assert(IsIsbn("9789027439642"));
        Debug.Assert(!IsIsbn("080442957X"));
        Debug.Assert(IsIsbn("080442957X", false));
        var codes = new List<object>() {"0012345678", "0012345679", "9971502100", "080442957X", 5, true,
                "The Practice of Computing Using Python", "9789027439642", "5486948320146"};
        Debug.Assert(AreIsbn(codes).SequenceEqual(new List<bool>() {false, true, true, true, false, false, false, true, false}));
        Debug.Assert(AreIsbn(codes, true).SequenceEqual(new List<bool>() {false, false, false, false, false, false, false, true, false}));
        Debug.Assert(AreIsbn(codes, false).SequenceEqual(new List<bool>() {false, true, true, true, false, false, false, false, false}));
    }
}
