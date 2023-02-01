using System;
using System.Linq;
using System.Collections.Generic;
using System.Diagnostics;

public class Submission
{
    public static bool IsIsbn10(string isbn) {
        if (isbn.Length != 10) {
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
        if (isbn.Length != 13) {
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

    public static bool IsIsbn(string isbn, bool isbn13) {
        if (isbn13) {
            return IsIsbn13(isbn);
        } else {
            return IsIsbn10(isbn);
        }
    }

    public static List<bool> AreIsbn(IEnumerable<string> codes, bool isbn13) {
        return codes.Select(isbn => IsIsbn(isbn, isbn13)).ToList();
    }
}
